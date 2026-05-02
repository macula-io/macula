//! TUN device lifecycle + packet I/O.
//!
//! Resource model: `TunResource` is the BEAM-side opaque handle. While the
//! resource is alive, the underlying TUN device exists. When BEAM GC drops
//! the resource, the Drop impl signals the reader thread (if any) and
//! closes the device.
//!
//! Reader thread: `nif_start_reader/2` spawns a Rust thread that does
//! blocking reads on the TUN fd and sends each packet as
//! `{macula_net_packet, Handle, Payload}` to the registered Pid via
//! `OwnedEnv::send_and_clear`. The thread exits when the stop flag is
//! set or when reads start returning errors (device closed).

use rustler::{Binary, Encoder, Env, LocalPid, NewBinary, OwnedEnv, ResourceArc, Term};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;
use tun_rs::{DeviceBuilder, SyncDevice};

mod atoms {
    rustler::atoms! {
        ok,
        error,
        macula_net_packet,
        already_started,
    }
}

/// BEAM-visible opaque handle.
pub struct TunResource {
    inner: Mutex<Option<TunInner>>,
}

struct TunInner {
    device: Arc<SyncDevice>,
    name: String,
    reader: Option<ReaderState>,
}

struct ReaderState {
    stop: Arc<AtomicBool>,
    handle: Option<thread::JoinHandle<()>>,
}

impl Drop for TunResource {
    fn drop(&mut self) {
        if let Ok(mut guard) = self.inner.lock() {
            if let Some(inner) = guard.take() {
                stop_reader(inner);
            }
        }
    }
}

fn stop_reader(mut inner: TunInner) {
    if let Some(reader) = inner.reader.take() {
        reader.stop.store(true, Ordering::Release);
        if let Some(h) = reader.handle {
            let _ = h.join();
        }
    }
    drop(inner.device);
}

// =============================================================================
// NIF: open
// =============================================================================

/// Open a TUN device.
///
/// Args:
///   if_name :: binary  — desired interface name (e.g. "macula0")
///   mtu     :: integer — IPv6 MTU (>= 1280)
///
/// Returns: `{ok, ResourceHandle}` | `{error, Reason}`.
///
/// Requires `CAP_NET_ADMIN`.
#[rustler::nif]
pub fn nif_open<'a>(
    env: Env<'a>,
    if_name: Binary<'a>,
    mtu: u32,
) -> Term<'a> {
    let name = match std::str::from_utf8(if_name.as_slice()) {
        Ok(s) => s.to_string(),
        Err(_) => return (atoms::error(), "invalid_utf8_ifname".to_string()).encode(env),
    };

    let device = DeviceBuilder::new()
        .name(&name)
        .mtu(mtu as u16)
        .build_sync();

    let device = match device {
        Ok(d) => d,
        Err(e) => return (atoms::error(), e.to_string()).encode(env),
    };

    let actual_name = device.name().unwrap_or_else(|_| name.clone());
    let resource = ResourceArc::new(TunResource {
        inner: Mutex::new(Some(TunInner {
            device: Arc::new(device),
            name: actual_name,
            reader: None,
        })),
    });

    (atoms::ok(), resource).encode(env)
}

// =============================================================================
// NIF: name
// =============================================================================

#[rustler::nif]
pub fn nif_name<'a>(env: Env<'a>, handle: ResourceArc<TunResource>) -> Term<'a> {
    let guard = match handle.inner.lock() {
        Ok(g) => g,
        Err(_) => return (atoms::error(), "lock_poisoned".to_string()).encode(env),
    };
    match guard.as_ref() {
        Some(inner) => (atoms::ok(), inner.name.clone()).encode(env),
        None => (atoms::error(), "closed".to_string()).encode(env),
    }
}

// =============================================================================
// NIF: close
// =============================================================================

/// Close the TUN device early. Otherwise it's closed when BEAM GCs the
/// resource. Idempotent.
#[rustler::nif]
pub fn nif_close<'a>(env: Env<'a>, handle: ResourceArc<TunResource>) -> Term<'a> {
    let mut guard = match handle.inner.lock() {
        Ok(g) => g,
        Err(_) => return (atoms::error(), "lock_poisoned".to_string()).encode(env),
    };
    if let Some(inner) = guard.take() {
        stop_reader(inner);
    }
    atoms::ok().encode(env)
}

// =============================================================================
// NIF: write
// =============================================================================

/// Write a raw IPv6 packet to the TUN device.
///
/// `packet` MUST be a complete IPv6 packet (header + payload). tun-rs
/// writes it verbatim to the kernel TUN fd.
#[rustler::nif]
pub fn nif_write<'a>(
    env: Env<'a>,
    handle: ResourceArc<TunResource>,
    packet: Binary<'a>,
) -> Term<'a> {
    let device = {
        let guard = match handle.inner.lock() {
            Ok(g) => g,
            Err(_) => return (atoms::error(), "lock_poisoned".to_string()).encode(env),
        };
        match guard.as_ref() {
            Some(inner) => inner.device.clone(),
            None => return (atoms::error(), "closed".to_string()).encode(env),
        }
    };
    match device.send(packet.as_slice()) {
        Ok(_n) => atoms::ok().encode(env),
        Err(e) => (atoms::error(), e.to_string()).encode(env),
    }
}

// =============================================================================
// NIF: start_reader
// =============================================================================

/// Spawn a Rust thread that reads packets from the TUN device and forwards
/// each as `{macula_net_packet, Handle, Payload}` to `pid`.
#[rustler::nif]
pub fn nif_start_reader<'a>(
    env: Env<'a>,
    handle: ResourceArc<TunResource>,
    pid: LocalPid,
) -> Term<'a> {
    let mut guard = match handle.inner.lock() {
        Ok(g) => g,
        Err(_) => return (atoms::error(), "lock_poisoned".to_string()).encode(env),
    };
    let inner = match guard.as_mut() {
        Some(i) => i,
        None => return (atoms::error(), "closed".to_string()).encode(env),
    };
    if inner.reader.is_some() {
        return (atoms::error(), atoms::already_started()).encode(env);
    }

    let stop = Arc::new(AtomicBool::new(false));
    let stop_clone = stop.clone();
    let device = inner.device.clone();
    let handle_clone = handle.clone();

    let thread_handle = thread::Builder::new()
        .name(format!("macula-tun-reader-{}", inner.name))
        .spawn(move || reader_loop(device, handle_clone, pid, stop_clone))
        .ok();

    inner.reader = Some(ReaderState {
        stop,
        handle: thread_handle,
    });
    atoms::ok().encode(env)
}

fn reader_loop(
    device: Arc<SyncDevice>,
    handle: ResourceArc<TunResource>,
    pid: LocalPid,
    stop: Arc<AtomicBool>,
) {
    let mut buf = vec![0u8; 65535];

    while !stop.load(Ordering::Acquire) {
        match device.recv(&mut buf) {
            Ok(n) if n > 0 => {
                let packet_bytes = buf[..n].to_vec();
                let mut owned = OwnedEnv::new();
                let send_result = owned.send_and_clear(&pid, |env| {
                    let mut bin = NewBinary::new(env, packet_bytes.len());
                    bin.as_mut_slice().copy_from_slice(&packet_bytes);
                    let bin_term: Binary = bin.into();
                    (
                        atoms::macula_net_packet(),
                        handle.clone(),
                        bin_term,
                    )
                        .encode(env)
                });
                if send_result.is_err() {
                    break;
                }
            }
            Ok(_) => {}
            Err(e) => {
                use std::io::ErrorKind;
                match e.kind() {
                    ErrorKind::WouldBlock | ErrorKind::Interrupted => {
                        thread::sleep(Duration::from_millis(1));
                    }
                    _ => break,
                }
            }
        }
    }
}
