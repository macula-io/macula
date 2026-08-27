use rustler::{Encoder, LocalPid, OwnedEnv, ResourceArc};

use crate::atoms;

/// Send `{quic, new_conn, ConnRef, ConnInfo}` to an Erlang process.
pub fn send_new_conn<T: Encoder + Send + 'static>(
    pid: &LocalPid,
    conn_ref: T,
    remote_addr: String,
) {
    let pid = pid.clone();
    let mut env = OwnedEnv::new();
    let _ = env.send_and_clear(&pid, |env| {
        let info = rustler::Term::map_new(env);
        let info = info
            .map_put(
                rustler::Atom::from_str(env, "remote_addr").unwrap().encode(env),
                remote_addr.encode(env),
            )
            .unwrap();
        (atoms::quic(), atoms::new_conn(), conn_ref.encode(env), info).encode(env)
    });
}

/// Send `{quic, new_stream, StreamRef, #{conn => ConnRef}}` to an Erlang process.
pub fn send_new_stream<S: Encoder + Send + 'static, C: Encoder + Send + 'static>(
    pid: &LocalPid,
    stream_ref: S,
    conn_ref: C,
    flags: u32,
) {
    let pid = pid.clone();
    let mut env = OwnedEnv::new();
    let _ = env.send_and_clear(&pid, |env| {
        let props = rustler::Term::map_new(env);
        let props = props
            .map_put(
                rustler::Atom::from_str(env, "conn").unwrap().encode(env),
                conn_ref.encode(env),
            )
            .unwrap();
        let props = props
            .map_put(
                rustler::Atom::from_str(env, "flags").unwrap().encode(env),
                flags.encode(env),
            )
            .unwrap();
        (atoms::quic(), atoms::new_stream(), stream_ref.encode(env), props).encode(env)
    });
}

/// Send `{quic, Data, StreamRef, Flags}` to an Erlang process.
pub fn send_data(pid: &LocalPid, data: Vec<u8>, stream_ref: impl Encoder + Send + 'static) {
    let pid = pid.clone();
    let mut env = OwnedEnv::new();
    let _ = env.send_and_clear(&pid, |env| {
        let binary = {
            let mut bin = rustler::OwnedBinary::new(data.len()).unwrap();
            bin.as_mut_slice().copy_from_slice(&data);
            bin.release(env)
        };
        let flags = 0u32;
        (atoms::quic(), binary, stream_ref.encode(env), flags).encode(env)
    });
}

/// Send `{quic, EventAtom, Handle, Detail}` for lifecycle events.
pub fn send_event(
    pid: &LocalPid,
    event: rustler::Atom,
    handle: impl Encoder + Send + 'static,
    detail: impl Encoder + Send + 'static,
) {
    let pid = pid.clone();
    let mut env = OwnedEnv::new();
    let _ = env.send_and_clear(&pid, |env| {
        (atoms::quic(), event, handle.encode(env), detail.encode(env)).encode(env)
    });
}
