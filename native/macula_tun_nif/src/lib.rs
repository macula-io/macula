//! macula_tun_nif — Linux TUN device lifecycle for macula-net.
//!
//! Owns the kernel-side substrate plumbing:
//!   - create / destroy TUN device
//!   - read packets (background thread → BEAM mailbox)
//!   - write packets
//!
//! Per PLAN_MACULA_NET.md §10.3.

mod tun;

use rustler::{Env, Term};

fn on_load(env: Env, _info: Term) -> bool {
    rustler::resource!(tun::TunResource, env);
    true
}

rustler::init!(
    "macula_tun_nif",
    [
        tun::nif_open,
        tun::nif_name,
        tun::nif_close,
        tun::nif_write,
        tun::nif_start_reader,
    ],
    load = on_load
);
