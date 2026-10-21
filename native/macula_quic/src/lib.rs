mod atoms;
mod cert;
mod config;
mod connection;
mod endpoint;
mod error_codes;
mod message;
mod runtime;
mod stream;

use rustler::{Env, Term};

/// Registers every resource type the NIFs hand out. A resource that cannot be
/// registered fails the load here, where the emulator reports which NIF it
/// was: answering true and failing at the first accepted connection would put
/// the error far from its cause.
fn on_load(env: Env, _info: Term) -> bool {
    runtime::init();
    [
        env.register::<endpoint::ListenerResource>(),
        env.register::<connection::ConnectionResource>(),
        env.register::<connection::DialResource>(),
        env.register::<connection::StreamOpenResource>(),
        env.register::<stream::StreamResource>(),
    ]
    .into_iter()
    .all(|registered| registered.is_ok())
}

rustler::init!("macula_quic", load = on_load);

/// The BEAM provides every `enif_*` symbol at NIF load time, so the cdylib
/// this crate ships links with them undefined and the emulator resolves them
/// when it loads the library.
///
/// A `cargo test` harness is an ORDINARY EXECUTABLE with no BEAM behind it.
/// The dynamic loader binds these symbols at load rather than lazily, because
/// they are reached through relocations rather than plain calls, so the test
/// binary dies with `symbol lookup error` before a single test runs. Ignoring
/// unresolved symbols at link time is not enough; the failure is at load.
///
/// ⚠ THESE ARE NOT A FAKE BEAM AND MUST NEVER BECOME ONE. Every one aborts.
/// Nothing under test reaches them: the TLS configuration and handshake tests
/// exercise `config.rs` and `cert.rs`, which never touch the NIF surface. A
/// stub that returned a plausible value would let a test appear to pass while
/// exercising a pretend emulator, which is worse than having no test.
///
/// The list is exactly the undefined `enif_*` symbols of the test binary,
/// taken from `nm -D -u`. If a new NIF call adds one, the harness fails at
/// load with its name and it belongs here.
#[cfg(test)]
mod enif_stubs {
    fn no_beam(name: &str) -> ! {
        panic!("{} called in a test binary: there is no BEAM here", name)
    }

    #[no_mangle]
    pub extern "C" fn enif_alloc_binary() -> ! {
        no_beam("enif_alloc_binary")
    }

    #[no_mangle]
    pub extern "C" fn enif_alloc_env() -> ! {
        no_beam("enif_alloc_env")
    }

    #[no_mangle]
    pub extern "C" fn enif_alloc_resource() -> ! {
        no_beam("enif_alloc_resource")
    }

    #[no_mangle]
    pub extern "C" fn enif_clear_env() -> ! {
        no_beam("enif_clear_env")
    }

    #[no_mangle]
    pub extern "C" fn enif_compare() -> ! {
        no_beam("enif_compare")
    }

    #[no_mangle]
    pub extern "C" fn enif_free_env() -> ! {
        no_beam("enif_free_env")
    }

    #[no_mangle]
    pub extern "C" fn enif_get_list_cell() -> ! {
        no_beam("enif_get_list_cell")
    }

    #[no_mangle]
    pub extern "C" fn enif_get_local_pid() -> ! {
        no_beam("enif_get_local_pid")
    }

    #[no_mangle]
    pub extern "C" fn enif_get_resource() -> ! {
        no_beam("enif_get_resource")
    }

    #[no_mangle]
    pub extern "C" fn enif_get_uint() -> ! {
        no_beam("enif_get_uint")
    }

    #[no_mangle]
    pub extern "C" fn enif_get_ulong() -> ! {
        no_beam("enif_get_ulong")
    }

    #[no_mangle]
    pub extern "C" fn enif_inspect_binary() -> ! {
        no_beam("enif_inspect_binary")
    }

    #[no_mangle]
    pub extern "C" fn enif_is_empty_list() -> ! {
        no_beam("enif_is_empty_list")
    }

    #[no_mangle]
    pub extern "C" fn enif_is_list() -> ! {
        no_beam("enif_is_list")
    }

    #[no_mangle]
    pub extern "C" fn enif_keep_resource() -> ! {
        no_beam("enif_keep_resource")
    }

    #[no_mangle]
    pub extern "C" fn enif_make_atom_len() -> ! {
        no_beam("enif_make_atom_len")
    }

    #[no_mangle]
    pub extern "C" fn enif_make_badarg() -> ! {
        no_beam("enif_make_badarg")
    }

    #[no_mangle]
    pub extern "C" fn enif_make_binary() -> ! {
        no_beam("enif_make_binary")
    }

    #[no_mangle]
    pub extern "C" fn enif_make_copy() -> ! {
        no_beam("enif_make_copy")
    }

    #[no_mangle]
    pub extern "C" fn enif_make_list_from_array() -> ! {
        no_beam("enif_make_list_from_array")
    }

    #[no_mangle]
    pub extern "C" fn enif_make_map_put() -> ! {
        no_beam("enif_make_map_put")
    }

    #[no_mangle]
    pub extern "C" fn enif_make_new_binary() -> ! {
        no_beam("enif_make_new_binary")
    }

    #[no_mangle]
    pub extern "C" fn enif_make_new_map() -> ! {
        no_beam("enif_make_new_map")
    }

    #[no_mangle]
    pub extern "C" fn enif_make_resource() -> ! {
        no_beam("enif_make_resource")
    }

    #[no_mangle]
    pub extern "C" fn enif_make_tuple_from_array() -> ! {
        no_beam("enif_make_tuple_from_array")
    }

    #[no_mangle]
    pub extern "C" fn enif_make_uint() -> ! {
        no_beam("enif_make_uint")
    }

    #[no_mangle]
    pub extern "C" fn enif_make_ulong() -> ! {
        no_beam("enif_make_ulong")
    }

    #[no_mangle]
    pub extern "C" fn enif_raise_exception() -> ! {
        no_beam("enif_raise_exception")
    }

    #[no_mangle]
    pub extern "C" fn enif_release_binary() -> ! {
        no_beam("enif_release_binary")
    }

    #[no_mangle]
    pub extern "C" fn enif_release_resource() -> ! {
        no_beam("enif_release_resource")
    }

    #[no_mangle]
    pub extern "C" fn enif_schedule_nif() -> ! {
        no_beam("enif_schedule_nif")
    }

    #[no_mangle]
    pub extern "C" fn enif_self() -> ! {
        no_beam("enif_self")
    }

    #[no_mangle]
    pub extern "C" fn enif_send() -> ! {
        no_beam("enif_send")
    }

    #[no_mangle]
    pub extern "C" fn enif_thread_type() -> ! {
        no_beam("enif_thread_type")
    }
}
