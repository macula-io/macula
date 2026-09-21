%% `macula_quic:lost_packets/1` is wired end to end: exported from the module,
%% registered in `rustler::init!', and reaching the NIF rather than the
%% `nif_not_loaded' stub.
%%
%% It cannot assert a COUNT here. The value comes from Quinn's congestion
%% controller on a live connection, and there is none in a unit test — which is
%% why the sibling `max_datagram_size/1` has no unit test either. What this
%% catches is the failure that costs an afternoon: adding a NIF to the Rust and
%% forgetting the line in `lib.rs`, which compiles clean, loads clean, and then
%% raises `nif_not_loaded` at the one moment someone needs the number.
-module(macula_quic_lost_packets_tests).

-include_lib("eunit/include/eunit.hrl").

lost_packets_is_exported_test() ->
    ?assert(lists:member({lost_packets, 1}, macula_quic:module_info(exports))).

%% A bare reference is not a connection resource, so the NIF refuses it with
%% `badarg'. That refusal is the evidence: the stub in `macula_quic.erl' raises
%% `nif_not_loaded' instead, so reaching `badarg' proves the real NIF ran.
lost_packets_reaches_the_nif_not_the_stub_test() ->
    Refusal = try macula_quic:lost_packets(erlang:make_ref()) of
                  Unexpected -> {returned, Unexpected}
              catch
                  Class:Reason -> {Class, Reason}
              end,
    ?assertEqual({error, badarg}, Refusal).
