%%%-------------------------------------------------------------------
%%% @doc Per-flow packet trace toggle for macula-net.
%%%
%%% Phase 4.1 — see PLAN_MACULA_NET_PHASE4_1_OBSERVABILITY.md.
%%%
%%% Disabled by default. Operators enable by address (16-byte binary)
%%% or wildcard ({@link enable_all/0}). When enabled for an address,
%%% any envelope whose `src' or `dst' matches the address is logged
%%% via OTP `logger' at `debug' level under topic
%%% `_macula.net.packet_trace.envelope'.
%%%
%%% The trace is stored as a persistent_term so the hot path can read
%%% it with no ETS lookup, no gen_server roundtrip. The trade-off:
%%% updates are slow (persistent_term is not designed for high-frequency
%%% mutation), but operators flip the trace at human cadence.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_packet_trace).

-export([
    enable/1,
    enable_all/0,
    disable/1,
    disable_all/0,
    is_enabled_for/1,
    state/0,
    trace_envelope/4
]).

-define(KEY, {?MODULE, state}).

-type addr() :: <<_:128>>.
-type direction() :: ingress | egress | relay.

-type state() :: #{
    enabled := boolean(),
    addrs   := [addr()]
}.

%% =============================================================================
%% Toggle API
%% =============================================================================

%% @doc Enable trace for a single 16-byte macula-net address. Idempotent.
-spec enable(addr()) -> ok.
enable(Addr) when is_binary(Addr), byte_size(Addr) =:= 16 ->
    set_state(add_addr(Addr, get_state())).

%% @doc Enable trace for ALL traffic. Use with care — high-volume.
-spec enable_all() -> ok.
enable_all() ->
    set_state(#{enabled => true, addrs => all}).

%% @doc Remove an address from the enabled set. Disables the trace
%% entirely if no addresses remain enabled.
-spec disable(addr()) -> ok.
disable(Addr) when is_binary(Addr), byte_size(Addr) =:= 16 ->
    set_state(remove_addr(Addr, get_state())).

-spec disable_all() -> ok.
disable_all() ->
    set_state(default_state()).

%% @doc True if `Addr' is currently being traced.
-spec is_enabled_for(addr()) -> boolean().
is_enabled_for(Addr) ->
    addr_enabled(get_state(), Addr).

%% @doc Snapshot of current trace state.
-spec state() -> state().
state() -> get_state().

%% =============================================================================
%% Hot-path entry point — called from the wired modules (see §6 of the spec)
%% =============================================================================

%% @doc Log this envelope if the src/dst matches the enabled set.
%% Cheap when disabled (one persistent_term lookup, one map check).
-spec trace_envelope(direction(), addr(), addr(), iodata() | non_neg_integer()) -> ok.
trace_envelope(Direction, Src, Dst, Payload) ->
    maybe_log(should_log(get_state(), Src, Dst),
              Direction, Src, Dst, Payload).

%% =============================================================================
%% Internals
%% =============================================================================

default_state() ->
    #{enabled => false, addrs => []}.

get_state() ->
    persistent_term:get(?KEY, default_state()).

set_state(S) ->
    persistent_term:put(?KEY, S),
    ok.

add_addr(_Addr, #{addrs := all} = S) ->
    S;
add_addr(Addr, #{addrs := L} = S) when is_list(L) ->
    S#{enabled => true, addrs => lists:usort([Addr | L])}.

remove_addr(_Addr, #{addrs := all}) ->
    default_state();
remove_addr(Addr, #{addrs := L} = S) when is_list(L) ->
    NewL = lists:delete(Addr, L),
    S#{enabled => NewL =/= [], addrs => NewL}.

addr_enabled(#{enabled := false}, _Addr) -> false;
addr_enabled(#{addrs := all}, _Addr)     -> true;
addr_enabled(#{addrs := L}, Addr)        -> lists:member(Addr, L).

should_log(#{enabled := false}, _Src, _Dst) -> false;
should_log(#{addrs := all},     _Src, _Dst) -> true;
should_log(#{addrs := L},        Src, Dst) ->
    lists:member(Src, L) orelse lists:member(Dst, L).

maybe_log(false, _Direction, _Src, _Dst, _Payload) -> ok;
maybe_log(true,  Direction,  Src,  Dst,  Payload)  ->
    Size = payload_size(Payload),
    macula_diagnostics:event(debug, <<"_macula.net.packet_trace.envelope">>,
        #{direction => Direction,
          src       => fmt(Src),
          dst       => fmt(Dst),
          payload_size => Size}).

fmt(Addr) when is_binary(Addr), byte_size(Addr) =:= 16 ->
    macula_address:format(Addr);
fmt(Other) -> Other.

payload_size(N) when is_integer(N) -> N;
payload_size(B) when is_binary(B)  -> byte_size(B);
payload_size(L) when is_list(L)    -> iolist_size(L).
