%%%-------------------------------------------------------------------
%%% @doc Host-side controller for the macula-net attach plane.
%%%
%%% PLAN_MACULA_NET_PHASE3_5_TRANSPORT_SEAM.md §5. When a station
%%% hosts daemons, this slice is the transport's inbound handler. It
%%% owns three responsibilities:
%%%
%%% <ol>
%%%   <li><b>attach_v1 dispatch</b> — On a `macula_attach_v1' frame,
%%%       parse + validate the daemon's signed delegation, then call
%%%       the configured `attach_fn' with the originating
%%%       <em>StreamRef</em> as the `attach_conn'. Owning the StreamRef
%%%       at attach time is what lets the host send replies on the
%%%       same bidi stream the daemon dialed in on.</li>
%%%   <li><b>hosted-data forwarding</b> — On a `data' frame whose `dst'
%%%       is in the host_identity table, look up the stored StreamRef
%%%       and re-send the original CBOR on it via `attach_send_fn'.
%%%       The dst is checked by ETS membership lookup, not by trusting
%%%       the daemon's claim — the address-binding-by-construction
%%%       discipline from the resolver applies here too.</li>
%%%   <li><b>fallback</b> — Anything else (non-data envelopes, data
%%%       for non-hosted dst, malformed CBOR) is handed to `fallback_fn',
%%%       typically {@link macula_deliver_packet:handle_envelope/1}.</li>
%%% </ol>
%%%
%%% Every external coupling is a callback in the config; production
%%% wiring lives in {@link macula_net} (host-mode boot path), tests
%%% pass capture functions to a process mailbox.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_host_attach_controller).

-behaviour(gen_server).

-export([
    start_link/1,
    stop/0,
    handle/2,
    delegation_from_wire/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export_type([config/0]).

-define(SERVER, ?MODULE).
-define(FRAME_HEADER_BYTES, 4).

-type stream_ref() :: macula_net_transport:stream_ref().

-type attach_send_fn() :: fun((stream_ref(), iodata()) -> ok | {error, term()}).
-type lookup_fn()      :: fun((<<_:128>>) -> {ok, stream_ref()} | not_found).
-type attach_fn()      :: fun((<<_:128>>, <<_:256>>,
                                macula_record:host_delegation(),
                                stream_ref()) -> ok | {error, term()}).
-type fallback_fn()    :: fun((binary()) -> any()).
-type forward_fn()     :: fun((Cbor :: binary(), Dst :: <<_:128>>) ->
                                  {ok, term()} | {error, term()}).

-type config() :: #{
    realm_pubkey   := <<_:256>>,
    host_pubkey    := <<_:256>>,
    attach_send_fn := attach_send_fn(),
    lookup_fn      := lookup_fn(),
    attach_fn      := attach_fn(),
    fallback_fn    := fallback_fn(),
    forward_fn     => forward_fn()
}.

-record(state, {
    config   :: config(),
    own_addr :: <<_:128>>
}).

%% =============================================================================
%% Public API
%% =============================================================================

-spec start_link(config()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

-spec stop() -> ok.
stop() ->
    case whereis(?SERVER) of
        undefined -> ok;
        _         -> gen_server:stop(?SERVER)
    end.

%% @doc Transport handler entry point. Suitable for direct use as the
%% argument to {@link macula_net_transport_quic:set_handler/1} when the
%% station is configured in host mode.
-spec handle(binary(), stream_ref()) -> ok.
handle(Cbor, StreamRef) when is_binary(Cbor) ->
    gen_server:cast(?SERVER, {frame, Cbor, StreamRef}).

%% @doc Convert the wire-shape (single-letter keys) delegation map into
%% the in-process atom-keyed form that
%% {@link macula_record:verify_host_delegation/1} expects. Exported for
%% test reuse.
-spec delegation_from_wire(map()) -> macula_record:host_delegation() | error.
delegation_from_wire(#{<<"d">>  := D, <<"h">>  := H, <<"r">>  := R,
                       <<"nb">> := NB, <<"na">> := NA, <<"s">> := S}) ->
    #{daemon_pubkey => D, host_pubkey => H, realm_pubkey => R,
      not_before_ms => NB, not_after_ms => NA, daemon_sig => S};
delegation_from_wire(_) ->
    error.

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init(#{realm_pubkey   := Realm, host_pubkey    := HostPk,
       attach_send_fn := S, lookup_fn      := L,
       attach_fn      := A, fallback_fn    := F} = Config)
  when is_function(S, 2), is_function(L, 1),
       is_function(A, 4), is_function(F, 1) ->
    process_flag(trap_exit, true),
    OwnAddr = macula_address:derive(Realm, HostPk),
    %% Default forward_fn: drop. Production wires
    %% fun macula_route_packet:dispatch_envelope/2 here. Tests pass a
    %% capture fn. With no forward_fn the controller falls through to
    %% fallback_fn for non-hosted-non-local data, preserving the
    %% Phase 3.5 behaviour for callers that don't opt in.
    {ok, #state{config = Config, own_addr = OwnAddr}}.

handle_call(_Other, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({frame, Cbor, StreamRef}, State) ->
    dispatch_decoded(macula_cbor_nif:unpack(Cbor), Cbor, StreamRef, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% Dispatch — multi-clause, no nesting
%% =============================================================================

dispatch_decoded({ok, Map}, Cbor, StreamRef, State) when is_map(Map) ->
    dispatch_typed(Map, Cbor, StreamRef, State);
dispatch_decoded(_Other, Cbor, _StreamRef, State) ->
    fallback(Cbor, State).

dispatch_typed(#{<<"type">> := <<"macula_attach_v1">>} = M, _Cbor, StreamRef, State) ->
    handle_attach(M, StreamRef, State);
dispatch_typed(#{<<"type">> := <<"data">>, <<"dst">> := Dst} = _M, Cbor, _StreamRef, State) ->
    route_data(safe_lookup(Dst, State), Dst, Cbor, State);
dispatch_typed(_Other, Cbor, _StreamRef, State) ->
    fallback(Cbor, State).

%% --- attach -----------------------------------------------------------------

handle_attach(#{<<"daemon_pubkey">> := DaemonPk,
                <<"daemon_addr">>   := DaemonAddr,
                <<"delegation">>    := Wire} = _Map,
              StreamRef, State)
  when is_binary(DaemonPk), byte_size(DaemonPk) =:= 32,
       is_binary(DaemonAddr), byte_size(DaemonAddr) =:= 16 ->
    attach_with_delegation(delegation_from_wire(Wire),
                            DaemonAddr, DaemonPk, StreamRef, State);
handle_attach(_Other, _StreamRef, _State) ->
    ok.

attach_with_delegation(error, _Addr, _Pk, _StreamRef, _State) ->
    ok;
attach_with_delegation(Delegation, Addr, Pk, StreamRef,
                        #state{config = #{attach_fn := AttachFn}}) ->
    log_attach(safe_call4(AttachFn, Addr, Pk, Delegation, StreamRef), Addr).

log_attach(ok, _Addr) ->
    ok;
log_attach({error, Reason}, Addr) ->
    error_logger:warning_msg(
      "[host_attach_controller] attach rejected for ~p: ~p",
      [binary:part(Addr, 0, 8), Reason]),
    ok.

%% --- data routing -----------------------------------------------------------
%%
%% Three-way split per the §7.1 sub-spec:
%%   1. dst is a hosted daemon address — forward on its attach stream.
%%   2. dst is the host station's own address — fallback (deliver_packet
%%      writes to TUN).
%%   3. otherwise — forward via route_packet:dispatch_envelope.

route_data({ok, OutStream}, _Dst, Cbor, #state{config = #{attach_send_fn := SendFn}}) ->
    %% Re-frame: the daemon expects the same length-prefixed CBOR shape
    %% the station wire uses.
    Frame = <<(byte_size(Cbor)):32/big, Cbor/binary>>,
    log_send(safe_call2(SendFn, OutStream, Frame));
route_data(not_found, Dst, Cbor, #state{own_addr = OwnAddr} = State)
  when Dst =:= OwnAddr ->
    fallback(Cbor, State);
route_data(not_found, Dst, Cbor, State) ->
    forward_or_fallback(Dst, Cbor, State).

forward_or_fallback(Dst, Cbor, #state{config = Config} = State) ->
    forward_dispatch(maps:get(forward_fn, Config, undefined), Dst, Cbor, State).

forward_dispatch(undefined, _Dst, Cbor, State) ->
    fallback(Cbor, State);
forward_dispatch(Fn, Dst, Cbor, _State) ->
    log_forward(safe_call2(Fn, Cbor, Dst)).

log_send(ok) -> ok;
log_send({error, Reason}) ->
    error_logger:warning_msg(
      "[host_attach_controller] send to hosted daemon failed: ~p",
      [Reason]),
    ok.

log_forward({ok, _}) -> ok;
log_forward(ok)      -> ok;
log_forward({error, Reason}) ->
    error_logger:warning_msg(
      "[host_attach_controller] forward failed: ~p", [Reason]),
    ok.

%% --- fallback ---------------------------------------------------------------

fallback(Cbor, #state{config = #{fallback_fn := Fun}}) ->
    safe_call1(Fun, Cbor),
    ok.

%% =============================================================================
%% Boundary helpers — user callbacks must not crash the controller.
%% =============================================================================

safe_lookup(Dst, #state{config = #{lookup_fn := L}}) ->
    try L(Dst)
    catch _:_ -> not_found
    end.

safe_call1(F, A) ->
    try F(A)
    catch _:_ -> ok
    end.

safe_call2(F, A, B) ->
    try F(A, B)
    catch _:_ -> {error, callback_crashed}
    end.

safe_call4(F, A, B, C, D) ->
    try F(A, B, C, D)
    catch _:_ -> {error, callback_crashed}
    end.
