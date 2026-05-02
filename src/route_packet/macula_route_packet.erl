%%%-------------------------------------------------------------------
%%% @doc Egress routing for macula-net packets.
%%%
%%% Per the macula-net spec (PLAN_MACULA_NET.md §5.2), this slice owns
%%% the egress flow:
%%%
%%%   IPv6 packet from TUN → look up dst → encapsulate → send to station
%%%
%%% Phase 1 simplification: static station table (no DHT). The table is
%%% an ETS map populated by {@link configure/1}; lookups are O(1). Phase
%%% 2 swaps this for a DHT-backed cache without changing the dispatch/1
%%% contract.
%%%
%%% The transport is decoupled via a callback (`SendFun`) supplied per
%%% station entry. Production wires `macula_net_transport_quic:send/2`;
%%% tests pass mocks that capture (StationId, Cbor) pairs.
%%%
%%% CBOR encoding uses {@link macula_cbor_nif:pack/1} — the SDK's
%%% existing wire-protocol primitive. No new CBOR implementation.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_route_packet).

-export([
    configure/1,
    routes/0,
    lookup/1,
    dispatch/1,
    encapsulate/3
]).

-export_type([
    station_entry/0,
    send_fun/0,
    own_address/0
]).

-type station_entry() :: #{
    address  := <<_:128>>,
    station  := binary(),
    send     := send_fun()
}.

-type send_fun()     :: fun((StationId :: binary(), CborEnvelope :: binary()) ->
                              ok | {error, term()}).

-type own_address()  :: <<_:128>>.

-define(TABLE,        macula_route_packet_table).
-define(OWN_ADDR_KEY, own_address).
-define(DEFAULT_TTL,  64).

%% =============================================================================
%% Public API
%% =============================================================================

%% @doc Install the static station table for this node. Idempotent.
-spec configure(#{own_address := own_address(),
                  stations    := [station_entry()]}) -> ok.
configure(#{own_address := OwnAddr, stations := Stations}) ->
    ensure_table(),
    true = ets:delete_all_objects(?TABLE),
    true = ets:insert(?TABLE, {?OWN_ADDR_KEY, OwnAddr}),
    lists:foreach(
        fun(#{address := A, station := S, send := F}) ->
            true = ets:insert(?TABLE, {{addr, A}, S, F})
        end, Stations),
    ok.

%% @doc Return the configured stations as a flat list (for diagnostics).
-spec routes() -> [#{address := <<_:128>>, station := binary()}].
routes() ->
    case ets:info(?TABLE) of
        undefined -> [];
        _ ->
            ets:foldl(
                fun({{addr, A}, S, _F}, Acc) ->
                        [#{address => A, station => S} | Acc];
                   (_, Acc) -> Acc
                end, [], ?TABLE)
    end.

%% @doc Look up the station that hosts `Address'.
-spec lookup(<<_:128>>) ->
    {ok, #{station := binary(), send := send_fun()}} | not_found.
lookup(Address) ->
    case ets:info(?TABLE) of
        undefined -> not_found;
        _ ->
            case ets:lookup(?TABLE, {addr, Address}) of
                [{_, Station, Send}] ->
                    {ok, #{station => Station, send => Send}};
                [] ->
                    not_found
            end
    end.

%% @doc Egress dispatch. Takes a raw IPv6 packet, looks up its
%% destination, wraps it in a macula-net envelope, and hands it to the
%% station's send callback. Returns the chosen station id on success.
-spec dispatch(Packet :: binary()) ->
    {ok, StationId :: binary()} | {error, no_route | malformed_packet | term()}.
dispatch(Packet) when is_binary(Packet), byte_size(Packet) >= 40 ->
    Src = macula_route_packet_ipv6:src(Packet),
    Dst = macula_route_packet_ipv6:dst(Packet),
    dispatch_to(lookup(Dst), Packet, Src, Dst);
dispatch(_) ->
    {error, malformed_packet}.

%% Pattern-matched dispatch helper (idiomatic — no nested case).
dispatch_to(not_found, _Packet, _Src, _Dst) ->
    {error, no_route};
dispatch_to({ok, #{station := Station, send := Send}}, Packet, Src, Dst) ->
    Envelope = encapsulate(Packet, Src, Dst),
    send_via(Send(Station, Envelope), Station).

send_via(ok, Station)               -> {ok, Station};
send_via({error, _} = Err, _Station) -> Err.

%% @doc Build a macula-net envelope around an IPv6 packet. Public so
%% other layers (e.g. daemon attachment) can craft envelopes directly.
-spec encapsulate(Payload :: binary(),
                  Src :: <<_:128>>,
                  Dst :: <<_:128>>) -> binary().
encapsulate(Payload, Src, Dst) ->
    macula_cbor_nif:pack(#{
        <<"v">>       => 1,
        <<"type">>    => <<"data">>,
        <<"src">>     => Src,
        <<"dst">>     => Dst,
        <<"ttl">>     => ?DEFAULT_TTL,
        <<"payload">> => Payload
    }).

%% =============================================================================
%% Internals
%% =============================================================================

ensure_table() ->
    case ets:info(?TABLE) of
        undefined ->
            _ = ets:new(?TABLE, [named_table, public, set,
                                 {read_concurrency, true}]),
            ok;
        _ ->
            ok
    end.
