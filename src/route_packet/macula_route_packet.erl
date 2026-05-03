%%%-------------------------------------------------------------------
%%% @doc Egress routing for macula-net packets.
%%%
%%% Per the macula-net spec (PLAN_MACULA_NET.md §5.2), this slice owns
%%% the egress flow:
%%%
%%%   IPv6 packet from TUN → look up dst → encapsulate → send to station
%%%
%%% Two modes:
%%%
%%% <dl>
%%%   <dt>`static'</dt>
%%%   <dd>Phase 1 default. Caller pre-populates an ETS table of
%%%       `{Address, StationId, SendFun}'. Used by tests and the
%%%       smoke / netns demos. Selected when `configure/1' receives
%%%       `stations'.</dd>
%%%   <dt>`dht'</dt>
%%%   <dd>Phase 2. On miss in {@link macula_cache_route}, calls
%%%       {@link macula_resolve_address:resolve/3} for the destination,
%%%       caches the answer (TTL = record's `expires_at'), and dials
%%%       the resolved host via the configured `connect_fn'. Selected
%%%       when `configure/1' receives `resolver'.</dd>
%%% </dl>
%%%
%%% CBOR encoding uses {@link macula_cbor_nif:pack/1}.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_route_packet).

-export([
    configure/1,
    mode/0,
    routes/0,
    lookup/1,
    dispatch/1,
    dispatch_envelope/2,
    encapsulate/3
]).

-export_type([
    station_entry/0,
    send_fun/0,
    own_address/0,
    resolver/0
]).

-type station_entry() :: #{
    address  := <<_:128>>,
    station  := binary(),
    send     := send_fun()
}.

-type send_fun()     :: fun((StationId :: binary(), CborEnvelope :: binary()) ->
                              ok | {error, term()}).

-type connect_fun()  :: fun((StationId :: binary(),
                             Host :: binary() | string(),
                             Port :: 1..65535) -> ok | {error, term()}).

-type resolver() :: #{
    realm_pubkey := <<_:256>>,
    find_fn      := macula_resolve_address:find_fn(),
    connect_fn   := connect_fun(),
    send_fn      := send_fun()
}.

-type own_address()  :: <<_:128>>.

-define(TABLE,        macula_route_packet_table).
-define(OWN_ADDR_KEY, own_address).
-define(MODE_KEY,     mode).
-define(RESOLVER_KEY, resolver).
-define(DEFAULT_TTL,  64).

%% =============================================================================
%% Public API
%% =============================================================================

%% @doc Install routing. Two shapes:
%%
%% Static:   `#{own_address, stations}'    — Phase 1 / tests.
%% DHT:      `#{own_address, resolver}'    — Phase 2.
%%
%% Idempotent. Calling again replaces the previous configuration.
-spec configure(#{own_address := own_address(),
                  stations    => [station_entry()],
                  resolver    => resolver()}) -> ok.
configure(#{own_address := OwnAddr, stations := Stations}) ->
    ensure_table(),
    true = ets:delete_all_objects(?TABLE),
    true = ets:insert(?TABLE, {?OWN_ADDR_KEY, OwnAddr}),
    true = ets:insert(?TABLE, {?MODE_KEY, static}),
    lists:foreach(
        fun(#{address := A, station := S, send := F}) ->
            true = ets:insert(?TABLE, {{addr, A}, S, F})
        end, Stations),
    ok;
configure(#{own_address := OwnAddr,
            resolver    := #{realm_pubkey := _,
                             find_fn      := _,
                             connect_fn   := _,
                             send_fn      := _} = Resolver}) ->
    ensure_table(),
    true = ets:delete_all_objects(?TABLE),
    true = ets:insert(?TABLE, {?OWN_ADDR_KEY, OwnAddr}),
    true = ets:insert(?TABLE, {?MODE_KEY, dht}),
    true = ets:insert(?TABLE, {?RESOLVER_KEY, Resolver}),
    ok.

-spec mode() -> static | dht | undefined.
mode() ->
    case ets:info(?TABLE) of
        undefined -> undefined;
        _ ->
            case ets:lookup(?TABLE, ?MODE_KEY) of
                [{_, M}] -> M;
                []       -> undefined
            end
    end.

%% @doc Return the configured static stations (diagnostics; empty in dht mode).
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

%% @doc Look up the route for `Address'.
%%
%% In static mode this is the synchronous lookup of Phase 1.
%% In dht mode the lookup is cache-only — a cold miss returns
%% `not_found' rather than triggering a DHT call (use {@link
%% dispatch/1} for the full resolve+connect+send path).
-spec lookup(<<_:128>>) ->
    {ok, #{station := binary(), send := send_fun()}} | not_found.
lookup(Address) ->
    lookup_in_mode(mode(), Address).

lookup_in_mode(undefined, _Address) -> not_found;
lookup_in_mode(static, Address) ->
    case ets:lookup(?TABLE, {addr, Address}) of
        [{_, Station, Send}] -> {ok, #{station => Station, send => Send}};
        []                   -> not_found
    end;
lookup_in_mode(dht, Address) ->
    case macula_cache_route:lookup(Address) of
        {ok, #{station_pubkey := Pk}} ->
            #{send_fn := Send} = resolver(),
            {ok, #{station => Pk, send => Send}};
        _ ->
            not_found
    end.

%% @doc Egress dispatch. Takes a raw IPv6 packet, looks up its
%% destination, wraps it in a macula-net envelope, sends to the
%% hosting station. In dht mode performs a DHT resolve on cache miss
%% and dials the resolved host via the configured `connect_fn'.
-spec dispatch(Packet :: binary()) ->
    {ok, StationId :: binary()} | {error, no_route | malformed_packet | term()}.
dispatch(Packet) when is_binary(Packet), byte_size(Packet) >= 40 ->
    T0 = erlang:monotonic_time(microsecond),
    Src = macula_route_packet_ipv6:src(Packet),
    Dst = macula_route_packet_ipv6:dst(Packet),
    Result = dispatch_in_mode(mode(), Packet, Src, Dst),
    emit_dispatch_telemetry(Result, T0),
    Result;
dispatch(_) ->
    telemetry:execute([macula, net, egress, dropped],
                      #{count => 1},
                      #{reason => <<"malformed_packet">>}),
    {error, malformed_packet}.

emit_dispatch_telemetry({ok, _Station}, T0) ->
    Latency = erlang:monotonic_time(microsecond) - T0,
    telemetry:execute([macula, net, egress, dispatched],
                      #{latency_us => Latency},
                      #{kind => <<"data">>});
emit_dispatch_telemetry({error, Reason}, _T0) ->
    telemetry:execute([macula, net, egress, dropped],
                      #{count => 1},
                      #{reason => reason_bin(Reason)}).

reason_bin(R) when is_atom(R)   -> atom_to_binary(R, utf8);
reason_bin(R) when is_binary(R) -> R;
reason_bin(_)                    -> <<"unknown">>.

%% Static mode (Phase 1).
dispatch_in_mode(static, Packet, Src, Dst) ->
    dispatch_to(lookup_in_mode(static, Dst), Packet, Src, Dst);

%% DHT mode (Phase 2). Cache-first; on miss/expired, resolve + connect
%% + cache + send.
dispatch_in_mode(dht, Packet, Src, Dst) ->
    Envelope = encapsulate(Packet, Src, Dst),
    Resolver = resolver(),
    Send     = maps:get(send_fn, Resolver),
    deliver_dht(macula_cache_route:lookup(Dst), Dst, Envelope, Send, Resolver);

dispatch_in_mode(undefined, _Packet, _Src, _Dst) ->
    {error, not_configured}.

deliver_dht({ok, #{station_pubkey := Pk}}, _Dst, Envelope, Send, _Resolver) ->
    send_via(Send(Pk, Envelope), Pk);
deliver_dht(_MissOrExpired, Dst, Envelope, Send, Resolver) ->
    Realm  = maps:get(realm_pubkey, Resolver),
    FindFn = maps:get(find_fn, Resolver),
    ConnFn = maps:get(connect_fn, Resolver),
    case macula_resolve_address:resolve(Dst, Realm, FindFn) of
        {ok, Endpoint} ->
            install_and_send(Endpoint, Dst, Envelope, Send, ConnFn);
        {error, _} = Err ->
            map_resolve_error(Err)
    end.

install_and_send(#{station_pubkey  := Pk,
                   quic_port       := Port,
                   host_advertised := Hosts,
                   expires_at      := X} = _Endpoint,
                 Dst, Envelope, Send, ConnFn) ->
    case pick_host(Hosts) of
        {error, _} = Err -> Err;
        {ok, Host} ->
            case ConnFn(Pk, Host, Port) of
                ok ->
                    ok = macula_cache_route:insert(Dst, #{
                        station_pubkey => Pk,
                        host           => Host,
                        port           => Port,
                        expires_at     => X
                    }),
                    send_via(Send(Pk, Envelope), Pk);
                {error, _} = Err ->
                    Err
            end
    end.

pick_host([]) -> {error, no_route};
pick_host([H | _]) when is_binary(H) -> {ok, H};
pick_host([H | _]) when is_list(H)   -> {ok, H};
pick_host(_)                          -> {error, no_route}.

map_resolve_error({error, not_found})            -> {error, no_route};
map_resolve_error({error, bad_address_binding})  -> {error, bad_address_binding};
map_resolve_error({error, bad_signature})        -> {error, bad_signature};
map_resolve_error({error, _} = E)                -> E.

%% Pattern-matched dispatch helper (static mode).
dispatch_to(not_found, _Packet, _Src, _Dst) ->
    {error, no_route};
dispatch_to({ok, #{station := Station, send := Send}}, Packet, Src, Dst) ->
    Envelope = encapsulate(Packet, Src, Dst),
    send_via(Send(Station, Envelope), Station).

send_via(ok, Station)               -> {ok, Station};
send_via({error, _} = Err, _Station) -> Err.

%% @doc Forward a pre-built macula-net envelope toward `Dst'.
%%
%% Same lookup + resolve + connect + send pipeline as {@link
%% dispatch/1}, but skips the IPv6-to-CBOR encapsulation step. Used
%% by {@link macula_host_attach_controller} when a hosted daemon
%% emits a data envelope whose `dst' is neither hosted on the same
%% station nor the station's own address — the host station forwards
%% the same bytes onward, preserving the envelope's `src' so the
%% routing is transparent at L3.
-spec dispatch_envelope(CborEnvelope :: binary(), Dst :: <<_:128>>) ->
    {ok, StationId :: binary()} | {error, term()}.
dispatch_envelope(Cbor, Dst) when is_binary(Cbor), is_binary(Dst), byte_size(Dst) =:= 16 ->
    Result = dispatch_envelope_in_mode(mode(), Cbor, Dst),
    emit_relay_telemetry(Result),
    Result.

emit_relay_telemetry({ok, _Station}) ->
    telemetry:execute([macula, net, relay, dispatched],
                      #{count => 1}, #{kind => <<"data">>});
emit_relay_telemetry({error, Reason}) ->
    telemetry:execute([macula, net, egress, dropped],
                      #{count => 1}, #{reason => reason_bin(Reason)}).

dispatch_envelope_in_mode(undefined, _Cbor, _Dst) ->
    {error, not_configured};
dispatch_envelope_in_mode(static, Cbor, Dst) ->
    forward_static(lookup_in_mode(static, Dst), Cbor);
dispatch_envelope_in_mode(dht, Cbor, Dst) ->
    Resolver = resolver(),
    Send     = maps:get(send_fn, Resolver),
    deliver_dht(macula_cache_route:lookup(Dst), Dst, Cbor, Send, Resolver).

forward_static(not_found, _Cbor) ->
    {error, no_route};
forward_static({ok, #{station := Station, send := Send}}, Cbor) ->
    send_via(Send(Station, Cbor), Station).

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

resolver() ->
    [{_, R}] = ets:lookup(?TABLE, ?RESOLVER_KEY),
    R.

ensure_table() ->
    case ets:info(?TABLE) of
        undefined ->
            _ = ets:new(?TABLE, [named_table, public, set,
                                 {read_concurrency, true}]),
            ok;
        _ ->
            ok
    end.
