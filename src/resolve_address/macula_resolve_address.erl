%%%-------------------------------------------------------------------
%%% @doc Resolve a macula-net IPv6 address to its hosting station's
%%% QUIC endpoint via the DHT.
%%%
%%% Phase 2 (PLAN_MACULA_NET_PHASE2.md §4.2). Two DHT lookups:
%%%
%%% <ol>
%%%   <li>`address_pubkey_map' record keyed by
%%%       `sha256("address_pubkey_map" || addr)' — returns the station
%%%       pubkey that owns the address.</li>
%%%   <li>`station_endpoint' record keyed by
%%%       `sha256("station_endpoint" || pubkey)' — returns
%%%       `{quic_port, host_advertised}'.</li>
%%% </ol>
%%%
%%% Both records are signature-verified. The redirect record is also
%%% address-bound: `derive_address(realm, record.key) == addr' MUST
%%% hold, otherwise the answer is rejected. This makes the cheapest
%%% spoof (sign a record claiming someone else's address) detectable
%%% without consulting any external trust authority — the address
%%% derivation IS the proof of ownership.
%%%
%%% The DHT is decoupled via a `find_fn' callback. Production wires
%%% `fun(Key) -> macula:find_record(Client, Key) end'; tests pass a
%%% capture function.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_resolve_address).

-export([resolve/3]).

-export_type([find_fn/0, endpoint/0, error_reason/0]).

-type find_fn() ::
    fun((Key :: <<_:256>>) ->
        {ok, macula_record:record()} | {error, not_found | term()}).

-type endpoint() :: #{
    station_pubkey  := <<_:256>>,
    quic_port       := 1..65535,
    host_advertised := [binary()],
    alpn            := binary() | undefined,
    expires_at      := pos_integer()
}.

-type error_reason() ::
    not_found
  | bad_signature
  | bad_address_binding
  | malformed_record
  | term().

%% =============================================================================
%% Public API
%% =============================================================================

%% @doc Resolve a macula-net address. `Realm' is the 32-byte realm
%% pubkey used for address derivation; the binding check rejects any
%% redirect whose signer doesn't derive to `Addr' under that realm.
-spec resolve(Addr :: <<_:128>>,
              Realm :: <<_:256>>,
              find_fn()) -> {ok, endpoint()} | {error, error_reason()}.
resolve(Addr, Realm, FindFn)
  when is_binary(Addr), byte_size(Addr) =:= 16,
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_function(FindFn, 1) ->
    redirect_lookup(FindFn(redirect_key(Addr)), Addr, Realm, FindFn).

%% =============================================================================
%% Internals
%% =============================================================================

redirect_key(Addr) ->
    crypto:hash(sha256, <<"address_pubkey_map", Addr/binary>>).

endpoint_key(Pubkey) ->
    crypto:hash(sha256, <<"station_endpoint", Pubkey/binary>>).

%% Step 1: redirect lookup result.
redirect_lookup({error, _} = E, _Addr, _Realm, _FindFn) ->
    E;
redirect_lookup({ok, RedirRecord}, Addr, Realm, FindFn) ->
    verify_redirect(macula_record:verify(RedirRecord), Addr, Realm, FindFn).

verify_redirect({error, _}, _Addr, _Realm, _FindFn) ->
    {error, bad_signature};
verify_redirect({ok, RedirRecord}, Addr, Realm, FindFn) ->
    bind_redirect(redirect_addr(RedirRecord), RedirRecord, Addr, Realm, FindFn).

redirect_addr(#{type := 16#13, payload := P}) ->
    maps:get({text, <<"addr">>}, P, undefined);
redirect_addr(_) ->
    undefined.

%% Address-binding check: the address claimed in the payload must
%% match `Addr', AND `derive_address(Realm, signer_key)' must also
%% match. Both checks together close the spoof surface.
bind_redirect(undefined, _R, _Addr, _Realm, _FindFn) ->
    {error, malformed_record};
bind_redirect(ClaimedAddr, _R, Addr, _Realm, _FindFn)
  when ClaimedAddr =/= Addr ->
    {error, bad_address_binding};
bind_redirect(Addr, RedirRecord, Addr, Realm, FindFn) ->
    PubKey = macula_record:key(RedirRecord),
    Derived = macula_address:derive(Realm, PubKey),
    bind_check(Derived, Addr, PubKey, FindFn).

bind_check(Addr, Addr, PubKey, FindFn) ->
    endpoint_lookup(FindFn(endpoint_key(PubKey)), PubKey);
bind_check(_Derived, _Addr, _PubKey, _FindFn) ->
    {error, bad_address_binding}.

%% Step 2: endpoint lookup result.
endpoint_lookup({error, _} = E, _PubKey) ->
    E;
endpoint_lookup({ok, EndpointRecord}, PubKey) ->
    verify_endpoint(macula_record:verify(EndpointRecord), PubKey).

verify_endpoint({error, _}, _PubKey) ->
    {error, bad_signature};
verify_endpoint({ok, EndpointRecord}, PubKey) ->
    %% The endpoint record's key MUST be the same pubkey we looked up.
    %% A signed-but-impostor record would fail this check.
    record_owner_check(macula_record:key(EndpointRecord), PubKey,
                       EndpointRecord).

record_owner_check(PubKey, PubKey, EndpointRecord) ->
    extract_endpoint(EndpointRecord, PubKey);
record_owner_check(_Other, _PubKey, _EndpointRecord) ->
    {error, bad_address_binding}.

extract_endpoint(#{type := 16#12, payload := P, expires_at := X}, PubKey) ->
    case maps:get({text, <<"quic_port">>}, P, undefined) of
        Port when is_integer(Port), Port >= 1, Port =< 65535 ->
            Hosts = case maps:get({text, <<"host_advertised">>}, P, []) of
                Bins when is_list(Bins) -> Bins;
                _ -> []
            end,
            Alpn = case maps:get({text, <<"alpn">>}, P, undefined) of
                {text, A} -> A;
                A when is_binary(A) -> A;
                _ -> undefined
            end,
            {ok, #{station_pubkey  => PubKey,
                   quic_port       => Port,
                   host_advertised => Hosts,
                   alpn            => Alpn,
                   expires_at      => X}};
        _ ->
            {error, malformed_record}
    end;
extract_endpoint(_Other, _PubKey) ->
    {error, malformed_record}.
