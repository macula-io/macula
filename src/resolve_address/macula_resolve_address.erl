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
    expires_at      := pos_integer(),
    %% Phase 3: when set, the resolved address is hosted by the
    %% station above on behalf of `daemon_pubkey'. Senders don't need
    %% to act on this; it's diagnostic.
    hosted_daemon   => <<_:256>>
}.

-type error_reason() ::
    not_found
  | bad_signature
  | bad_address_binding
  | bad_delegation
  | delegation_expired
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
    %% Try station-owned first (Phase 2). On `not_found', fall back
    %% to hosted (Phase 3). The two namespaces are disjoint, so a
    %% legitimate address has at most one redirect; a misconfigured
    %% one with both would silently take the station path here.
    %% Phase 4 hardening: detect + flag the conflict.
    redirect_lookup(FindFn(redirect_key(Addr)), Addr, Realm, FindFn).

%% =============================================================================
%% Internals
%% =============================================================================

redirect_key(Addr) ->
    crypto:hash(sha256, <<"address_pubkey_map", Addr/binary>>).

hosted_redirect_key(Addr) ->
    crypto:hash(sha256, <<"hosted_address_map", Addr/binary>>).

endpoint_key(Pubkey) ->
    crypto:hash(sha256, <<"station_endpoint", Pubkey/binary>>).

%% Step 1: redirect lookup result.
%% On `not_found' we fall through to the hosted-redirect path (Phase 3).
%% Any other error short-circuits.
redirect_lookup({error, not_found}, Addr, Realm, FindFn) ->
    hosted_redirect_lookup(FindFn(hosted_redirect_key(Addr)),
                            Addr, Realm, FindFn);
redirect_lookup({error, _} = E, _Addr, _Realm, _FindFn) ->
    E;
redirect_lookup({ok, RedirRecord}, Addr, Realm, FindFn) ->
    verify_redirect(macula_record:verify(RedirRecord), Addr, Realm, FindFn).

%% Phase 3 hosted-redirect lookup. Same shape, different verification.
hosted_redirect_lookup({error, _} = E, _Addr, _Realm, _FindFn) ->
    E;
hosted_redirect_lookup({ok, HRec}, Addr, Realm, FindFn) ->
    verify_hosted_redirect(macula_record:verify(HRec), Addr, Realm, FindFn).

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
    extract_endpoint_payload(P, PubKey, X, undefined);
extract_endpoint(_Other, _PubKey) ->
    {error, malformed_record}.

%% Same extraction with an optional `hosted_daemon' tag for Phase 3.
extract_endpoint_payload(P, PubKey, X, HostedDaemon) ->
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
            Base = #{station_pubkey  => PubKey,
                     quic_port       => Port,
                     host_advertised => Hosts,
                     alpn            => Alpn,
                     expires_at      => X},
            {ok, attach_hosted_daemon(Base, HostedDaemon)};
        _ ->
            {error, malformed_record}
    end.

attach_hosted_daemon(Map, undefined) -> Map;
attach_hosted_daemon(Map, DaemonPk) when is_binary(DaemonPk) ->
    Map#{hosted_daemon => DaemonPk}.

%% =============================================================================
%% Phase 3: hosted-redirect verification + delegation chain check
%% =============================================================================

verify_hosted_redirect({error, _}, _Addr, _Realm, _FindFn) ->
    {error, bad_signature};
verify_hosted_redirect({ok, HRec}, Addr, Realm, FindFn) ->
    HostPk    = macula_record:key(HRec),
    Payload   = macula_record:payload(HRec),
    ClaimAddr = maps:get({text, <<"addr">>},   Payload, undefined),
    DaemonPk  = maps:get({text, <<"daemon">>}, Payload, undefined),
    DelMap    = maps:get({text, <<"delegation">>}, Payload, undefined),
    chain_check(ClaimAddr, DaemonPk, DelMap, HRec, HostPk, Addr, Realm, FindFn).

chain_check(undefined, _, _, _, _, _, _, _)            -> {error, malformed_record};
chain_check(_, undefined, _, _, _, _, _, _)            -> {error, malformed_record};
chain_check(_, _, undefined, _, _, _, _, _)            -> {error, malformed_record};
chain_check(ClaimAddr, _, _, _, _, Addr, _, _) when ClaimAddr =/= Addr ->
    {error, bad_address_binding};
chain_check(Addr, DaemonPk, DelMap, HRec, HostPk, Addr, Realm, FindFn) ->
    %% derive_address must bind to the daemon, not the host.
    Derived = macula_address:derive(Realm, DaemonPk),
    derived_check(Derived, Addr, DelMap, DaemonPk, HRec, HostPk, Realm, FindFn).

derived_check(Addr, Addr, DelMap, DaemonPk, HRec, HostPk, Realm, FindFn) ->
    Delegation = parse_delegation(DelMap),
    delegation_field_check(Delegation, DaemonPk, HostPk, Realm, HRec, FindFn);
derived_check(_Derived, _Addr, _, _, _, _, _, _) ->
    {error, bad_address_binding}.

delegation_field_check(undefined, _, _, _, _, _) ->
    {error, malformed_record};
delegation_field_check(#{daemon_pubkey := DaemonPk,
                          host_pubkey   := HostPk,
                          realm_pubkey  := Realm} = Delegation,
                       DaemonPk, HostPk, Realm, HRec, FindFn) ->
    delegation_time_check(Delegation, DaemonPk, HRec, FindFn);
delegation_field_check(_OtherDel, _DaemonPk, _HostPk, _Realm, _HRec, _FindFn) ->
    {error, bad_delegation}.

delegation_time_check(#{not_after_ms := NotAfter} = Delegation, DaemonPk, HRec, FindFn) ->
    Now = erlang:system_time(millisecond),
    delegation_time_result(Now < NotAfter, Delegation, DaemonPk, HRec, FindFn).

delegation_time_result(false, _Delegation, _DaemonPk, _HRec, _FindFn) ->
    {error, delegation_expired};
delegation_time_result(true, Delegation, DaemonPk, HRec, FindFn) ->
    case macula_record:verify_host_delegation(Delegation) of
        {ok, _}    -> hosted_endpoint_lookup(HRec, DaemonPk, FindFn);
        {error, _} -> {error, bad_delegation}
    end.

hosted_endpoint_lookup(HRec, DaemonPk, FindFn) ->
    HostPk = macula_record:key(HRec),
    finish_hosted_endpoint(FindFn(endpoint_key(HostPk)), HostPk, DaemonPk).

finish_hosted_endpoint({error, _} = E, _HostPk, _DaemonPk) -> E;
finish_hosted_endpoint({ok, ERec}, HostPk, DaemonPk) ->
    case macula_record:verify(ERec) of
        {ok, _} ->
            case macula_record:key(ERec) of
                HostPk ->
                    extract_endpoint_payload(macula_record:payload(ERec),
                                             HostPk,
                                             macula_record:expires_at(ERec),
                                             DaemonPk);
                _Other ->
                    {error, bad_address_binding}
            end;
        {error, _} ->
            {error, bad_signature}
    end.

%% Pull the embedded delegation back out of the CBOR map representation.
parse_delegation(#{ {text, <<"d">>}  := DaemonPk,
                    {text, <<"h">>}  := HostPk,
                    {text, <<"r">>}  := Realm,
                    {text, <<"nb">>} := NB,
                    {text, <<"na">>} := NA,
                    {text, <<"s">>}  := Sig })
  when byte_size(DaemonPk) =:= 32, byte_size(HostPk) =:= 32,
       byte_size(Realm) =:= 32, byte_size(Sig) =:= 64,
       is_integer(NB), is_integer(NA) ->
    #{daemon_pubkey => DaemonPk,
      host_pubkey   => HostPk,
      realm_pubkey  => Realm,
      not_before_ms => NB,
      not_after_ms  => NA,
      daemon_sig    => Sig};
parse_delegation(_Other) ->
    undefined.
