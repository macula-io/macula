%%%-------------------------------------------------------------------
%%% @doc Direct-dial resolve-and-call: shared internals for
%%% `macula_request'/`macula_response', `macula_streamer'/
%%% `macula_stream_sink', and `macula_feeder'/`macula_download'.
%%%
%%% Not a public API on its own — `macula_request:start_link_direct/6,7,8',
%%% `macula_response:advertise_direct/6,7',
%%% `macula_stream_sink:start_link_direct/5,6',
%%% `macula_streamer:advertise_direct/6,7',
%%% `macula_download:start_link_direct/4,5', and
%%% `macula_feeder:start_link_direct/5,6' are the entry points.
%%% Factored out because RPC, streaming, and content-download all need
%%% the same shape of resolve sequence (`find_records' -> verify -> read
%%% the record -> build a `quic://' dial URL, retrying past DHT
%%% propagation lag throughout). Streaming and RPC share the IDENTICAL
%%% discovery mechanism — a `procedure_advertisement' does not
%%% distinguish RPC from streaming, only the eventual dial
%%% (`call_station/7' vs `call_stream_station/6') does — so
%%% `publish_advertisement/4,5' is reused as-is by both providers, and
%%% `call/6'/`call_stream/6' share the same `resolve_dial_url/4' and
%%% "Trust model" below. Content has no publish step here at all — see
%%% "Content" further down.
%%%
%%% The resolve side retries: a record just published on the provider's
%%% station has not necessarily replicated to the caller's station yet,
%%% and treating the first miss as failure would make every direct-dial
%%% call racy against DHT propagation lag. `macula_station_cert_chain_SUITE'
%%% (macula-station) proved this exact resolve+dial sequence works
%%% cross-station against the live fleet; this module is that sequence,
%%% lifted out of the test and made reusable.
%%%
%%% == Trust model ==
%%%
%%% Two independent checks, both mandatory, cover what the QUIC/TLS
%%% layer cannot. (1) Every candidate `procedure_advertisement' arrives
%%% verified under the node's crypto profile (`macula:find_records/2'),
%%% and is trusted only when it advertises the resolved procedure in the
%%% resolved realm and its provider authorization verifies
%%% (`macula_record:verify_authorization/3', D25 item 6): a procedure
%%% with an org namespace needs an authorization for that org, and a
%%% procedure without one carries none. Otherwise any node able to sign
%%% SOME record could name a real, legitimate station as the server for
%%% a procedure it has no authority over, and the station_endpoint check
%%% below would still pass (it only proves we reached the station we
%%% were told to reach, not that whoever told us so was authorized to).
%%% (2) The resolved `station_endpoint' must be signed by the station
%%% itself (`station_signed_endpoint/2'). The actual QUIC dial
%%% trusts NEITHER the TLS certificate (`pin_tls_cert => false' — a
%%% production station's TLS is terminated by an unrelated PKI, e.g.
%%% Let's Encrypt, so pinning the cert key can never succeed) NOR
%%% nothing (`verify => none' alone would trust whoever answers): trust
%%% is enforced at the application layer instead, via the
%%% cryptographically signed CONNECT/HELLO handshake
%%% (the peer identity binding in `macula_peering_conn') checked against the
%%% exact node_id the signed DHT chain above resolved.
%%%
%%% An authorization verifies against the realm trust in `Opts',
%%% `realm_trust => #{realm_key => RealmKey, realm_ca => RealmCaPem}':
%%% the realm key as carried, for an org directory with a procedure
%%% delegation, and the realm CA in PEM, for a certificate chain.
%%% Without the realm trust its form needs, an advertisement for an org
%%% namespaced procedure is never trusted.
%%%
%%% == Content ==
%%%
%%% `put_content/4' has no resolve step at all — unlike a GET, a PUT
%%% names its OWN target: the caller already knows (or is choosing)
%%% which station to seed, so it takes `Station' directly and resolves
%%% only that station's own `station_endpoint' (`resolve_station_endpoint/2',
%%% the same machinery `call/6' uses internally for `serving_station').
%%%
%%% `get_content/3' resolves and fetches deliberately WITHOUT the
%%% authorization check above: content's threat model genuinely
%%% differs from RPC's. An RPC reply is opaque and unverifiable except
%%% by trusting whoever answered, so proving the ADVERTISER is
%%% authorized matters. Content is content-addressed, and the fetched
%%% bytes are checked against the MCID client-side regardless of which
%%% peer served them. Single-block content is re-hashed against the MCID
%%% (in `macula_content_transfer'). For chunked content
%%% the fetched manifest is used only if its MCID, recomputed from its
%%% canonical fields, is the one requested (`macula_manifest:verify_mcid/2');
%%% each chunk is then hashed against that manifest, and the reassembled
%%% bytes are checked against its size and root hash
%%% (`macula_manifest:verify/2'). A rogue or unauthorized announcer can at
%%% most refuse to serve or waste a dial; it cannot make a caller accept
%%% content that does not match the MCID it asked for. What still
%%% matters, and is still mandatory, is (1)'s analogue for
%%% `content_announcement': the signer must equal the `announcer_node'
%%% it claims (checked by `macula:find_content_providers/2'), so an
%%% attacker cannot at least misattribute who is claiming to serve what.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_direct_dial).

-export([call/5, call/6, call_stream/5, call_stream/6,
        publish_advertisement/4, publish_advertisement/5,
        get_content/3, resolve_content_provider/2,
        put_content/4, resolve_station_endpoint/2]).

-ifdef(TEST).
%% Exports for unit tests — pure helpers that are otherwise private.
-export([advertisement_trusted/2, adv_opts/1]).
-endif.

-define(RESOLVE_RETRIES, 50).
-define(RESOLVE_RETRY_MS, 100).
-define(TYPE_PROCEDURE_ADVERTISEMENT, 16#06).
-define(TYPE_STATION_ENDPOINT, 16#12).

%% @doc As `call/6' with no realm trust.
-spec call(macula:pool(), macula:realm(), macula:procedure(), term(),
          pos_integer()) -> {ok, term()} | {error, term()}.
call(Pool, Realm, Procedure, Payload, TimeoutMs) ->
    call(Pool, Realm, Procedure, Payload, TimeoutMs, #{}).

%% @doc Resolve `Procedure''s provider and call it there directly. Same
%% return shape as `macula:call/5'; resolve failures surface as
%% `{error, {unresolved, Reason}}' so a caller can tell "nobody has
%% advertised this via direct-dial yet" apart from a real call failure.
%% `Opts' may include `realm_trust', the realm trust an org namespaced
%% procedure's authorization is checked against: see the module doc's
%% "Trust model" section.
-spec call(macula:pool(), macula:realm(), macula:procedure(), term(),
          pos_integer(), map()) -> {ok, term()} | {error, term()}.
call(Pool, Realm, Procedure, Payload, TimeoutMs, Opts) ->
    case resolve_dial_url(Pool, Realm, Procedure, Opts) of
        {ok, {Station, DialUrl}} ->
            %% See the module doc's "Trust model" section: trust is
            %% pinned to the exact node_id the signed DHT chain resolved,
            %% but enforced at the application layer, not the TLS
            %% layer — a production station's TLS certificate has no
            %% relationship to its macula identity.
            macula:call_station(Pool, DialUrl, Realm, Procedure, Payload,
                                TimeoutMs, #{expected_node_id => Station,
                                             pin_tls_cert => false,
                                             verify => none});
        {error, Reason} ->
            {error, {unresolved, Reason}}
    end.

%% @doc As `call_stream/6' with no realm trust.
-spec call_stream(macula:pool(), macula:realm(), macula:procedure(), term(),
                  map()) -> {ok, macula:stream()} | {error, term()}.
call_stream(Pool, Realm, Procedure, Args, StreamOpts) ->
    call_stream(Pool, Realm, Procedure, Args, StreamOpts, #{}).

%% @doc As `call/6', but opens a stream (`macula:call_stream_station/6''s
%% shape) instead of making a single-reply call, built on the exact
%% same resolve+trust machinery — see the module doc. `StreamOpts' is
%% forwarded to `call_stream_station/6' alongside the resolved trust
%% override (`mode', `owner', `dial_timeout_ms', etc); `Opts' is the
%% resolve-side `realm_trust' opt, same as `call/6'.
-spec call_stream(macula:pool(), macula:realm(), macula:procedure(), term(),
                  map(), map()) -> {ok, macula:stream()} | {error, term()}.
call_stream(Pool, Realm, Procedure, Args, StreamOpts, Opts) ->
    case resolve_dial_url(Pool, Realm, Procedure, Opts) of
        {ok, {Station, DialUrl}} ->
            macula:call_stream_station(Pool, DialUrl, Realm, Procedure, Args,
                                       StreamOpts#{expected_node_id => Station,
                                                   pin_tls_cert => false,
                                                   verify => none});
        {error, Reason} ->
            {error, {unresolved, Reason}}
    end.

%% @doc As `publish_advertisement/5' with no provider authorization.
-spec publish_advertisement(macula:pool(), macula:realm(), macula:procedure(),
                            macula_node_keys:node_key()) -> ok | {error, term()}.
publish_advertisement(Pool, Realm, Procedure, NodeIdentity) ->
    publish_advertisement(Pool, Realm, Procedure, NodeIdentity, #{}).

%% @doc Publish a signed `procedure_advertisement' for `Procedure',
%% naming `Pool''s currently-connected station as the serving station.
%% `NodeIdentity', the provider's node identity key, signs it, and its
%% node_id is the advertiser (reuse the same key across re-advertises;
%% a fresh key per call is valid but needless DHT churn). `Opts' may
%% include `authorization', the provider authorization an org
%% namespaced procedure needs (D25 item 6), as
%% `#{org_directory => Wire, procedure_delegation => Wire}' or
%% `#{certificate_chain => [Der]}', and `ttl_ms'.
-spec publish_advertisement(macula:pool(), macula:realm(), macula:procedure(),
                            macula_node_keys:node_key(), map()) ->
    ok | {error, term()}.
publish_advertisement(Pool, Realm, Procedure, NodeIdentity, Opts) ->
    case macula:links(Pool) of
        {ok, Links} -> on_links(connected_station(Links), Pool, Realm,
                                Procedure, NodeIdentity, Opts);
        {error, _} = Error -> Error
    end.

on_links({ok, Station}, Pool, Realm, Procedure, NodeIdentity, Opts) ->
    Advertiser = macula_node_keys:key_id(NodeIdentity),
    Ad = macula_record:sign(
           macula_record:procedure_advertisement(Advertiser, Realm, Procedure, Station,
                                                 adv_opts(Opts)),
           NodeIdentity),
    macula:put_record(Pool, Ad);
on_links({error, _} = Error, _Pool, _Realm, _Procedure, _NodeIdentity, _Opts) ->
    Error.

%% Forwards each opt `procedure_advertisement/5' actually recognizes,
%% independently: a caller passing `ttl_ms' alone once got `#{}' back,
%% silently dropping `ttl_ms' too, because an older single-clause match
%% only ever produced one opt or nothing. Found while wiring a
%% proportioned `ttl_ms' through `advertise_direct/7' from
%% `hecate_om_capabilities'; `procedure_advertisement/5' reads `ttl_ms'
%% from its own Opts, so the bug was purely in this forwarder.
adv_opts(Opts) ->
    maps:merge(authorization_opt(Opts), ttl_ms_opt(Opts)).

authorization_opt(#{authorization := Authorization}) when is_map(Authorization) ->
    #{authorization => Authorization};
authorization_opt(_Opts) ->
    #{}.

ttl_ms_opt(#{ttl_ms := Ttl}) when is_integer(Ttl), Ttl > 0 ->
    #{ttl_ms => Ttl};
ttl_ms_opt(_Opts) ->
    #{}.

%% The first CONNECTED link with a known peer pubkey — `links/1' can
%% include configured-but-not-yet-spawned or dead entries, and taking
%% one of those blindly would publish an advertisement pointing at a
%% station this pool cannot currently prove it can reach.
connected_station(Links) ->
    case [S || #{connected := true, node_id := S} <- Links, is_binary(S)] of
        [Station | _] -> {ok, Station};
        [] -> {error, no_healthy_link}
    end.

%% @doc Resolve `MCID''s provider via its signed `content_announcement'
%% and fetch it there directly, retrying past DHT propagation lag the
%% same way `call/6' does for procedures. Same return shape as
%% `macula:get_content/2'; resolve failures surface as
%% `{error, {unresolved, Reason}}'. `TimeoutMs' bounds only the QUIC
%% handshake if a fresh link must be dialed
%% (`macula:get_content_station/5') — the underlying block/manifest
%% transfer has its own internal timeouts. See the module doc's
%% "Content" section for why this has no `realm_trust'-equivalent
%% opt, unlike `call/6'. Only chunked content is discoverable this way
%% — see `macula:find_content_providers/2'.
-spec get_content(macula:pool(), macula:mcid(), pos_integer()) ->
    {ok, binary()} | {error, term()}.
get_content(Pool, MCID, TimeoutMs) ->
    case resolve_content_provider(Pool, MCID) of
        {ok, #{announcer_node := Node, endpoint := Endpoint}} ->
            macula:get_content_station(Pool, Endpoint, MCID, TimeoutMs,
                                       #{expected_node_id => Node,
                                         pin_tls_cert => false,
                                         verify => none});
        {error, Reason} ->
            {error, {unresolved, Reason}}
    end.

%% @doc Resolve `MCID''s provider via a signed `content_announcement',
%% retrying past a not-yet-replicated announcement. Returns the first
%% candidate `macula:find_content_providers/2' finds — that function
%% already discards unsigned or signer-mismatched announcements before
%% this ever sees them.
-spec resolve_content_provider(macula:pool(), macula:mcid()) ->
    {ok, map()} | {error, term()}.
resolve_content_provider(Pool, MCID) ->
    resolve_content_provider(Pool, MCID, ?RESOLVE_RETRIES).

resolve_content_provider(_Pool, _MCID, 0) ->
    {error, content_not_announced};
resolve_content_provider(Pool, MCID, N) ->
    on_providers_found(macula:find_content_providers(Pool, MCID), Pool, MCID, N).

on_providers_found({ok, [Provider | _]}, _Pool, _MCID, _N) ->
    {ok, Provider};
on_providers_found({ok, []}, Pool, MCID, N) ->
    timer:sleep(?RESOLVE_RETRY_MS),
    resolve_content_provider(Pool, MCID, N - 1);
on_providers_found({error, _} = Error, _Pool, _MCID, _N) ->
    Error.

%% @doc Resolve `Station''s dialable `quic://' URL from its own signed
%% `station_endpoint' record and put `Bytes' there directly. Same
%% return shape as `macula:put_content/2'; resolve failures surface as
%% `{error, {unresolved, Reason}}'. `TimeoutMs' bounds only the QUIC
%% handshake if a fresh link must be dialed
%% (`macula:put_content_station/5') — the underlying block/manifest
%% transfer has its own internal timeouts.
-spec put_content(macula:pool(), <<_:256>>, binary(),
                  pos_integer()) -> {ok, macula:mcid()} | {error, term()}.
put_content(Pool, Station, Bytes, TimeoutMs) ->
    case resolve_station_endpoint(Pool, Station) of
        {ok, DialUrl} ->
            macula:put_content_station(Pool, DialUrl, Bytes, TimeoutMs,
                                       #{expected_node_id => Station,
                                         pin_tls_cert => false,
                                         verify => none});
        {error, Reason} ->
            {error, {unresolved, Reason}}
    end.

%% @doc Resolve `Station''s dialable `quic://' URL from its own signed
%% `station_endpoint' record, verifying the record's signer is exactly
%% `Station' and retrying past a stale/expired replica — the same
%% discipline `call/6' applies internally once it has resolved a
%% procedure's `serving_station'.
-spec resolve_station_endpoint(macula:pool(), <<_:256>>) ->
    {ok, binary()} | {error, term()}.
resolve_station_endpoint(Pool, Station) ->
    case resolve_endpoint(Pool, Station) of
        {ok, {Station, DialUrl}} -> {ok, DialUrl};
        {error, _} = Error -> Error
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

resolve_dial_url(Pool, Realm, Procedure, Opts) ->
    Key = macula_record:procedure_key(Realm, Procedure),
    case find_records_retry(Pool, Key, ?RESOLVE_RETRIES) of
        {ok, [_ | _] = Recs} ->
            resolve_station(Pool, trusted_advertisements(Recs, trust(Realm, Procedure, Opts)));
        {ok, []} -> {error, procedure_not_advertised};
        {error, _} = Error -> Error
    end.

%% What an advertisement for `Procedure' in `Realm' is checked against:
%% the node's crypto profile, under which `macula:find_records/2' just
%% verified the records, and the realm trust the caller holds.
trust(Realm, Procedure, Opts) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    RealmTrust = maps:with([realm_key, realm_ca], maps:get(realm_trust, Opts, #{})),
    RealmTrust#{realm => Realm, procedure => Procedure, profile => Profile}.

%% Only an advertisement that passes the trust check is a candidate at
%% all: see the module doc's "Trust model" section.
trusted_advertisements(Recs, Trust) ->
    [Rec || Rec <- Recs, advertisement_trusted(Rec, Trust)].

%% A verified advertisement is trusted when it advertises the resolved
%% procedure in the resolved realm and its provider authorization
%% verifies against the realm trust (D25 item 6).
advertisement_trusted(#{type := ?TYPE_PROCEDURE_ADVERTISEMENT} = Rec,
                      #{realm := Realm, procedure := Procedure} = Trust) ->
    for_procedure(macula_record:read_procedure_advertisement(Rec), Rec, Realm, Procedure, Trust);
advertisement_trusted(_OtherRecord, _Trust) ->
    false.

for_procedure(#{realm_id := Realm, procedure := Procedure}, Rec, Realm, Procedure, Trust) ->
    ok =:= macula_record:verify_authorization(Rec, maps:without([realm, procedure], Trust),
                                              erlang:system_time(millisecond));
for_procedure(_OtherProcedure, _Rec, _Realm, _Procedure, _Trust) ->
    false.

resolve_station(_Pool, []) ->
    {error, no_trusted_advertisement};
resolve_station(Pool, [Rec | Rest]) ->
    case macula_record:read_procedure_advertisement(Rec) of
        #{serving_station := Station} ->
            resolve_endpoint(Pool, Station);
        _ ->
            resolve_station(Pool, Rest)
    end.

%% Retries past a resolved-but-stale record, not just an absent one:
%% the DHT can hand back a replica that hasn't been evicted or
%% refreshed yet even though the station's own current publish is
%% live, and giving up on the first stale hit would make an otherwise
%% healthy station unreachable via direct-dial until that one replica
%% happens to age out on its own.
resolve_endpoint(Pool, Station) ->
    resolve_endpoint(Pool, Station, ?RESOLVE_RETRIES).

resolve_endpoint(_Pool, _Station, 0) ->
    {error, station_endpoint_not_found};
resolve_endpoint(Pool, Station, N) ->
    Key = macula_record:station_endpoint_key(Station),
    on_endpoint_fetch(find_record_retry(Pool, Key, 1), Pool, Station, N).

on_endpoint_fetch({ok, EpRec}, Pool, Station, N) ->
    on_endpoint_verified(station_signed_endpoint(Station, EpRec), Pool, Station, N);
on_endpoint_fetch({error, not_found}, Pool, Station, N) ->
    timer:sleep(?RESOLVE_RETRY_MS),
    resolve_endpoint(Pool, Station, N - 1).

on_endpoint_verified({ok, _} = Ok, _Pool, _Station, _N) -> Ok;
on_endpoint_verified({error, _} = Error, _Pool, _Station, _N) -> Error.

%% The `station_endpoint' record for `Station' must be SIGNED BY
%% `Station' itself (macula_station_announcer publishes it self-signed:
%% the station describing its own reachable address). The record arrives
%% verified under the node's crypto profile (`macula:find_record/2');
%% checking that its signer's node_id is exactly `Station', not just any
%% valid signer, is what makes pinning `expected_node_id => Station' on
%% the dial meaningful: without it, a record merely stored under the
%% right DHT key but signed by someone else would still be trusted, and
%% per-call pinning would authenticate the wrong thing. A stale or
%% refused replica never gets here: `find_record_retry/3' retries past it.
station_signed_endpoint(Station, #{type := ?TYPE_STATION_ENDPOINT, key_id := Station} = EpRec) ->
    build_dial_url(Station, EpRec);
station_signed_endpoint(_Station, _EpRec) ->
    {error, station_endpoint_signer_mismatch}.

build_dial_url(Station, EpRec) ->
    case macula_record:read_station_endpoint(EpRec) of
        #{quic_port := Port, host_advertised := [Host | _]} ->
            {ok, {Station, <<"quic://[", Host/binary, "]:",
                            (integer_to_binary(Port))/binary>>}};
        _ ->
            {error, malformed_station_endpoint}
    end.

find_records_retry(_Pool, _Key, 0) -> {ok, []};
find_records_retry(Pool, Key, N) ->
    on_find_records(macula:find_records(Pool, Key), Pool, Key, N).

on_find_records({ok, [_ | _] = Recs}, _Pool, _Key, _N) -> {ok, Recs};
on_find_records(_Other, Pool, Key, N) ->
    timer:sleep(?RESOLVE_RETRY_MS),
    find_records_retry(Pool, Key, N - 1).

find_record_retry(_Pool, _Key, 0) -> {error, not_found};
find_record_retry(Pool, Key, N) ->
    on_find_record(macula:find_record(Pool, Key), Pool, Key, N).

on_find_record({ok, Rec}, _Pool, _Key, _N) -> {ok, Rec};
on_find_record(_Other, Pool, Key, N) ->
    timer:sleep(?RESOLVE_RETRY_MS),
    find_record_retry(Pool, Key, N - 1).
