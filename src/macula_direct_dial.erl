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
%%% layer cannot: (1) every candidate `procedure_advertisement' must
%%% carry a valid Ed25519 signature before its `serving_station' is
%%% trusted at all — otherwise any identity able to sign SOME record
%%% could name a real, legitimate station as the server for a
%%% procedure it has no authority over, and the station_endpoint check
%%% below would still pass (it only proves we reached the station we
%%% were told to reach, not that whoever told us so was authorized to);
%%% (2) the resolved `station_endpoint' must be signed by the station
%%% itself (`verify_and_build/2', unchanged). The actual QUIC dial
%%% trusts NEITHER the TLS certificate (`pin_tls_cert => false' — a
%%% production station's TLS is terminated by an unrelated PKI, e.g.
%%% Let's Encrypt, so pinning the cert key can never succeed) NOR
%%% nothing (`verify => none' alone would trust whoever answers): trust
%%% is enforced at the application layer instead, via the
%%% cryptographically signed CONNECT/HELLO handshake
%%% (`macula_peering_conn:bind_peer_identity/2') checked against the
%%% exact pubkey the signed DHT chain above resolved.
%%%
%%% A third check is available but OPT-IN, via `Opts' (managed realms
%%% only — see `macula_record:verify_advertisement_cert_chain/3'):
%%% `verify_cert_chain => {RealmCaPem, Org}' additionally requires the
%%% advertisement's embedded X.509 service-cert chain to verify to
%%% `RealmCaPem' under `Org' (Slice 7c Direction B), proving the
%%% ADVERTISER — not just the station it names — is an org/realm-
%%% authorized identity. Without it, (1) above still rejects an
%%% unsigned or badly-signed advertisement, but not one signed by an
%%% unauthorized (if self-consistent) identity; unmanaged realms have
%%% no realm CA to check against, so this stays opt-in rather than
%%% mandatory.
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
%%% cert-chain machinery above — content's threat model genuinely
%%% differs from RPC's. An RPC reply is opaque and unverifiable except
%%% by trusting whoever answered, so proving the ADVERTISER is
%%% authorized matters. Content is content-addressed: the fetched bytes
%%% are independently re-hashed against the MCID client-side
%%% (`macula:verify_block_hash/2' for single-block,
%%% `macula_manifest:verify/2' for chunked) regardless of which peer
%%% served them, so a rogue or unauthorized announcer can at most
%%% refuse to serve or waste a dial — it cannot make a caller accept
%%% content that does not hash to the MCID it asked for. What still
%%% matters, and is still mandatory, is (1)'s analogue for
%%% `content_announcement': the signer must equal the `announcer_node'
%%% it claims (`macula:decode_provider/1'), so an attacker cannot at
%%% least misattribute who is claiming to serve what.
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

%% @doc As `call/6' with no cert-chain verification.
-spec call(macula:pool(), macula:realm(), macula:procedure(), term(),
          pos_integer()) -> {ok, term()} | {error, term()}.
call(Pool, Realm, Procedure, Payload, TimeoutMs) ->
    call(Pool, Realm, Procedure, Payload, TimeoutMs, #{}).

%% @doc Resolve `Procedure''s provider and call it there directly. Same
%% return shape as `macula:call/5'; resolve failures surface as
%% `{error, {unresolved, Reason}}' so a caller can tell "nobody has
%% advertised this via direct-dial yet" apart from a real call failure.
%% `Opts' may include `verify_cert_chain => {RealmCaPem, Org}' — see
%% the module doc's "Trust model" section.
-spec call(macula:pool(), macula:realm(), macula:procedure(), term(),
          pos_integer(), map()) -> {ok, term()} | {error, term()}.
call(Pool, Realm, Procedure, Payload, TimeoutMs, Opts) ->
    case resolve_dial_url(Pool, Realm, Procedure, Opts) of
        {ok, {Station, DialUrl}} ->
            %% See the module doc's "Trust model" section: trust is
            %% pinned to the exact pubkey the signed DHT chain resolved,
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

%% @doc As `call_stream/6' with no cert-chain verification.
-spec call_stream(macula:pool(), macula:realm(), macula:procedure(), term(),
                  map()) -> {ok, macula:stream()} | {error, term()}.
call_stream(Pool, Realm, Procedure, Args, StreamOpts) ->
    call_stream(Pool, Realm, Procedure, Args, StreamOpts, #{}).

%% @doc As `call/6', but opens a stream (`macula:call_stream_station/6''s
%% shape) instead of making a single-reply call, built on the exact
%% same resolve+trust machinery — see the module doc. `StreamOpts' is
%% forwarded to `call_stream_station/6' alongside the resolved trust
%% override (`mode', `owner', `dial_timeout_ms', etc); `Opts' is the
%% resolve-side `verify_cert_chain' opt, same as `call/6'.
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

%% @doc As `publish_advertisement/5' with no cert chain embedded.
-spec publish_advertisement(macula:pool(), macula:realm(), macula:procedure(),
                            macula_identity:key_pair()) -> ok | {error, term()}.
publish_advertisement(Pool, Realm, Procedure, Identity) ->
    publish_advertisement(Pool, Realm, Procedure, Identity, #{}).

%% @doc Publish a signed `procedure_advertisement' for `Procedure',
%% naming `Pool''s currently-connected station as the serving station.
%% `Identity' signs it — the pool itself has no identity to sign with,
%% so the caller supplies one (reuse the same one across re-advertises;
%% a fresh identity per call is valid but needless DHT churn). `Opts'
%% may include `cert_chain => ChainPem' (leaf ++ org CA, PEM) so a
%% verifying consumer's `verify_cert_chain' opt (see `call/6') can
%% check this advertiser's org/realm authorization — Slice 7c
%% Direction B, managed realms only.
-spec publish_advertisement(macula:pool(), macula:realm(), macula:procedure(),
                            macula_identity:key_pair(), map()) ->
    ok | {error, term()}.
publish_advertisement(Pool, Realm, Procedure, Identity, Opts) ->
    case macula:links(Pool) of
        {ok, Links} -> on_links(connected_station(Links), Pool, Realm,
                                Procedure, Identity, Opts);
        {error, _} = Error -> Error
    end.

on_links({ok, Station}, Pool, Realm, Procedure, Identity, Opts) ->
    AdvPub = macula_identity:public(Identity),
    Uri = discovery_uri(Realm, Procedure),
    Ad = macula_record:sign(
           macula_record:procedure_advertisement(AdvPub, Uri, Station,
                                                 adv_opts(Opts)),
           Identity),
    macula:put_record(Pool, Ad);
on_links({error, _} = Error, _Pool, _Realm, _Procedure, _Identity, _Opts) ->
    Error.

%% Forwards each opt `procedure_advertisement/4' actually recognizes,
%% independently — a caller passing `ttl_ms' alone (no `cert_chain') used
%% to get `#{}' back, silently dropping `ttl_ms' too, because the old
%% single-clause match only ever produced `cert_chain' or nothing. Found
%% while wiring a proportioned `ttl_ms' through `advertise_direct/7' from
%% `hecate_om_capabilities'; `procedure_advertisement/4' already reads
%% `ttl_ms' from its own Opts (falls back to `?DEFAULT_TTL_MS'), so the
%% bug was purely in this forwarder never passing it through.
adv_opts(Opts) ->
    maps:merge(cert_chain_opt(Opts), ttl_ms_opt(Opts)).

cert_chain_opt(#{cert_chain := ChainPem}) when is_binary(ChainPem) ->
    #{cert_chain => ChainPem};
cert_chain_opt(_Opts) ->
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
%% "Content" section for why this has no `verify_cert_chain'-equivalent
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
-spec put_content(macula:pool(), macula_identity:pubkey(), binary(),
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
-spec resolve_station_endpoint(macula:pool(), macula_identity:pubkey()) ->
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
    Uri = discovery_uri(Realm, Procedure),
    Key = macula_record:procedure_key(Uri),
    case find_records_retry(Pool, Key, ?RESOLVE_RETRIES) of
        {ok, [_ | _] = Recs} ->
            resolve_station(Pool, trusted_advertisements(Recs, Opts));
        {ok, []} -> {error, procedure_not_advertised};
        {error, _} = Error -> Error
    end.

%% Only a record that passes trust filtering is a candidate at all —
%% see the module doc's "Trust model" section. Base signature check is
%% mandatory; `verify_cert_chain' additionally requires the embedded
%% X.509 chain when the caller opted in.
trusted_advertisements(Recs, Opts) ->
    [Rec || Rec <- Recs, advertisement_trusted(Rec, Opts)].

advertisement_trusted(Rec, #{verify_cert_chain := {RealmCaPem, Org}}) ->
    ok =:= macula_record:verify_advertisement_cert_chain(RealmCaPem, Rec, Org);
advertisement_trusted(Rec, _Opts) ->
    signature_ok(macula_record:verify(Rec)).

signature_ok({ok, _})    -> true;
signature_ok({error, _}) -> false.

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
    on_endpoint_verified(verify_and_build(Station, EpRec), Pool, Station, N);
on_endpoint_fetch({error, not_found}, Pool, Station, N) ->
    timer:sleep(?RESOLVE_RETRY_MS),
    resolve_endpoint(Pool, Station, N - 1).

on_endpoint_verified({ok, _} = Ok, _Pool, _Station, _N) -> Ok;
on_endpoint_verified({error, expired}, Pool, Station, N) ->
    timer:sleep(?RESOLVE_RETRY_MS),
    resolve_endpoint(Pool, Station, N - 1);
on_endpoint_verified({error, _} = Error, _Pool, _Station, _N) -> Error.

%% The `station_endpoint' record for `Station' must be SIGNED BY
%% `Station' itself (macula_station_announcer publishes it self-signed
%% — the station describing its own reachable address). Checking the
%% signature AND that the signer is exactly `Station', not just any
%% valid signature, is what makes pinning `expected_node_id => Station'
%% on the dial meaningful: without it, a record merely stored under the
%% right DHT key (but signed, or not, by someone else) would still be
%% trusted, and per-call pinning would authenticate the wrong thing.
verify_and_build(Station, #{key := Station} = EpRec) ->
    case macula_record:verify(EpRec) of
        {ok, Verified} -> build_dial_url(Station, Verified);
        {error, _} = Error -> Error
    end;
verify_and_build(_Station, _EpRec) ->
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

%% No `Org' segment in the discovery URI: `Org' is only consulted
%% post-resolve, as the `verify_cert_chain' opt's expected leaf-cert
%% organization (see `advertisement_trusted/2') — it does not affect
%% how a `procedure_advertisement' is keyed or found.
discovery_uri(Realm, Procedure) ->
    <<(binary:encode_hex(Realm))/binary, "/", Procedure/binary>>.
