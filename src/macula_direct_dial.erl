%%%-------------------------------------------------------------------
%%% @doc Direct-dial resolve-and-call: shared internals for
%%% `macula_request'/`macula_response' (and, when they grow a
%%% direct-dial mode, `macula_streamer'/`macula_stream_sink').
%%%
%%% Not a public API on its own — `macula_request:start_link_direct/6,7'
%%% and `macula_response:advertise_direct/6' are the entry points.
%%% Factored out because both the RPC and streaming supervised pairs
%%% need the identical resolve sequence (`find_records' ->
%%% `read_procedure_advertisement' -> `find_record' ->
%%% `read_station_endpoint' -> build a `quic://' dial URL), and the
%%% identical provider-side publish (sign + `put_record' a
%%% `procedure_advertisement').
%%%
%%% The resolve side retries: a record just published on the provider's
%%% station has not necessarily replicated to the caller's station yet,
%%% and treating the first miss as failure would make every direct-dial
%%% call racy against DHT propagation lag. `macula_station_cert_chain_SUITE'
%%% (macula-station) proved this exact resolve+dial sequence works
%%% cross-station against the live fleet; this module is that sequence,
%%% lifted out of the test and made reusable.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_direct_dial).

-export([call/5, publish_advertisement/4]).

-define(RESOLVE_RETRIES, 50).
-define(RESOLVE_RETRY_MS, 100).

%% @doc Resolve `Procedure''s provider and call it there directly. Same
%% return shape as `macula:call/5'; resolve failures surface as
%% `{error, {unresolved, Reason}}' so a caller can tell "nobody has
%% advertised this via direct-dial yet" apart from a real call failure.
-spec call(macula:pool(), macula:realm(), macula:procedure(), term(),
          pos_integer()) -> {ok, term()} | {error, term()}.
call(Pool, Realm, Procedure, Payload, TimeoutMs) ->
    case resolve_dial_url(Pool, Realm, Procedure) of
        {ok, {Station, DialUrl}} ->
            %% Pin trust to the exact pubkey the signed DHT chain
            %% resolved, per-call — see macula_client:call_station/8.
            %% Never falls back to the pool's default `webpki' (which
            %% cannot validate a bare-IP dial against a hostname cert)
            %% and never uses `verify => none' (which would trust
            %% whatever answers, defeating the point of having
            %% resolved a specific, signed identity at all).
            macula:call_station(Pool, DialUrl, Realm, Procedure, Payload,
                                TimeoutMs, #{expected_node_id => Station});
        {error, Reason} ->
            {error, {unresolved, Reason}}
    end.

%% @doc Publish a signed `procedure_advertisement' for `Procedure',
%% naming `Pool''s currently-connected station as the serving station.
%% `Identity' signs it — the pool itself has no identity to sign with,
%% so the caller supplies one (reuse the same one across re-advertises;
%% a fresh identity per call is valid but needless DHT churn).
-spec publish_advertisement(macula:pool(), macula:realm(), macula:procedure(),
                            macula_identity:key_pair()) -> ok | {error, term()}.
publish_advertisement(Pool, Realm, Procedure, Identity) ->
    case macula:links(Pool) of
        {ok, Links} -> on_links(connected_station(Links), Pool, Realm,
                                Procedure, Identity);
        {error, _} = Error -> Error
    end.

on_links({ok, Station}, Pool, Realm, Procedure, Identity) ->
    AdvPub = macula_identity:public(Identity),
    Uri = discovery_uri(Realm, Procedure),
    Ad = macula_record:sign(
           macula_record:procedure_advertisement(AdvPub, Uri, Station),
           Identity),
    macula:put_record(Pool, Ad);
on_links({error, _} = Error, _Pool, _Realm, _Procedure, _Identity) ->
    Error.

%% The first CONNECTED link with a known peer pubkey — `links/1' can
%% include configured-but-not-yet-spawned or dead entries, and taking
%% one of those blindly would publish an advertisement pointing at a
%% station this pool cannot currently prove it can reach.
connected_station(Links) ->
    case [S || #{connected := true, node_id := S} <- Links, is_binary(S)] of
        [Station | _] -> {ok, Station};
        [] -> {error, no_healthy_link}
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

resolve_dial_url(Pool, Realm, Procedure) ->
    Uri = discovery_uri(Realm, Procedure),
    Key = macula_record:procedure_key(Uri),
    case find_records_retry(Pool, Key, ?RESOLVE_RETRIES) of
        {ok, [AdRec | _]} -> resolve_station(Pool, AdRec);
        {ok, []} -> {error, procedure_not_advertised};
        {error, _} = Error -> Error
    end.

resolve_station(Pool, AdRec) ->
    case macula_record:read_procedure_advertisement(AdRec) of
        #{serving_station := Station} ->
            resolve_endpoint(Pool, Station);
        _ ->
            {error, malformed_advertisement}
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

%% No `Org'/cert-chain segment: that trust layer is opt-in (Slice 7c,
%% managed realms only) and orthogonal to basic direct-dial, which
%% plain `call_station/6' itself does not require either.
discovery_uri(Realm, Procedure) ->
    <<(binary:encode_hex(Realm))/binary, "/", Procedure/binary>>.
