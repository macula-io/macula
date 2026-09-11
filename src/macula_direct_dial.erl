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
%%% the record -> build a `quic://' dial URL). Streaming and RPC share the
%%% IDENTICAL discovery mechanism — a `procedure_advertisement' does not
%%% distinguish RPC from streaming, only the eventual dial
%%% (`call_station/7' vs `call_stream_station/6') does — so
%%% `publish_advertisement/4,5' is reused as-is by both providers, and
%%% `call/6'/`call_stream/6' share the same candidate resolution and
%%% "Trust model" below. Content has no publish step here at all — see
%%% "Content" further down.
%%%
%%% == Resolution ==
%%%
%%% Every advertisement that passes trust filtering is a candidate, in the
%%% order the DHT returns them. A candidate whose `station_endpoint' can't
%%% be resolved, or whose link doesn't connect (`{error, not_connected}'
%%% from `call_station/7' or `call_stream_station/6'), is passed over for
%%% the next one, but only before the request is sent: once a CALL or a
%%% stream has gone out, its outcome is returned as it is. When no
%%% candidate qualifies, or every one failed before sending, resolution
%%% asks the DHT again after a pause that doubles from 100 ms to at most
%%% 1 s, and tries a candidate that already failed again only when its
%%% advertisement or its `station_endpoint' record has changed. A record
%%% just published on the provider's station has not necessarily
%%% replicated to the caller's station yet, so a miss is not final until
%%% the deadline. One deadline bounds all of it: each DHT lookup, each
%%% candidate's endpoint lookup and connect wait (within a share of the
%%% time that remains, at least one second while that much remains), and
%%% the request. At the deadline the result is, in this order, the most
%%% recent candidate's failure, why the latest answered DHT lookup found
%%% nothing qualifying, the latest failed lookup's error, or
%%% `{error, {unresolved, timeout}}'. A failed lookup is retried like an empty
%%% pass, and one the deadline cuts off records nothing.
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
%%% (the peer identity binding in `macula_peering_conn') checked against the
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
%%% only that station's own `station_endpoint' (`resolve_station_endpoint/2,3',
%%% the same machinery `call/6' uses internally for `serving_station').
%%%
%%% `get_content/3' and `fetch_content/4' resolve and fetch deliberately
%%% WITHOUT the cert-chain machinery above — content's threat model
%%% genuinely differs from RPC's. An RPC reply is opaque and unverifiable
%%% except by trusting whoever answered, so proving the ADVERTISER is
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
%%% content that does not match the MCID it asked for. The same holds for
%%% trying the next provider after a fetch that fails, so every announced
%%% provider is a candidate the way advertisements are for calls. What
%%% still matters, and is still mandatory, is (1)'s analogue for
%%% `content_announcement': the signer must equal the `announcer_node'
%%% it claims (the check `macula:find_content_providers/2' makes too), so
%%% an attacker cannot at least misattribute who is claiming to serve what.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_direct_dial).

-export([call/5, call/6, call_stream/5, call_stream/6,
        publish_advertisement/4, publish_advertisement/5,
        get_content/3, fetch_content/4, resolve_content_provider/2,
        put_content/4, resolve_station_endpoint/2, resolve_station_endpoint/3]).

-ifdef(TEST).
%% Exports for unit tests — pure helpers that are otherwise private.
-export([advertisement_trusted/2, adv_opts/1]).
-endif.

%% The first pause between passes over the DHT; it doubles up to
%% ?MAX_RETRY_MS. The retry within one candidate's endpoint lookup stays at
%% ?RETRY_MS.
-define(RETRY_MS, 100).
-define(MAX_RETRY_MS, 1_000).
%% The least time one candidate gets for its endpoint lookup and connect
%% wait, while that much of the deadline remains.
-define(MIN_CANDIDATE_SHARE_MS, 1_000).
%% One DHT lookup's own timeout when more time remains (`macula''s
%% ?DHT_RECORD_TIMEOUT_MS).
-define(LOOKUP_TIMEOUT_MS, 5_000).
%% `resolve_station_endpoint/2''s budget.
-define(DEFAULT_RESOLVE_TIMEOUT_MS, 10_000).
%% `call_stream/6''s budget without `dial_timeout_ms', matching
%% `macula:call_stream_station/6''s own default.
-define(DEFAULT_DIAL_TIMEOUT_MS, 10_000).

%% @doc As `call/6' with no cert-chain verification.
-spec call(macula:pool(), macula:realm(), macula:procedure(), term(),
          pos_integer()) -> {ok, term()} | {error, term()}.
call(Pool, Realm, Procedure, Payload, TimeoutMs) ->
    call(Pool, Realm, Procedure, Payload, TimeoutMs, #{}).

%% @doc Resolve `Procedure''s provider and call it there directly. Same
%% return shape as `macula:call/5'; resolve failures surface as
%% `{error, {unresolved, Reason}}' so a caller can tell "nobody has
%% advertised this via direct-dial yet" apart from a real call failure;
%% once a candidate has failed before the CALL was sent, the most recent
%% candidate's failure is the result instead, such as
%% `{error, not_connected}'.
%% `TimeoutMs' bounds resolution, each candidate's connect wait and the
%% CALL itself (see "Resolution" in the module doc). `Opts' may include
%% `verify_cert_chain => {RealmCaPem, Org}' — see the module doc's "Trust
%% model" section.
-spec call(macula:pool(), macula:realm(), macula:procedure(), term(),
          pos_integer(), map()) -> {ok, term()} | {error, term()}.
call(Pool, Realm, Procedure, Payload, TimeoutMs, Opts) ->
    Deadline = deadline(TimeoutMs),
    each_candidate(advertised_stations(Pool, Realm, Procedure, Opts),
                   station_try(Pool, call_work(Pool, Realm, Procedure, Payload, Deadline)),
                   Deadline).

%% @doc As `call_stream/6' with no cert-chain verification.
-spec call_stream(macula:pool(), macula:realm(), macula:procedure(), term(),
                  map()) -> {ok, macula:stream()} | {error, term()}.
call_stream(Pool, Realm, Procedure, Args, StreamOpts) ->
    call_stream(Pool, Realm, Procedure, Args, StreamOpts, #{}).

%% @doc As `call/6', but opens a stream (`macula:call_stream_station/6''s
%% shape) instead of making a single-reply call, built on the exact
%% same resolve+trust machinery — see the module doc. `StreamOpts' is
%% forwarded to `call_stream_station/6' alongside the resolved trust
%% override (`mode', `owner', etc); its `dial_timeout_ms' (default
%% 10_000) bounds resolution and each candidate's connect wait, and the
%% stream itself keeps its own deadline. `Opts' is the resolve-side
%% `verify_cert_chain' opt, same as `call/6'.
-spec call_stream(macula:pool(), macula:realm(), macula:procedure(), term(),
                  map(), map()) -> {ok, macula:stream()} | {error, term()}.
call_stream(Pool, Realm, Procedure, Args, StreamOpts, Opts) ->
    Deadline = deadline(maps:get(dial_timeout_ms, StreamOpts, ?DEFAULT_DIAL_TIMEOUT_MS)),
    each_candidate(advertised_stations(Pool, Realm, Procedure, Opts),
                   station_try(Pool, stream_work(Pool, Realm, Procedure, Args, StreamOpts)),
                   Deadline).

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

%% @doc Fetch `MCID' from one of its providers, resolved via their signed
%% `content_announcement's, and dialed directly. Same return shape as
%% `macula:get_content/2'; resolve failures surface as `{error,
%% {unresolved, Reason}}', and once a provider's fetch has failed, the
%% most recent failure is the result instead. `TimeoutMs' bounds the whole
%% fetch: lookups, each provider's connect wait and the transfers. See
%% `fetch_content/4' for how providers are chosen, and the module doc's
%% "Content" section
%% for why this has no `verify_cert_chain'-equivalent opt, unlike
%% `call/6'. Only chunked content is discoverable this way — see
%% `macula:find_content_providers/2'.
-spec get_content(macula:pool(), macula:mcid(), pos_integer()) ->
    {ok, binary()} | {error, term()}.
get_content(Pool, <<1, Codec, _/binary>> = MCID, TimeoutMs)
  when Codec =:= 16#55; Codec =:= 16#56 ->
    fetch_content(Pool, MCID, TimeoutMs, transfer_fetch(Pool, MCID));
get_content(_Pool, _MCID, _TimeoutMs) ->
    {error, invalid_mcid}.

%% @doc Fetch `MCID' from the first of its announced providers whose fetch
%% succeeds, within `TimeoutMs', by the rules `call/6' resolves by (see
%% "Resolution" in the module doc): a provider whose fetch fails, including
%% one whose bytes don't verify against `MCID', is passed over for the
%% next, and one that failed is tried again only when its announcement has
%% changed. `Fetch(Endpoint, Pinned, ConnectMs, RemainingMs)' runs one
%% fetch: `Pinned' is the dial trust override that pins the announcer,
%% `ConnectMs' the time that provider gets to connect, `RemainingMs' what
%% remains of the deadline; it returns `{ok, Bytes}' or `{error, Reason}'.
%% `macula_download' supplies its own, so a cancel reaches whichever
%% transfer is running.
-spec fetch_content(macula:pool(), macula:mcid(), pos_integer(),
                    fun((binary(), map(), pos_integer(), pos_integer()) ->
                            {ok, binary()} | {error, term()})) ->
    {ok, binary()} | {error, term()}.
fetch_content(Pool, MCID, TimeoutMs, Fetch) ->
    Deadline = deadline(TimeoutMs),
    each_candidate(content_providers(Pool, MCID), provider_try(Fetch, Deadline),
                   Deadline).

%% @doc Resolve `MCID''s provider via its signed `content_announcement',
%% asking the DHT again past a not-yet-replicated announcement for up to
%% 10 seconds. Returns the announcement of the first provider that
%% qualifies, `{error, content_not_announced}' when none has by then, or
%% the last lookup's own error when that lookup failed. Deprecated: removed
%% in 11.0.0. Use `fetch_content/4', which also moves on to the next
%% provider when a fetch fails.
-spec resolve_content_provider(macula:pool(), macula:mcid()) ->
    {ok, map()} | {error, term()}.
resolve_content_provider(Pool, MCID) ->
    bare_error(each_candidate(content_providers(Pool, MCID),
                              fun(#{announcement := Announcement}, _Share, _Seen) ->
                                  {done, {ok, Announcement}}
                              end,
                              deadline(?DEFAULT_RESOLVE_TIMEOUT_MS))).

bare_error({ok, _} = Resolved) -> Resolved;
bare_error(Error) -> bare_reason(Error).

%% @doc Resolve `Station''s dialable `quic://' URL from its own signed
%% `station_endpoint' record and put `Bytes' there directly. Same
%% return shape as `macula:put_content/2'; resolve failures surface as
%% `{error, {unresolved, Reason}}'. `TimeoutMs' bounds the endpoint
%% lookup and the connect wait (`macula:put_content_station/5'); the
%% underlying block/manifest transfer has its own internal timeouts.
-spec put_content(macula:pool(), macula_identity:pubkey(), binary(),
                  pos_integer()) -> {ok, macula:mcid()} | {error, term()}.
put_content(Pool, Station, Bytes, TimeoutMs) ->
    Deadline = deadline(TimeoutMs),
    put_at(station_endpoint(Pool, Station, Deadline), Pool, Bytes, Deadline).

put_at({found, {ok, {Station, DialUrl}}, _Version}, Pool, Bytes, Deadline) ->
    macula:put_content_station(Pool, DialUrl, Bytes, budget(Deadline),
                               pinned(Station));
put_at(Lookup, _Pool, _Bytes, _Deadline) ->
    lookup_error(Lookup).

%% @doc As `resolve_station_endpoint/3', within 10 seconds.
-spec resolve_station_endpoint(macula:pool(), macula_identity:pubkey()) ->
    {ok, binary()} | {error, term()}.
resolve_station_endpoint(Pool, Station) ->
    resolve_station_endpoint(Pool, Station, ?DEFAULT_RESOLVE_TIMEOUT_MS).

%% @doc Resolve `Station''s dialable `quic://' URL from its own signed
%% `station_endpoint' record, verifying the record's signer is exactly
%% `Station' and asking again past an absent, expired or malformed record,
%% or a failed lookup, until `TimeoutMs' has passed — the same discipline
%% `call/6' applies once it has a procedure's `serving_station'. The error
%% is, in this order, `station_endpoint_not_found' (or the malformed record's
%% reason) when a lookup answered, a failed lookup's own reason, or
%% `timeout'.
-spec resolve_station_endpoint(macula:pool(), macula_identity:pubkey(),
                               pos_integer()) ->
    {ok, binary()} | {error, term()}.
resolve_station_endpoint(Pool, Station, TimeoutMs) ->
    dial_url(station_endpoint(Pool, Station, deadline(TimeoutMs))).

dial_url({found, {ok, {_Station, DialUrl}}, _Version}) -> {ok, DialUrl};
dial_url(Lookup) -> bare_reason(lookup_error(Lookup)).

bare_reason({error, {unresolved, Reason}}) -> {error, Reason}.

%%%===================================================================
%%% Internal
%%%===================================================================

%% Works through candidates until one settles the request or `Deadline'
%% passes. `Find(Deadline)' returns one pass's `{ok, Candidates}',
%% `{answered, Error}' when the DHT answered but nothing qualifies, or
%% `{failed, Error}' when the lookup itself failed. `Try(Candidate, Share,
%% Seen)' works one candidate within the `Share' deadline and returns
%% `{done, Result}' once the request is settled, or `{next, Error, Seen}'
%% when nothing was sent. `Seen' carries what each candidate failed on from
%% one pass to the next. At the deadline the result is, in this order, the
%% most recent candidate failure, why the latest answered lookup found nothing
%% qualifying, the latest failed lookup's error, or a timeout.
each_candidate(Find, Try, Deadline) ->
    pass(Find, Try, Deadline, #{}, ?RETRY_MS, {timeout, {error, {unresolved, timeout}}}).

pass(Find, Try, Deadline, Seen, Delay, Last) ->
    pass_in_time(remaining(Deadline) > 0, Find, Try, Deadline, Seen, Delay, Last).

pass_in_time(false, _Find, _Try, _Deadline, _Seen, _Delay, {_Kind, Error}) ->
    Error;
pass_in_time(true, Find, Try, Deadline, Seen, Delay, Last) ->
    after_pass(candidates(Find(Deadline), Try, Deadline, Seen, Last),
               Find, Try, Deadline, Delay).

after_pass({done, Result}, _Find, _Try, _Deadline, _Delay) ->
    Result;
after_pass({next, Last, Seen}, Find, Try, Deadline, Delay) ->
    pause(Deadline, Delay),
    pass(Find, Try, Deadline, Seen, min(2 * Delay, ?MAX_RETRY_MS), Last).

candidates({ok, Candidates}, Try, Deadline, Seen, Last) ->
    each(Candidates, length(Candidates), Try, Deadline, Seen, Last);
candidates(NoCandidate, _Try, Deadline, Seen, Last) ->
    {next, recorded(Last, NoCandidate, remaining(Deadline) > 0), Seen}.

%% What a pass that found no candidate leaves as the result: nothing replaces
%% a candidate's failure, an answered lookup replaces anything else, a failed
%% lookup replaces only a timeout or an earlier failed lookup, and a failed
%% lookup that returned with no time left was cut off and records nothing.
recorded({candidate, _} = Last, _Pass, _InTime) -> Last;
recorded(_Last, {answered, _} = Answered, _InTime) -> Answered;
recorded(Last, {failed, _}, false) -> Last;
recorded({answered, _} = Last, {failed, _}, true) -> Last;
recorded(_Last, {failed, _} = Failed, true) -> Failed.

each([], _Untried, _Try, _Deadline, Seen, Last) ->
    {next, Last, Seen};
each([Candidate | Rest], Untried, Try, Deadline, Seen, Last) ->
    each_in_time(remaining(Deadline) > 0, Candidate, Rest, Untried, Try,
                 Deadline, Seen, Last).

each_in_time(false, _Candidate, _Rest, _Untried, _Try, _Deadline, Seen, Last) ->
    {next, Last, Seen};
each_in_time(true, Candidate, Rest, Untried, Try, Deadline, Seen, _Last) ->
    tried(Try(Candidate, share(Deadline, Untried), Seen), Rest, Untried - 1,
          Try, Deadline).

tried({done, _Result} = Done, _Rest, _Untried, _Try, _Deadline) ->
    Done;
tried({next, Error, Seen}, Rest, Untried, Try, Deadline) ->
    each(Rest, Untried, Try, Deadline, Seen, {candidate, Error}).

%% One pass over `Procedure''s advertisements.
advertised_stations(Pool, Realm, Procedure, Opts) ->
    Key = macula_record:procedure_key(discovery_uri(Realm, Procedure)),
    fun(Deadline) ->
        qualifying_stations(macula:find_records(Pool, Key, lookup_timeout(Deadline)),
                            Opts)
    end.

qualifying_stations({ok, []}, _Opts) ->
    {answered, {error, {unresolved, procedure_not_advertised}}};
qualifying_stations({ok, Recs}, Opts) ->
    candidates_or(trusted_stations(Recs, Opts), no_trusted_advertisement);
qualifying_stations({error, Reason}, _Opts) ->
    {failed, {error, {unresolved, Reason}}}.

candidates_or([], Reason) -> {answered, {error, {unresolved, Reason}}};
candidates_or(Candidates, _Reason) -> {ok, Candidates}.

%% Every advertisement that passes trust filtering, in the order given, as a
%% candidate: the provider that signed it, its record's version, and the
%% station it names.
trusted_stations(Recs, Opts) ->
    [#{provider => Key, version => Version, station => Station}
     || #{key := Key, version := Version} = Rec <- Recs,
        advertisement_trusted(Rec, Opts),
        Station <- serving_station(Rec)].

serving_station(Rec) ->
    try macula_record:read_procedure_advertisement(Rec) of
        #{serving_station := Station} when is_binary(Station) -> [Station];
        _Unreadable -> []
    catch _:_ -> []
    end.

%% Only a record that passes trust filtering is a candidate at all —
%% see the module doc's "Trust model" section. Base signature check is
%% mandatory; `verify_cert_chain' additionally requires the embedded
%% X.509 chain when the caller opted in.
advertisement_trusted(Rec, #{verify_cert_chain := {RealmCaPem, Org}}) ->
    ok =:= macula_record:verify_advertisement_cert_chain(RealmCaPem, Rec, Org);
advertisement_trusted(Rec, _Opts) ->
    signature_ok(macula_record:verify(Rec)).

signature_ok({ok, _})    -> true;
signature_ok({error, _}) -> false.

%% Sends the CALL to one resolved station. `not_connected' means the link
%% never came up within the candidate's share, so nothing was sent and the
%% next candidate may be tried; any other outcome means the CALL went out.
call_work(Pool, Realm, Procedure, Payload, Deadline) ->
    fun(Station, DialUrl, Share) ->
        sent_or_not(macula:call_station(Pool, DialUrl, Realm, Procedure, Payload,
                                        budget(Deadline),
                                        (pinned(Station))#{dial_timeout_ms => budget(Share)}))
    end.

%% Opens the stream at one resolved station, on the same terms as `call_work/5'.
stream_work(Pool, Realm, Procedure, Args, StreamOpts) ->
    fun(Station, DialUrl, Share) ->
        sent_or_not(macula:call_stream_station(Pool, DialUrl, Realm, Procedure, Args,
                                               maps:merge(StreamOpts, (pinned(Station))#{
                                                   dial_timeout_ms => budget(Share)})))
    end.

sent_or_not({error, not_connected} = NotSent) -> {not_sent, NotSent};
sent_or_not(Sent) -> {sent, Sent}.

station_try(Pool, Work) ->
    fun(Candidate, Share, Seen) -> reach(Pool, Candidate, Share, Seen, Work) end.

%% Resolves a candidate's station endpoint within `Share' and hands it to
%% `Work', unless the candidate already failed on the same advertisement:
%% then one lookup tells whether its endpoint record has changed, and only a
%% change is worth another dial. A lookup that itself fails teaches nothing.
reach(Pool, #{provider := Key, version := Version, station := Station}, Share, Seen,
      Work) ->
    reach_known(maps:find(Key, Seen), Pool, Key, Version, Station, Share, Seen, Work).

reach_known({ok, #{record := Version} = Prior}, Pool, Key, Version, Station, Share,
            Seen, Work) ->
    unless_unchanged(lookup_endpoint(Pool, Station, Share), Prior, Key, Version,
                     Share, Seen, Work);
reach_known(_NewOrChanged, Pool, Key, Version, Station, Share, Seen, Work) ->
    attempt(station_endpoint(Pool, Station, Share), Key, Version, Share, Seen, Work).

unless_unchanged({failed, _Error}, #{error := Error}, _Key, _Version, _Share, Seen,
                 _Work) ->
    {next, Error, Seen};
unless_unchanged(Lookup, #{endpoint := Endpoint, error := Error}, Key, Version,
                 Share, Seen, Work) ->
    changed_or_not(endpoint_version(Lookup) =:= Endpoint, Lookup, Error, Key, Version,
                   Share, Seen, Work).

changed_or_not(true, _Lookup, Error, _Key, _Version, _Share, Seen, _Work) ->
    {next, Error, Seen};
changed_or_not(false, Lookup, _Error, Key, Version, Share, Seen, Work) ->
    attempt(Lookup, Key, Version, Share, Seen, Work).

endpoint_version({found, _Result, EndpointVersion}) -> EndpointVersion;
endpoint_version({absent, _Error}) -> none.

attempt({found, {ok, {Station, DialUrl}}, EndpointVersion}, Key, Version, Share, Seen,
        Work) ->
    worked(Work(Station, DialUrl, Share), Key, Version, EndpointVersion, Seen);
attempt({found, {error, expired}, EndpointVersion}, Key, Version, _Share, Seen,
        _Work) ->
    failed(Key, Version, EndpointVersion,
           {error, {unresolved, station_endpoint_not_found}}, Seen);
attempt({found, {error, _} = Error, EndpointVersion}, Key, Version, _Share, Seen,
        _Work) ->
    failed(Key, Version, EndpointVersion, Error, Seen);
attempt({absent, Error}, Key, Version, _Share, Seen, _Work) ->
    failed(Key, Version, none, Error, Seen);
attempt({failed, Error}, Key, Version, _Share, Seen, _Work) ->
    failed(Key, Version, none, Error, Seen).

worked({sent, Result}, _Key, _Version, _EndpointVersion, _Seen) ->
    {done, Result};
worked({not_sent, Error}, Key, Version, EndpointVersion, Seen) ->
    failed(Key, Version, EndpointVersion, Error, Seen).

%% Remembers what a candidate failed on: its own record's version, its
%% station's endpoint record version (`none' when there was none), and the
%% error it failed with.
failed(Key, Version, EndpointVersion, Error, Seen) ->
    {next, Error, Seen#{Key => #{record => Version, endpoint => EndpointVersion,
                                 error => Error}}}.

%% One read of `Station''s own `station_endpoint' record: `{found, Result,
%% Version}' when there is a record, with `Result' `{ok, {Station, DialUrl}}',
%% `{error, expired}' or another `{error, {unresolved, Reason}}';
%% `{absent, Error}' when there is none; `{failed, Error}' when the lookup
%% itself failed.
lookup_endpoint(Pool, Station, Deadline) ->
    Key = macula_record:station_endpoint_key(Station),
    endpoint_lookup(macula:find_record(Pool, Key, lookup_timeout(Deadline)), Station).

endpoint_lookup({ok, #{version := Version} = Rec}, Station) ->
    {found, endpoint_result(verify_and_build(Station, Rec)), Version};
endpoint_lookup({error, not_found}, _Station) ->
    {absent, {error, {unresolved, station_endpoint_not_found}}};
endpoint_lookup({error, Reason}, _Station) ->
    {failed, {error, {unresolved, Reason}}}.

endpoint_result({ok, _} = Ok) -> Ok;
endpoint_result({error, expired} = Expired) -> Expired;
endpoint_result({error, Reason}) -> {error, {unresolved, Reason}}.

%% `lookup_endpoint/3', asked again until `Deadline' past a lookup that found
%% no usable record (absent, expired or malformed) or that failed. A record
%% that doesn't verify ends the lookup. When no lookup found a usable record,
%% the result is, in this order, the latest lookup that answered with none,
%% the latest failed lookup, or a timeout; a failed lookup that returned with
%% no time left was cut off and records nothing.
station_endpoint(Pool, Station, Deadline) ->
    endpoint_lookups(Pool, Station, Deadline, {failed, {error, {unresolved, timeout}}}).

endpoint_lookups(Pool, Station, Deadline, Best) ->
    endpoint_or_again(lookup_endpoint(Pool, Station, Deadline), Pool, Station, Deadline,
                      Best).

endpoint_or_again({found, {ok, _}, _Version} = Found, _Pool, _Station, _Deadline, _Best) ->
    Found;
endpoint_or_again({found, {error, {unresolved, Reason}}, _Version} = Untrusted, _Pool,
                  _Station, _Deadline, _Best)
  when Reason =/= malformed_station_endpoint ->
    Untrusted;
endpoint_or_again(Lookup, Pool, Station, Deadline, Best) ->
    Recorded = endpoint_recorded(Best, Lookup, remaining(Deadline) > 0),
    pause(Deadline, ?RETRY_MS),
    endpoint_in_time(remaining(Deadline) > 0, Recorded, Pool, Station, Deadline).

endpoint_in_time(true, Best, Pool, Station, Deadline) ->
    endpoint_lookups(Pool, Station, Deadline, Best);
endpoint_in_time(false, Best, _Pool, _Station, _Deadline) ->
    Best.

%% An answered lookup that found no usable record replaces anything; a failed
%% lookup replaces only a timeout or an earlier failed lookup, and records
%% nothing when it returned with no time left.
endpoint_recorded(_Best, {absent, _} = Answered, _InTime) -> Answered;
endpoint_recorded(_Best, {found, {error, _}, _Version} = Answered, _InTime) -> Answered;
endpoint_recorded(Best, {failed, _}, false) -> Best;
endpoint_recorded({failed, _}, {failed, _} = Failed, true) -> Failed;
endpoint_recorded(Best, {failed, _}, true) -> Best.

lookup_error({found, {error, expired}, _Version}) ->
    {error, {unresolved, station_endpoint_not_found}};
lookup_error({found, {error, _} = Error, _Version}) -> Error;
lookup_error({absent, Error}) -> Error;
lookup_error({failed, Error}) -> Error.

%% The `station_endpoint' record for `Station' must be SIGNED BY
%% `Station' itself (macula_station_announcer publishes it self-signed
%% — the station describing its own reachable address). Checking the
%% signature AND that the signer is exactly `Station', not just any
%% valid signature, is what makes pinning `expected_node_id => Station'
%% on the dial meaningful: without it, a record merely stored under the
%% right key (but signed, or not, by someone else) would still be
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

%% One pass over `MCID''s content announcements.
content_providers(Pool, MCID) ->
    Key = macula_record:content_key(MCID),
    fun(Deadline) ->
        qualifying_providers(macula:find_records(Pool, Key, lookup_timeout(Deadline)))
    end.

qualifying_providers({ok, Recs}) ->
    candidates_or(lists:flatmap(fun trusted_provider/1, Recs), content_not_announced);
qualifying_providers({error, Reason}) ->
    {failed, {error, {unresolved, Reason}}}.

%% A `content_announcement' qualifies when its own signature verifies and
%% its signer is the `announcer_node' it claims, keeping the record's
%% version.
trusted_provider(#{key := Key, version := Version} = Rec) ->
    announced(macula_record:verify(Rec), read_announcement(Rec), Key, Version);
trusted_provider(_Rec) ->
    [].

announced({ok, _}, #{announcer_node := Key, endpoint := Endpoint} = Announcement, Key,
          Version)
  when is_binary(Endpoint) ->
    [#{provider => Key, version => Version, endpoint => Endpoint,
       announcement => Announcement}];
announced(_Verified, _Announcement, _Key, _Version) ->
    [].

read_announcement(Rec) ->
    try macula_record:read_content_announcement(Rec)
    catch _:_ -> undefined
    end.

%% Fetches from one provider, unless it already failed on the same
%% announcement.
provider_try(Fetch, Deadline) ->
    fun(#{provider := Key} = Provider, Share, Seen) ->
        fetch_unless_failed(maps:find(Key, Seen), Provider, Share, Seen, Fetch, Deadline)
    end.

fetch_unless_failed({ok, #{record := Version, error := Error}}, #{version := Version},
                    _Share, Seen, _Fetch, _Deadline) ->
    {next, Error, Seen};
fetch_unless_failed(_NewOrChanged, #{provider := Key, version := Version,
                                     endpoint := Endpoint},
                    Share, Seen, Fetch, Deadline) ->
    fetched(Fetch(Endpoint, pinned(Key), budget(Share), budget(Deadline)), Key, Version,
            Seen).

fetched({ok, _} = Fetched, _Key, _Version, _Seen) ->
    {done, Fetched};
fetched({error, _} = Error, Key, Version, Seen) ->
    failed(Key, Version, none, Error, Seen).

%% Fetches from one provider through `macula_content_transfer', within what
%% remains of the deadline, and reaps the transfer whatever its outcome.
transfer_fetch(Pool, MCID) ->
    fun(Endpoint, Pinned, ConnectMs, RemainingMs) ->
        {ok, Transfer} = macula_content_transfer:start_get_station(
                           Pool, Endpoint, MCID, ConnectMs, Pinned),
        Result = await_transfer(Transfer, RemainingMs),
        _ = macula_content_transfer:cancel(Transfer),
        Result
    end.

await_transfer(Transfer, RemainingMs) ->
    try macula_content_transfer:await(Transfer, RemainingMs)
    catch exit:{timeout, _} -> {error, timeout}
    end.

%% The trust override for a dial pinned to the identity a signed DHT record
%% resolved — see the module doc's "Trust model".
pinned(Node) ->
    #{expected_node_id => Node, pin_tls_cert => false, verify => none}.

deadline(TimeoutMs) -> erlang:monotonic_time(millisecond) + TimeoutMs.

remaining(Deadline) -> Deadline - erlang:monotonic_time(millisecond).

%% A timeout argument for what remains of `Deadline', at least 1 ms.
budget(Deadline) -> max(1, remaining(Deadline)).

%% The deadline one candidate works within: what remains split evenly over
%% the candidates not yet tried, but no less than ?MIN_CANDIDATE_SHARE_MS
%% while that much remains.
share(Deadline, Untried) ->
    Remaining = remaining(Deadline),
    erlang:monotonic_time(millisecond)
        + max(Remaining div Untried, min(?MIN_CANDIDATE_SHARE_MS, Remaining)).

lookup_timeout(Deadline) -> max(1, min(?LOOKUP_TIMEOUT_MS, remaining(Deadline))).

pause(Deadline, Ms) -> timer:sleep(max(0, min(Ms, remaining(Deadline)))).

%% No `Org' segment in the discovery URI: `Org' is only consulted
%% post-resolve, as the `verify_cert_chain' opt's expected leaf-cert
%% organization (see `advertisement_trusted/2') — it does not affect
%% how a `procedure_advertisement' is keyed or found.
discovery_uri(Realm, Procedure) ->
    <<(binary:encode_hex(Realm, uppercase))/binary, "/", Procedure/binary>>.
