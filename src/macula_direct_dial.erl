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
%%% (`call_station/7' vs `call_stream_station/7') does — so
%%% `publish_advertisement/4,5' is reused as-is by both providers, and
%%% `call/6'/`call_stream/6' share the same candidate resolution and
%%% "Trust model" below. Content has no publish step here at all — see
%%% "Content" further down.
%%%
%%% == Resolution ==
%%%
%%% A call starts from a HEAD START when it can. The pool remembers the
%%% station that last answered a procedure, and hands it back as a candidate
%%% to try before the DHT is asked at all, but only while two things hold:
%%% the advertisement it was built from has not reached the end of the
%%% lifetime it was remembered with, and the pool STILL HOLDS A LIVE LINK to
%%% that station. The live link is what makes this safe to do without a
%%% `station_endpoint' lookup: a link either exists or it does not, so it
%%% cannot be stale the way a signed record up to five minutes old can.
%%%
%%% It is a head start and never a substitute. The remembered candidate goes
%%% through the same trust-independent machinery as any other, gets the same
%%% share of the same deadline, and when it fails resolution carries on into
%%% the DHT passes exactly as it would have without one. Nothing is skipped
%%% except a lookup. What it cannot cover is an advertisement SUPERSEDED by
%%% one naming a different station while the remembered one is still inside
%%% its own lifetime: the call then goes out to a station that no longer
%%% serves the procedure and its answer is returned, where an uncached call
%%% would have found the new station. That window is bounded by the
%%% remembered lifetime and nothing else, deliberately, because a CALL that
%%% has already gone out must not be sent again somewhere else (see
%%% `macula_station_link:not_sent/1').
%%%
%%% Every advertisement that passes trust filtering is a candidate, in the
%%% order the DHT returns them. A candidate whose `station_endpoint' can't
%%% be resolved, or whose link doesn't connect (`{error, not_connected}'
%%% from `call_station/7' or `call_stream_station/7'), is passed over for
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
%%% itself (`station_signed_endpoint/2'). The actual QUIC dial proves
%%% only that the station holds the key of the self-signed ML-DSA-87
%%% certificate it presents (`macula_quic:connect/4'), which says nothing
%%% of who it is: that is enforced at the application layer, via the
%%% cryptographically signed CONNECT/HELLO handshake (the peer identity
%%% binding in `macula_peering_conn') checked against the exact node_id
%%% the signed DHT chain above resolved.
%%%
%%% An authorization verifies against the realm key the pool pinned for the
%%% call's realm when it started (`macula:connect/2''s
%%% `realm_trust => #{RealmId => RealmKey}'): the realm key as carried, for
%%% the org directory and the procedure delegation, the only authorization
%%% form. Without a key pinned for the realm, an advertisement for an org
%%% namespaced procedure is never trusted. A realm key never arrives with a
%%% request: `realm_trust' on a call, like the 10.x options
%%% `verify_cert_chain' and `cert_chain', is refused by name, with
%%% `{error, {removed_option, Key}}' (see `removed_option/2').
%%%
%%% A caller checks the authorization from the advertisement alone and looks
%%% up no tombstone. A delegation its org withdraws is honoured until it
%%% expires, so the caller-side revocation bound is the delegation's maximum
%%% lifetime: 30 minutes (D32, `macula_record''s
%%% PROCEDURE_DELEGATION_MAX_LIFETIME_MS), plus 5 minutes of clock tolerance.
%%% The realm reissues every 10 minutes, and a revoked provider gets no fresh
%%% one.
%%%
%%% == Content ==
%%%
%%% Content is fetched from the node that shares it (D27): see
%%% `macula_content_fetch', which resolves a serving station's endpoint with
%%% `resolve_station_endpoint/3' and opens its stream with
%%% `macula:call_stream_station/7', and checks no advertisement's
%%% authorization: content is verified by its content id.
%%%
%%% == Dial I/O ==
%%%
%%% The DHT lookups and dials a call runs on come from the `dial_io' in its
%%% `Opts', or else from `macula'. A given `dial_io' has every function the call
%%% runs on, each at the arity its key takes, and may carry the other
%%% `dial_io()' functions; any other is refused with `function_clause', in
%%% the caller. The option is for tests and for embedding direct dial; other
%%% callers leave it out.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_direct_dial).

-export([call/5, call/6, call_stream/5, call_stream/6, providers/4, providers/5,
        publish_advertisement/4, publish_advertisement/5,
        resolve_station_endpoint/2, resolve_station_endpoint/3,
        resolve_station_endpoint/4, removed_option/2]).

%% Only tests call these arities so far, each with a dial_io.
-ignore_xref([{resolve_station_endpoint, 4}]).

-ifdef(TEST).
%% Exports for unit tests: pure helpers that are otherwise private.
-export([advertisement_trusted/2, adv_opts/1]).
-endif.

%% The functions direct dial looks up, dials and fetches with, by key. See
%% "Dial I/O" in the module doc.
-type dial_io() :: #{links => fun((macula:pool()) -> {ok, [map()]} | {error, term()}),
                     put_record => fun((macula:pool(), map()) -> ok | {error, term()}),
                     find_records => fun((macula:pool(), binary(), pos_integer()) ->
                                             {ok, [map()]} | {error, term()}),
                     find_record => fun((macula:pool(), binary(), pos_integer()) ->
                                            {ok, map()} | {error, term()}),
                     call_station => fun((macula:pool(), macula_client:seed(), <<_:256>>,
                                          macula:realm(), macula:procedure(), term(),
                                          pos_integer(), map()) ->
                                             {ok, term()} | {error, term()}),
                     call_stream_station => fun((macula:pool(), macula_client:seed(), <<_:256>>,
                                                 macula:realm(), macula:procedure(), term(),
                                                 map()) ->
                                                    {ok, macula:stream()} | {error, term()}),
                     resolved_candidate => fun((macula:pool(), macula:realm(),
                                                macula:procedure()) ->
                                                   {ok, map(), macula_client:seed()} | none),
                     remember_resolved => fun((macula:pool(), macula:realm(),
                                               macula:procedure(), map(),
                                               non_neg_integer()) -> ok)}.

-export_type([dial_io/0]).

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
%% `macula:call_stream_station/7''s own default.
-define(DEFAULT_DIAL_TIMEOUT_MS, 10_000).
-define(TYPE_PROCEDURE_ADVERTISEMENT, 16#06).
-define(TYPE_STATION_ENDPOINT, 16#12).
%% A refusal `macula:find_record/3' returns for a record that did not verify
%% under the node's crypto profile (`macula_record:verify/2'), other than
%% `expired'. EVERY other member of `macula_record:refusal()' belongs here:
%% each one is a property of the record itself, so asking the same station
%% again returns the same refusal. One left out falls through to the lookup
%% failure clause and is retried to the deadline as though it were a
%% transport fault, which costs the caller its whole budget on an answer that
%% cannot change. Keep this in step with that type.
-define(RECORD_REFUSAL(Reason),
        (Reason =:= record_too_large orelse Reason =:= malformed orelse
         Reason =:= signature_invalid orelse Reason =:= alg_mismatch orelse
         Reason =:= not_yet_valid orelse Reason =:= key_id_mismatch orelse
         Reason =:= lifetime_too_long orelse Reason =:= lifetime_reversed)).
%% Options a call or an advertisement no longer reads. `verify_cert_chain'
%% turned a trust check on in 10.x, and the realm keys a pool pins replace it;
%% `realm_trust' on a call is gone because a realm key never arrives with a
%% request; `authorization' replaces `cert_chain' on an advertisement. One
%% still given is refused by name, so nobody goes on without the check they
%% asked for.
-define(REMOVED_CALL_OPTIONS, [verify_cert_chain, realm_trust]).
-define(REMOVED_ADVERTISE_OPTIONS, [cert_chain]).

%% @doc As `call/6' with no options.
-spec call(macula:pool(), macula:realm(), macula:procedure(), term(),
          1..600_000) -> {ok, term()} | {error, term()}.
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
%% CALL itself (see "Resolution" in the module doc). An org namespaced
%% procedure's authorization is checked against the realm key the pool
%% pinned for `Realm': see the module doc's "Trust model" section.
%%
%% `Opts' takes `provider', a provider's node_id: the call then goes to THAT
%% provider only. Resolution and the trust check are exactly as without it;
%% only candidates whose advertisement that provider signed are tried, the
%% pool's remembered head start included, and a provider with no trusted
%% advertisement by the deadline is `{error, {unresolved,
%% provider_not_advertised}}'. A caller that wants one answer from each of
%% several providers makes one call per provider (see `providers/4').
%% A `provider' that is not a 32-byte node_id is `{error, {invalid_option,
%% provider}}', and `realm_trust' and `verify_cert_chain' are refused with
%% `{error, {removed_option, Key}}', both before anything is looked up.
-spec call(macula:pool(), macula:realm(), macula:procedure(), term(),
          1..600_000, map()) -> {ok, term()} | {error, term()}.
call(Pool, Realm, Procedure, Payload, TimeoutMs, Opts)
  when is_integer(TimeoutMs), TimeoutMs > 0, TimeoutMs =< 600_000 ->
    call_unless_removed(removed_option(call, Opts), Pool, Realm, Procedure, Payload, TimeoutMs,
                        Opts).

call_unless_removed(none, Pool, Realm, Procedure, Payload, TimeoutMs, Opts) ->
    call_to(provider_option(Opts), Pool, Realm, Procedure, Payload, TimeoutMs, Opts);
call_unless_removed(Removed, _Pool, _Realm, _Procedure, _Payload, _TimeoutMs, _Opts) ->
    {error, Removed}.

call_to({error, _} = Invalid, _Pool, _Realm, _Procedure, _Payload, _TimeoutMs, _Opts) ->
    Invalid;
call_to({ok, Only}, Pool, Realm, Procedure, Payload, TimeoutMs, Opts) ->
    Dial = dial(Pool, [find_records, find_record, call_station, resolved_candidate,
                       remember_resolved], Opts),
    Deadline = deadline(TimeoutMs),
    each_candidate(only_provider(Only, head_start(Dial, Realm, Procedure)),
                   only_provider_found(Only, advertised_stations(Dial, Realm, Procedure)),
                   station_try(Dial, call_work(Dial, Realm, Procedure, Payload, Deadline)),
                   Deadline).

%% Which provider a call is limited to, `any' when the caller named none.
provider_option(#{provider := <<_:256>> = Provider}) -> {ok, Provider};
provider_option(#{provider := _}) -> {error, {invalid_option, provider}};
provider_option(#{}) -> {ok, any}.

%% The candidates a call named provider may try: those its advertisements
%% name. Resolution and trust ran before this, unchanged.
only_provider(any, Candidates) ->
    Candidates;
only_provider(Provider, Candidates) ->
    [C || #{provider := P} = C <- Candidates, P =:= Provider].

%% A pass that found candidates, none of them the named provider's, is a pass
%% that answered with nothing qualifying, and says whose absence it was.
only_provider_found(any, Find) ->
    Find;
only_provider_found(Provider, Find) ->
    fun(Deadline) -> provider_pass(Find(Deadline), Provider) end.

provider_pass({ok, Candidates}, Provider) ->
    candidates_or(only_provider(Provider, Candidates), provider_not_advertised);
provider_pass(NoCandidate, _Provider) ->
    NoCandidate.

%% @doc As `providers/5' with no options.
-spec providers(macula:pool(), macula:realm(), macula:procedure(), pos_integer()) ->
    {ok, [#{provider := <<_:256>>, station := <<_:256>>}]} | {error, term()}.
providers(Pool, Realm, Procedure, TimeoutMs) ->
    providers(Pool, Realm, Procedure, TimeoutMs, #{}).

%% @doc Who provides `Procedure' in `Realm': every advertisement that passes
%% the trust check a call applies (see "Trust model"), as the provider that
%% signed it and the station it names, in the order the DHT answered. ONE
%% lookup, bounded by `TimeoutMs', and no retry: a provider whose record has
%% not replicated to this pool's stations yet is simply not listed. `{error,
%% {unresolved, procedure_not_advertised}}' when the DHT has no advertisement,
%% `{error, {unresolved, no_trusted_advertisement}}' when none passes the trust
%% check, `{error, {unresolved, Reason}}' when the lookup failed. A provider
%% advertising through two stations is listed once per station. `Opts' takes
%% only `dial_io' (see "Dial I/O").
-spec providers(macula:pool(), macula:realm(), macula:procedure(), pos_integer(), map()) ->
    {ok, [#{provider := <<_:256>>, station := <<_:256>>}]} | {error, term()}.
providers(Pool, Realm, Procedure, TimeoutMs, Opts)
  when is_integer(TimeoutMs), TimeoutMs > 0, TimeoutMs =< 600_000 ->
    Find = advertised_stations(dial(Pool, [find_records], Opts), Realm, Procedure),
    listed(Find(deadline(TimeoutMs))).

listed({ok, Candidates}) ->
    {ok, [maps:with([provider, station], C) || C <- Candidates]};
listed({_AnsweredOrFailed, Error}) ->
    Error.

%% @doc As `call_stream/6' with no options.
-spec call_stream(macula:pool(), macula:realm(), macula:procedure(), term(),
                  map()) -> {ok, macula:stream()} | {error, term()}.
call_stream(Pool, Realm, Procedure, Args, StreamOpts) ->
    call_stream(Pool, Realm, Procedure, Args, StreamOpts, #{}).

%% @doc As `call/6', but opens a stream (`macula:call_stream_station/7''s
%% shape) instead of making a single-reply call, built on the exact
%% same resolve+trust machinery — see the module doc. `StreamOpts' is
%% forwarded to `call_stream_station/7' alongside the resolved trust
%% override (`mode', `owner', etc); its `dial_timeout_ms' (default
%% 10_000, from 1 to 600_000 as a call's timeout) bounds resolution and
%% each candidate's connect wait, and the stream itself keeps its own
%% deadline. `Opts' takes no option: `realm_trust' and `verify_cert_chain'
%% in it are refused as `call/6' refuses them.
-spec call_stream(macula:pool(), macula:realm(), macula:procedure(), term(),
                  map(), map()) -> {ok, macula:stream()} | {error, term()}.
call_stream(Pool, Realm, Procedure, Args, StreamOpts, Opts)
  when not is_map_key(dial_timeout_ms, StreamOpts);
       is_integer(map_get(dial_timeout_ms, StreamOpts)), map_get(dial_timeout_ms, StreamOpts) > 0,
       map_get(dial_timeout_ms, StreamOpts) =< 600_000 ->
    call_stream_unless_removed(removed_option(call, Opts), Pool, Realm, Procedure, Args,
                               StreamOpts, Opts).

call_stream_unless_removed(none, Pool, Realm, Procedure, Args, StreamOpts, Opts) ->
    Dial = dial(Pool, [find_records, find_record, call_stream_station, resolved_candidate,
                       remember_resolved], Opts),
    Deadline = deadline(maps:get(dial_timeout_ms, StreamOpts, ?DEFAULT_DIAL_TIMEOUT_MS)),
    each_candidate(head_start(Dial, Realm, Procedure),
                   advertised_stations(Dial, Realm, Procedure),
                   station_try(Dial, stream_work(Dial, Realm, Procedure, Args, StreamOpts)),
                   Deadline);
call_stream_unless_removed(Removed, _Pool, _Realm, _Procedure, _Args, _StreamOpts, _Opts) ->
    {error, Removed}.

%% @doc As `publish_advertisement/5' with no provider authorization.
-spec publish_advertisement(macula:pool(), macula:realm(), macula:procedure(),
                            macula_node_keys:node_key()) -> ok | {error, term()}.
publish_advertisement(Pool, Realm, Procedure, NodeIdentity) ->
    publish_advertisement(Pool, Realm, Procedure, NodeIdentity, #{}).

%% @doc Publish a signed `procedure_advertisement' for `Procedure',
%% naming `Pool''s currently-connected station as the serving station.
%% `NodeIdentity' signs it, and its node_id is the advertiser: it must be
%% the node identity key `Pool' was started with, since a caller targets
%% that node_id and the station knows the pool's connection by it.
%% `Opts' may include `authorization', the provider authorization an org
%% namespaced procedure needs (D25 item 6), as
%% `#{org_directory => Wire, procedure_delegation => Wire}', `ttl_ms', and
%% `stations', which makes the serving station the first of those node ids the
%% pool is connected to (`macula:advertise/5' registers there).
%% `cert_chain', a 10.x
%% option `authorization' replaces, is refused with
%% `{error, {removed_option, cert_chain}}' before anything is read or put.
-spec publish_advertisement(macula:pool(), macula:realm(), macula:procedure(),
                            macula_node_keys:node_key(), map()) ->
    ok | {error, term()}.
publish_advertisement(Pool, Realm, Procedure, NodeIdentity, Opts) ->
    publish_unless_removed(removed_option(advertise, Opts), Pool, Realm, Procedure,
                           NodeIdentity, Opts).

publish_unless_removed(none, Pool, Realm, Procedure, NodeIdentity, Opts) ->
    #{links := Links} = Dial = dial(Pool, [links, put_record], Opts),
    case Links(Pool) of
        {ok, Linked} -> on_links(connected_station(Linked, maps:get(stations, Opts, all)), Dial,
                                 Realm, Procedure, NodeIdentity, Opts);
        {error, _} = Error -> Error
    end;
publish_unless_removed(Removed, _Pool, _Realm, _Procedure, _NodeIdentity, _Opts) ->
    {error, Removed}.

%% @doc The first option in `Opts' that 11.0.0 removed from a call or an
%% advertisement, as `{removed_option, Key}', or `none'. On a call, the realm
%% keys the pool pins replace `verify_cert_chain' and `realm_trust'; on an
%% advertisement, `authorization' replaces `cert_chain'.
-spec removed_option(call | advertise, map()) -> none | {removed_option, atom()}.
removed_option(call, Opts) -> first_given(?REMOVED_CALL_OPTIONS, Opts);
removed_option(advertise, Opts) -> first_given(?REMOVED_ADVERTISE_OPTIONS, Opts).

first_given(Keys, Opts) ->
    given([Key || Key <- Keys, maps:is_key(Key, Opts)]).

given([Key | _]) -> {removed_option, Key};
given([]) -> none.

on_links({ok, Station}, #{pool := Pool, put_record := PutRecord}, Realm, Procedure,
         NodeIdentity, Opts) ->
    Advertiser = macula_node_keys:key_id(NodeIdentity),
    Ad = macula_record:sign(
           macula_record:procedure_advertisement(Advertiser, Realm, Procedure, Station,
                                                  adv_opts(Opts)),
           NodeIdentity),
    PutRecord(Pool, Ad);
on_links({error, _} = Error, _Dial, _Realm, _Procedure, _NodeIdentity, _Opts) ->
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
%% The station the record names: the pool's first connected station, or with
%% `stations' the first of them it is connected to, where the procedure is
%% registered.
connected_station(Links, Stations) ->
    first_station([S || #{connected := true, node_id := S} <- Links, is_binary(S)], Stations).

first_station([Station | _], all) -> {ok, Station};
first_station(Connected, Stations) when is_list(Stations) ->
    first_station([S || S <- Stations, lists:member(S, Connected)], all);
first_station([], all) -> {error, no_healthy_link}.

%% @doc As `resolve_station_endpoint/3', within 10 seconds.
-spec resolve_station_endpoint(macula:pool(), <<_:256>>) ->
    {ok, binary()} | {error, term()}.
resolve_station_endpoint(Pool, Station) ->
    resolve_station_endpoint(Pool, Station, ?DEFAULT_RESOLVE_TIMEOUT_MS).

%% @doc Resolve `Station''s dialable `quic://' URL from its own signed
%% `station_endpoint' record, verifying the record's signer is exactly
%% `Station' and asking again past an absent, expired or malformed record,
%% or a failed lookup, until `TimeoutMs' has passed — the same discipline
%% `call/6' applies once it has a procedure's `serving_station'. The error
%% is, in this order, the latest answered lookup's own reason
%% (`station_endpoint_not_found' for no record at all,
%% `station_endpoint_expired' for one refused as stale, or the malformed
%% record's reason), a failed lookup's own reason, or `timeout'. An absent
%% record and an expired one are reported apart: the first says the station
%% published no endpoint, the second that it published one and the caller's
%% own clock check refused it.
-spec resolve_station_endpoint(macula:pool(), <<_:256>>, pos_integer()) ->
    {ok, binary()} | {error, term()}.
resolve_station_endpoint(Pool, Station, TimeoutMs) ->
    resolve_station_endpoint(Pool, Station, TimeoutMs, #{}).

%% @doc As `resolve_station_endpoint/3', on the `dial_io' in `Opts' (see
%% "Dial I/O" in the module doc).
-spec resolve_station_endpoint(macula:pool(), <<_:256>>, pos_integer(), map()) ->
    {ok, binary()} | {error, term()}.
resolve_station_endpoint(Pool, Station, TimeoutMs, Opts) ->
    dial_url(station_endpoint(dial(Pool, [find_record], Opts), Station,
                              deadline(TimeoutMs))).

dial_url({found, {ok, {_Station, DialUrl}}, _Version}) -> {ok, DialUrl};
dial_url(Lookup) -> bare_reason(lookup_error(Lookup)).

bare_reason({error, {unresolved, Reason}}) -> {error, Reason}.

%%%===================================================================
%%% Internal
%%%===================================================================

%% The pool and the functions a call runs on, `Keys' of them: the `dial_io'
%% in `Opts', or else the defaults.
dial(Pool, Keys, Opts) ->
    Io = dial_io(maps:with(Keys, default_dial_io()), maps:get(dial_io, Opts, undefined)),
    Io#{pool => Pool}.

%% `Defaults', or `Given' when it has every key in `Defaults', each function
%% at the arity its key takes, and no key outside `dial_io()'. Any other set
%% is refused with function_clause.
dial_io(Defaults, undefined) ->
    Defaults;
dial_io(Defaults, Given) when is_map(Given) ->
    ok = maps:foreach(fun dial_function/2, Given),
    ok = lists:foreach(fun(Key) -> given_key(Key, Given) end, maps:keys(Defaults)),
    Given.

dial_function(links, Fun) when is_function(Fun, 1) -> ok;
dial_function(put_record, Fun) when is_function(Fun, 2) -> ok;
dial_function(find_records, Fun) when is_function(Fun, 3) -> ok;
dial_function(find_record, Fun) when is_function(Fun, 3) -> ok;
dial_function(call_station, Fun) when is_function(Fun, 8) -> ok;
dial_function(call_stream_station, Fun) when is_function(Fun, 7) -> ok;
dial_function(resolved_candidate, Fun) when is_function(Fun, 3) -> ok;
dial_function(remember_resolved, Fun) when is_function(Fun, 5) -> ok.

given_key(Key, Given) when is_map_key(Key, Given) -> ok.

-spec default_dial_io() -> dial_io().
default_dial_io() ->
    #{links => fun macula:links/1,
      put_record => fun macula:put_record/2,
      find_records => fun macula:find_records/3,
      find_record => fun macula:find_record/3,
      call_station => fun macula:call_station/8,
      call_stream_station => fun macula:call_stream_station/7,
      resolved_candidate => fun macula_client:resolved_candidate/3,
      remember_resolved => fun macula_client:remember_resolved/5}.

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
%%
%% `HeadStart' is tried before the DHT is asked at all.
%%
%% A HEAD START, NEVER A SUBSTITUTE. Its candidates go through the SAME
%% `Try', the SAME `share/2' and the SAME `Deadline' as any other, they
%% record what they failed on in the SAME `Seen', and when none of them
%% settles the request the DHT passes run exactly as they do without one.
%% What it removes is a lookup, not a check.
%%
%% There is no pause before the first DHT pass, unlike between two DHT
%% passes: a pass waits because asking the same DHT again immediately
%% teaches nothing, and the head start never asked it.
each_candidate(HeadStart, Find, Try, Deadline) ->
    first_pass(HeadStart, Find, Try, Deadline, #{},
               {timeout, {error, {unresolved, timeout}}}).

first_pass([], Find, Try, Deadline, Seen, Last) ->
    pass(Find, Try, Deadline, Seen, ?RETRY_MS, Last);
first_pass(HeadStart, Find, Try, Deadline, Seen, Last) ->
    after_head_start(candidates({ok, HeadStart}, Try, Deadline, Seen, Last),
                     Find, Try, Deadline).

after_head_start({done, Result}, _Find, _Try, _Deadline) ->
    Result;
after_head_start({next, Last, Seen}, Find, Try, Deadline) ->
    pass(Find, Try, Deadline, Seen, ?RETRY_MS, Last).

%% The station that last answered `Procedure' in `Realm' from this pool, as
%% a one-candidate head start, or none. The pool hands one back only while
%% it still holds a LIVE LINK to that station, so the candidate carries the
%% seed that link is keyed by (`dial') and needs no `station_endpoint'
%% lookup: see `macula_client:resolved_candidate/3' and `reach/5'.
head_start(#{pool := Pool, resolved_candidate := Resolved}, Realm, Procedure) ->
    remembered_candidate(Resolved(Pool, Realm, Procedure)).

remembered_candidate({ok, Candidate, Seed}) -> [Candidate#{dial => Seed}];
remembered_candidate(none)                  -> [].

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
advertised_stations(#{pool := Pool, find_records := FindRecords}, Realm, Procedure) ->
    Key = macula_record:procedure_key(Realm, Procedure),
    Trust = fun() -> trust(Pool, Realm, Procedure) end,
    fun(Deadline) ->
        Records = FindRecords(Pool, Key, lookup_timeout(Deadline)),
        qualifying_stations(Records, Trust)
    end.

%% The pool is asked for its realm key only once a lookup has answered with
%% records to check.
qualifying_stations({ok, []}, _Trust) ->
    {answered, {error, {unresolved, procedure_not_advertised}}};
qualifying_stations({ok, Recs}, Trust) ->
    candidates_or(trusted_stations(Recs, Trust(), erlang:system_time(millisecond)),
                  no_trusted_advertisement);
qualifying_stations({error, Reason}, _Trust) ->
    {failed, {error, {unresolved, Reason}}}.

candidates_or([], Reason) -> {answered, {error, {unresolved, Reason}}};
candidates_or(Candidates, _Reason) -> {ok, Candidates}.

%% Every advertisement that passes the trust check, in the order given, as a
%% candidate: the node_id of the provider that signed it, its record's
%% version, and the station it names.
trusted_stations(Recs, Trust, Now) ->
    [#{provider => KeyId, version => Version, station => Station,
       ttl_ms => reusable_for(Rec, Now)}
     || #{key_id := KeyId, version := Version} = Rec <- Recs,
        advertisement_trusted(Rec, Trust),
        Station <- serving_station(Rec)].

%% How long a candidate built from `Rec' may be tried as a head start before
%% the DHT is asked about it again, as a DURATION from `Now'.
%%
%% RELATIVE ON PURPOSE. An absolute expiry has to be compared later against
%% a clock that may have stepped in between, and a client whose clock is a
%% minute out eats a fifth of a five-minute bound for nothing. A duration is
%% handed to the pool, which anchors it once against its own MONOTONIC clock
%% and never reads a wall clock again, so only elapsed time can end it.
%%
%% ⚠ THE SURFACES ARE THREE AND THE ADVERTISEMENT'S IS THE MINIMUM OF THEM,
%% BY AN INVARIANT THE RECORD MODULE ENFORCES. An advertisement stands on
%% its own lifetime, its org directory's and its procedure delegation's.
%% `macula_record:delegation_read/4' already computes
%% `min(expires_at(Dir), expires_at(Del))', and the advertisement is REFUSED
%% with `authorization_outlived' unless its own `expires_at' is at or before
%% that minimum. Only an advertisement that passed `verify_authorization/3'
%% becomes a candidate at all (`advertisement_trusted/2'), and a procedure
%% with no org namespace has no directory or delegation to outlive, so for
%% every record that reaches here the advertisement's own expiry IS the
%% minimum of the three. Reading the other two would mean decoding and
%% verifying two further ML-DSA-signed records per candidate, on the very
%% path this head start exists to shorten, to re-derive a number that is
%% already in hand. `macula_record_advertisement_tests' pins the invariant
%% this relies on; if that test goes, this has to compute the minimum
%% itself.
reusable_for(#{expires_at := ExpiresAt}, Now) ->
    max(0, ExpiresAt - Now).

serving_station(Rec) ->
    try macula_record:read_procedure_advertisement(Rec) of
        #{serving_station := Station} when is_binary(Station) -> [Station];
        _Unreadable -> []
    catch _:_ -> []
    end.

%% What an advertisement for `Procedure' in `Realm' is checked against:
%% the node's crypto profile, under which `macula:find_records/3' just
%% verified the records, and the realm key the pool pinned for `Realm', when
%% it pinned one. Only an advertisement that passes the check is a candidate
%% at all: see the module doc's "Trust model" section.
trust(Pool, Realm, Procedure) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    with_realm_key(macula_client:realm_key(Pool, Realm),
                   #{realm => Realm, procedure => Procedure, profile => Profile}).

with_realm_key({ok, RealmKey}, Trust) -> Trust#{realm_key => RealmKey};
with_realm_key(none, Trust)           -> Trust.

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

%% Sends the CALL to one resolved station. `not_connected' means the link
%% never came up within the candidate's share, so nothing was sent and the
%% next candidate may be tried; any other outcome means the CALL went out.
call_work(#{pool := Pool, call_station := CallStation, remember_resolved := Remember},
          Realm, Procedure, Payload, Deadline) ->
    fun(#{provider := Provider} = Candidate) ->
        fun(Station, DialUrl, Share) ->
            sent_or_not(
              settled(CallStation(Pool, DialUrl, Provider, Realm, Procedure, Payload,
                                  budget(Deadline),
                                  (pinned(Station))#{dial_timeout_ms => budget(Share)}),
                      Remember, Pool, Realm, Procedure, Candidate))
        end
    end.

%% Opens the stream at one resolved station, on the same terms as `call_work/5'.
stream_work(#{pool := Pool, call_stream_station := CallStreamStation,
              remember_resolved := Remember}, Realm, Procedure, Args, StreamOpts) ->
    fun(#{provider := Provider} = Candidate) ->
        fun(Station, DialUrl, Share) ->
            sent_or_not(
              settled(CallStreamStation(Pool, DialUrl, Provider, Realm, Procedure, Args,
                                        maps:merge(StreamOpts, (pinned(Station))#{
                                            dial_timeout_ms => budget(Share)})),
                      Remember, Pool, Realm, Procedure, Candidate))
        end
    end.

%% Whether a candidate's outcome settles the request or leaves the next one
%% worth trying. The judgement is `macula_station_link:failure_scope/1''s and
%% is NOT repeated here: this used to be a second, narrower copy of it,
%% recognising only `not_connected', and the two disagreed on five error
%% shapes, so a pool at its link cap or with its new-peer budget spent ended
%% resolution with every remaining candidate untried (macula#20).
%%
%% `candidate' is the only scope worth another station. `request' stops
%% because every candidate refuses it identically and walking on would spend
%% the caller's deadline collecting the same answer. `provider' stops because
%% the CALL may have reached one, and a call that has gone out must never be
%% sent somewhere else.
sent_or_not({error, _} = Failed) ->
    settled_or_next(macula_station_link:failure_scope(Failed), Failed);
sent_or_not(Sent) ->
    {sent, Sent}.

settled_or_next(candidate, NotSent) -> {not_sent, NotSent};
settled_or_next(Settled, Failed) when Settled =:= request; Settled =:= provider ->
    {sent, Failed}.

%% What one candidate's outcome settles beyond the result itself: it is
%% REPORTED, so a measurement can tell a call that started from a remembered
%% station apart from one that asked the DHT, and it is REMEMBERED when a
%% DHT-resolved candidate answered.
settled(Result, Remember, Pool, Realm, Procedure, Candidate) ->
    report_candidate(candidate_source(Candidate), outcome(Result), Candidate),
    remembering(Result, Remember, Pool, Realm, Procedure, Candidate).

candidate_source(#{dial := _Seed}) -> head_start;
candidate_source(_Resolved)        -> dht.

outcome({ok, _Answered})  -> answered;
outcome(_NotAnswered)     -> not_answered.

%% Agnostic by construction: every outcome of every candidate, no threshold.
%% Reading the split of head starts to DHT resolutions is the measurement's
%% job; producing it honestly is this one's.
report_candidate(Source, Outcome, #{provider := Provider, station := Station}) ->
    macula_diagnostics:event(<<"_macula.direct_dial.candidate_tried">>,
                             #{source => Source, outcome => Outcome,
                               provider => Provider, station => Station}).

%% Remembers the candidate that ANSWERED, and only a DHT-resolved one.
%%
%% Answered, because a CALL that went out and came back an error proves a
%% route to the station but not that the station still serves the procedure:
%% a station answering `unknown_next_peer' is precisely the one not to try
%% first next time.
%%
%% DHT-resolved, because only a candidate the DHT just produced carries a
%% `ttl_ms', and that is deliberate. A head start that answers is no fresh
%% evidence about the ADVERTISEMENT, so refreshing the horizon on a hit
%% would let one remembered station live for as long as it kept answering
%% and the DHT would never be asked again.
remembering({ok, _} = Answered, Remember, Pool, Realm, Procedure,
            #{ttl_ms := TtlMs} = Candidate) ->
    _ = Remember(Pool, Realm, Procedure, maps:without([ttl_ms], Candidate), TtlMs),
    Answered;
remembering(Result, _Remember, _Pool, _Realm, _Procedure, _Candidate) ->
    Result.

station_try(Dial, Work) ->
    fun(Candidate, Share, Seen) -> reach(Dial, Candidate, Share, Seen, Work(Candidate)) end.

%% Resolves a candidate's station endpoint within `Share' and hands it to
%% `Work', unless the candidate already failed on the same advertisement:
%% then one lookup tells whether its endpoint record has changed, and only a
%% change is worth another dial. A lookup that itself fails teaches nothing.
%%
%% A HEAD-START candidate (`dial') skips the lookup entirely. It carries the
%% seed the pool's own LIVE link to that station is keyed by, and a live
%% link is better evidence about a station than a signed record up to five
%% minutes old: a link either exists or it does not, so it cannot be stale.
%% The pool checks that the link is still live at the moment it hands the
%% candidate back (`macula_client:resolved_candidate/3'), and `ensure_link/3'
%% matches that seed before anything else, so the CALL reaches the link the
%% pool already holds and dials nothing.
%%
%% Its endpoint version is `none', which is the truth -- no endpoint record
%% was read -- and it also keeps a head-start failure from suppressing the
%% full attempt the first DHT pass makes on the same provider, since
%% `unless_unchanged/7' then sees a real version where it recorded `none'
%% and reads it as changed.
reach(_Dial, #{dial := Seed, provider := Key, version := Version, station := Station},
      Share, Seen, Work) ->
    attempt({found, {ok, {Station, Seed}}, none}, Key, Version, Share, Seen, Work);
reach(Dial, #{provider := Key, version := Version, station := Station}, Share, Seen,
      Work) ->
    reach_known(maps:find(Key, Seen), Dial, Key, Version, Station, Share, Seen, Work).

reach_known({ok, #{record := Version} = Prior}, Dial, Key, Version, Station, Share,
            Seen, Work) ->
    unless_unchanged(lookup_endpoint(Dial, Station, Share), Prior, Key, Version,
                     Share, Seen, Work);
reach_known(_NewOrChanged, Dial, Key, Version, Station, Share, Seen, Work) ->
    attempt(station_endpoint(Dial, Station, Share), Key, Version, Share, Seen, Work).

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
           {error, {unresolved, station_endpoint_expired}}, Seen);
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
lookup_endpoint(#{pool := Pool, find_record := FindRecord}, Station, Deadline) ->
    Key = macula_record:station_endpoint_key(Station),
    endpoint_lookup(FindRecord(Pool, Key, lookup_timeout(Deadline)), Station).

%% The record arrives verified under the node's crypto profile, so a record
%% that did not verify comes back as its refusal, with no version: an expired
%% one counts as a record with no usable endpoint, and any other refusal as a
%% record that does not verify.
endpoint_lookup({ok, #{version := Version} = Rec}, Station) ->
    {found, endpoint_result(station_signed_endpoint(Station, Rec)), Version};
endpoint_lookup({error, not_found}, _Station) ->
    {absent, {error, {unresolved, station_endpoint_not_found}}};
endpoint_lookup({error, expired}, _Station) ->
    {found, {error, expired}, none};
endpoint_lookup({error, Refusal}, _Station) when ?RECORD_REFUSAL(Refusal) ->
    {found, {error, {unresolved, Refusal}}, none};
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
station_endpoint(Dial, Station, Deadline) ->
    endpoint_lookups(Dial, Station, Deadline, {failed, {error, {unresolved, timeout}}}).

endpoint_lookups(Dial, Station, Deadline, Best) ->
    endpoint_or_again(lookup_endpoint(Dial, Station, Deadline), Dial, Station, Deadline,
                      Best).

endpoint_or_again({found, {ok, _}, _Version} = Found, _Dial, _Station, _Deadline, _Best) ->
    Found;
endpoint_or_again({found, {error, {unresolved, Reason}}, _Version} = Untrusted, _Dial,
                  _Station, _Deadline, _Best)
  when Reason =/= malformed_station_endpoint ->
    Untrusted;
endpoint_or_again(Lookup, Dial, Station, Deadline, Best) ->
    Recorded = endpoint_recorded(Best, Lookup, remaining(Deadline) > 0),
    pause(Deadline, ?RETRY_MS),
    endpoint_in_time(remaining(Deadline) > 0, Recorded, Dial, Station, Deadline).

endpoint_in_time(true, Best, Dial, Station, Deadline) ->
    endpoint_lookups(Dial, Station, Deadline, Best);
endpoint_in_time(false, Best, _Dial, _Station, _Deadline) ->
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
    {error, {unresolved, station_endpoint_expired}};
lookup_error({found, {error, _} = Error, _Version}) -> Error;
lookup_error({absent, Error}) -> Error;
lookup_error({failed, Error}) -> Error.

%% The `station_endpoint' record for `Station' must be SIGNED BY
%% `Station' itself (macula_station_announcer publishes it self-signed:
%% the station describing its own reachable address). The record arrives
%% verified under the node's crypto profile (`macula:find_record/3');
%% checking that its signer's node_id is exactly `Station', not just any
%% valid signer, is what makes pinning `expected_node_id => Station' on
%% the dial meaningful: without it, a record merely stored under the
%% right DHT key but signed by someone else would still be trusted, and
%% per-call pinning would authenticate the wrong thing.
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

pinned(Node) ->
    #{expected_node_id => Node}.

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
