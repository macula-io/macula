%%%-------------------------------------------------------------------
%%% @doc The client's station-authorization check (macula-station#63, hook 3).
%%%
%%% A client about to dial a node as a station reads that station's
%%% authorization slot — `station_authorization_key(Realm, Station)' — through
%%% its own station link (the `find_record' it dials with) and requires a live
%%% authorization. The check is mode-aware, and the mode is fleet
%%% configuration (`application:get_env(macula, station_authorization_mode)');
%%% S1/S2 ship in `open', the flip to `closed' is S4:
%%%
%%%   <ul>
%%%     <li>`open' — the check runs, the dial proceeds, and a `would_refuse'
%%%     counter (the pool's `station_checks' status) increments when there is
%%%     no live authorization;</li>
%%%     <li>`log' — as `open', plus one log line per would-be refusal;</li>
%%%     <li>`closed' — no live authorization refuses the dial.</li>
%%%   </ul>
%%%
%%% The first dial a pool makes is its configured seed, from config trust:
%%% there is no link yet to read a slot through. Every later dial of a node
%%% as a station — the stations `macula_direct_dial' resolves — is
%%% slot-verified here. Nothing is presented on the wire (presentation
%%% decision (c), DHT-lookup-only), so the check has no replay surface.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_station_check).

-export([authorized/4, classify/2, enforce/2, mode/0]).

-export_type([mode/0, verdict/0, trust/0]).

%% The type tags this check reads: the realm's statement that a node is one of
%% its stations (`station_authorization', 16#13), and the realm's revocation of
%% it (a tombstone at the same slot, 16#0C).
-define(STATION_AUTHORIZATION, 16#13).
-define(TOMBSTONE, 16#0C).

-type mode() :: open | log | closed.
-type verdict() :: live | tombstoned | stale | none | unanchored | unreadable.
%% What one slot read is judged against: the dialed pair, the realm key the
%% pool pinned for the realm (the realm's signing key, the one that signs its
%% station authorizations and their tombstones), and the verifier's clock.
-type trust() :: #{realm := <<_:256>>, station := <<_:256>>, realm_key := binary(), now := integer()}.

%% @doc The check: read `Station''s authorization slot for `Realm' — through
%% `Dial''s `find_record', within `TimeoutMs' — judge it against the realm key
%% the pool pinned for `Realm', and settle it under the configured mode. `ok'
%% admits the dial; `{error, {station_not_authorized, Verdict}}' refuses it
%% (mode `closed' only). A pool that pinned no key for `Realm' has nothing to
%% judge a record by: its verdict is `unanchored' and the slot is not read.
%% Every check is counted on the pool's `station_checks'.
-spec authorized(#{pool := macula:pool(),
                   find_record := fun((macula:pool(), binary(), pos_integer()) ->
                                          {ok, macula_record:m_record()} | {error, term()}),
                   realm_key := {ok, binary()} | none},
                 <<_:256>>, <<_:256>>, pos_integer()) ->
          ok | {error, {station_not_authorized, verdict()}}.
authorized(#{realm_key := RealmKey} = Dial, Realm, Station, TimeoutMs) ->
    Mode = mode(),
    Verdict = judged(RealmKey, Dial, Realm, Station, TimeoutMs),
    settle(enforce(Mode, Verdict), Mode, Dial, Realm, Station).

judged({ok, RealmKey}, Dial, Realm, Station, TimeoutMs) ->
    classify(read(Dial, Realm, Station, TimeoutMs),
             #{realm => Realm, station => Station, realm_key => RealmKey,
               now => erlang:system_time(millisecond)});
judged(none, _Dial, _Realm, _Station, _TimeoutMs) ->
    unanchored.

settle(admit, _Mode, Dial, _Realm, _Station) ->
    note(Dial, live),
    ok;
settle({would_refuse, Verdict}, Mode, Dial, Realm, Station) ->
    note(Dial, would_refuse),
    log_would_refuse(Verdict, Realm, Station, Mode),
    ok;
settle({refuse, Verdict}, _Mode, Dial, _Realm, _Station) ->
    note(Dial, refused),
    {error, {station_not_authorized, Verdict}}.

%% @doc What one slot read means, judged against `Trust'. A record counts only
%% when the pinned realm key signed it: the read verified a signature and the
%% signer's purpose, and any process can mint a key of purpose `realm', so the
%% signer itself must be the realm's. Then an authorization for exactly the
%% dialed realm and station is `live' inside its validity window (with the
%% records' clock tolerance) and `stale' outside it; one for another realm or
%% station is `none'; the realm's tombstone over the type is `tombstoned';
%% nothing at all is `none'; anything else — a record or tombstone another key
%% signed, a record of another type, or a failed lookup — is `unreadable'. So
%% `live' is the only verdict that admits a dial under `closed'.
-spec classify({ok, macula_record:m_record()} | {error, term()}, trust()) -> verdict().
classify({ok, #{key := RealmKey, type := ?STATION_AUTHORIZATION} = Record}, #{realm_key := RealmKey} = Trust) ->
    for_pair(macula_record:read_station_authorization(Record), Trust);
classify({ok, #{key := RealmKey, type := ?TOMBSTONE} = Record}, #{realm_key := RealmKey}) ->
    case macula_record:read_tombstone(Record) of
        #{withdrawn_type := ?STATION_AUTHORIZATION} -> tombstoned;
        _AnotherType -> unreadable
    end;
classify({error, not_found}, _Trust) ->
    none;
classify(_Other, _Trust) ->
    unreadable.

for_pair(#{realm_id := Realm, station_node := Station, valid_from := From, valid_until := Until},
         #{realm := Realm, station := Station, now := Now}) ->
    in_window(Now, From, Until, macula_record:clock_tolerance_ms());
for_pair(_AnotherRealmOrStation, _Trust) ->
    none.

in_window(Now, From, Until, Tolerance) when Now >= From - Tolerance, Now =< Until + Tolerance -> live;
in_window(_Now, _From, _Until, _Tolerance) -> stale.

%% @doc What a verdict means under a mode: `live' admits under every mode;
%% anything else is a would-be refusal under `open'/`log' and a refusal under
%% `closed'.
-spec enforce(mode(), verdict()) -> admit | {would_refuse, verdict()} | {refuse, verdict()}.
enforce(_Mode, live) -> admit;
enforce(closed, Verdict) -> {refuse, Verdict};
enforce(_OpenOrLog, Verdict) -> {would_refuse, Verdict}.

%% @doc The mode the client checks in, from fleet config. Unset reads as
%% `open' (S1/S2 ship open); a value that is none of the three also reads as
%% `open', reported as an error — returning to `open' needs no release, so the
%% configured value must be one of the three deliberately.
-spec mode() -> mode().
mode() ->
    case application:get_env(macula, station_authorization_mode, open) of
        open -> open;
        log -> log;
        closed -> closed;
        Other ->
            logger:error("[macula] station_authorization_mode ~p is not open|log|closed; reading as open",
                         [Other]),
            open
    end.

read(#{pool := Pool, find_record := FindRecord}, Realm, Station, TimeoutMs) ->
    FindRecord(Pool, macula_record:station_authorization_key(Realm, Station), TimeoutMs).

note(#{pool := Pool}, Outcome) ->
    macula_client:note_station_check(Pool, Outcome).

log_would_refuse(Verdict, Realm, Station, log) ->
    logger:notice("[macula] a station authorization check would refuse: realm ~p, station ~p (~p); the dial proceeds",
                  [Realm, Station, Verdict]);
log_would_refuse(_Verdict, _Realm, _Station, _Open) ->
    ok.
