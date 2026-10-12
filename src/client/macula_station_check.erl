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

-export([authorized/4, classify/3, enforce/2, mode/0]).

-export_type([mode/0, verdict/0]).

%% The type tags this check reads: the realm's statement that a node is one of
%% its stations (`station_authorization', 16#13), and the realm's revocation of
%% it (a tombstone at the same slot, 16#0C).
-define(STATION_AUTHORIZATION, 16#13).
-define(TOMBSTONE, 16#0C).

-type mode() :: open | log | closed.
-type verdict() :: live | tombstoned | none | unreadable.

%% @doc The check: read `Station''s authorization slot for `Realm' — through
%% `Dial''s `find_record', within `TimeoutMs' — and settle it under the
%% configured mode. `ok' admits the dial; `{error, {station_not_authorized,
%% Verdict}}' refuses it (mode `closed' only). Every check is counted on the
%% pool's `station_checks'.
-spec authorized(#{pool := macula:pool(),
                   find_record := fun((macula:pool(), binary(), pos_integer()) ->
                                          {ok, macula_record:m_record()} | {error, term()})},
                 <<_:256>>, <<_:256>>, pos_integer()) ->
          ok | {error, {station_not_authorized, verdict()}}.
authorized(Dial, Realm, Station, TimeoutMs) ->
    Verdict = classify(read(Dial, Realm, Station, TimeoutMs), Realm, Station),
    settle(enforce(mode(), Verdict), Dial, Realm, Station, Verdict).

settle(admit, Dial, _Realm, _Station, _Verdict) ->
    note(Dial, live),
    ok;
settle({would_refuse, Verdict}, Dial, Realm, Station, _Verdict) ->
    note(Dial, would_refuse),
    log_would_refuse(Verdict, Realm, Station, mode()),
    ok;
settle({refuse, Verdict}, Dial, _Realm, _Station, _Verdict) ->
    note(Dial, refused),
    {error, {station_not_authorized, Verdict}}.

%% @doc What one slot read means: an authorization for exactly `Realm' and
%% `Station' is `live' (the read verified it); the realm's tombstone over that
%% type is `tombstoned'; nothing at all is `none'; anything else — a record of
%% another realm or station, another type, or a failed lookup — is
%% `unreadable'. So `live' is the only verdict that admits a dial under
%% `closed'.
-spec classify({ok, macula_record:m_record()} | {error, term()}, <<_:256>>, <<_:256>>) -> verdict().
classify({ok, #{type := ?STATION_AUTHORIZATION} = Record}, Realm, Station) ->
    case macula_record:read_station_authorization(Record) of
        #{realm_id := Realm, station_node := Station} -> live;
        _AnotherRealmOrStation -> none
    end;
classify({ok, #{type := ?TOMBSTONE} = Record}, _Realm, _Station) ->
    case macula_record:read_tombstone(Record) of
        #{withdrawn_type := ?STATION_AUTHORIZATION} -> tombstoned;
        _AnotherType -> unreadable
    end;
classify({error, not_found}, _Realm, _Station) ->
    none;
classify(_Other, _Realm, _Station) ->
    unreadable.

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
