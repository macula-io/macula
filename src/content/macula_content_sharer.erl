%% @doc Shares a pool's content (D27): the node keeps what it shares and serves it itself, and stations only relay.
%% One sharer per pool, ending with the pool.
%%
%% == Serving ==
%%
%% Content shared in a realm is served there on the node's own content procedure, a `server_stream' advertised
%% through the pool like any procedure (`macula:advertise_stream/6'):
%%
%% <ul>
%%   <li>`<org>/content_v1_<node id hex>' when `share/4' is given an `org' the node holds a delegation for;</li>
%%   <li>`~<node id hex>/content_v1', the node's own namespace (D25 item 6, revised 2026-09-24), otherwise.</li>
%% </ul>
%%
%% The node id is in the name because a station routes a procedure to one provider (macula-station#8): a shared
%% name would send every fetch to whichever node advertised last. One procedure per realm, fixed by its first share;
%% a later share in the realm under another org is refused. The procedure is withdrawn with the realm's last share.
%%
%% A realm serves what is shared in it and nothing else: each realm keeps its own store, and its procedure looks up
%% only there.
%%
%% == Announcing ==
%%
%% Each root is announced in the DHT under its content id, signed by the pool, naming the realm, the station the node
%% is reachable through now (its first connected link) and the procedure. An announcement lives `announce_ttl_ms' (an
%% hour by default) and is renewed at half that. Signing and storing it runs in a worker, so sharing and serving never
%% wait on the DHT. Every `station_check_ms' (30 s) the sharer compares the station it announced with the one it is
%% linked to: it announces everything again when they differ, and otherwise announces what is due, which is every
%% root whose last announcement did not land and every root whose renewal fell while no station was connected. With
%% no station connected the content is kept and served, and announced once a station is. Unsharing withdraws the last
%% announcement under the pool's signature, and one still in flight when it lands.
%%
%% == The mesh ==
%%
%% Every call to the mesh goes through the io map `start_link/2' takes (`status', `links', `advertise_stream',
%% `unadvertise_stream', `sign_node_record', `put_record', `withdraw_node_record'), the facade's own functions by
%% default, so a test replaces the mesh without replacing a module.
-module(macula_content_sharer).

-behaviour(gen_server).

-export([start_link/2, share/4, unshare/3, lookup/4, io/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(ANNOUNCE_TTL_MS, 3_600_000).
-define(STATION_CHECK_MS, 30_000).
-define(CALL_TIMEOUT_MS, 30_000).

%% A root shared in a realm. `announced' is its last announcement that landed, which an unshare withdraws. `renewal'
%% is `{announcing, Ref}' while a worker announces it, `{armed, Ref}' until its renewal fires, and `due' when it waits
%% for the next station check: its announcement did not land, or its renewal fell while no station was connected.
-record(share, {
    meta      :: map(),
    announced :: macula_record:m_record() | undefined,
    renewal   :: {announcing | armed, reference()} | due
}).

-record(state, {
    pool                :: pid(),
    io                  :: map(),
    node_id             :: <<_:256>>,
    stores = #{}        :: #{<<_:256>> => macula_content_store:store()},
    procedures = #{}    :: #{<<_:256>> => binary()},
    shares = #{}        :: #{{<<_:256>>, macula:mcid()} => #share{}},
    station             :: <<_:256>> | undefined,
    %% The announcing workers: pid to its monitor and the share and reference it announces.
    workers = #{}       :: #{pid() => {reference(), {<<_:256>>, macula:mcid()}, reference()}},
    announce_ttl_ms     :: pos_integer(),
    station_check_ms    :: pos_integer()
}).

%% @doc The facade's own functions, the io a sharer uses when not given another.
-spec io() -> map().
io() ->
    #{status => fun macula:status/1,
      links => fun macula:links/1,
      advertise_stream => fun macula:advertise_stream/6,
      unadvertise_stream => fun macula:unadvertise_stream/3,
      sign_node_record => fun macula:sign_node_record/3,
      put_record => fun macula:put_record/2,
      withdraw_node_record => fun macula:withdraw_node_record/3}.

%% @doc Start a sharer for `Pool'. `Opts' holds io entries (see `io/0') and `announce_ttl_ms', `station_check_ms'.
-spec start_link(pid(), map()) -> {ok, pid()} | {error, term()}.
start_link(Pool, Opts) when is_pid(Pool), is_map(Opts) ->
    gen_server:start_link(?MODULE, {Pool, Opts}, []).

%% @doc Share `Bytes' in `Realm': keep them, serve them, announce their root. `Opts' may name the content (`name')
%% and the org whose delegation serves it (`org').
-spec share(pid(), <<_:256>>, binary(), map()) -> {ok, macula:mcid()} | {error, term()}.
share(Sharer, Realm, Bytes, Opts) when is_binary(Realm), byte_size(Realm) =:= 32, is_binary(Bytes), is_map(Opts) ->
    gen_server:call(Sharer, {share, Realm, Bytes, Opts}, ?CALL_TIMEOUT_MS).

%% @doc Stop sharing the root `MCID' in `Realm'. Idempotent.
-spec unshare(pid(), <<_:256>>, macula:mcid()) -> ok.
unshare(Sharer, Realm, MCID) ->
    gen_server:call(Sharer, {unshare, Realm, MCID}, ?CALL_TIMEOUT_MS).

%% @doc What this sharer answers in `Realm' for `Want' of `MCID' (`macula_content_serve:lookup/3').
-spec lookup(pid(), <<_:256>>, macula_content_serve:want(), macula:mcid()) ->
    {ok, macula_content_serve:body()} | not_found.
lookup(Sharer, Realm, Want, MCID) ->
    gen_server:call(Sharer, {lookup, Realm, Want, MCID}).

%%====================================================================
%% gen_server
%%====================================================================

init({Pool, Opts}) ->
    Io = maps:merge(io(), maps:with(maps:keys(io()), Opts)),
    erlang:monitor(process, Pool),
    {ok, #{self_node_id := NodeId}} = (maps:get(status, Io))(Pool),
    CheckMs = maps:get(station_check_ms, Opts, ?STATION_CHECK_MS),
    erlang:send_after(CheckMs, self(), station_check),
    {ok, #state{pool = Pool, io = Io, node_id = NodeId,
                announce_ttl_ms = maps:get(announce_ttl_ms, Opts, ?ANNOUNCE_TTL_MS),
                station_check_ms = CheckMs}}.

handle_call({share, Realm, Bytes, Opts}, _From, S) ->
    shared(procedure_for(Realm, Opts, S), Realm, Bytes, Opts, S);
handle_call({unshare, Realm, MCID}, _From, S) ->
    {reply, ok, unshared(maps:find({Realm, MCID}, S#state.shares), Realm, MCID, S)};
handle_call({lookup, Realm, Want, MCID}, _From, #state{stores = Stores} = S) ->
    {reply, looked_up(maps:find(Realm, Stores), Want, MCID), S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info(station_check, #state{station_check_ms = Ms} = S) ->
    erlang:send_after(Ms, self(), station_check),
    {noreply, station_checked(connected_station(S), S)};
handle_info({renew, Key, Ref}, #state{shares = Shares} = S) ->
    {noreply, renewed(maps:find(Key, Shares), Key, Ref, S)};
handle_info({announce_done, Worker, Result}, #state{workers = Workers} = S) ->
    {{Mon, Key, Ref}, Rest} = maps:take(Worker, Workers),
    true = erlang:demonitor(Mon, [flush]),
    {noreply, landed(maps:find(Key, S#state.shares), Key, Ref, Result, S#state{workers = Rest})};
handle_info({'DOWN', _Mon, process, Pool, _Reason}, #state{pool = Pool} = S) ->
    {stop, normal, S};
handle_info({'DOWN', _Mon, process, Worker, Reason}, #state{workers = Workers} = S) ->
    {noreply, worker_lost(maps:take(Worker, Workers), Reason, S)};
handle_info(_Msg, S) ->
    {noreply, S}.

looked_up({ok, Store}, Want, MCID) -> macula_content_serve:lookup(Want, MCID, Store);
looked_up(error, _Want, _MCID) -> not_found.

%%====================================================================
%% Sharing
%%====================================================================

%% The realm's procedure: the one registered already, or the one this share names. Another org than the realm's
%% procedure serves under is refused.
procedure_for(Realm, Opts, #state{procedures = Procs, node_id = NodeId}) ->
    same_procedure(maps:find(Realm, Procs), procedure_name(maps:get(org, Opts, undefined), NodeId)).

same_procedure({ok, Proc}, Proc) -> {registered, Proc};
same_procedure({ok, Registered}, _Other) -> {error, {content_procedure, Registered}};
same_procedure(error, Proc) -> {new, Proc}.

procedure_name(undefined, NodeId) -> <<"~", (hex(NodeId))/binary, "/content_v1">>;
procedure_name(Org, NodeId) when is_binary(Org) -> <<Org/binary, "/content_v1_", (hex(NodeId))/binary>>.

shared({error, _} = Refusal, _Realm, _Bytes, _Opts, S) ->
    {reply, Refusal, S};
shared({new, Proc}, Realm, Bytes, Opts, S) ->
    registered(advertised(Proc, Realm, S), Proc, Realm, Bytes, Opts, S);
shared({registered, _Proc}, Realm, Bytes, Opts, S) ->
    kept(Realm, Bytes, Opts, S).

%% The realm's procedure serves from the realm's own store.
advertised(Proc, Realm, #state{pool = Pool, io = #{advertise_stream := Advertise}}) ->
    Sharer = self(),
    Handler = fun(Stream, Args) ->
                  macula_content_serve:serve(Stream, Args, fun(Want, MCID) -> lookup(Sharer, Realm, Want, MCID) end)
              end,
    Advertise(Pool, Realm, Proc, server_stream, Handler, #{}).

registered(ok, Proc, Realm, Bytes, Opts, #state{procedures = Procs} = S) ->
    kept(Realm, Bytes, Opts, S#state{procedures = Procs#{Realm => Proc}});
%% The pool keeps a registration no link could take yet and replays it when one comes up, so the share stands.
registered({error, no_healthy_station}, Proc, Realm, Bytes, Opts, S) ->
    registered(ok, Proc, Realm, Bytes, Opts, S);
registered({error, _} = Refusal, _Proc, _Realm, _Bytes, _Opts, S) ->
    {reply, Refusal, S}.

%% Sharing what the realm shares already announces nothing new.
kept(Realm, Bytes, Opts, #state{stores = Stores, shares = Shares} = S) ->
    {MCID, Store} = macula_content_store:added(Bytes, Opts, maps:get(Realm, Stores, macula_content_store:new())),
    S1 = S#state{stores = Stores#{Realm => Store}},
    {reply, {ok, MCID}, new_share(maps:is_key({Realm, MCID}, Shares), {Realm, MCID}, share_meta(Bytes, Opts, Store, MCID), S1)}.

new_share(true, _Key, _Meta, S) ->
    S;
new_share(false, Key, Meta, #state{shares = Shares} = S) ->
    S1 = S#state{shares = Shares#{Key => #share{meta = Meta, renewal = due}}},
    announced_through(connected_station(S1), Key, S1).

%% What an announcement carries besides where: the name, the size and, for a manifest, the chunk count.
share_meta(Bytes, Opts, Store, MCID) ->
    maps:merge(#{size => byte_size(Bytes)},
               maps:merge(maps:with([name], Opts), chunk_meta(macula_content_store:root(MCID, Store)))).

chunk_meta({manifest, #{chunk_count := Count}}) -> #{chunk_count => Count};
chunk_meta(_Block) -> #{}.

unshared({ok, #share{announced = Announced}}, Realm, MCID, #state{stores = Stores, shares = Shares} = S) ->
    withdrawn(Announced, S),
    S1 = S#state{stores = Stores#{Realm := macula_content_store:removed(MCID, maps:get(Realm, Stores))},
                 shares = maps:remove({Realm, MCID}, Shares)},
    procedure_released(realm_still_shared(Realm, S1), Realm, S1);
unshared(error, _Realm, _MCID, S) ->
    S.

withdrawn(undefined, _S) ->
    ok;
withdrawn(Announced, #state{pool = Pool, io = #{withdraw_node_record := Withdraw, put_record := Put}}) ->
    tombstone_put(Withdraw(Pool, Announced, shutdown), Pool, Put).

tombstone_put({ok, Tombstone}, Pool, Put) -> _ = Put(Pool, Tombstone), ok;
tombstone_put({error, Reason}, _Pool, _Put) ->
    logger:warning("[macula_content_sharer] announcement not withdrawn: ~p", [Reason]).

realm_still_shared(Realm, #state{shares = Shares}) ->
    lists:any(fun({R, _MCID}) -> R =:= Realm end, maps:keys(Shares)).

procedure_released(true, _Realm, S) ->
    S;
procedure_released(false, Realm, #state{pool = Pool, procedures = Procs, stores = Stores,
                                        io = #{unadvertise_stream := Unadvertise}} = S) ->
    _ = Unadvertise(Pool, Realm, maps:get(Realm, Procs)),
    S#state{procedures = maps:remove(Realm, Procs), stores = maps:remove(Realm, Stores)}.

%%====================================================================
%% Announcing
%%====================================================================

connected_station(#state{pool = Pool, io = #{links := Links}}) ->
    first_connected(Links(Pool)).

first_connected({ok, Links}) ->
    case [S || #{connected := true, node_id := S} <- Links, is_binary(S), byte_size(S) =:= 32] of
        [Station | _] -> Station;
        [] -> undefined
    end;
first_connected(_Error) ->
    undefined.

%% Announce one root through `Station', in a worker that answers `{announce_done, ...}'. With no station it is due at
%% the next station check.
announced(Key, undefined, #state{shares = Shares} = S) ->
    S#state{shares = Shares#{Key := (maps:get(Key, Shares))#share{renewal = due}}};
announced(Key, Station, #state{shares = Shares} = S) ->
    Share = maps:get(Key, Shares),
    Ref = make_ref(),
    Sharer = self(),
    Unsigned = unsigned_announcement(Key, Station, Share, S),
    {Worker, Mon} = spawn_monitor(fun() -> Sharer ! {announce_done, self(), announce(S, Unsigned)} end),
    S#state{workers = (S#state.workers)#{Worker => {Mon, Key, Ref}},
            shares = Shares#{Key := Share#share{renewal = {announcing, Ref}}}}.

unsigned_announcement({Realm, MCID}, Station, #share{meta = Meta},
                      #state{node_id = NodeId, procedures = Procs, announce_ttl_ms = Ttl}) ->
    macula_record:content_announcement(
      NodeId, MCID,
      Meta#{realm_id => Realm, serving_station => Station, procedure => maps:get(Realm, Procs), ttl_ms => Ttl}).

%% Sign and store one announcement: the signed record once it landed, or why it did not.
announce(#state{pool = Pool, io = #{sign_node_record := Sign, put_record := Put}}, Unsigned) ->
    stored(Sign(Pool, Unsigned, #{}), Pool, Put).

stored({ok, Signed}, Pool, Put) -> put_answered(Put(Pool, Signed), Signed);
stored({error, Reason}, _Pool, _Put) -> {error, {not_signed, Reason}}.

put_answered(ok, Signed) -> {ok, Signed};
put_answered({error, Reason}, Signed) -> {error, {not_stored, Reason, Signed}}.

%% An announcement's outcome. The share's current one arms its renewal, or makes it due; an outcome for a share
%% unshared meanwhile has its record withdrawn, since it may have landed after the unshare's withdrawal.
landed({ok, #share{renewal = {announcing, Ref}} = Share}, Key, Ref, {ok, Signed}, #state{shares = Shares} = S) ->
    erlang:send_after(S#state.announce_ttl_ms div 2, self(), {renew, Key, Ref}),
    S#state{shares = Shares#{Key := Share#share{announced = Signed, renewal = {armed, Ref}}}};
landed({ok, #share{renewal = {announcing, Ref}} = Share}, Key, Ref, {error, Reason}, #state{shares = Shares} = S) ->
    logger:warning("[macula_content_sharer] announcement not made, again at the next station check: ~p",
                   [reason_of(Reason)]),
    S#state{shares = Shares#{Key := Share#share{renewal = due}}};
landed(error, _Key, _Ref, Result, S) ->
    withdrawn(record_of(Result), S),
    S;
landed({ok, _Superseded}, _Key, _Ref, _Result, S) ->
    S.

reason_of({not_stored, Reason, _Signed}) -> Reason;
reason_of(Reason) -> Reason.

record_of({ok, Signed}) -> Signed;
record_of({error, {not_stored, _Reason, Signed}}) -> Signed;
record_of({error, _}) -> undefined.

%% A worker that died without answering: its share, if still announcing that reference, is due at the next station
%% check.
worker_lost({{_Mon, Key, Ref}, Rest}, Reason, S) ->
    logger:warning("[macula_content_sharer] announcing worker died: ~p", [Reason]),
    landed(maps:find(Key, S#state.shares), Key, Ref, {error, {worker_died, Reason}}, S#state{workers = Rest});
worker_lost(error, _Reason, S) ->
    S.

%% A renewal that finds the node on another station than the one announced moves every share there, as a station check
%% would.
renewed({ok, #share{renewal = {armed, Ref}}}, Key, Ref, S) ->
    announced_through(connected_station(S), Key, S);
renewed(_StaleOrGone, _Key, _Ref, S) ->
    S.

%% One share announced through the node's station, or, when the node is on another station than the one announced,
%% every share through it.
announced_through(Station, Key, #state{station = Station} = S) ->
    announced(Key, Station, S);
announced_through(undefined, Key, S) ->
    announced(Key, undefined, S);
announced_through(Station, _Key, S) ->
    station_checked(Station, S).

%% Announce everything again when the node is now reachable through another station, or what is due when through the
%% same one.
station_checked(undefined, S) ->
    S;
station_checked(Station, #state{station = Station, shares = Shares} = S) ->
    announced_all([K || K := #share{renewal = due} <- Shares], Station, S);
station_checked(Station, #state{shares = Shares} = S) ->
    announced_all(maps:keys(Shares), Station, S#state{station = Station}).

announced_all(Keys, Station, S) ->
    lists:foldl(fun(Key, Acc) -> announced(Key, Station, Acc) end, S, Keys).

hex(NodeId) -> binary:encode_hex(NodeId, lowercase).
