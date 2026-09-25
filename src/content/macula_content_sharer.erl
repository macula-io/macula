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
%% == Announcing ==
%%
%% Each root is announced in the DHT under its content id, signed by the pool, naming the realm, the station the node
%% is reachable through now (its first connected link) and the procedure. An announcement lives `announce_ttl_ms' (an
%% hour by default) and is renewed at half that; every `station_check_ms' (30 s) the sharer compares the station it
%% announced with the one it is linked to, and announces everything again when they differ. With no station
%% connected the content is kept and served, and announced once a station is. Unsharing withdraws the announcement
%% under the pool's signature.
%%
%% == The mesh ==
%%
%% Every call to the mesh goes through the io map `start_link/2' takes (`status', `links', `advertise_stream',
%% `unadvertise_stream', `sign_node_record', `put_record', `withdraw_node_record'), the facade's own functions by
%% default, so a test replaces the mesh without replacing a module.
-module(macula_content_sharer).

-behaviour(gen_server).

-export([start_link/2, share/4, unshare/3, lookup/3, io/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(ANNOUNCE_TTL_MS, 3_600_000).
-define(STATION_CHECK_MS, 30_000).
-define(CALL_TIMEOUT_MS, 30_000).

-record(share, {
    realm     :: <<_:256>>,
    opts      :: map(),
    announced :: macula_record:m_record() | undefined,
    renewal   :: reference() | undefined
}).

-record(state, {
    pool                :: pid(),
    io                  :: map(),
    node_id             :: <<_:256>>,
    store               :: macula_content_store:store(),
    procedures = #{}    :: #{<<_:256>> => binary()},
    shares = #{}        :: #{macula:mcid() => #share{}},
    station             :: <<_:256>> | undefined,
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

%% @doc What this sharer answers for `Want' of `MCID' (`macula_content_serve:lookup/3').
-spec lookup(pid(), macula_content_serve:want(), macula:mcid()) -> {ok, macula_content_serve:body()} | not_found.
lookup(Sharer, Want, MCID) ->
    gen_server:call(Sharer, {lookup, Want, MCID}).

%%====================================================================
%% gen_server
%%====================================================================

init({Pool, Opts}) ->
    Io = maps:merge(io(), maps:with(maps:keys(io()), Opts)),
    erlang:monitor(process, Pool),
    {ok, #{self_node_id := NodeId}} = (maps:get(status, Io))(Pool),
    CheckMs = maps:get(station_check_ms, Opts, ?STATION_CHECK_MS),
    erlang:send_after(CheckMs, self(), station_check),
    {ok, #state{pool = Pool, io = Io, node_id = NodeId, store = macula_content_store:new(),
                announce_ttl_ms = maps:get(announce_ttl_ms, Opts, ?ANNOUNCE_TTL_MS),
                station_check_ms = CheckMs}}.

handle_call({share, Realm, Bytes, Opts}, _From, S) ->
    shared(procedure_for(Realm, Opts, S), Realm, Bytes, Opts, S);
handle_call({unshare, Realm, MCID}, _From, S) ->
    {reply, ok, unshared(maps:find(MCID, S#state.shares), Realm, MCID, S)};
handle_call({lookup, Want, MCID}, _From, #state{store = Store} = S) ->
    {reply, macula_content_serve:lookup(Want, MCID, Store), S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info(station_check, #state{station_check_ms = Ms} = S) ->
    erlang:send_after(Ms, self(), station_check),
    {noreply, station_checked(connected_station(S), S)};
handle_info({renew, MCID, Ref}, #state{shares = Shares} = S) ->
    {noreply, renewed(maps:find(MCID, Shares), MCID, Ref, S)};
handle_info({'DOWN', _Mon, process, Pool, _Reason}, #state{pool = Pool} = S) ->
    {stop, normal, S};
handle_info(_Msg, S) ->
    {noreply, S}.

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

advertised(Proc, Realm, #state{pool = Pool, io = #{advertise_stream := Advertise}}) ->
    Sharer = self(),
    Handler = fun(Stream, Args) ->
                  macula_content_serve:serve(Stream, Args, fun(Want, MCID) -> lookup(Sharer, Want, MCID) end)
              end,
    Advertise(Pool, Realm, Proc, server_stream, Handler, #{}).

registered(ok, Proc, Realm, Bytes, Opts, #state{procedures = Procs} = S) ->
    kept(Realm, Bytes, Opts, S#state{procedures = Procs#{Realm => Proc}});
%% The pool keeps a registration no link could take yet and replays it when one comes up, so the share stands.
registered({error, no_healthy_station}, Proc, Realm, Bytes, Opts, S) ->
    registered(ok, Proc, Realm, Bytes, Opts, S);
registered({error, _} = Refusal, _Proc, _Realm, _Bytes, _Opts, S) ->
    {reply, Refusal, S}.

kept(Realm, Bytes, Opts, #state{store = Store, shares = Shares} = S) ->
    {MCID, Store1} = macula_content_store:added(Bytes, Opts, Store),
    Share = maps:get(MCID, Shares, #share{realm = Realm, opts = share_meta(Bytes, Opts, Store1, MCID)}),
    S1 = S#state{store = Store1, shares = Shares#{MCID => Share}},
    {reply, {ok, MCID}, announced(MCID, connected_station(S1), S1)}.

%% What an announcement carries besides where: the name, the size and, for a manifest, the chunk count.
share_meta(Bytes, Opts, Store, MCID) ->
    maps:merge(#{size => byte_size(Bytes)},
               maps:merge(maps:with([name], Opts), chunk_meta(macula_content_store:root(MCID, Store)))).

chunk_meta({manifest, #{chunk_count := Count}}) -> #{chunk_count => Count};
chunk_meta(_Block) -> #{}.

unshared({ok, #share{realm = Realm, announced = Announced}}, Realm, MCID,
         #state{store = Store, shares = Shares} = S) ->
    withdrawn(Announced, S),
    S1 = S#state{store = macula_content_store:removed(MCID, Store), shares = maps:remove(MCID, Shares)},
    procedure_released(realm_still_shared(Realm, S1), Realm, S1);
unshared(_NotSharedHere, _Realm, _MCID, S) ->
    S.

withdrawn(undefined, _S) ->
    ok;
withdrawn(Announced, #state{pool = Pool, io = #{withdraw_node_record := Withdraw, put_record := Put}}) ->
    tombstone_put(Withdraw(Pool, Announced, shutdown), Pool, Put).

tombstone_put({ok, Tombstone}, Pool, Put) -> _ = Put(Pool, Tombstone), ok;
tombstone_put({error, Reason}, _Pool, _Put) ->
    logger:warning("[macula_content_sharer] announcement not withdrawn: ~p", [Reason]).

realm_still_shared(Realm, #state{shares = Shares}) ->
    lists:any(fun(#share{realm = R}) -> R =:= Realm end, maps:values(Shares)).

procedure_released(true, _Realm, S) ->
    S;
procedure_released(false, Realm, #state{pool = Pool, procedures = Procs, io = #{unadvertise_stream := Unadvertise}} = S) ->
    _ = Unadvertise(Pool, Realm, maps:get(Realm, Procs)),
    S#state{procedures = maps:remove(Realm, Procs)}.

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

%% Announce one root through `Station'; nothing to announce through with no station.
announced(_MCID, undefined, S) ->
    S;
announced(MCID, Station, #state{shares = Shares} = S) ->
    %% A renewal armed earlier is left to fire: it carries the reference it was armed with, and only the latest
    %% reference renews (renewed/4).
    #share{realm = Realm, opts = Meta} = Share = maps:get(MCID, Shares),
    Announced = announcement_put(signed_announcement(MCID, Realm, Station, Meta, S), S),
    Ref = make_ref(),
    erlang:send_after(S#state.announce_ttl_ms div 2, self(), {renew, MCID, Ref}),
    S#state{station = Station,
            shares = Shares#{MCID => Share#share{announced = Announced, renewal = Ref}}}.

signed_announcement(MCID, Realm, Station, Meta,
                    #state{pool = Pool, node_id = NodeId, procedures = Procs, announce_ttl_ms = Ttl,
                           io = #{sign_node_record := Sign}}) ->
    Unsigned = macula_record:content_announcement(
                 NodeId, MCID,
                 Meta#{realm_id => Realm, serving_station => Station, procedure => maps:get(Realm, Procs),
                       ttl_ms => Ttl}),
    Sign(Pool, Unsigned, #{}).

announcement_put({ok, Signed}, #state{pool = Pool, io = #{put_record := Put}}) ->
    put_logged(Put(Pool, Signed)),
    Signed;
announcement_put({error, Reason}, _S) ->
    logger:warning("[macula_content_sharer] announcement not signed: ~p", [Reason]),
    undefined.

put_logged(ok) -> ok;
put_logged({error, Reason}) -> logger:warning("[macula_content_sharer] announcement not stored: ~p", [Reason]).

renewed({ok, #share{renewal = Ref}}, MCID, Ref, S) ->
    announced(MCID, connected_station(S), S);
renewed(_StaleOrGone, _MCID, _Ref, S) ->
    S.

%% Announce everything again when the node is now reachable through another station, or through one at all.
station_checked(undefined, S) ->
    S;
station_checked(Station, #state{station = Station} = S) ->
    unannounced_announced(Station, S);
station_checked(Station, #state{shares = Shares} = S) ->
    lists:foldl(fun(MCID, Acc) -> announced(MCID, Station, Acc) end, S, maps:keys(Shares)).

%% Roots shared while no station was connected are announced as soon as one is.
unannounced_announced(Station, #state{shares = Shares} = S) ->
    lists:foldl(fun(MCID, Acc) -> announced(MCID, Station, Acc) end, S,
                [M || M := #share{announced = undefined} <- Shares]).

hex(NodeId) -> binary:encode_hex(NodeId, lowercase).
