%% @doc How many handshake v5 session proofs this station signs: at most 30 a minute for one client node, and 30 a
%% second for all clients together (plans/DESIGN_NEIGHBOUR_CHANNEL_BINDING.md section 3, "Signing cost as an attack
%% surface"). A composite session proof costs 6 to 10 ms of signing, so the total bounds this station's signing to
%% about a quarter of one core, and a reconnect storm of 1000 clients is admitted in about 30 seconds. The station asks
%% only after the client's CONNECT proof has verified, so a refusal here costs the client a composite signature of its
%% own. Past either limit the station refuses CONNECT with session_proof_rate.
%%
%% Fixed windows: the minute and the second a request falls in. This process only owns the table and purges the
%% windows that ended, once a minute; every count goes to the table directly.
-module(macula_session_proof_rate).
-behaviour(gen_server).

-export([start_link/0, allow/2, purge/1, windows/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(TABLE, ?MODULE).
-define(PER_NODE_PER_MINUTE, 30).
-define(TOTAL_PER_SECOND, 30).
-define(PURGE_INTERVAL_MS, 60000).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Whether the station may sign one more session proof for the client NodeId at Now (milliseconds).
-spec allow(<<_:256>>, integer()) -> ok | {error, session_proof_rate}.
allow(NodeId, Now) ->
    within(counted({node, NodeId, Now div 60000}) =< ?PER_NODE_PER_MINUTE
               andalso counted({total, Now div 1000}) =< ?TOTAL_PER_SECOND).

counted(Key) ->
    ets:update_counter(?TABLE, Key, 1, {Key, 0}).

within(true) -> ok;
within(false) -> {error, session_proof_rate}.

%% @doc Delete the windows that ended before Now.
-spec purge(integer()) -> ok.
purge(Now) ->
    Minute = Now div 60000,
    Second = Now div 1000,
    _ = ets:select_delete(?TABLE, [{{{node, '_', '$1'}, '_'}, [{'<', '$1', Minute}], [true]},
                                   {{{total, '$1'}, '_'}, [{'<', '$1', Second}], [true]}]),
    ok.

%% @doc How many windows the table holds.
-spec windows() -> non_neg_integer().
windows() ->
    ets:info(?TABLE, size).

%%------------------------------------------------------------------
%% The table's owner
%%------------------------------------------------------------------

init([]) ->
    ?TABLE = ets:new(?TABLE, [named_table, public, set, {write_concurrency, true}]),
    {ok, schedule_purge(#{})}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(purge, State) ->
    ok = purge(erlang:system_time(millisecond)),
    {noreply, schedule_purge(State)};
handle_info(_Other, State) ->
    {noreply, State}.

schedule_purge(State) ->
    _ = erlang:send_after(?PURGE_INTERVAL_MS, self(), purge),
    State.
