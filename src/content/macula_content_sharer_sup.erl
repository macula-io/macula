%% @doc Supervises the content sharers, one per pool (`macula_content_sharer'). A sharer is temporary: it ends with
%% its pool and is not restarted. `sharer/1' finds a pool's sharer or starts it; the supervisor serializes the start,
%% so two callers sharing on one pool at once get the same sharer.
-module(macula_content_sharer_sup).

-behaviour(supervisor).

-export([start_link/0, sharer/1]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc The sharer of `Pool', started if it has none.
-spec sharer(pid()) -> {ok, pid()} | {error, term()}.
sharer(Pool) when is_pid(Pool) ->
    started(supervisor:start_child(?MODULE, #{id => {sharer, Pool},
                                              start => {macula_content_sharer, start_link, [Pool, #{}]},
                                              restart => temporary,
                                              shutdown => 5_000,
                                              type => worker,
                                              modules => [macula_content_sharer]})).

started({ok, Pid}) -> {ok, Pid};
started({error, {already_started, Pid}}) -> {ok, Pid};
started({error, _} = Error) -> Error.

%% @private
init([]) ->
    {ok, {#{strategy => one_for_one, intensity => 10, period => 10}, []}}.
