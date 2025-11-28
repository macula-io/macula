%%%-------------------------------------------------------------------
%%% @doc Cluster Connector for Distribution Testing.
%%%
%%% Periodically attempts to connect to bootstrap nodes and reports
%%% cluster status. Uses environment variables for configuration.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_test_connector).

-behaviour(gen_server).

-export([start_link/0, status/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(CONNECT_INTERVAL, 5000).
-define(STATUS_INTERVAL, 10000).

-record(state, {
    bootstrap_nodes = [] :: [atom()],
    connect_timer :: reference() | undefined,
    status_timer :: reference() | undefined
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

status() ->
    gen_server:call(?SERVER, get_status).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    BootstrapNodes = get_bootstrap_nodes(),
    io:format("[Connector] Bootstrap nodes: ~p~n", [BootstrapNodes]),

    ConnectTimer = erlang:send_after(?CONNECT_INTERVAL, self(), try_connect),
    StatusTimer = erlang:send_after(?STATUS_INTERVAL, self(), report_status),

    {ok, #state{
        bootstrap_nodes = BootstrapNodes,
        connect_timer = ConnectTimer,
        status_timer = StatusTimer
    }}.

handle_call(get_status, _From, State) ->
    Status = #{
        this_node => node(),
        connected_nodes => nodes(),
        cluster_size => length(nodes()) + 1,
        bootstrap_nodes => State#state.bootstrap_nodes
    },
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(try_connect, State) ->
    connect_to_nodes(State#state.bootstrap_nodes),
    Timer = erlang:send_after(?CONNECT_INTERVAL, self(), try_connect),
    {noreply, State#state{connect_timer = Timer}};

handle_info(report_status, State) ->
    Connected = nodes(),
    io:format("[Connector] Cluster status: ~p nodes connected~n", [length(Connected)]),
    lists:foreach(
        fun(Node) ->
            io:format("[Connector]   - ~p~n", [Node])
        end,
        Connected
    ),
    Timer = erlang:send_after(?STATUS_INTERVAL, self(), report_status),
    {noreply, State#state{status_timer = Timer}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    cancel_timer(State#state.connect_timer),
    cancel_timer(State#state.status_timer),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

get_bootstrap_nodes() ->
    case os:getenv("BOOTSTRAP_NODES") of
        false -> [];
        "" -> [];
        NodesStr ->
            NodeStrings = string:tokens(NodesStr, ","),
            lists:filtermap(
                fun(NodeStr) ->
                    Trimmed = string:trim(NodeStr),
                    case Trimmed of
                        "" -> false;
                        _ -> {true, list_to_atom(Trimmed)}
                    end
                end,
                NodeStrings
            )
    end.

connect_to_nodes([]) ->
    ok;
connect_to_nodes(Nodes) ->
    lists:foreach(fun try_connect_node/1, Nodes).

try_connect_node(Node) when Node =:= node() ->
    ok;
try_connect_node(Node) ->
    case lists:member(Node, nodes()) of
        true ->
            ok;
        false ->
            io:format("[Connector] Attempting to connect to ~p~n", [Node]),
            case net_kernel:connect_node(Node) of
                true ->
                    io:format("[Connector] Connected to ~p~n", [Node]);
                false ->
                    io:format("[Connector] Failed to connect to ~p~n", [Node]);
                ignored ->
                    io:format("[Connector] Connection ignored for ~p~n", [Node])
            end
    end.

cancel_timer(undefined) ->
    ok;
cancel_timer(Timer) ->
    erlang:cancel_timer(Timer).
