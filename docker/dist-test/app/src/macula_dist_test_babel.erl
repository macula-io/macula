%%%-------------------------------------------------------------------
%%% @doc Babel - Chinese Whispers Demo for QUIC Distribution
%%%
%%% A simple GenServer that demonstrates distributed message passing
%%% by playing "Chinese Whispers" (telephone game) across nodes.
%%%
%%% Each node has a babel process. When whisper/1 is called, it starts
%%% a message chain where each node receives the message, optionally
%%% mutates it slightly, and passes it to the next node.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_test_babel).

-behaviour(gen_server).

-export([start_link/0]).
-export([whisper/1, whisper/2, status/0, history/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {
    history = [] :: list(),
    node_name :: atom()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Start a whisper chain with a message
-spec whisper(binary() | string()) -> ok.
whisper(Message) ->
    whisper(Message, []).

%% @doc Start a whisper chain with options
%% Options: [{mutate, boolean()}] - whether to slightly change the message
-spec whisper(binary() | string(), list()) -> ok.
whisper(Message, Opts) ->
    Msg = ensure_binary(Message),
    gen_server:cast(?SERVER, {start_whisper, Msg, Opts}).

%% @doc Get the current status
-spec status() -> map().
status() ->
    gen_server:call(?SERVER, status).

%% @doc Get the whisper history
-spec history() -> list().
history() ->
    gen_server:call(?SERVER, history).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("[Babel] Started on ~p~n", [node()]),
    {ok, #state{node_name = node(), history = []}}.

handle_call(status, _From, State) ->
    Status = #{
        node => State#state.node_name,
        history_count => length(State#state.history),
        connected_nodes => nodes()
    },
    {reply, Status, State};

handle_call(history, _From, State) ->
    {reply, lists:reverse(State#state.history), State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({start_whisper, Message, Opts}, State) ->
    io:format("[Babel@~p] Starting whisper: ~p~n", [node(), Message]),

    %% Record that we started the whisper
    Entry = #{
        type => start,
        message => Message,
        from => self,
        timestamp => erlang:system_time(millisecond)
    },
    NewHistory = [Entry | State#state.history],

    %% Get list of other nodes and send to the first one
    case nodes() of
        [] ->
            io:format("[Babel@~p] No other nodes - whisper ends here~n", [node()]),
            FinalEntry = #{
                type => 'end',
                message => Message,
                reason => no_other_nodes,
                timestamp => erlang:system_time(millisecond)
            },
            {noreply, State#state{history = [FinalEntry | NewHistory]}};
        OtherNodes ->
            %% Sort nodes for consistent ordering
            SortedNodes = lists:sort(OtherNodes),
            [FirstNode | RestNodes] = SortedNodes,

            %% Build the chain: this node -> FirstNode -> ... -> LastNode
            Chain = RestNodes ++ [node()],  %% Message will return to us at the end

            MutatedMsg = maybe_mutate(Message, Opts),
            io:format("[Babel@~p] Passing to ~p: ~p~n", [node(), FirstNode, MutatedMsg]),

            %% Send to first node in chain
            gen_server:cast({?SERVER, FirstNode},
                           {whisper, MutatedMsg, Chain, node(), Opts}),

            {noreply, State#state{history = NewHistory}}
    end;

handle_cast({whisper, Message, [], Origin, _Opts}, State) ->
    %% End of chain - we're the last node (or back to origin)
    io:format("[Babel@~p] FINAL MESSAGE: ~p (started at ~p)~n",
              [node(), Message, Origin]),

    Entry = #{
        type => received_final,
        message => Message,
        from => Origin,
        timestamp => erlang:system_time(millisecond)
    },

    %% Notify origin that whisper completed (if we're not the origin)
    case node() =:= Origin of
        true ->
            io:format("[Babel@~p] Whisper complete! Final message: ~p~n", [node(), Message]);
        false ->
            gen_server:cast({?SERVER, Origin}, {whisper_complete, Message, node()})
    end,

    {noreply, State#state{history = [Entry | State#state.history]}};

handle_cast({whisper, Message, [NextNode | RestChain], Origin, Opts}, State) ->
    io:format("[Babel@~p] Received: ~p, passing to ~p~n",
              [node(), Message, NextNode]),

    Entry = #{
        type => relay,
        message => Message,
        from => Origin,
        next => NextNode,
        timestamp => erlang:system_time(millisecond)
    },

    %% Maybe mutate the message
    MutatedMsg = maybe_mutate(Message, Opts),

    %% Pass to next node
    gen_server:cast({?SERVER, NextNode},
                   {whisper, MutatedMsg, RestChain, Origin, Opts}),

    {noreply, State#state{history = [Entry | State#state.history]}};

handle_cast({whisper_complete, FinalMessage, LastNode}, State) ->
    io:format("[Babel@~p] Whisper complete! Final message from ~p: ~p~n",
              [node(), LastNode, FinalMessage]),

    Entry = #{
        type => complete,
        final_message => FinalMessage,
        last_node => LastNode,
        timestamp => erlang:system_time(millisecond)
    },

    {noreply, State#state{history = [Entry | State#state.history]}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

ensure_binary(Msg) when is_binary(Msg) -> Msg;
ensure_binary(Msg) when is_list(Msg) -> list_to_binary(Msg);
ensure_binary(Msg) when is_atom(Msg) -> atom_to_binary(Msg).

%% @doc Maybe mutate the message slightly (like in real Chinese Whispers)
maybe_mutate(Message, Opts) ->
    case proplists:get_value(mutate, Opts, false) of
        true -> mutate_message(Message);
        false -> Message
    end.

%% @doc Simple mutation - randomly change one character
mutate_message(Message) when byte_size(Message) > 0 ->
    Len = byte_size(Message),
    Pos = rand:uniform(Len) - 1,
    <<Before:Pos/binary, Char:8, After/binary>> = Message,

    %% Shift the character by 1 (wrapping)
    NewChar = case Char of
        $z -> $a;
        $Z -> $A;
        _ -> Char + 1
    end,

    <<Before/binary, NewChar:8, After/binary>>;
mutate_message(Message) ->
    Message.
