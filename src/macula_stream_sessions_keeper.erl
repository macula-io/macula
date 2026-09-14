%%%-------------------------------------------------------------------
%%% @doc Holds the table of the node's served stream sessions.
%%%
%%% This process creates the table with `macula_stream_sessions:new_table/0'
%%% and does nothing but hold it, so the sessions kept there outlive a
%%% restart of `macula_stream_sessions', the process that counts them. It
%%% starts before that process. The table is public so the counter can write
%%% it; no other process writes it.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_stream_sessions_keeper).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2]).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    _Table = macula_stream_sessions:new_table(),
    {ok, holding}.

handle_call(_Request, _From, holding) ->
    {reply, {error, not_supported}, holding}.

handle_cast(_Msg, holding) ->
    {noreply, holding}.
