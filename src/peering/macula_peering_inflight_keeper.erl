%%%-------------------------------------------------------------------
%%% @doc Holds the tables of the node's in-flight reservations.
%%%
%%% This process creates the tables with `macula_peering_inflight:new_table/0'
%%% and does nothing but hold them, so the reservations, their counts, their
%%% ages and the readers waiting for room outlive a restart of
%%% `macula_peering_inflight', the process that watches them. It starts
%%% before that process. The tables are public so readers and holders can
%%% write them in their own processes.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peering_inflight_keeper).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2]).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    _Table = macula_peering_inflight:new_table(),
    {ok, holding}.

handle_call(_Request, _From, holding) ->
    {reply, {error, not_supported}, holding}.

handle_cast(_Msg, holding) ->
    {noreply, holding}.
