%% Test support for stream session lifetimes: the processes serving a stream session on this node, those a given process
%% started, and bounded waits until none is left beyond a set taken before the sessions started, or until given ones ended.
-module(macula_test_sessions).

-export([serving/0, await_none_new/1, started_by/1, await_ended/1]).

-define(POLL_MS, 20).
-define(POLLS, 50).

%% @doc Every process on this node that serves a stream session: a macula_stream, a macula_streamer, a process
%% parked in a stream host loop, or a handler process still waiting for the stream it serves.
-spec serving() -> [pid()].
serving() ->
    [Pid || Pid <- erlang:processes(), serves_a_session(erlang:process_info(Pid, [current_function, dictionary]))].

%% @doc The session processes still alive that are not in Before, after waiting up to a second for them to end: []
%% once every one has ended.
-spec await_none_new([pid()]) -> [pid()].
await_none_new(Before) when is_list(Before) ->
    await_none_new(Before, ?POLLS).

await_none_new(Before, 0) ->
    serving() -- Before;
await_none_new(Before, Polls) ->
    none_new(serving() -- Before, Before, Polls).

none_new([], _Before, _Polls) ->
    [];
none_new(_New, Before, Polls) ->
    timer:sleep(?POLL_MS),
    await_none_new(Before, Polls - 1).

%% @doc The processes serving/0 finds that Parent started, whether or not Parent still runs: a process started through
%% proc_lib names its parent first among its ancestors.
-spec started_by(pid()) -> [pid()].
started_by(Parent) when is_pid(Parent) ->
    [Pid || Pid <- serving(), parent(erlang:process_info(Pid, dictionary)) =:= Parent].

%% @doc The processes in Pids still alive after waiting up to a second for them to end: [] once every one has ended.
-spec await_ended([pid()]) -> [pid()].
await_ended(Pids) when is_list(Pids) ->
    await_ended(Pids, ?POLLS).

await_ended(Pids, 0) ->
    alive(Pids);
await_ended(Pids, Polls) ->
    none_alive(alive(Pids), Pids, Polls).

none_alive([], _Pids, _Polls) ->
    [];
none_alive(_Alive, Pids, Polls) ->
    timer:sleep(?POLL_MS),
    await_ended(Pids, Polls - 1).

alive(Pids) ->
    [Pid || Pid <- Pids, is_process_alive(Pid)].

parent({dictionary, Dictionary}) ->
    first_ancestor(proplists:get_value('$ancestors', Dictionary, []));
parent(undefined) ->
    undefined.

first_ancestor([Parent | _Older]) -> Parent;
first_ancestor([])                -> undefined.

serves_a_session([{current_function, {macula_station_link, stream_host_loop, 0}} | _]) ->
    true;
serves_a_session([{current_function, {macula_stream_local, host_loop, 0}} | _]) ->
    true;
serves_a_session([{current_function, {macula_station_link, serve_stream_when_attached, _}} | _]) ->
    true;
serves_a_session([{current_function, {macula_stream_local, serve_when_paired, _}} | _]) ->
    true;
serves_a_session([{current_function, _}, {dictionary, Dictionary}]) ->
    lists:member(proplists:get_value('$initial_call', Dictionary),
                 [{macula_stream, init, 1}, {macula_streamer, init, 1}]);
serves_a_session(undefined) ->
    false.
