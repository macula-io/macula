%%%-------------------------------------------------------------------
%%% @doc The functions a streaming provider under test runs on, over a
%%% scripted stream, so its tests replace no module.
%%%
%%% Each function is made in the test process and sends that process what
%%% it was called with: a stream function {stream_call, Name, Args}, the
%%% advertise function {advertised, Procedure, Mode, Handler, Opts}, the
%%% advertisement publish {advertisement_published, Procedure, Identity,
%%% Opts}, and the fact publish {published, Topic, Payload}. recv/2
%%% returns the scripted results in order and then waits, as a stream with
%%% nothing more to read does. The drain functions return, in order, what
%%% is already in the test's mailbox; a test drains once its provider has
%%% signalled that the calls it checks were made.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_scripted_stream).

-export([options/1, stream_io/1]).
-export([calls/0, advertised/0, advertisements_published/0, published/0]).

%% @doc Every function macula_streamer:advertise/6 and advertise_direct/7
%% take in their options, with recv/2 returning Results.
-spec options([term()]) -> map().
options(Results) ->
    Test = self(),
    #{stream_io => stream_io(Results),
      advertise_stream => fun(_Pool, _Realm, Procedure, Mode, Handler, Opts) ->
                                  Test ! {advertised, Procedure, Mode, Handler, Opts},
                                  ok
                          end,
      publish_advertisement => fun(_Pool, _Realm, Procedure, Identity, Opts) ->
                                       Test ! {advertisement_published, Procedure, Identity, Opts},
                                       ok
                               end,
      fact_publish => fun(_Pool, _Realm, Topic, Payload) ->
                              Test ! {published, Topic, Payload},
                              ok
                      end}.

%% @doc The stream functions a provider runs on, with recv/2 returning
%% Results.
-spec stream_io([term()]) -> macula_stream:stream_io().
stream_io(Results) ->
    Test = self(),
    Next = atomics:new(1, []),
    Record = fun(Name, Args) ->
                     Test ! {stream_call, Name, Args},
                     ok
             end,
    #{recv => fun(_Stream, _Timeout) -> next_result(atomics:add_get(Next, 1, 1), Results) end,
      send => fun(Stream, Chunk, Encoding) -> Record(send, [Stream, Chunk, Encoding]) end,
      close_send => fun(Stream) -> Record(close_send, [Stream]) end,
      close => fun(Stream) -> Record(close, [Stream]) end,
      abort => fun(Stream, Code, Message) -> Record(abort, [Stream, Code, Message]) end,
      set_reply => fun(Stream, Value) -> Record(set_reply, [Stream, Value]) end,
      set_error => fun(Stream, Reason) -> Record(set_error, [Stream, Reason]) end}.

%% @doc The stream calls in the mailbox, as {Name, Args}.
-spec calls() -> [{atom(), [term()]}].
calls() ->
    receive
        {stream_call, Name, Args} -> [{Name, Args} | calls()]
    after 0 ->
        []
    end.

%% @doc The advertisements in the mailbox, as {Procedure, Mode, Handler, Opts}.
-spec advertised() -> [{binary(), atom(), fun(), map()}].
advertised() ->
    receive
        {advertised, Procedure, Mode, Handler, Opts} ->
            [{Procedure, Mode, Handler, Opts} | advertised()]
    after 0 ->
        []
    end.

%% @doc The advertisement publishes in the mailbox, as {Procedure, Identity, Opts}.
-spec advertisements_published() -> [{binary(), term(), map()}].
advertisements_published() ->
    receive
        {advertisement_published, Procedure, Identity, Opts} ->
            [{Procedure, Identity, Opts} | advertisements_published()]
    after 0 ->
        []
    end.

%% @doc The facts published in the mailbox, as {Topic, Payload}.
-spec published() -> [{binary(), term()}].
published() ->
    receive
        {published, Topic, Payload} -> [{Topic, Payload} | published()]
    after 0 ->
        []
    end.

next_result(N, Results) when N =< length(Results) ->
    lists:nth(N, Results);
next_result(_N, _Results) ->
    receive after infinity -> ok end.
