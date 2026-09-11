%%%-------------------------------------------------------------------
%%% @doc The link functions a content transfer under test runs on, so its
%%% tests replace no module.
%%%
%%% Each function is made in the test process and sends that process
%%% {link_call, Name, Args} when it is called. pick_connected_link/1 and
%%% ensure_content_link/4 answer with the test process as the link,
%%% open_content_stream/1 with a fresh reference each time,
%%% call_on_stream/6 with {ok, ok}, and the close and abort functions with
%%% ok, for a test to give other ones where it needs them. calls/0 returns,
%%% in order, the calls already in the test's mailbox.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_scripted_link).

-export([link_io/0, calls/0]).

%% @doc Every link function a content transfer takes in its link_io option.
-spec link_io() -> macula_content_transfer:link_io().
link_io() ->
    Test = self(),
    Record = fun(Name, Args) ->
                     Test ! {link_call, Name, Args},
                     ok
             end,
    #{pick_connected_link => fun(Pool) ->
                                     ok = Record(pick_connected_link, [Pool]),
                                     {ok, Test}
                             end,
      ensure_content_link => fun(Pool, Station, LinkOpts, TimeoutMs) ->
                                     ok = Record(ensure_content_link,
                                                 [Pool, Station, LinkOpts, TimeoutMs]),
                                     {ok, Test}
                             end,
      open_content_stream => fun(LinkPid) ->
                                     ok = Record(open_content_stream, [LinkPid]),
                                     {ok, make_ref()}
                             end,
      call_on_stream => fun(LinkPid, Stream, Realm, Procedure, Payload, TimeoutMs) ->
                                ok = Record(call_on_stream,
                                            [LinkPid, Stream, Realm, Procedure, Payload,
                                             TimeoutMs]),
                                {ok, ok}
                        end,
      close_content_stream => fun(LinkPid, Stream) ->
                                      Record(close_content_stream, [LinkPid, Stream])
                              end,
      abort_content_stream => fun(LinkPid, Stream, Code, Message) ->
                                      Record(abort_content_stream,
                                             [LinkPid, Stream, Code, Message])
                              end}.

%% @doc The link calls in the mailbox, as {Name, Args}.
-spec calls() -> [{atom(), [term()]}].
calls() ->
    receive
        {link_call, Name, Args} -> [{Name, Args} | calls()]
    after 0 ->
        []
    end.
