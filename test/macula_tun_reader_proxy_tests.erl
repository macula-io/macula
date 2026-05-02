%%%-------------------------------------------------------------------
%%% @doc Tests for macula_tun_reader_proxy.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_tun_reader_proxy_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SINK, macula_tun_reader_proxy_tests_sink).

setup() ->
    %% NB: register inside the test body, not setup — eunit fixtures
    %% run setup and the test in different processes.
    ok.

cleanup(_) ->
    catch unregister(?SINK),
    flush().

flush() ->
    receive _ -> flush() after 0 -> ok end.

register_sink() ->
    catch unregister(?SINK),
    true = register(?SINK, self()),
    ok.

start_proxy() ->
    register_sink(),
    Sink = ?SINK,
    {ok, Pid} = macula_tun_reader_proxy:start_link(#{
        dispatch_fn => fun(P) -> Sink ! {dispatched, P}, ok end
    }),
    Pid.

await(Tag, Timeout) ->
    receive
        Msg when element(1, Msg) =:= Tag -> Msg
    after Timeout -> timeout
    end.

%% =============================================================================
%% Tests
%% =============================================================================

proxy_test_() ->
    {inorder, [
        {timeout, 5,
         {setup, fun setup/0, fun cleanup/1, fun(_) -> dispatches_each_packet() end}},
        {timeout, 5,
         {setup, fun setup/0, fun cleanup/1, fun(_) -> survives_dispatcher_crash() end}},
        {timeout, 5,
         {setup, fun setup/0, fun cleanup/1, fun(_) -> ignores_other_messages() end}}
    ]}.

dispatches_each_packet() ->
    fun() ->
        Pid = start_proxy(),
        Pid ! {macula_net_packet, fake_handle, <<"a">>},
        Pid ! {macula_net_packet, fake_handle, <<"b">>},
        ?assertEqual({dispatched, <<"a">>}, await(dispatched, 1000)),
        ?assertEqual({dispatched, <<"b">>}, await(dispatched, 1000)),
        macula_tun_reader_proxy:stop(Pid)
    end.

survives_dispatcher_crash() ->
    fun() ->
        register_sink(),
        Sink = ?SINK,
        {ok, Pid} = macula_tun_reader_proxy:start_link(#{
            dispatch_fn => fun(<<"crash">>) -> error(boom);
                              (P)           -> Sink ! {dispatched, P}, ok
                           end
        }),
        Pid ! {macula_net_packet, fake_handle, <<"crash">>},
        timer:sleep(50),
        ?assert(is_process_alive(Pid)),
        Pid ! {macula_net_packet, fake_handle, <<"ok">>},
        ?assertEqual({dispatched, <<"ok">>}, await(dispatched, 1000)),
        macula_tun_reader_proxy:stop(Pid)
    end.

ignores_other_messages() ->
    fun() ->
        Pid = start_proxy(),
        Pid ! {something, irrelevant},
        Pid ! {macula_net_packet, fake_handle, <<"x">>},
        ?assertEqual({dispatched, <<"x">>}, await(dispatched, 1000)),
        macula_tun_reader_proxy:stop(Pid)
    end.
