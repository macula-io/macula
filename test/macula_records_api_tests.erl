%% @doc Eunit tests for the records section of the SDK public API.
%%
%% Each test mocks the macula_mesh_client gen_server's `call/4'
%% (the underlying RPC primitive) and asserts:
%%   1. SDK helpers translate to the right `_dht.*' procedure name
%%      and arguments map
%%   2. Successful relay replies are unwrapped into the expected
%%      shape (`ok' / `{ok, Record}' / `{ok, [Record]}')
%%   3. Failure shapes (`not_found', `{error, _}') propagate verbatim
%%
%% Mocks are scoped per-test via the `with_mock' setup so each test
%% sees a fresh meck'd module — multiple generators do not share
%% expectations.
-module(macula_records_api_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TYPE_STATION, 16#02).
-define(TTL_MS, 600_000).

%%%===================================================================
%%% Per-test setup/cleanup
%%%===================================================================

setup() ->
    meck:new(macula_mesh_client, [passthrough]),
    %% Idle process — only needs to satisfy is_pid/1 + the mocks
    %% intercept all messages before they would reach it.
    spawn(fun() -> receive _ -> ok end end).

cleanup(Pid) ->
    catch meck:unload(macula_mesh_client),
    exit(Pid, kill),
    ok.

%%%===================================================================
%%% put_record
%%%===================================================================

put_record_calls_dht_put_record_proc_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Pid) ->
        ?_test(begin
            Self = self(),
            meck:expect(macula_mesh_client, call,
                fun(_P, Proc, Args, _T) ->
                    Self ! {seen, Proc, Args},
                    {ok, ok}
                end),
            Kp = macula_identity:generate(),
            R  = macula_record:build(?TYPE_STATION, <<"x">>, Kp, ?TTL_MS),
            ?assertEqual(ok, macula:put_record(Pid, R)),
            receive
                {seen, <<"_dht.put_record">>, R} -> ok
            after 1_000 ->
                ?assert(false)
            end
        end)
    end}.

put_record_propagates_relay_error_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Pid) ->
        ?_test(begin
            meck:expect(macula_mesh_client, call,
                fun(_, _, _, _) -> {error, bad_signature} end),
            Kp = macula_identity:generate(),
            R  = macula_record:build(?TYPE_STATION, <<"x">>, Kp, ?TTL_MS),
            ?assertEqual({error, bad_signature}, macula:put_record(Pid, R))
        end)
    end}.

put_record_unexpected_reply_returns_error_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Pid) ->
        ?_test(begin
            meck:expect(macula_mesh_client, call,
                fun(_, _, _, _) -> {ok, <<"weird">>} end),
            Kp = macula_identity:generate(),
            R  = macula_record:build(?TYPE_STATION, <<"x">>, Kp, ?TTL_MS),
            ?assertMatch({error, {unexpected_reply, _}},
                         macula:put_record(Pid, R))
        end)
    end}.

%%%===================================================================
%%% find_record
%%%===================================================================

find_record_returns_decoded_record_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Pid) ->
        ?_test(begin
            Kp = macula_identity:generate(),
            R  = macula_record:build(?TYPE_STATION, <<"hello">>,
                                      Kp, ?TTL_MS),
            ExpectedKey = macula_record:key_of(R),
            Self = self(),
            meck:expect(macula_mesh_client, call,
                fun(_, Proc, #{key := K}, _) ->
                    Self ! {seen_proc, Proc, K},
                    {ok, R}
                end),
            ?assertEqual({ok, R}, macula:find_record(Pid, ExpectedKey)),
            receive
                {seen_proc, <<"_dht.find_record">>, K} when K =:= ExpectedKey -> ok
            after 1_000 ->
                ?assert(false)
            end
        end)
    end}.

find_record_translates_not_found_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Pid) ->
        ?_test(begin
            meck:expect(macula_mesh_client, call,
                fun(_, _, _, _) -> {ok, not_found} end),
            ?assertEqual({error, not_found},
                         macula:find_record(Pid, <<0:256>>))
        end)
    end}.

find_record_rejects_non_32_byte_key_test() ->
    ?assertError(function_clause,
                 macula:find_record(self(), <<"too short">>)).

%%%===================================================================
%%% find_records_by_type
%%%===================================================================

find_records_by_type_returns_list_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Pid) ->
        ?_test(begin
            Kp = macula_identity:generate(),
            R1 = macula_record:build(?TYPE_STATION, <<"a">>, Kp, ?TTL_MS),
            R2 = macula_record:build(?TYPE_STATION, <<"b">>, Kp, ?TTL_MS),
            Self = self(),
            meck:expect(macula_mesh_client, call,
                fun(_, Proc, #{type := T}, _) ->
                    Self ! {seen_proc, Proc, T},
                    {ok, [R1, R2]}
                end),
            ?assertEqual({ok, [R1, R2]},
                         macula:find_records_by_type(Pid, ?TYPE_STATION)),
            receive
                {seen_proc, <<"_dht.find_records_by_type">>, ?TYPE_STATION} ->
                    ok
            after 1_000 ->
                ?assert(false)
            end
        end)
    end}.

find_records_by_type_empty_list_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Pid) ->
        ?_test(begin
            meck:expect(macula_mesh_client, call,
                fun(_, _, _, _) -> {ok, []} end),
            ?assertEqual({ok, []},
                         macula:find_records_by_type(Pid, ?TYPE_STATION))
        end)
    end}.

find_records_by_type_rejects_out_of_range_test() ->
    ?assertError(function_clause,
                 macula:find_records_by_type(self(), 256)),
    ?assertError(function_clause,
                 macula:find_records_by_type(self(), -1)).

%%%===================================================================
%%% subscribe_records
%%%===================================================================

subscribe_records_uses_correct_topic_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Pid) ->
        ?_test(begin
            Self = self(),
            Ref  = make_ref(),
            meck:expect(macula_mesh_client, subscribe,
                fun(_P, Topic, Cb) ->
                    Self ! {subscribed_to, Topic, is_function(Cb, 1)},
                    {ok, Ref}
                end),
            ?assertEqual({ok, Ref},
                macula:subscribe_records(Pid, ?TYPE_STATION, self())),
            receive
                {subscribed_to, <<"_dht.records.2.stored">>, true} -> ok
            after 1_000 ->
                ?assert(false)
            end
        end)
    end}.

subscribe_records_pid_callback_delivers_record_message_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Pid) ->
        ?_test(begin
            Self = self(),
            Kp   = macula_identity:generate(),
            R    = macula_record:build(?TYPE_STATION, <<"x">>,
                                        Kp, ?TTL_MS),
            meck:expect(macula_mesh_client, subscribe,
                fun(_P, _Topic, Cb) ->
                    Cb(R),
                    {ok, make_ref()}
                end),
            {ok, _} = macula:subscribe_records(Pid, ?TYPE_STATION, Self),
            receive
                {record, Got} -> ?assertEqual(R, Got)
            after 1_000 ->
                ?assert(false)
            end
        end)
    end}.

subscribe_records_function_callback_invoked_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Pid) ->
        ?_test(begin
            Self = self(),
            Kp   = macula_identity:generate(),
            R    = macula_record:build(?TYPE_STATION, <<"y">>,
                                        Kp, ?TTL_MS),
            meck:expect(macula_mesh_client, subscribe,
                fun(_P, _Topic, Cb) -> Cb(R), {ok, make_ref()} end),
            {ok, _} =
                macula:subscribe_records(Pid, ?TYPE_STATION,
                    fun(Got) -> Self ! {got, Got} end),
            receive
                {got, Got} -> ?assertEqual(R, Got)
            after 1_000 ->
                ?assert(false)
            end
        end)
    end}.

unsubscribe_records_delegates_to_unsubscribe_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Pid) ->
        ?_test(begin
            Ref = make_ref(),
            Self = self(),
            meck:expect(macula_mesh_client, unsubscribe,
                fun(_P, R) -> Self ! {unsub, R}, ok end),
            ?assertEqual(ok, macula:unsubscribe_records(Pid, Ref)),
            receive
                {unsub, Got} -> ?assertEqual(Ref, Got)
            after 1_000 ->
                ?assert(false)
            end
        end)
    end}.
