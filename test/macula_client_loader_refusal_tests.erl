%% EUnit tests for why a pool's node identity loader was refused. A loader that returns one of macula_node_keys:load/3's
%% documented refusals, or a file error, refuses the pool with that reason nested under loader_failed, so the service
%% that holds the pool can say why without loading the key itself. Any other reason, and a loader that returns no
%% error tuple, refuses with plain loader_failed, so a reason that carries the key never leaves the pool's start.
-module(macula_client_loader_refusal_tests).

-include_lib("eunit/include/eunit.hrl").

%% The loader the pools in these tests name.
-export([returned/1]).

loader_refusal_test_() ->
    {timeout, 60, {spawn, fun() ->
        {ok, _} = application:ensure_all_started(macula),
        process_flag(trap_exit, true),
        {ok, Profile} = macula_crypto_profile:configured(),
        {ok, Key} = macula_node_keys:generate(identity, Profile),
        Named = [key_file_permissions, bad_key_file, private_key_invalid, public_key_mismatch, round_trip_failed,
                 enoent, eacces, enospc, {wrong_profile, pq_hybrid}, {wrong_purpose, connect},
                 {wrong_algorithms, [rsa_pss]}, {wrong_key_size, {4896, 32}}, {unknown_purpose, gizmo},
                 {crypto_profile_unknown, pq_other}],
        Unnamed = [{error, {no_key_here, Key}}, {error, {unknown_purpose, <<"not an atom">>}}, {error, vault_locked},
                   nothing, {ok, not_a_key_here}],
        NamedResults = [started({error, Reason}) || Reason <- Named],
        UnnamedResults = [started(Result) || Result <- Unnamed],
        ?assertEqual([{error, {node_identity, {loader_failed, Reason}}} || Reason <- Named], NamedResults),
        ?assertEqual(lists:duplicate(length(Unnamed), {error, {node_identity, loader_failed}}), UnnamedResults),
        ?assertEqual([], [Private || #{private := Private} <- maps:get(components, Key),
                                      binary:match(term_to_binary(UnnamedResults), Private) =/= nomatch])
    end}}.

%% What connect/2 gives for a pool whose loader returns Result.
started(Result) ->
    catch macula_client:connect([], #{node_identity => {?MODULE, returned, [Result]}}).

returned(Result) ->
    Result.
