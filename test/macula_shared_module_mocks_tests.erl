%%%-------------------------------------------------------------------
%%% @doc No test replaces a module that running processes call.
%%%
%%% meck:new/1,2 replaces a module for the whole VM, and unloading the
%%% mock at the end of a test loads the module again. That load fails
%%% with not_purged when another process in the VM still runs the mock's
%%% code, and the failure ends the whole eunit run. So a test gives the
%%% module under test its own functions through that module's options
%%% instead of replacing a module on the watched list.
%%%
%%% The test fails for every test source that calls meck:new on a watched
%%% module, alone or in a list, and is not on the allowlist, and for
%%% every allowlist entry that no longer does, so the allowlist only
%%% shrinks.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_shared_module_mocks_tests).

-include_lib("eunit/include/eunit.hrl").

%% Modules that processes all over a running node call.
-define(WATCHED, [macula, macula_stream, macula_direct_dial, macula_client,
                  macula_content_transfer, macula_station_link, macula_peering,
                  macula_quic]).

%% Test sources that still replace a watched module, until their module
%% under test takes its functions from its options.
%% macula_dht_records_tests.erl replaces macula_client until the DHT
%% contract gives macula's record functions their call seam, as the
%% tests above took their module's functions from its options.
%% (macula_provider_advertise_tests.erl left this list when the
%% provider-advertisement resolution took its DHT calls from the
%% provider_io seam, 11.4.0.)
-define(ALLOWED, ["macula_dht_records_tests.erl",
                  "macula_direct_dial_resolve_tests.erl",
                  "macula_link_respawn_replay_tests.erl",
                  "macula_station_link_tests.erl"]).

no_test_replaces_a_shared_module_test() ->
    Sources = test_sources(),
    ?assert(lists:member(filename:basename(?FILE), [filename:basename(F) || F <- Sources])),
    Replacing = lists:sort([filename:basename(F) || F <- Sources, replaces_a_watched_module(F)]),
    ?assertEqual([], Replacing -- ?ALLOWED),
    ?assertEqual([], ?ALLOWED -- Replacing).

%% Every .erl file under the directory this module's source is in.
test_sources() ->
    filelib:fold_files(filename:dirname(?FILE), "\\.erl$", true, fun(F, Acc) -> [F | Acc] end, []).

replaces_a_watched_module(File) ->
    {ok, Source} = file:read_file(File),
    Watched = [atom_to_binary(Module) || Module <- ?WATCHED],
    lists:any(fun(Argument) -> names_a_watched_module(Argument, Watched) end,
              meck_new_first_arguments(Source)).

%% The first argument of every meck:new call: a module name, or a list.
meck_new_first_arguments(Source) ->
    captured(re:run(Source, "meck:new\\(\\s*(\\[[^\\]]*\\]|[a-z][a-zA-Z0-9_]*)",
                    [global, {capture, [1], binary}])).

names_a_watched_module(Argument, Watched) ->
    Names = captured(re:run(Argument, "[a-z][a-zA-Z0-9_]*", [global, {capture, all, binary}])),
    lists:any(fun(Name) -> lists:member(Name, Watched) end, Names).

captured({match, Matches}) -> [Captured || [Captured] <- Matches];
captured(nomatch) -> [].
