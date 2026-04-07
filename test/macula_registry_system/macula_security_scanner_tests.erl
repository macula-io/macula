%%%-------------------------------------------------------------------
%%% @doc Security Scanner Tests
%%%
%%% Tests for macula_security_scanner static analysis engine.
%%% Covers dangerous BIF detection, NIF detection, score calculation,
%%% and manifest scanning.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_security_scanner_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Helpers
%%%===================================================================

%% Compile an Erlang source string into a {ModuleName, BeamBinary} tuple
%% with debug_info so abstract code is available for scanning.
compile_module(ModName, Source) ->
    TmpDir = filename:join(["/tmp", "macula_scanner_test_" ++
        integer_to_list(erlang:unique_integer([positive]))]),
    ok = filelib:ensure_dir(filename:join(TmpDir, "dummy")),
    SrcFile = filename:join(TmpDir, atom_to_list(ModName) ++ ".erl"),
    ok = file:write_file(SrcFile, Source),
    {ok, ModName, BeamBin} = compile:file(SrcFile, [binary, debug_info]),
    _ = file:del_dir_r(TmpDir),
    {ModName, BeamBin}.

%%%===================================================================
%%% scan_beam_files tests
%%%===================================================================

scan_safe_module_high_score_test() ->
    {Mod, Beam} = compile_module(safe_mod, <<
        "-module(safe_mod).\n"
        "-export([add/2]).\n"
        "add(A, B) -> A + B.\n"
    >>),
    {ok, Result} = macula_security_scanner:scan_beam_files([{Mod, Beam}]),
    ?assertEqual([], maps:get(dangerous_bifs, Result)),
    ?assertEqual([], maps:get(nif_usage, Result)),
    ?assert(maps:get(score, Result) >= 90).

scan_os_cmd_detected_test() ->
    {Mod, Beam} = compile_module(os_cmd_mod, <<
        "-module(os_cmd_mod).\n"
        "-export([run/0]).\n"
        "run() -> os:cmd(\"ls\").\n"
    >>),
    {ok, Result} = macula_security_scanner:scan_beam_files([{Mod, Beam}]),
    Bifs = maps:get(dangerous_bifs, Result),
    ?assert(length(Bifs) > 0),
    Found = lists:any(fun(#{module := M, function := F}) ->
        M =:= os andalso F =:= cmd
    end, Bifs),
    ?assert(Found).

scan_open_port_detected_test() ->
    {Mod, Beam} = compile_module(open_port_mod, <<
        "-module(open_port_mod).\n"
        "-export([run/0]).\n"
        "run() -> erlang:open_port({spawn, \"cat\"}, []).\n"
    >>),
    {ok, Result} = macula_security_scanner:scan_beam_files([{Mod, Beam}]),
    Bifs = maps:get(dangerous_bifs, Result),
    Found = lists:any(fun(#{module := M, function := F}) ->
        M =:= erlang andalso F =:= open_port
    end, Bifs),
    ?assert(Found).

scan_file_delete_detected_test() ->
    {Mod, Beam} = compile_module(file_del_mod, <<
        "-module(file_del_mod).\n"
        "-export([run/0]).\n"
        "run() -> file:delete(\"/tmp/x\").\n"
    >>),
    {ok, Result} = macula_security_scanner:scan_beam_files([{Mod, Beam}]),
    Bifs = maps:get(dangerous_bifs, Result),
    Found = lists:any(fun(#{module := M, function := F}) ->
        M =:= file andalso F =:= delete
    end, Bifs),
    ?assert(Found).

scan_nif_loading_detected_test() ->
    {Mod, Beam} = compile_module(nif_mod, <<
        "-module(nif_mod).\n"
        "-export([init/0]).\n"
        "init() -> erlang:load_nif(\"my_nif\", 0).\n"
    >>),
    {ok, Result} = macula_security_scanner:scan_beam_files([{Mod, Beam}]),
    NifUsage = maps:get(nif_usage, Result),
    ?assert(length(NifUsage) > 0),
    Found = lists:any(fun(#{module := M}) -> M =:= nif_mod end, NifUsage),
    ?assert(Found),
    %% Also detected as a dangerous BIF
    Bifs = maps:get(dangerous_bifs, Result),
    BifFound = lists:any(fun(#{module := M, function := F}) ->
        M =:= erlang andalso F =:= load_nif
    end, Bifs),
    ?assert(BifFound).

scan_empty_beam_list_test() ->
    {ok, Result} = macula_security_scanner:scan_beam_files([]),
    ?assertEqual([], maps:get(dangerous_bifs, Result)),
    ?assertEqual([], maps:get(nif_usage, Result)),
    ?assertEqual([], maps:get(warnings, Result)),
    ?assertEqual(100, maps:get(score, Result)).

scan_multiple_dangerous_calls_test() ->
    {Mod, Beam} = compile_module(multi_danger_mod, <<
        "-module(multi_danger_mod).\n"
        "-export([run/0]).\n"
        "run() ->\n"
        "    os:cmd(\"ls\"),\n"
        "    file:delete(\"/tmp/x\"),\n"
        "    erlang:halt().\n"
    >>),
    {ok, Result} = macula_security_scanner:scan_beam_files([{Mod, Beam}]),
    Bifs = maps:get(dangerous_bifs, Result),
    ?assert(length(Bifs) >= 3),
    ?assert(maps:get(score, Result) < 60).

scan_code_load_binary_detected_test() ->
    {Mod, Beam} = compile_module(code_load_mod, <<
        "-module(code_load_mod).\n"
        "-export([run/0]).\n"
        "run() -> code:load_binary(my_mod, \"my_mod.beam\", <<>>).\n"
    >>),
    {ok, Result} = macula_security_scanner:scan_beam_files([{Mod, Beam}]),
    Bifs = maps:get(dangerous_bifs, Result),
    Found = lists:any(fun(#{module := M, function := F}) ->
        M =:= code andalso F =:= load_binary
    end, Bifs),
    ?assert(Found).

%%%===================================================================
%%% calculate_score tests
%%%===================================================================

calculate_score_perfect_test() ->
    Score = macula_security_scanner:calculate_score(#{
        dangerous_bifs => [],
        nif_usage => [],
        warnings => []
    }),
    ?assertEqual(100, Score).

calculate_score_one_bif_penalty_test() ->
    Score = macula_security_scanner:calculate_score(#{
        dangerous_bifs => [#{module => os, function => cmd, arity => 1}],
        nif_usage => [],
        warnings => []
    }),
    ?assertEqual(85, Score).

calculate_score_nif_penalty_test() ->
    Score = macula_security_scanner:calculate_score(#{
        dangerous_bifs => [],
        nif_usage => [#{module => my_mod, nif_lib => <<"test">>}],
        warnings => []
    }),
    ?assertEqual(80, Score).

calculate_score_floor_at_zero_test() ->
    Score = macula_security_scanner:calculate_score(#{
        dangerous_bifs => [#{} || _ <- lists:seq(1, 10)],
        nif_usage => [#{} || _ <- lists:seq(1, 5)],
        warnings => [#{severity => critical} || _ <- lists:seq(1, 5)]
    }),
    ?assertEqual(0, Score).

calculate_score_warning_severity_test() ->
    ScoreCrit = macula_security_scanner:calculate_score(#{
        dangerous_bifs => [],
        nif_usage => [],
        warnings => [#{severity => critical}]
    }),
    ScoreLow = macula_security_scanner:calculate_score(#{
        dangerous_bifs => [],
        nif_usage => [],
        warnings => [#{severity => low}]
    }),
    ?assert(ScoreLow > ScoreCrit).

%%%===================================================================
%%% get_dangerous_bifs tests
%%%===================================================================

dangerous_bifs_is_list_test() ->
    Bifs = macula_security_scanner:get_dangerous_bifs(),
    ?assert(is_list(Bifs)),
    ?assert(length(Bifs) > 10).

dangerous_bifs_includes_halt_test() ->
    Bifs = macula_security_scanner:get_dangerous_bifs(),
    ?assert(lists:member({erlang, halt, 0}, Bifs)).

dangerous_bifs_includes_net_kernel_test() ->
    Bifs = macula_security_scanner:get_dangerous_bifs(),
    ?assert(lists:member({net_kernel, connect_node, 1}, Bifs)).

%%%===================================================================
%%% scan_manifest tests
%%%===================================================================

scan_manifest_clean_test() ->
    {ok, Warnings} = macula_security_scanner:scan_manifest(#{capabilities => []}),
    ?assertEqual([], Warnings).

scan_manifest_broad_network_test() ->
    {ok, Warnings} = macula_security_scanner:scan_manifest(#{
        capabilities => [{network, [{connect, <<"**">>}]}]
    }),
    ?assert(length(Warnings) > 0),
    HasBroadNetwork = lists:any(fun(#{type := T}) ->
        T =:= broad_network_access
    end, Warnings),
    ?assert(HasBroadNetwork).

scan_manifest_root_file_access_test() ->
    {ok, Warnings} = macula_security_scanner:scan_manifest(#{
        capabilities => [{file_access, [{write, <<"/">>}]}]
    }),
    HasRootAccess = lists:any(fun(#{type := T}) ->
        T =:= root_file_access
    end, Warnings),
    ?assert(HasRootAccess).

scan_manifest_nif_declared_test() ->
    {ok, Warnings} = macula_security_scanner:scan_manifest(#{
        capabilities => [{nifs, [<<"my_nif">>]}]
    }),
    HasNif = lists:any(fun(#{type := T}) ->
        T =:= nif_declared
    end, Warnings),
    ?assert(HasNif).
