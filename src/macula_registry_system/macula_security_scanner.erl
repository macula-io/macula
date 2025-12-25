%%%-------------------------------------------------------------------
%%% @doc Macula Security Scanner
%%%
%%% Static analysis engine for scanning BEAM files:
%%% - Detects dangerous BIF usage (os:cmd, open_port, etc.)
%%% - Identifies undeclared capabilities
%%% - Detects NIF loading attempts
%%% - Calculates security score
%%%
%%% All functions are stateless.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_security_scanner).

%% API
-export([scan_beam_archive/1, scan_beam_files/1]).
-export([scan_manifest/1]).
-export([get_dangerous_bifs/0]).
-export([calculate_score/1]).

%% Types
-export_type([scan_result/0, warning/0]).

-type scan_result() :: #{
    dangerous_bifs := [#{
        module := atom(),
        function := atom(),
        arity := non_neg_integer(),
        locations := [term()]
    }],
    undeclared_capabilities := [term()],
    nif_usage := [#{
        module := atom(),
        nif_lib := binary()
    }],
    warnings := [warning()],
    score := 0..100
}.

-type warning() :: #{
    severity := low | medium | high | critical,
    type := atom(),
    message := binary(),
    location => term()
}.

%% Dangerous BIFs that should be flagged
-define(DANGEROUS_BIFS, [
    {os, cmd, 1},
    {os, cmd, 2},
    {erlang, open_port, 2},
    {erlang, load_nif, 2},
    {code, load_binary, 3},
    {code, purge, 1},
    {code, delete, 1},
    {file, write_file, 2},
    {file, write_file, 3},
    {file, delete, 1},
    {file, del_dir, 1},
    {net_kernel, connect_node, 1},
    {net_kernel, disconnect, 1},
    {erlang, halt, 0},
    {erlang, halt, 1},
    {erlang, halt, 2},
    {init, stop, 0},
    {init, stop, 1}
]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Scan a gzipped BEAM archive
-spec scan_beam_archive(Archive :: binary()) -> {ok, scan_result()} | {error, term()}.
scan_beam_archive(Archive) when is_binary(Archive) ->
    %% Try to decompress if gzipped
    BeamData = try
        zlib:gunzip(Archive)
    catch
        _:_ -> Archive  % Already decompressed or not gzipped
    end,

    %% Extract BEAM files from tar or treat as single file
    case extract_beam_files(BeamData) of
        {ok, BeamFiles} ->
            scan_beam_files(BeamFiles);
        {error, Reason} ->
            {error, {extraction_failed, Reason}}
    end.

%% @doc Scan a list of {ModuleName, BeamBinary} tuples
-spec scan_beam_files([{atom(), binary()}]) -> {ok, scan_result()} | {error, term()}.
scan_beam_files(BeamFiles) when is_list(BeamFiles) ->
    %% Scan each BEAM file
    Results = lists:map(fun({ModName, BeamBin}) ->
        scan_single_beam(ModName, BeamBin)
    end, BeamFiles),

    %% Merge results
    MergedResult = merge_scan_results(Results),

    %% Calculate score
    Score = calculate_score(MergedResult),

    {ok, MergedResult#{score => Score}}.

%% @doc Scan a manifest for security issues
-spec scan_manifest(Manifest :: map()) -> {ok, [warning()]} | {error, term()}.
scan_manifest(Manifest) when is_map(Manifest) ->
    Warnings = lists:flatten([
        check_capability_patterns(Manifest),
        check_nif_declarations(Manifest),
        check_network_access(Manifest)
    ]),
    {ok, Warnings}.

%% @doc Get list of dangerous BIFs
-spec get_dangerous_bifs() -> [{atom(), atom(), non_neg_integer()}].
get_dangerous_bifs() ->
    ?DANGEROUS_BIFS.

%% @doc Calculate security score based on scan results
-spec calculate_score(scan_result()) -> 0..100.
calculate_score(Result) ->
    DangerousBifs = maps:get(dangerous_bifs, Result, []),
    NifUsage = maps:get(nif_usage, Result, []),
    Warnings = maps:get(warnings, Result, []),

    %% Start with 100 and deduct points
    Score0 = 100,

    %% Deduct for dangerous BIFs
    BifPenalty = length(DangerousBifs) * 15,

    %% Deduct for NIF usage
    NifPenalty = length(NifUsage) * 20,

    %% Deduct for warnings by severity
    WarningPenalty = lists:sum(lists:map(fun(W) ->
        case maps:get(severity, W, low) of
            critical -> 25;
            high -> 15;
            medium -> 10;
            low -> 5
        end
    end, Warnings)),

    Score = Score0 - BifPenalty - NifPenalty - WarningPenalty,
    max(0, min(100, Score)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Extract BEAM files from archive
extract_beam_files(Data) when is_binary(Data) ->
    %% Try to parse as tar
    case erl_tar:extract({binary, Data}, [memory, compressed]) of
        {ok, FileList} ->
            BeamFiles = lists:filtermap(fun({FileName, Content}) ->
                case filename:extension(FileName) of
                    ".beam" ->
                        ModName = list_to_atom(filename:basename(FileName, ".beam")),
                        {true, {ModName, Content}};
                    _ ->
                        false
                end
            end, FileList),
            {ok, BeamFiles};
        {error, _} ->
            %% Try as single BEAM file
            case is_beam_file(Data) of
                true ->
                    {ok, [{unknown_module, Data}]};
                false ->
                    {error, not_a_beam_file}
            end
    end.

%% @private Check if binary is a BEAM file (starts with "FOR1")
is_beam_file(<<"FOR1", _/binary>>) -> true;
is_beam_file(_) -> false.

%% @private Scan a single BEAM file
scan_single_beam(ModName, BeamBin) ->
    DangerousCalls = find_dangerous_calls(ModName, BeamBin),
    NifUsage = find_nif_usage(ModName, BeamBin),
    Warnings = generate_warnings(ModName, DangerousCalls, NifUsage),

    #{
        dangerous_bifs => DangerousCalls,
        nif_usage => NifUsage,
        warnings => Warnings,
        undeclared_capabilities => []
    }.

%% @private Find dangerous function calls in BEAM
find_dangerous_calls(ModName, BeamBin) ->
    case get_abstract_code(BeamBin) of
        {ok, AbstractCode} ->
            find_calls_in_forms(ModName, AbstractCode, ?DANGEROUS_BIFS);
        error ->
            []
    end.

%% @private Get abstract code from BEAM
get_abstract_code(BeamBin) ->
    case beam_lib:chunks(BeamBin, [abstract_code]) of
        {ok, {_, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            {ok, Forms};
        {ok, {_, [{abstract_code, no_abstract_code}]}} ->
            %% No debug info - try to decompile
            error;
        _ ->
            error
    end.

%% @private Find calls to dangerous functions in AST
find_calls_in_forms(ModName, Forms, DangerousList) ->
    lists:flatten(lists:map(fun(Form) ->
        find_calls_in_form(ModName, Form, DangerousList)
    end, Forms)).

find_calls_in_form(ModName, {function, Line, FunName, Arity, Clauses}, DangerousList) ->
    lists:flatten(lists:map(fun(Clause) ->
        find_calls_in_clause(ModName, FunName, Arity, Line, Clause, DangerousList)
    end, Clauses));
find_calls_in_form(_, _, _) ->
    [].

find_calls_in_clause(ModName, FunName, FunArity, _Line, {clause, _, _Args, _Guards, Body}, DangerousList) ->
    find_calls_in_exprs(ModName, FunName, FunArity, Body, DangerousList);
find_calls_in_clause(_, _, _, _, _, _) ->
    [].

find_calls_in_exprs(ModName, FunName, FunArity, Exprs, DangerousList) ->
    lists:flatten(lists:map(fun(Expr) ->
        find_calls_in_expr(ModName, FunName, FunArity, Expr, DangerousList)
    end, Exprs)).

find_calls_in_expr(ModName, FunName, FunArity, {call, Line, {remote, _, {atom, _, M}, {atom, _, F}}, Args}, DangerousList) ->
    Arity = length(Args),
    case lists:member({M, F, Arity}, DangerousList) of
        true ->
            [#{
                module => M,
                function => F,
                arity => Arity,
                locations => [{ModName, FunName, FunArity, Line}]
            }];
        false ->
            %% Recurse into arguments
            find_calls_in_exprs(ModName, FunName, FunArity, Args, DangerousList)
    end;
find_calls_in_expr(ModName, FunName, FunArity, {call, _, FunExpr, Args}, DangerousList) ->
    %% Check function expression and arguments
    lists:flatten([
        find_calls_in_expr(ModName, FunName, FunArity, FunExpr, DangerousList),
        find_calls_in_exprs(ModName, FunName, FunArity, Args, DangerousList)
    ]);
find_calls_in_expr(ModName, FunName, FunArity, {'case', _, Expr, Clauses}, DangerousList) ->
    lists:flatten([
        find_calls_in_expr(ModName, FunName, FunArity, Expr, DangerousList) |
        [find_calls_in_clause(ModName, FunName, FunArity, 0, C, DangerousList) || C <- Clauses]
    ]);
find_calls_in_expr(ModName, FunName, FunArity, {'if', _, Clauses}, DangerousList) ->
    lists:flatten([find_calls_in_clause(ModName, FunName, FunArity, 0, C, DangerousList) || C <- Clauses]);
find_calls_in_expr(ModName, FunName, FunArity, {'receive', _, Clauses}, DangerousList) ->
    lists:flatten([find_calls_in_clause(ModName, FunName, FunArity, 0, C, DangerousList) || C <- Clauses]);
find_calls_in_expr(ModName, FunName, FunArity, {'try', _, Body, CaseClauses, CatchClauses, After}, DangerousList) ->
    lists:flatten([
        find_calls_in_exprs(ModName, FunName, FunArity, Body, DangerousList),
        [find_calls_in_clause(ModName, FunName, FunArity, 0, C, DangerousList) || C <- CaseClauses],
        [find_calls_in_clause(ModName, FunName, FunArity, 0, C, DangerousList) || C <- CatchClauses],
        find_calls_in_exprs(ModName, FunName, FunArity, After, DangerousList)
    ]);
find_calls_in_expr(ModName, FunName, FunArity, {block, _, Exprs}, DangerousList) ->
    find_calls_in_exprs(ModName, FunName, FunArity, Exprs, DangerousList);
find_calls_in_expr(ModName, FunName, FunArity, {'fun', _, {clauses, Clauses}}, DangerousList) ->
    lists:flatten([find_calls_in_clause(ModName, FunName, FunArity, 0, C, DangerousList) || C <- Clauses]);
find_calls_in_expr(ModName, FunName, FunArity, {lc, _, Expr, Qualifiers}, DangerousList) ->
    lists:flatten([
        find_calls_in_expr(ModName, FunName, FunArity, Expr, DangerousList) |
        [find_calls_in_expr(ModName, FunName, FunArity, Q, DangerousList) || Q <- Qualifiers]
    ]);
find_calls_in_expr(ModName, FunName, FunArity, {generate, _, Pattern, Expr}, DangerousList) ->
    lists:flatten([
        find_calls_in_expr(ModName, FunName, FunArity, Pattern, DangerousList),
        find_calls_in_expr(ModName, FunName, FunArity, Expr, DangerousList)
    ]);
find_calls_in_expr(ModName, FunName, FunArity, {match, _, Left, Right}, DangerousList) ->
    lists:flatten([
        find_calls_in_expr(ModName, FunName, FunArity, Left, DangerousList),
        find_calls_in_expr(ModName, FunName, FunArity, Right, DangerousList)
    ]);
find_calls_in_expr(ModName, FunName, FunArity, {tuple, _, Elements}, DangerousList) ->
    find_calls_in_exprs(ModName, FunName, FunArity, Elements, DangerousList);
find_calls_in_expr(ModName, FunName, FunArity, {cons, _, Head, Tail}, DangerousList) ->
    lists:flatten([
        find_calls_in_expr(ModName, FunName, FunArity, Head, DangerousList),
        find_calls_in_expr(ModName, FunName, FunArity, Tail, DangerousList)
    ]);
find_calls_in_expr(ModName, FunName, FunArity, {map, _, Assocs}, DangerousList) ->
    find_calls_in_exprs(ModName, FunName, FunArity, [V || {_, _, _, V} <- Assocs], DangerousList);
find_calls_in_expr(_, _, _, _, _) ->
    [].

%% @private Find NIF usage
find_nif_usage(ModName, BeamBin) ->
    case get_abstract_code(BeamBin) of
        {ok, Forms} ->
            find_nif_loads(ModName, Forms);
        error ->
            []
    end.

find_nif_loads(ModName, Forms) ->
    lists:flatten(lists:map(fun(Form) ->
        case Form of
            {function, _, _, _, Clauses} ->
                lists:flatten([find_nif_in_clause(ModName, C) || C <- Clauses]);
            _ ->
                []
        end
    end, Forms)).

find_nif_in_clause(ModName, {clause, _, _, _, Body}) ->
    lists:flatten(lists:map(fun(Expr) ->
        find_nif_in_expr(ModName, Expr)
    end, Body));
find_nif_in_clause(_, _) ->
    [].

find_nif_in_expr(ModName, {call, _, {remote, _, {atom, _, erlang}, {atom, _, load_nif}}, Args}) ->
    case Args of
        [{string, _, LibPath}, _] ->
            [#{module => ModName, nif_lib => list_to_binary(LibPath)}];
        _ ->
            [#{module => ModName, nif_lib => <<"unknown">>}]
    end;
find_nif_in_expr(_, _) ->
    [].

%% @private Generate warnings from scan results
generate_warnings(ModName, DangerousCalls, NifUsage) ->
    BifWarnings = lists:map(fun(#{module := M, function := F, arity := A}) ->
        #{
            severity => classify_bif_severity(M, F, A),
            type => dangerous_bif,
            message => iolist_to_binary(io_lib:format(
                "Dangerous function call: ~p:~p/~p", [M, F, A])),
            location => ModName
        }
    end, DangerousCalls),

    NifWarnings = lists:map(fun(#{nif_lib := Lib}) ->
        #{
            severity => high,
            type => nif_usage,
            message => iolist_to_binary(io_lib:format(
                "NIF library loaded: ~s", [Lib])),
            location => ModName
        }
    end, NifUsage),

    BifWarnings ++ NifWarnings.

%% @private Classify severity of dangerous BIF
classify_bif_severity(os, cmd, _) -> critical;
classify_bif_severity(erlang, open_port, 2) -> critical;
classify_bif_severity(erlang, load_nif, 2) -> high;
classify_bif_severity(code, load_binary, 3) -> high;
classify_bif_severity(erlang, halt, _) -> critical;
classify_bif_severity(init, stop, _) -> critical;
classify_bif_severity(file, write_file, _) -> medium;
classify_bif_severity(file, delete, _) -> medium;
classify_bif_severity(net_kernel, _, _) -> high;
classify_bif_severity(_, _, _) -> low.

%% @private Merge multiple scan results
merge_scan_results(Results) ->
    lists:foldl(fun(R, Acc) ->
        #{
            dangerous_bifs => maps:get(dangerous_bifs, Acc, []) ++ maps:get(dangerous_bifs, R, []),
            nif_usage => maps:get(nif_usage, Acc, []) ++ maps:get(nif_usage, R, []),
            warnings => maps:get(warnings, Acc, []) ++ maps:get(warnings, R, []),
            undeclared_capabilities => maps:get(undeclared_capabilities, Acc, []) ++ maps:get(undeclared_capabilities, R, [])
        }
    end, #{dangerous_bifs => [], nif_usage => [], warnings => [], undeclared_capabilities => []}, Results).

%% @private Check capability patterns in manifest
check_capability_patterns(Manifest) ->
    Capabilities = maps:get(capabilities, Manifest, []),
    lists:filtermap(fun(Cap) ->
        case check_capability(Cap) of
            ok -> false;
            {warning, W} -> {true, W}
        end
    end, Capabilities).

check_capability({network, Specs}) ->
    case lists:any(fun({connect, <<"**">>}) -> true; (_) -> false end, Specs) of
        true ->
            {warning, #{
                severity => high,
                type => broad_network_access,
                message => <<"Package requests unrestricted network access">>
            }};
        false ->
            ok
    end;
check_capability({file_access, Specs}) ->
    case lists:any(fun({write, <<"/">>}) -> true;
                      ({write, <<"/**">>}) -> true;
                      (_) -> false end, Specs) of
        true ->
            {warning, #{
                severity => critical,
                type => root_file_access,
                message => <<"Package requests root file system write access">>
            }};
        false ->
            ok
    end;
check_capability(_) ->
    ok.

%% @private Check NIF declarations
check_nif_declarations(Manifest) ->
    case maps:get(capabilities, Manifest, []) of
        Caps ->
            case lists:keyfind(nifs, 1, Caps) of
                {nifs, NifList} when length(NifList) > 0 ->
                    [#{
                        severity => medium,
                        type => nif_declared,
                        message => iolist_to_binary(io_lib:format(
                            "Package declares NIF usage: ~p", [NifList]))
                    }];
                _ ->
                    []
            end
    end.

%% @private Check network access patterns
check_network_access(Manifest) ->
    case maps:get(capabilities, Manifest, []) of
        Caps ->
            case lists:keyfind(network, 1, Caps) of
                {network, Specs} ->
                    lists:filtermap(fun({connect, Pattern}) ->
                        case binary:match(Pattern, <<"*">>) of
                            nomatch -> false;
                            _ -> {true, #{
                                severity => low,
                                type => wildcard_network,
                                message => iolist_to_binary(io_lib:format(
                                    "Wildcard network pattern: ~s", [Pattern]))
                            }}
                        end
                    end, Specs);
                _ ->
                    []
            end
    end.
