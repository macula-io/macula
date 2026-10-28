%% Reads cover export files and reports the line coverage of them merged, exiting non-zero below a floor.
%%
%% WHY THIS EXISTS. eunit and ct run as separate steps, so each writes its own cover export
%% (_build/test/cover/<name>.coverdata) and neither is the whole picture: a module exercised only by the fleet
%% suites reads as dead in the eunit file, and a module exercised only by the unit tests reads as dead in the ct
%% file. Line coverage is a property of the SUITE, not of one of its halves, so the numbers are merged here and
%% the floor is applied to the merge (macula#153).
%%
%% What that merge is worth today, measured rather than assumed: across CI runs on 2026-10-28 it read 10612-10613
%% covered lines of 12934 executable ones over 125 modules (82.05-82.06%), and eunit alone accounts for all but a
%% couple of them. The two halves overlap almost entirely at the moment. The merge is still the right shape: which
%% modules ct reaches is an accident of which fleets the suites build, and a number that leaves a suite out must not
%% quietly look complete when that changes.
%%
%% The merge is cover's own: importing several export files into one node adds their per-line hit counts when
%% analysed (cover:import/1, OTP tools). A line either suite executed counts as covered once. Doing the union by
%% hand from the export files would re-implement that, and the export file is a length-prefixed sequence of
%% compressed terms rather than one term, so re-reading it is a format dependency this does not need.
%%
%% THE DENOMINATOR. Every cover-compiled module is exported, including one no test ever loaded, so a wholly
%% untested module arrives as zero covered lines rather than as a module missing from the total. That is what
%% makes the floor able to see a new untested module at all.
%%
%% main([FloorPercent, ListFile]) -> no_return()
%%
%% ListFile holds one export file path per line, so the caller never has to quote a path into an -eval string.
-module(line_coverage).

-export([main/1]).

%% How many of the worst modules to name. A number below the floor is useless without somewhere to look.
-define(WORST, 10).

-spec main([string()]) -> no_return().
main([FloorArg, ListFile]) ->
    Floor = floor_of(FloorArg),
    Files = paths(ListFile),
    cover:start(),
    Imported = lists:append([imported(File) || File <- Files]),
    Rows = analysis(lists:usort(Imported)),
    {Covered, Total} = sum(Rows),
    Percent = percent(Covered, Total),
    report(Files, Imported, Rows, Covered, Total, Percent),
    verdict(Floor, Percent);
main(Args) ->
    io:format(standard_error,
              "usage: line_coverage <floor-percent> <file-list>~n"
              "got: ~p~n", [Args]),
    halt(2).

%% -- the floor -------------------------------------------------------------------------------------

floor_of(Arg) ->
    case string:to_integer(Arg) of
        {N, ""} when N >= 0, N =< 100 -> N;
        _ -> refuse(3, "the floor is not a whole percent: ~ts", [Arg])
    end.

%% -- the paths -------------------------------------------------------------------------------------

paths(ListFile) ->
    case file:read_file(ListFile) of
        {ok, Bin} ->
            nonempty(string:lexemes(binary_to_list(Bin), "\n"));
        {error, Reason} ->
            refuse(2, "could not read the export file list ~ts: ~p", [ListFile, Reason])
    end.

nonempty(Paths) ->
    case [P || P <- Paths, P =/= ""] of
        [] -> refuse(2, "the export file list is empty: did the suites run with --cover?", []);
        Paths -> Paths
    end.

%% -- importing -------------------------------------------------------------------------------------

%% An export file that is missing or unreadable is a refused run, never a module-less one: a renamed file or a
%% suite that never ran would otherwise shrink the denominator and raise the percentage.
imported(File) ->
    require_regular(File),
    case cover:import(File) of
        ok -> cover:imported_modules();
        {error, Reason} -> refuse(2, "could not import ~ts: ~p", [File, Reason])
    end.

require_regular(File) ->
    case filelib:is_regular(File) of
        true -> ok;
        false -> refuse(2, "no cover export file at ~ts: did that suite run with --cover?", [File])
    end.

%% -- analysing -------------------------------------------------------------------------------------

%% One row per module: {Module, Covered, NotCovered, Percent}. Level `module` is the aggregate over executable
%% lines; level `line` answers per line, which is not what a floor over the suite is measured on.
%%
%% Every module asked about is expected to come back with data. One that does not would be silently absent from
%% both numerator and denominator, which raises the percentage, so it is a refused run.
analysis(Modules) ->
    case cover:analyse(Modules, coverage, module) of
        {result, [], []} ->
            refuse(2, "cover analysed no modules at all: the export files carry no data", []);
        {result, Result, []} ->
            [{Mod, Cov, NotCov, percent(Cov, Cov + NotCov)} || {Mod, {Cov, NotCov}} <- Result];
        {result, _Result, Failed} ->
            refuse(2, "cover has no data for modules it was told were imported: ~p", [Failed]);
        {error, Reason} ->
            refuse(2, "cover could not analyse the imported data: ~p", [Reason])
    end.

sum(Rows) ->
    {lists:sum([Cov || {_, Cov, _, _} <- Rows]),
     lists:sum([Cov + NotCov || {_, Cov, NotCov, _} <- Rows])}.

percent(_, 0) -> 0.0;
percent(Covered, Total) -> 100 * Covered / Total.

%% -- reporting -------------------------------------------------------------------------------------

report(Files, Merged, Rows, Covered, Total, Percent) ->
    io:format("Merged line coverage from ~b export file(s):~n", [length(Files)]),
    [io:format("  ~ts~n", [File]) || File <- Files],
    io:format("~n~b module(s) merged (~b after de-duplication).~n~n",
              [length(Merged), length(lists:usort(Merged))]),
    io:format("  covered lines      ~b~n", [Covered]),
    io:format("  executable lines   ~b~n", [Total]),
    io:format("  line coverage      ~.2f%~n~n", [Percent]),
    worst(Rows).

%% Named rather than counted, because "0.4% below the floor" is not actionable and the module that did it is.
worst(Rows) ->
    Uncovered = lists:reverse(lists:sort([{NotCov, Mod} || {Mod, _, NotCov, _} <- Rows, NotCov > 0])),
    case lists:sublist(Uncovered, ?WORST) of
        [] -> ok;
        Top ->
            io:format("Modules with the most uncovered lines:~n"),
            [io:format("  ~-45s ~5b uncovered~n", [atom_to_list(Mod), NotCov]) || {NotCov, Mod} <- Top],
            io:format("~n")
    end.

verdict(Floor, Percent) ->
    case Percent >= Floor of
        true ->
            io:format("PASS: ~.2f% is at or above the ~b% floor.~n", [Percent, Floor]),
            halt(0);
        false ->
            io:format(standard_error,
                      "FAIL: ~.2f% is below the ~b% floor.~n"
                      "The floor only goes up, in a commit that says why (CONTRIBUTING, Testing).~n",
                      [Percent, Floor]),
            halt(1)
    end.

refuse(Code, Fmt, Args) ->
    io:format(standard_error, Fmt ++ "~n", Args),
    halt(Code).
