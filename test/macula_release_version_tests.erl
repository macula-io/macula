%% @doc The release files, asserted against each other.
%%
%% relx names the directory of a release out of rebar.config's release tuple,
%% and nothing ties that string to the app's own vsn. Here it sat at 3.0.0
%% while the app was 15.6.0, which is the same drift mcl-fovea#18 fixed: a
%% release built from this tree is labelled 3.0.0 while every other place
%% calls it 15.6.0. Since macula-ci-images#5 the release guard refuses a tag
%% whose versions disagree, so the drift also blocks every v15 release; this
%% test fails sooner, on the eunit run that bumps one and forgets the other.
-module(macula_release_version_tests).

-include_lib("eunit/include/eunit.hrl").

%% Two places name ONE version: the OTP application and the relx release.
the_release_version_is_the_apps_version_test() ->
    _ = application:load(macula),
    {ok, App} = application:get_key(macula, vsn),
    {match, [Release]} = re:run(read("rebar.config"),
                                <<"\\{release, \\{macula, \"([^\"]+)\"\\}">>,
                                [{capture, all_but_first, binary}]),
    ?assertEqual(list_to_binary(App), Release).

%%==============================================================================

read(Name) ->
    {ok, Text} = file:read_file(alongside(Name)),
    Text.

%% Relative to the beam rather than the working directory, because eunit runs
%% from wherever the developer happens to be standing.
alongside(Name) -> climb(filename:dirname(code:which(?MODULE)), Name, 8).

climb(_Dir, Name, 0) -> Name;
climb(Dir, Name, Left) ->
    Candidate = filename:join(Dir, Name),
    found(filelib:is_regular(Candidate), Candidate, Dir, Name, Left).

found(true, Candidate, _Dir, _Name, _Left) -> Candidate;
found(false, _Candidate, Dir, Name, Left) ->
    climb(filename:dirname(Dir), Name, Left - 1).
