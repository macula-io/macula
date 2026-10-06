%%%-------------------------------------------------------------------
%%% @doc A node has ONE identity, and pools use it rather than minting
%%% their own.
%%%
%%% Raf's ruling, 2026-09-23: one identity key per node, persisted, shared
%%% by pools and the distribution tunnel, with an application still free to
%%% supply its own key.
%%%
%%% Before this, `macula_client:node_identity/2' called
%%% `macula_node_keys:generate/3' whenever no key was supplied, so:
%%%
%%% <ul>
%%%   <li>two pools on one machine were two different nodes, each with its
%%%       own node_id;</li>
%%%   <li>every restart made a stranger of anything not handed a key, and
%%%       its `(org, node_id)' grants stopped matching;</li>
%%%   <li>the puzzle was ground once per POOL rather than once per node.</li>
%%% </ul>
%%%
%%% "One node, one node_id" (D5) is what the invite-only design and every
%%% grant already assume, so this is those assumptions becoming true rather
%%% than a new guarantee.
%%%
%%% ⚠ EVERY CASE HERE POINTS THE NODE AT ITS OWN KEY FILE, in a directory
%%% the fixture creates and removes. A test that used the configured path
%%% would read, and possibly WRITE, the identity of the machine running the
%%% suite. That is someone's real node_id, and the grants that point at it
%%% are real.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_client_node_identity_tests).

-include_lib("eunit/include/eunit.hrl").

%% The logger handler callback that captures what a connect logs.
-export([log/2]).

node_identity_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun(Ctx) ->
          {"two pools on one node have the SAME node_id",
           {timeout, 60, fun() -> two_pools_share_one_node_id(Ctx) end}}
      end,
      fun(Ctx) ->
          {"a pool started after the first one is the same node again, so the "
           "node_id survives a restart",
           {timeout, 60, fun() -> the_node_id_survives_a_restart(Ctx) end}}
      end,
      fun(Ctx) ->
          {"the identity is written to disk, so the puzzle is ground once per "
           "node and not once per pool",
           {timeout, 60, fun() -> the_identity_is_persisted(Ctx) end}}
      end,
      fun(Ctx) ->
          {"a pool handed its own identity uses THAT one, not the node's",
           {timeout, 60, fun() -> a_supplied_identity_still_wins(Ctx) end}}
      end,
      fun(Ctx) ->
          {"concurrent first starts all end up with the SAME key, so a race "
           "cannot give one node two node_ids",
           {timeout, 60, fun() -> a_race_to_store_converges_on_one_key(Ctx) end}}
      end,
      fun(Ctx) ->
          {"an existing key that will not load is REFUSED, never replaced",
           {timeout, 60, fun() -> an_unreadable_key_is_never_overwritten(Ctx) end}}
      end,
      fun(Ctx) ->
          {"a stored key in another profile is refused naming the file and both "
           "profiles (macula#40)",
           {timeout, 60, fun() -> a_stored_key_in_another_profile_names_the_file(Ctx) end}}
      end,
      fun(Ctx) ->
          {"the key is stored as <name>.<profile>.key, default when the program names no identity (macula#76)",
           {timeout, 60, fun() -> the_key_is_stored_by_name_and_profile(Ctx) end}}
      end,
      fun(Ctx) ->
          {"a named identity is its own node, the same one on every start",
           {timeout, 60, fun() -> a_named_identity_is_its_own_stable_node(Ctx) end}}
      end,
      fun(Ctx) ->
          {"the identity_name application env names the program's identity",
           {timeout, 60, fun() -> the_application_env_names_the_identity(Ctx) end}}
      end,
      fun(Ctx) ->
          {"a name that is not lowercase letters, digits, - and _ is refused and nothing is written",
           {timeout, 60, fun() -> an_unusable_name_is_refused(Ctx) end}}
      end,
      fun(Ctx) ->
          {"each profile has its own key under one name, each with its own stable node_id",
           {timeout, 120, fun() -> each_profile_has_its_own_key(Ctx) end}}
      end,
      fun(Ctx) ->
          {"every connect logs the key file and the node_id it uses",
           {timeout, 60, fun() -> every_connect_logs_its_key_file_and_node_id(Ctx) end}}
      end,
      fun(Ctx) ->
          {"a connect with a supplied key logs that the key was supplied, and its node_id",
           {timeout, 60, fun() -> a_supplied_key_is_logged_as_supplied(Ctx) end}}
      end,
      fun(Ctx) ->
          {"the old identity.key is moved to default.<its profile>.key, byte for byte, and not read in place",
           {timeout, 60, fun() -> the_old_key_moves_into_the_layout(Ctx) end}}
      end,
      fun(Ctx) ->
          {"an old identity.key of the other profile moves under that profile, and the node gets its own",
           {timeout, 120, fun() -> an_old_key_of_the_other_profile_moves_under_it(Ctx) end}}
      end,
      fun(Ctx) ->
          {"an old identity.key whose place is taken by another key is refused, naming both, and both stay",
           {timeout, 60, fun() -> an_old_key_whose_place_is_taken_is_refused(Ctx) end}}
      end,
      fun(Ctx) ->
          {"an old identity.key already linked into place is finished, not refused",
           {timeout, 60, fun() -> an_old_key_already_in_place_is_finished(Ctx) end}}
      end,
      fun(Ctx) ->
          {"an old identity.key that will not load is refused, naming it, and left as it is",
           {timeout, 60, fun() -> an_unreadable_old_key_is_refused(Ctx) end}}
      end,
      fun(Ctx) ->
          {"a node_identity_path still set refuses the connect, naming it and identity_dir, and writes nothing",
           {timeout, 60, fun() -> a_leftover_node_identity_path_refuses_the_connect(Ctx) end}}
      end,
      fun(Ctx) ->
          {"a node_identity_path still set refuses the application's start",
           {timeout, 60, fun() -> a_leftover_node_identity_path_refuses_the_start(Ctx) end}}
      end]}.

setup() ->
    {ok, _} = application:ensure_all_started(macula),
    Dir = macula_test_tmp:dir("macula-node-identity"),
    IdentityDir = filename:join(Dir, "identity"),
    Path = filename:join(IdentityDir, "default." ++ atom_to_list(profile()) ++ ".key"),
    Before = [{Env, application:get_env(macula, Env)} || Env <- [identity_dir, identity_name]],
    ok = application:set_env(macula, identity_dir, IdentityDir),
    ok = application:unset_env(macula, identity_name),
    #{dir => Dir, identity_dir => IdentityDir, path => Path, old => filename:join(Dir, "identity.key"),
      before => Before}.

cleanup(#{dir := Dir, before := Before}) ->
    ok = application:unset_env(macula, node_identity_path),
    [restore_env(Env, Was) || {Env, Was} <- Before],
    ok = file:del_dir_r(Dir),
    ok.

restore_env(Env, {ok, Was}) -> application:set_env(macula, Env, Was);
restore_env(Env, undefined) -> application:unset_env(macula, Env).

%%%===================================================================
%%% Test bodies
%%%===================================================================

%% The premise of the whole change: one machine is one node. Two pools
%% started without a key of their own are the same node as each other.
two_pools_share_one_node_id(_Ctx) ->
    {ok, A} = macula_client:connect([], #{}),
    {ok, B} = macula_client:connect([], #{}),
    try
        ?assertEqual(node_id_of(A), node_id_of(B))
    after
        close([A, B])
    end.

%% What "stored" buys: the node is the same node tomorrow. Today anything not
%% handed a key becomes a stranger on every start, and its (org, node_id)
%% grants stop matching a node that no longer exists.
%%
%% A pool closed and another started is the closest this suite can get to a
%% restart in one VM. It is not a full restart: the application stays up, so
%% it proves the identity outlives a POOL rather than the node. Said plainly
%% rather than dressed up, because what it does not cover is the case where
%% the identity is cached in application state and never actually read back
%% from disk -- `the_identity_is_persisted/1' is what covers that.
the_node_id_survives_a_restart(_Ctx) ->
    {ok, First} = macula_client:connect([], #{}),
    Was = node_id_of(First),
    ok = macula_client:close(First),
    {ok, Second} = macula_client:connect([], #{}),
    try
        ?assertEqual(Was, node_id_of(Second))
    after
        close([Second])
    end.

%% The puzzle is expensive, and the point of storing the identity is to grind
%% it ONCE for the node. The file existing is what makes the next start cheap
%% and the next node_id the same one.
the_identity_is_persisted(#{path := Path}) ->
    ?assertNot(filelib:is_regular(Path)),
    {ok, Pool} = macula_client:connect([], #{}),
    try
        ?assert(filelib:is_regular(Path))
    after
        close([Pool])
    end.

%% ⚠ NOT A RED-FIRST CASE, AND IT IS NOT PRETENDING TO BE. The supplied-key
%% path is unchanged by this work and this passes today. It is a
%% characterisation test guarding the escape hatch the ruling keeps on
%% purpose: an application may run a deliberately separate participant on one
%% machine, and a change that made every pool the node would break that
%% silently. Retro-validate it by mutation, not by claiming a red it never
%% had.
%%
%% The escape hatch, which the ruling keeps on purpose: an application may
%% run a deliberately separate participant on the same machine. A pool
%% handed a key uses it, and does not quietly become the node.
a_supplied_identity_still_wins(_Ctx) ->
    {ok, Own} = macula_node_keys:generate(identity, profile(),
                                          #{puzzle_difficulty =>
                                                macula_node_keys:puzzle_difficulty()}),
    {ok, Expected} = macula_node_keys:node_id(Own),
    {ok, Node} = macula_client:connect([], #{}),
    {ok, Supplied} = macula_client:connect([], #{node_identity => Own}),
    try
        ?assertEqual(Expected, node_id_of(Supplied)),
        ?assertNotEqual(node_id_of(Node), node_id_of(Supplied))
    after
        close([Node, Supplied])
    end.

%% ⚠ THE CASE THE WHOLE STORAGE DESIGN EXISTS FOR. Nothing serialises two
%% first starts: `macula_client:connect/2' is a plain `gen_server:start_link'
%% so `init/1' runs in the new process, and the distribution tunnel reaches
%% the identity before the application is even started, so no supervised
%% owner could arbitrate. Several callers therefore find no file at once and
%% all of them grind.
%%
%% A plain save would let the last writer replace the file while every
%% earlier caller carried on holding the key IT ground: one node, several
%% node_ids, appearing as a rare startup flake. `create_new/2' only creates,
%% so one caller wins and the rest read the winner's key back.
%%
%% Eight at once, started from one spawn loop, because the window is the
%% microseconds between "no file" and "file linked" and one pair would
%% usually miss it.
a_race_to_store_converges_on_one_key(#{path := Path}) ->
    Profile = profile(),
    Test = self(),
    Racers = [spawn_link(fun() ->
                             Test ! {self(), macula_node_keys:node_identity(Path, Profile)}
                         end) || _ <- lists:seq(1, 8)],
    Keys = [receive {P, R} -> R after 60_000 -> error(racer_never_answered) end || P <- Racers],
    NodeIds = [begin {ok, K} = R, {ok, Id} = macula_node_keys:node_id(K), Id end || R <- Keys],

    ?assertEqual(8, length(NodeIds)),
    ?assertEqual(1, length(lists:usort(NodeIds))),
    %% And the one they agreed on is the one on disk, not merely one they
    %% agreed between themselves.
    {ok, Stored} = macula_node_keys:load(Path, identity, Profile),
    {ok, StoredId} = macula_node_keys:node_id(Stored),
    ?assertEqual([StoredId], lists:usort(NodeIds)).

%% ⛔ THE MOST DESTRUCTIVE THING THIS CODE COULD DO is grind a replacement for
%% a key file it cannot read. The node's permissions point at an id it could
%% then no longer prove, and the evidence of what went wrong is gone. Wrong
%% profile, corrupt, truncated, wrong permissions: every one is refused and
%% the file is left exactly as it was, byte for byte, for an operator to look
%% at and move aside deliberately.
an_unreadable_key_is_never_overwritten(#{path := Path}) ->
    Garbage = <<"this is not a macula node key">>,
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, Garbage),
    ok = file:change_mode(Path, 8#0600),

    ?assertMatch({error, _}, macula_node_keys:node_identity(Path, profile())),
    ?assertEqual({ok, Garbage}, file:read_file(Path)).

%% macula#40: a host that runs tools in both profiles can hold a stored
%% identity of the other one. The refusal used to be a bare
%% `{wrong_profile, Found}': no file, no expected profile, and not the
%% `{node_identity, _}' wrapper a supplied key's refusal carries. It now says
%% what to fix: which file, which profile it holds, which the node runs. The
%% file is left exactly as it was.
a_stored_key_in_another_profile_names_the_file(#{path := Path}) ->
    Profile = profile(),
    [Other] = macula_crypto_profile:profiles() -- [Profile],
    {ok, Key} = macula_node_keys:generate(identity, Other),
    ok = macula_node_keys:save(Path, Key),
    {ok, Before} = file:read_file(Path),

    ?assertEqual({error, {node_identity,
                          {stored_key, Path, {wrong_profile, #{found => Other, expected => Profile}}}}},
                 macula_client:connect([], #{})),
    ?assertEqual({ok, Before}, file:read_file(Path)).

%% macula#76: the key's file says whose it is and which profile it is for. A program that names no identity is
%% `default', so the file a plain connect uses is <identity_dir>/default.<profile>.key.
the_key_is_stored_by_name_and_profile(#{path := Path}) ->
    {ok, Pool} = macula_client:connect([], #{}),
    try
        {ok, Stored} = macula_node_keys:load(Path, identity, profile()),
        ?assertEqual(macula_node_keys:node_id(Stored), {ok, node_id_of(Pool)})
    after
        close([Pool])
    end.

%% Named identities are how one account runs several programs that must not share rights, as the crew's agents do.
%% A named pool is a different node from the default one, and the same node again on the next start.
a_named_identity_is_its_own_stable_node(#{identity_dir := IdentityDir}) ->
    {ok, Default} = macula_client:connect([], #{}),
    {ok, Named} = macula_client:connect([], #{identity_name => <<"agent-venus">>}),
    NamedId = node_id_of(Named),
    ok = macula_client:close(Named),
    {ok, Again} = macula_client:connect([], #{identity_name => <<"agent-venus">>}),
    try
        ?assertNotEqual(node_id_of(Default), NamedId),
        ?assertEqual(NamedId, node_id_of(Again)),
        ?assert(filelib:is_regular(filename:join(IdentityDir, "agent-venus." ++ atom_to_list(profile()) ++ ".key")))
    after
        close([Default, Again])
    end.

the_application_env_names_the_identity(#{identity_dir := IdentityDir}) ->
    ok = application:set_env(macula, identity_name, <<"weather-station">>),
    {ok, Pool} = macula_client:connect([], #{}),
    try
        Path = filename:join(IdentityDir, "weather-station." ++ atom_to_list(profile()) ++ ".key"),
        {ok, Stored} = macula_node_keys:load(Path, identity, profile()),
        ?assertEqual(macula_node_keys:node_id(Stored), {ok, node_id_of(Pool)})
    after
        close([Pool])
    end.

%% The name is a file name and the dot separates it from the profile, so only a plain token passes: no dot, no
%% path separator, nothing that could name a file outside the directory or another profile's key.
an_unusable_name_is_refused(#{identity_dir := IdentityDir}) ->
    [?assertEqual({error, {node_identity, {identity_name, invalid}}},
                  macula_client:connect([], #{identity_name => Name}))
     || Name <- [<<>>, <<"a.pq_pure">>, <<"../evil">>, <<"a/b">>, <<"Upper">>, <<"-lead">>, <<"sp ace">>,
                 binary:copy(<<"a">>, 65), "a-list", default]],
    ?assertNot(filelib:is_dir(IdentityDir)).

%% The case #40 hid: a tool in each profile on one account. A key works in one profile only, so they are two nodes,
%% and the layout keeps both keys side by side instead of refusing one of them.
each_profile_has_its_own_key(#{identity_dir := IdentityDir}) ->
    Ids = [begin
               {ok, Key, Path} = macula_node_keys:stored_identity(<<"default">>, P),
               {ok, Again, Path} = macula_node_keys:stored_identity(<<"default">>, P),
               ?assertEqual(filename:join(IdentityDir, "default." ++ atom_to_list(P) ++ ".key"), Path),
               ?assertEqual(macula_node_keys:node_id(Key), macula_node_keys:node_id(Again)),
               macula_node_keys:node_id(Key)
           end || P <- macula_crypto_profile:profiles()],
    ?assertEqual(length(Ids), length(lists:usort(Ids))).

%% The risk of the layout is a silent identity change: a tool switches profile or name and becomes another node, and
%% every grant pinned to the old id stops matching with no error. The connect's log line is what shows it.
every_connect_logs_its_key_file_and_node_id(#{path := Path}) ->
    {Pool, Events} = logged(fun() -> {ok, P} = macula_client:connect([], #{}), P end),
    try
        ?assertMatch([#{key_file := Path, node_id := _, profile := _}], identity_reports(Events)),
        [#{node_id := Logged, profile := Profile}] = identity_reports(Events),
        ?assertEqual(binary:encode_hex(node_id_of(Pool), lowercase), Logged),
        ?assertEqual(profile(), Profile)
    after
        close([Pool])
    end.

a_supplied_key_is_logged_as_supplied(_Ctx) ->
    {ok, Own} = macula_node_keys:generate(identity, profile(),
                                          #{puzzle_difficulty => macula_node_keys:puzzle_difficulty()}),
    {ok, Id} = macula_node_keys:node_id(Own),
    {Pool, Events} = logged(fun() -> {ok, P} = macula_client:connect([], #{node_identity => Own}), P end),
    try
        ?assertEqual([#{key_file => supplied, node_id => binary:encode_hex(Id, lowercase), profile => profile()}],
                     identity_reports(Events))
    after
        close([Pool])
    end.

%% The old single identity.key, next to the identity directory, is moved into the layout under its own profile and
%% name `default'. It is not read in place as a fallback, so after the first connect there is one place it lives.
the_old_key_moves_into_the_layout(#{old := Old, path := Path}) ->
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    ok = macula_node_keys:save(Old, Key),
    {ok, Bytes} = file:read_file(Old),
    {ok, Pool} = macula_client:connect([], #{}),
    try
        ?assertNot(filelib:is_regular(Old)),
        ?assertEqual({ok, Bytes}, file:read_file(Path)),
        ?assertEqual(macula_node_keys:node_id(Key), {ok, node_id_of(Pool)})
    after
        close([Pool])
    end.

%% The #40 host: its old key was made in the other profile. It moves under ITS profile, where a tool of that profile
%% will find it, and this node grinds its own key beside it rather than being refused.
an_old_key_of_the_other_profile_moves_under_it(#{old := Old, identity_dir := IdentityDir, path := Path}) ->
    [Other] = macula_crypto_profile:profiles() -- [profile()],
    {ok, Key} = macula_node_keys:generate(identity, Other),
    ok = macula_node_keys:save(Old, Key),
    {ok, Bytes} = file:read_file(Old),
    {ok, Pool} = macula_client:connect([], #{}),
    try
        ?assertNot(filelib:is_regular(Old)),
        ?assertEqual({ok, Bytes}, file:read_file(filename:join(IdentityDir, "default." ++ atom_to_list(Other) ++ ".key"))),
        ?assert(filelib:is_regular(Path)),
        ?assertNotEqual(macula_node_keys:node_id(Key), {ok, node_id_of(Pool)})
    after
        close([Pool])
    end.

%% Two keys for one place: neither is chosen, neither is touched, and the refusal names both files.
an_old_key_whose_place_is_taken_is_refused(#{old := Old, path := Path}) ->
    {ok, OldKey} = macula_node_keys:generate(identity, profile()),
    {ok, NewKey} = macula_node_keys:generate(identity, profile()),
    ok = macula_node_keys:save(Old, OldKey),
    ok = macula_node_keys:save(Path, NewKey),
    {ok, OldBytes} = file:read_file(Old),
    {ok, NewBytes} = file:read_file(Path),
    ?assertEqual({error, {node_identity, {old_identity_key, #{from => Old, to => Path}, place_taken}}},
                 macula_client:connect([], #{})),
    ?assertEqual({ok, OldBytes}, file:read_file(Old)),
    ?assertEqual({ok, NewBytes}, file:read_file(Path)).

%% A move cut short after the link and before the old name went leaves the same key under both names: the move is
%% finished, since nothing is lost by removing the old name.
an_old_key_already_in_place_is_finished(#{old := Old, path := Path}) ->
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    ok = macula_node_keys:save(Old, Key),
    ok = filelib:ensure_dir(Path),
    ok = file:make_link(Old, Path),
    {ok, Pool} = macula_client:connect([], #{}),
    try
        ?assertNot(filelib:is_regular(Old)),
        ?assertEqual(macula_node_keys:node_id(Key), {ok, node_id_of(Pool)})
    after
        close([Pool])
    end.

an_unreadable_old_key_is_refused(#{old := Old, identity_dir := IdentityDir}) ->
    Garbage = <<"this is not a macula node key">>,
    ok = file:write_file(Old, Garbage),
    ok = file:change_mode(Old, 8#0600),
    ?assertEqual({error, {node_identity, {old_identity_key, #{from => Old}, bad_key_file}}},
                 macula_client:connect([], #{})),
    ?assertEqual({ok, Garbage}, file:read_file(Old)),
    ?assertNot(filelib:is_dir(IdentityDir)).

%% The setting #76 removed. A consumer that still sets it, as a station harness does with one key per station,
%% would otherwise fall back to the account's shared directory without a word: several stations, one key, one
%% node_id, the unnoticed identity change #76 exists to prevent. So it is refused, naming what replaces it.
a_leftover_node_identity_path_refuses_the_connect(#{dir := Dir, identity_dir := IdentityDir}) ->
    Old = filename:join(Dir, "station-1.key"),
    ok = application:set_env(macula, node_identity_path, Old),
    ?assertEqual({error, {node_identity, {node_identity_path_removed,
                                          #{node_identity_path => Old, use_instead => [identity_dir, identity_name]}}}},
                 macula_client:connect([], #{})),
    ?assertNot(filelib:is_dir(IdentityDir)),
    ?assertNot(filelib:is_regular(Old)).

a_leftover_node_identity_path_refuses_the_start(#{dir := Dir}) ->
    Old = filename:join(Dir, "station-1.key"),
    ok = application:set_env(macula, node_identity_path, Old),
    ?assertEqual({error, {node_identity_path_removed,
                          #{node_identity_path => Old, use_instead => [identity_dir, identity_name]}}},
                 macula_app:start(normal, [])).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% What a connect logged about the identity it uses: the macula_identity metadata of its log events.
identity_reports(Events) ->
    [Report || #{meta := #{macula_identity := Report}} <- Events].

logged(Act) ->
    Handler = list_to_atom("identity_log_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = logger:add_handler(Handler, ?MODULE, #{config => #{test => self()}, level => all, filter_default => log}),
    Result = try Act() after ok = logger:remove_handler(Handler) end,
    {Result, drained([])}.

log(Event, #{config := #{test := Test}}) ->
    Test ! {captured, Event}.

drained(Events) ->
    receive {captured, Event} -> drained([Event | Events]) after 0 -> lists:reverse(Events) end.

node_id_of(Pool) ->
    {ok, #{self_node_id := NodeId}} = macula_client:status(Pool),
    NodeId.

profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.

close(Pools) ->
    [catch macula_client:close(P) || P <- Pools],
    ok.
