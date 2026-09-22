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
      end]}.

setup() ->
    {ok, _} = application:ensure_all_started(macula),
    Dir = macula_test_tmp:dir("macula-node-identity"),
    Path = filename:join(Dir, "identity.key"),
    Before = application:get_env(macula, node_identity_path),
    ok = application:set_env(macula, node_identity_path, Path),
    #{dir => Dir, path => Path, before => Before}.

cleanup(#{dir := Dir, before := Before}) ->
    restore_env(Before),
    ok = file:del_dir_r(Dir),
    ok.

restore_env({ok, Was}) -> application:set_env(macula, node_identity_path, Was);
restore_env(undefined)  -> application:unset_env(macula, node_identity_path).

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

%%%===================================================================
%%% Helpers
%%%===================================================================

node_id_of(Pool) ->
    {ok, #{self_node_id := NodeId}} = macula_client:status(Pool),
    NodeId.

profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.

close(Pools) ->
    [catch macula_client:close(P) || P <- Pools],
    ok.
