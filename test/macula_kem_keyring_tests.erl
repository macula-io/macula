%% A node's KEM keys (E2E design, Amendment A1): held in memory, one current
%% key per node identity, shared by every pool of that identity in the VM,
%% rotated on a schedule, and an old key kept for a while after rotation so
%% calls sealed to it still open, then deleted.
-module(macula_kem_keyring_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NODE_A, <<1:256>>).
-define(NODE_B, <<2:256>>).

%% A node's current key is of its profile's size, and named by its id.
a_node_gets_a_current_key_of_its_profile_test_() ->
    [{atom_to_list(Profile), fun() ->
          with_keyring(#{}, fun(Table) ->
              ok = macula_kem_keyring:ensure(Table, ?NODE_A, Profile),
              {ok, #{key_id := KeyId, key := Carried}} = macula_kem_keyring:current(Table, ?NODE_A),
              ?assertEqual(macula_seal:carried_key_size(Profile), byte_size(Carried)),
              ?assertEqual(macula_seal:key_id(Carried), KeyId)
          end)
      end} || Profile <- [pq_pure, pq_hybrid]].

%% A node that was never ensured has no key.
a_node_never_ensured_has_no_key_test() ->
    with_keyring(#{}, fun(Table) ->
        ?assertEqual(error, macula_kem_keyring:current(Table, ?NODE_A))
    end).

%% Ensuring again keeps the key: every pool of one identity shares it.
ensuring_again_keeps_the_key_test() ->
    with_keyring(#{}, fun(Table) ->
        ok = macula_kem_keyring:ensure(Table, ?NODE_A, pq_pure),
        {ok, First} = macula_kem_keyring:current(Table, ?NODE_A),
        ok = macula_kem_keyring:ensure(Table, ?NODE_A, pq_pure),
        ?assertEqual({ok, First}, macula_kem_keyring:current(Table, ?NODE_A))
    end).

%% Pools of one identity ensuring at the same moment still share one key:
%% only the first makes it.
concurrent_ensures_make_one_key_test() ->
    with_keyring(#{}, fun(Table) ->
        Self = self(),
        [spawn(fun() -> Self ! {ensured, macula_kem_keyring:ensure(Table, ?NODE_A, pq_pure)} end)
         || _ <- lists:seq(1, 16)],
        [receive {ensured, ok} -> ok after 5_000 -> error(no_ensure) end || _ <- lists:seq(1, 16)],
        ?assertEqual(1, length(ets:match(Table, {{key, ?NODE_A, '_'}, '_', '_'})))
    end).

%% Two identities hold two keys.
two_nodes_hold_two_keys_test() ->
    with_keyring(#{}, fun(Table) ->
        ok = macula_kem_keyring:ensure(Table, ?NODE_A, pq_pure),
        ok = macula_kem_keyring:ensure(Table, ?NODE_B, pq_pure),
        {ok, #{key_id := A}} = macula_kem_keyring:current(Table, ?NODE_A),
        {ok, #{key_id := B}} = macula_kem_keyring:current(Table, ?NODE_B),
        ?assertNotEqual(A, B)
    end).

%% What the node's holder looks up opens what a caller sealed to the current
%% key, in both profiles.
the_holder_opens_what_is_sealed_to_the_current_key_test_() ->
    [{atom_to_list(Profile), fun() ->
          with_keyring(#{}, fun(Table) ->
              ok = macula_kem_keyring:ensure(Table, ?NODE_A, Profile),
              {ok, Holder} = macula_kem_keyring:holder(Table, ?NODE_A),
              {ok, #{key := Carried}} = macula_kem_keyring:current(Table, ?NODE_A),
              Request = request(),
              {ok, Profile, Public} = macula_seal:public_key(Carried),
              {Sealed, Keys} = macula_sealed_call:seal_request(Profile, Public, Request, <<"hi">>),
              ?assertEqual({ok, <<"hi">>, Keys}, macula_sealed_call:open_request(Profile, Holder, Request, Sealed))
          end)
      end} || Profile <- [pq_pure, pq_hybrid]].

%% A rotation names a new current key; the old one still opens until it is
%% retired, then it is gone, and the holder refuses naming the new key.
a_rotated_key_opens_until_it_is_retired_test() ->
    with_keyring(#{retain_ms => 200}, fun(Table) ->
        ok = macula_kem_keyring:ensure(Table, ?NODE_A, pq_pure),
        {ok, #{key := OldKey, key_id := OldId}} = macula_kem_keyring:current(Table, ?NODE_A),
        Request = request(),
        {ok, pq_pure, Public} = macula_seal:public_key(OldKey),
        {Sealed, _} = macula_sealed_call:seal_request(pq_pure, Public, Request, <<"hi">>),
        ok = macula_kem_keyring:rotate(Table, ?NODE_A),
        {ok, #{key_id := NewId}} = macula_kem_keyring:current(Table, ?NODE_A),
        ?assertNotEqual(OldId, NewId),
        {ok, Holder} = macula_kem_keyring:holder(Table, ?NODE_A),
        ?assertMatch({ok, <<"hi">>, _}, macula_sealed_call:open_request(pq_pure, Holder, Request, Sealed)),
        timer:sleep(400),
        {ok, Later} = macula_kem_keyring:holder(Table, ?NODE_A),
        ?assertEqual({error, {sealed_refused, NewId}}, macula_sealed_call:open_request(pq_pure, Later, Request, Sealed))
    end).

%% The schedule rotates by itself.
the_schedule_rotates_the_key_test() ->
    with_keyring(#{rotate_after_ms => 150}, fun(Table) ->
        ok = macula_kem_keyring:ensure(Table, ?NODE_A, pq_pure),
        {ok, #{key_id := First}} = macula_kem_keyring:current(Table, ?NODE_A),
        timer:sleep(400),
        {ok, #{key_id := Later}} = macula_kem_keyring:current(Table, ?NODE_A),
        ?assertNotEqual(First, Later)
    end).

%% No private key is in the server's state, so no crash report or state
%% dump can print one.
no_private_key_sits_in_the_server_state_test() ->
    with_keyring(#{}, fun(Table) ->
        ok = macula_kem_keyring:ensure(Table, ?NODE_A, pq_hybrid),
        Owner = ets:info(Table, owner),
        State = term_to_binary(sys:get_state(Owner)),
        {ok, Holder} = macula_kem_keyring:holder(Table, ?NODE_A),
        #{current_key_id := KeyId, lookup := Lookup} = Holder,
        {ok, #{mlkem_dk := Dk, p384_priv := P384}, _Carried} = Lookup(KeyId),
        ?assertEqual(nomatch, binary:match(State, Dk)),
        ?assertEqual(nomatch, binary:match(State, P384))
    end).

%% The table is protected: only the keyring writes it.
only_the_keyring_writes_its_table_test() ->
    with_keyring(#{}, fun(Table) ->
        ?assertEqual(protected, ets:info(Table, protection))
    end).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

with_keyring(Options, Test) ->
    Table = list_to_atom("kem_keyring_test_" ++ integer_to_list(erlang:unique_integer([positive]))),
    {ok, Pid} = macula_kem_keyring:start_link(Options#{table => Table}),
    unlink(Pid),
    try Test(Table)
    after exit(Pid, shutdown)
    end.

request() ->
    #{frame_type => <<"call">>, realm => <<5:256>>, procedure => <<"acme/count_v1">>, caller => <<6:256>>,
      target => ?NODE_A, request_id => <<8:128>>, deadline => 1790000000000}.
