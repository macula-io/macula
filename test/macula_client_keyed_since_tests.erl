%% The pool keeps the moment a registration's spec first named the KEM key
%% (`keyed_since'), and carries it in the spec, so neither a renewal nor a link
%% respawn reopens the provider's clear-call window (E2E design §8.1, "Opting
%% in"; Fable's review of 13.0.0). A pre-signed advertisement that names a key
%% is refused: no keyless history of it is knowable.
-module(macula_client_keyed_since_tests).

-include_lib("eunit/include/eunit.hrl").

a_first_keyed_registration_is_stamped_now_test() ->
    Before = erlang:system_time(millisecond),
    #{ad := #{keyed_since := Since}} = macula_client:keyed_since(#{ad => #{kem => true}}, error),
    ?assert(Since >= Before).

a_renewal_keeps_the_first_moment_test() ->
    Previous = {ok, #{ad => #{kem => true, keyed_since => 42}}},
    ?assertMatch(#{ad := #{keyed_since := 42}},
                 macula_client:keyed_since(#{ad => #{kem => true, not_after => 7}}, Previous)).

a_keyless_registration_carries_no_moment_test() ->
    ?assertEqual(#{ad => #{ttl_ms => 1}},
                 macula_client:keyed_since(#{ad => #{ttl_ms => 1}}, {ok, #{ad => #{kem => true, keyed_since => 42}}})).

a_keyed_registration_after_a_keyless_one_starts_its_moment_now_test() ->
    Before = erlang:system_time(millisecond),
    #{ad := #{keyed_since := Since}} =
        macula_client:keyed_since(#{ad => #{kem => true}}, {ok, #{ad => #{ttl_ms => 1}}}),
    ?assert(Since >= Before).

%% A link respawn replays the registration the pool kept: the stamp in it.
a_replayed_registration_keeps_its_moment_test_() ->
    {timeout, 15, fun() ->
        {ok, _} = application:ensure_all_started(macula),
        {ok, Pool} = macula_client:connect([], #{}),
        {ok, NodeId} = own_node_id(Pool),
        Proc = <<"~", (binary:encode_hex(NodeId, lowercase))/binary, "/ring">>,
        _ = macula_client:advertise(Pool, <<1:256>>, Proc, fun(_) -> ok end, open, #{kem => true}, all, undefined),
        Procs = element(macula_client:state_field_index(procs), sys:get_state(Pool)),
        #{ad := #{keyed_since := Since}} = maps:get({<<1:256>>, Proc}, Procs),
        ?assert(is_integer(Since)),
        ok = macula_client:close(Pool)
    end}.

%% A pre-signed advertisement that names a key is refused at registration.
a_presigned_keyed_advertisement_is_refused_test_() ->
    {timeout, 15, fun() ->
        {ok, _} = application:ensure_all_started(macula),
        {ok, Profile} = macula_crypto_profile:configured(),
        {ok, Key} = macula_node_keys:generate(identity, Profile),
        NodeId = macula_node_keys:key_id(Key),
        {Public, _} = macula_seal:generate_key(Profile),
        Proc = <<"~", (binary:encode_hex(NodeId, lowercase))/binary, "/ring">>,
        Ad = macula_record:encode(macula_record:sign(macula_record:procedure_advertisement(
                 NodeId, <<1:256>>, Proc, <<2:256>>, #{kem_key => macula_seal:key_as_carried(Public)}), Key)),
        {ok, Pool} = macula_client:connect([], #{}),
        ?assertEqual({error, {confidentiality, presigned_keyed_advertisement}},
                     macula_client:advertise(Pool, <<1:256>>, Proc, fun(_) -> ok end, open, Ad, all, undefined)),
        ok = macula_client:close(Pool)
    end}.

own_node_id(Pool) ->
    {ok, #{self_node_id := NodeId}} = macula_client:status(Pool),
    {ok, NodeId}.
