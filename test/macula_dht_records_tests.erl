%% EUnit tests for the DHT record paths of the macula facade: a record is put as its wire form, and a record reaches a
%% caller of a find or a subscription only after it verifies under the node's crypto profile. The pool's RPC is mocked
%% at macula_client:call/5 and the subscription at macula_pubsub:subscribe_callback/4; what a station does with the
%% bytes is the station's.
-module(macula_dht_records_tests).

-include_lib("eunit/include/eunit.hrl").

-define(EU_TIMEOUT, 120).
-define(KEY, <<7:256>>).

dht_records_test_() ->
    {timeout, ?EU_TIMEOUT, {setup, fun start/0, fun stop/1, fun cases/1}}.

%% Every case runs on its own, so each passes or fails by itself.
cases(Keys) ->
    [{case_name(Case), fun() -> Case(Keys) end}
     || Case <- [fun a_signed_record_is_put_as_its_wire_form/1,
                 fun a_wire_form_is_put_as_it_is/1,
                 fun a_found_record_is_returned_verified/1,
                 fun a_found_record_that_does_not_verify_returns_its_refusal/1,
                 fun a_not_found_reply_answers_not_found_in_both_forms/1,
                 fun an_ok_reply_answers_ok_in_both_forms/1,
                 fun found_records_that_do_not_verify_are_dropped/1,
                 fun records_found_by_type_that_do_not_verify_are_dropped/1,
                 fun a_subscription_delivers_only_verified_records/1]].

%%------------------------------------------------------------------
%% Putting a record
%%------------------------------------------------------------------

a_signed_record_is_put_as_its_wire_form(Keys) ->
    Signed = node_record(Keys),
    replying({ok, ok}),
    ?assertEqual(ok, macula:put_record(self(), Signed)),
    ?assertEqual({<<"_dht.put_record">>, macula_record:encode(Signed)}, called()).

a_wire_form_is_put_as_it_is(Keys) ->
    Wire = macula_record:encode(node_record(Keys)),
    replying({ok, ok}),
    ?assertEqual(ok, macula:put_record(self(), Wire)),
    ?assertEqual({<<"_dht.put_record">>, Wire}, called()).

%%------------------------------------------------------------------
%% Finding records
%%------------------------------------------------------------------

a_found_record_is_returned_verified(#{key := Key} = Keys) ->
    KeyId = macula_node_keys:key_id(Key),
    replying({ok, macula_record:encode(node_record(Keys))}),
    ?assertMatch({ok, #{key_id := KeyId, type := _, payload := _}}, macula:find_record(self(), ?KEY)).

a_found_record_that_does_not_verify_returns_its_refusal(Keys) ->
    replying({ok, tampered(node_record(Keys))}),
    ?assertEqual({error, signature_invalid}, macula:find_record(self(), ?KEY)).

%% The station's not_found answer crosses the wire as text and decodes
%% back to the codec's {text, _} marker; the facade reads both forms.
a_not_found_reply_answers_not_found_in_both_forms(_Keys) ->
    replying({ok, not_found}),
    ?assertEqual({error, not_found}, macula:find_record(self(), ?KEY)),
    replying({ok, {text, <<"not_found">>}}),
    ?assertEqual({error, not_found}, macula:find_record(self(), ?KEY)).

%% The station's ok answer crosses the wire as text and decodes back to
%% the codec's {text, _} marker; the facade reads both forms.
an_ok_reply_answers_ok_in_both_forms(Keys) ->
    replying({ok, ok}),
    ?assertEqual(ok, macula:put_record(self(), macula_record:encode(node_record(Keys)))),
    replying({ok, {text, <<"ok">>}}),
    ?assertEqual(ok, macula:put_record(self(), macula_record:encode(node_record(Keys)))).

found_records_that_do_not_verify_are_dropped(#{key := Key} = Keys) ->
    KeyId = macula_node_keys:key_id(Key),
    replying({ok, [macula_record:encode(node_record(Keys)), tampered(node_record(Keys)), <<"not a record">>]}),
    ?assertMatch({ok, [#{key_id := KeyId}]}, macula:find_records(self(), ?KEY)).

records_found_by_type_that_do_not_verify_are_dropped(#{key := Key} = Keys) ->
    KeyId = macula_node_keys:key_id(Key),
    replying({ok, [tampered(node_record(Keys)), macula_record:encode(node_record(Keys))]}),
    ?assertMatch({ok, [#{key_id := KeyId}]}, macula:find_records_by_type(self(), 1)).

%%------------------------------------------------------------------
%% Subscribing to records
%%------------------------------------------------------------------

a_subscription_delivers_only_verified_records(#{key := Key} = Keys) ->
    Test = self(),
    ok = meck:expect(macula_pubsub, subscribe_callback,
                     fun(_Pool, _Realm, _Topic, Wrapped) -> Test ! {wrapped, Wrapped}, {ok, make_ref()} end),
    {ok, _Ref} = macula:subscribe_records(self(), 1, fun(Record) -> Test ! {delivered, Record} end),
    Wrapped = receive {wrapped, Fun} -> Fun after 1000 -> erlang:error(not_subscribed) end,
    Wrapped(<<"_dht.records.1.stored">>, tampered(node_record(Keys)), #{}),
    Wrapped(<<"_dht.records.1.stored">>, macula_record:encode(node_record(Keys)), #{}),
    Wrapped(<<"_dht.records.1.stored">>, #{not_a => record}, #{}),
    KeyId = macula_node_keys:key_id(Key),
    ?assertMatch([#{key_id := KeyId}], delivered()).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

start() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    ok = meck:new(macula_client, [passthrough]),
    ok = meck:new(macula_pubsub, [passthrough]),
    #{profile => Profile, key => Key}.

stop(_Keys) ->
    meck:unload([macula_client, macula_pubsub]).

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).

node_record(#{key := Key}) ->
    {ok, NodeId} = macula_node_keys:node_id(Key),
    macula_record:sign(macula_record:node_record(NodeId, [], 0), Key).

tampered(#{tbs := <<Head:20/binary, Byte, Tail/binary>>} = Signed) ->
    macula_record:encode(Signed#{tbs := <<Head/binary, (Byte bxor 1), Tail/binary>>}).

%% The pool's RPC answers Reply, and tells the test what was called with what.
replying(Reply) ->
    Test = self(),
    ok = meck:expect(macula_client, call_linked_station,
                     fun(_Pool, _Realm, Procedure, Payload, _TimeoutMs) ->
                         Test ! {called, Procedure, Payload},
                         Reply
                     end).

called() ->
    receive {called, Procedure, Payload} -> {Procedure, Payload} after 1000 -> erlang:error(not_called) end.

delivered() ->
    receive {delivered, Record} -> [Record | delivered()] after 100 -> [] end.
