%% EUnit tests for `macula_client_dedup'. The pool delivers each publication at most once: it keys its table on the
%% SHA-384 of the publication's tbs and keeps each key until the publication expires
%% (DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md, Publications).
-module(macula_client_dedup_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NOW, 1789000000000).
-define(MINUTE, 60000).
-define(REALM, <<1:256>>).

new_returns_table_test() ->
    Tab = macula_client_dedup:new(),
    ?assert(is_reference(Tab) orelse is_atom(Tab) orelse is_integer(Tab)),
    ets:delete(Tab).

first_sighting_is_new_test() ->
    Tab = macula_client_dedup:new(),
    ?assertEqual(new, macula_client_dedup:check(Tab, hash(<<"one">>), ?NOW)),
    ets:delete(Tab).

repeat_sighting_is_duplicate_test() ->
    Tab = macula_client_dedup:new(),
    ?assertEqual(new, macula_client_dedup:check(Tab, hash(<<"one">>), ?NOW)),
    ?assertEqual(duplicate, macula_client_dedup:check(Tab, hash(<<"one">>), ?NOW)),
    ?assertEqual(duplicate, macula_client_dedup:check(Tab, hash(<<"one">>), ?NOW)),
    ets:delete(Tab).

another_hash_is_new_test() ->
    Tab = macula_client_dedup:new(),
    ?assertEqual(new, macula_client_dedup:check(Tab, hash(<<"one">>), ?NOW)),
    ?assertEqual(new, macula_client_dedup:check(Tab, hash(<<"two">>), ?NOW)),
    ets:delete(Tab).

%% The key is a SHA-384 hash, never a 32-byte id such as a publisher's node_id.
a_key_that_is_not_48_bytes_is_refused_test() ->
    Tab = macula_client_dedup:new(),
    ?assertError(function_clause, macula_client_dedup:check(Tab, <<0:256>>, ?NOW)),
    ets:delete(Tab).

%% An entry holds up to and including the moment its publication expires, and a sweep after that moment drops it.
an_entry_is_kept_until_its_publication_has_expired_test() ->
    Tab = macula_client_dedup:new(),
    ExpiresAt = ?NOW + 15 * ?MINUTE,
    new = macula_client_dedup:check(Tab, hash(<<"one">>), ExpiresAt),
    ?assertEqual(0, macula_client_dedup:sweep(Tab, ExpiresAt)),
    ?assertEqual(duplicate, macula_client_dedup:check(Tab, hash(<<"one">>), ExpiresAt)),
    ?assertEqual(1, macula_client_dedup:sweep(Tab, ExpiresAt + 1)),
    ?assertEqual(new, macula_client_dedup:check(Tab, hash(<<"one">>), ExpiresAt)),
    ets:delete(Tab).

sweep_removes_only_the_expired_entries_test() ->
    Tab = macula_client_dedup:new(),
    new = macula_client_dedup:check(Tab, hash(<<"zero">>), ?NOW),
    new = macula_client_dedup:check(Tab, hash(<<"one">>), ?NOW + 1),
    new = macula_client_dedup:check(Tab, hash(<<"two">>), ?NOW + 2),
    ?assertEqual(2, macula_client_dedup:sweep(Tab, ?NOW + 2)),
    ?assertEqual(duplicate, macula_client_dedup:check(Tab, hash(<<"two">>), ?NOW + 2)),
    ets:delete(Tab).

%%------------------------------------------------------------------
%% Signed publications: the key is the hash of the publication's tbs
%%------------------------------------------------------------------

publications_test_() ->
    {setup, fun publisher/0,
     fun(Publisher) ->
         [{case_name(Case), fun() -> Case(Publisher) end}
          || Case <- [fun one_publication_is_one_key_in_a_publish_and_in_an_event/1,
                      fun a_publication_reusing_realm_publisher_and_seq_is_new/1]]
     end}.

%% The same publication bytes ride in the PUBLISH and in every EVENT made from it, so they are one delivery.
one_publication_is_one_key_in_a_publish_and_in_an_event(Publisher) ->
    Tab = macula_client_dedup:new(),
    #{publication := Publication} = Publish = wire(macula_frame:publish(publish_spec(<<"hello">>), Publisher)),
    Event = wire(macula_frame:event(#{publication => Publication, delivered_via => plumtree})),
    ?assertEqual(new, check(Tab, Publish)),
    ?assertEqual(duplicate, check(Tab, Event)),
    ets:delete(Tab).

%% Another publication under the same realm, publisher and seq has another tbs and so another key: a copy that reuses
%% a real publication's realm, publisher and seq never suppresses it.
a_publication_reusing_realm_publisher_and_seq_is_new(Publisher) ->
    Tab = macula_client_dedup:new(),
    ?assertEqual(new, check(Tab, wire(macula_frame:publish(publish_spec(<<"hello">>), Publisher)))),
    ?assertEqual(new, check(Tab, wire(macula_frame:publish(publish_spec(<<"hello again">>), Publisher)))),
    ets:delete(Tab).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

publisher() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    Key.

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).

%% Verify a frame as a subscriber's link does, then check its publication as the pool does.
check(Tab, Frame) ->
    {ok, #{publication_hash := Hash, expires_at := ExpiresAt}} = macula_frame:verify_publication(Frame, pq_pure, ?NOW),
    macula_client_dedup:check(Tab, Hash, ExpiresAt).

publish_spec(Payload) ->
    #{realm => ?REALM, topic => <<"weather.tienen">>, seq => 7, published_at => ?NOW, payload => Payload}.

%% A frame as a peer receives it: encoded and decoded.
wire(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

hash(Bytes) ->
    crypto:hash(sha384, Bytes).
