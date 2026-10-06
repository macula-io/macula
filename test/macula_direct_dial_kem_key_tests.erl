%% A direct-dial advertisement makes the same decision as the pool's
%% ADVERTISE (E2E design §8.2, Amendment A1): it names the node's current KEM
%% key only when the node is switched on with `kem_advertise' and
%% `confidential' is not `off', and `required' while switched off is refused.
%% Otherwise a caller resolving the DHT record would call in the clear a
%% provider whose ADVERTISE named a key, and be refused once its window closed.
-module(macula_direct_dial_kem_key_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<0:256>>).
-define(STATION, <<1:256>>).

kem_key_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [{Name, fun() -> Case() end} || {Name, Case} <- cases()]}.

cases() ->
    [{"switched off, the record names no key", fun switched_off_names_no_key/0},
     {"switched off, required is refused", fun switched_off_refuses_required/0},
     {"switched on, the record names the current key", fun switched_on_names_the_current_key/0},
     {"switched on, off names no key", fun switched_on_off_names_no_key/0}].

switched_off_names_no_key() ->
    {ok, Read, _NodeId} = published(#{}),
    ?assertNot(is_map_key(kem_key, Read)).

switched_off_refuses_required() ->
    ?assertEqual({error, {confidentiality, kem_advertise_disabled}}, published(#{confidential => required})).

switched_on_names_the_current_key() ->
    ok = application:set_env(macula, kem_advertise, enabled),
    {ok, Read, NodeId} = published(#{}),
    {ok, #{key := Current, key_id := CurrentId}} = macula_kem_keyring:current(NodeId),
    ?assertMatch(#{kem_key := Current, kem_key_id := CurrentId}, Read).

switched_on_off_names_no_key() ->
    ok = application:set_env(macula, kem_advertise, enabled),
    {ok, Read, _NodeId} = published(#{confidential => off}),
    ?assertNot(is_map_key(kem_key, Read)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

setup() ->
    {ok, _} = application:ensure_all_started(macula),
    Previous = application:get_env(macula, kem_advertise),
    ok = application:unset_env(macula, kem_advertise),
    Previous.

teardown(undefined) -> application:unset_env(macula, kem_advertise);
teardown({ok, Value}) -> application:set_env(macula, kem_advertise, Value).

%% Publish an own-namespace advertisement and answer what the record it put
%% reads as, and the advertiser's node id.
published(Opts) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    NodeId = macula_node_keys:key_id(Key),
    Procedure = <<"~", (binary:encode_hex(NodeId, lowercase))/binary, "/echo">>,
    Test = self(),
    Io = #{links => fun(_Pool) -> {ok, [#{node_id => ?STATION, connected => true}]} end,
           put_record => fun(_Pool, Record) -> Test ! {put, Record}, ok end,
           %% The pool holding Key signs, as macula_client:sign_node_record/3 does.
           sign_node_record => fun(_Pool, Unsigned, _Opts) -> {ok, macula_record:sign(Unsigned, Key)} end},
    put_read(macula_direct_dial:publish_advertisement(self(), ?REALM, Procedure, Key, Opts#{dial_io => Io}), NodeId).

put_read(ok, NodeId) ->
    receive {put, Record} -> {ok, macula_record:read_procedure_advertisement(Record), NodeId}
    after 1_000 -> error(nothing_put)
    end;
put_read({error, _} = Refused, _NodeId) ->
    Refused.
