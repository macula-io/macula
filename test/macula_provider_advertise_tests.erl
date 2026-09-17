%% EUnit tests for the facade's provider advertise: `macula:advertise/5'
%% resolves the pool's own D25 provider authorization — the
%% realm-signed org directory and the org-signed procedure delegation
%% naming the pool's node id — from the DHT, signs the advertisement,
%% verifies it against the realm key the pool pins, and hands its wire
%% form to the pool fan-out. Every missing piece of the chain fails
%% fast with `{error, {provider_authorization, _}}', and nothing is
%% sent.
%%
%% The facade's collaborators are stubbed with meck at the
%% `macula_client' boundary (the facade's own local calls bypass a
%% meck on `macula'); the records are real: signed with realm, org and
%% identity keys in the node's profile.
-module(macula_provider_advertise_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<7:256>>).
-define(ORG, <<"acme">>).
-define(PROC, <<"acme/count_v1">>).

provider_advertise_test_() ->
    {timeout, 120, {setup, fun start/0, fun stop/1, fun cases/1}}.

cases(Keys) ->
    [{case_name(Case), fun() -> Case(Keys) end}
     || Case <- [fun a_complete_chain_advertises_the_signed_wire_form/1,
                 fun a_procedure_without_an_org_namespace_is_refused/1,
                 fun a_missing_org_directory_is_refused/1,
                 fun a_missing_delegation_is_refused/1,
                 fun an_unpinned_realm_key_is_refused/1]].

%%------------------------------------------------------------------
%% Happy path
%%------------------------------------------------------------------

a_complete_chain_advertises_the_signed_wire_form(
  #{node := NodeId, key := Key, realm_key := RealmPub} = Keys) ->
    {OrgDir, Deleg} = published_chain(Keys),
    stub_find(Keys, OrgDir, Deleg),
    stub_status(NodeId),
    stub_realm_key({ok, RealmPub}),
    %% The facade's signer: sign with the pool's own identity key, as
    %% the pool would.
    ok = meck:expect(macula_client, sign_node_record,
                     fun(_Pool, Unsigned) ->
                         {ok, macula_record:sign(Unsigned, Key)}
                     end),
    Handler = fun(_Payload) -> {ok, counted} end,
    ?assertEqual(ok, macula:advertise(self(), ?REALM, ?PROC, Handler, #{})),
    Ad = advertise_sent(?REALM, ?PROC),
    %% The sent advertisement decodes, names the pool's node, and its
    %% authorization verifies against the pinned realm key.
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Decoded} = macula_record:verify(Ad, Profile),
    Read = macula_record:read_procedure_advertisement(Decoded),
    ?assertEqual(NodeId, maps:get(advertiser_node, Read)),
    ?assertEqual(NodeId, maps:get(serving_station, Read)),
    ?assertEqual(ok,
                 macula_record:verify_authorization(
                   Decoded, #{profile => Profile, realm_key => RealmPub},
                   erlang:system_time(millisecond))).

%%------------------------------------------------------------------
%% Refusals — nothing is sent under any of them
%%------------------------------------------------------------------

a_procedure_without_an_org_namespace_is_refused(_Keys) ->
    Before = advertise_count(),
    ?assertEqual({error, {provider_authorization, no_org_namespace}},
                 macula:advertise(self(), ?REALM, <<"_dht.nothing">>,
                                  fun(_P) -> {ok, counted} end, #{})),
    ?assertEqual(Before, advertise_count()),
    ok.

a_missing_org_directory_is_refused(Keys) ->
    stub_find_missing(Keys, org_directory),
    stub_status(maps:get(node, Keys)),
    stub_realm_key({ok, maps:get(realm_key, Keys)}),
    ok = meck:expect(macula_client, sign_node_record,
                     fun(_Pool, Unsigned) ->
                         {ok, macula_record:sign(Unsigned, maps:get(key, Keys))}
                     end),
    Before = advertise_count(),
    ?assertEqual({error, {provider_authorization, {org_directory, not_found}}},
                 macula:advertise(self(), ?REALM, ?PROC,
                                  fun(_P) -> {ok, counted} end, #{})),
    ?assertEqual(Before, advertise_count()),
    ok.

a_missing_delegation_is_refused(Keys) ->
    stub_find_missing(Keys, procedure_delegation),
    stub_status(maps:get(node, Keys)),
    stub_realm_key({ok, maps:get(realm_key, Keys)}),
    ok = meck:expect(macula_client, sign_node_record,
                     fun(_Pool, Unsigned) ->
                         {ok, macula_record:sign(Unsigned, maps:get(key, Keys))}
                     end),
    Before = advertise_count(),
    ?assertEqual({error, {provider_authorization,
                          {procedure_delegation, not_found}}},
                 macula:advertise(self(), ?REALM, ?PROC,
                                  fun(_P) -> {ok, counted} end, #{})),
    ?assertEqual(Before, advertise_count()),
    ok.

an_unpinned_realm_key_is_refused(Keys) ->
    {OrgDir, Deleg} = published_chain(Keys),
    stub_find(Keys, OrgDir, Deleg),
    stub_status(maps:get(node, Keys)),
    stub_realm_key(none),
    ok = meck:expect(macula_client, sign_node_record,
                     fun(_Pool, Unsigned) ->
                         {ok, macula_record:sign(Unsigned, maps:get(key, Keys))}
                     end),
    Before = advertise_count(),
    ?assertEqual({error, {provider_authorization, no_realm_key}},
                 macula:advertise(self(), ?REALM, ?PROC,
                                  fun(_P) -> {ok, counted} end, #{})),
    ?assertEqual(Before, advertise_count()),
    ok.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

start() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    {ok, NodeId} = macula_node_keys:node_id(Key),
    {ok, RealmKey} = macula_node_keys:generate(realm, Profile),
    {ok, OrgKey} = macula_node_keys:generate(org, Profile),
    OrgKeyId = macula_node_keys:key_id(OrgKey),
    ok = meck:new(macula_client, [passthrough, non_strict]),
    %% The fan-out itself: accept the call and record it (the links'
    %% behaviour is covered by macula_station_link_advertise_tests).
    ok = meck:expect(macula_client, advertise,
                     fun(_Pool, _Realm, _Proc, _Handler, _Policy, _EncodedAd) ->
                         ok
                     end),
    #{profile => Profile, key => Key, node => NodeId,
      realm_key => macula_node_keys:public_key(RealmKey),
      realm_kp => RealmKey, org_key => OrgKey,
      org_key_id => OrgKeyId,
      org_dir_key => macula_record:org_directory_key(?REALM, ?ORG),
      deleg_key => macula_record:procedure_delegation_key(OrgKeyId, NodeId)}.

stop(_Keys) ->
    meck:unload(macula_client).

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).

%% The chain the org published: a realm-signed org directory and an
%% org-signed delegation naming the pool's node.
published_chain(#{node := NodeId, realm_kp := RealmKp, org_key := OrgKey,
                  org_key_id := OrgKeyId}) ->
    OrgDir = macula_record:sign(
               macula_record:org_directory(?REALM, ?ORG, OrgKeyId), RealmKp),
    Deleg = macula_record:sign(
              macula_record:procedure_delegation(OrgKeyId, NodeId), OrgKey),
    {OrgDir, Deleg}.

%% The DHT lookups underneath the facade's find_record: the pool RPC
%% answers the record's wire form, or a not_found for a missing piece.
stub_find(Keys, OrgDir, Deleg) ->
    ok = meck:expect(macula_client, call_linked_station,
                     fun(_Pool, _Realm, _Proc, #{key := Key}, _TimeoutMs) ->
                         find_reply(Keys, Key, OrgDir, Deleg)
                     end),
    ok.

find_reply(#{org_dir_key := OrgDirKey}, OrgDirKey, OrgDir, _Deleg) ->
    {ok, macula_record:encode(OrgDir)};
find_reply(#{deleg_key := DelegKey}, DelegKey, _OrgDir, Deleg) ->
    {ok, macula_record:encode(Deleg)};
find_reply(Keys, Key, _OrgDir, _Deleg) ->
    io:format("FINDREPLY-FALLBACK key=~p mapkeys=~p~n",
              [Key, maps:keys(Keys)]),
    {ok, not_found}.

%% The find stub with one chain record missing: the other still
%% resolves normally, the missing one answers not_found.
stub_find_missing(Keys, Which) ->
    {OrgDir, Deleg} = published_chain(Keys),
    OrgDirKey = maps:get(org_dir_key, Keys),
    DelegKey  = maps:get(deleg_key, Keys),
    ok = meck:expect(macula_client, call_linked_station,
                     fun(_Pool, _Realm, _Proc, #{key := Key}, _TimeoutMs) ->
                         case {Which, Key} of
                             {org_directory, OrgDirKey} ->
                                 {ok, not_found};
                             {procedure_delegation, DelegKey} ->
                                 {ok, not_found};
                             _Other ->
                                 find_reply(Keys, Key, OrgDir, Deleg)
                         end
                     end),
    ok.

stub_status(NodeId) ->
    ok = meck:expect(macula_client, status, fun(_Pool) ->
        {ok, #{self_node_id => NodeId}}
    end).

stub_realm_key(Reply) ->
    ok = meck:expect(macula_client, realm_key,
                     fun(_Pool, _Realm) -> Reply end).

%% The encoded advertisement the facade handed to the pool fan-out, or
%% not_sent.
advertise_sent(_Realm, _Proc) ->
    case [EncodedAd || {_, {macula_client, advertise,
                            [_Pool, _Realm0, _Proc0, _Handler, _Policy,
                             EncodedAd]}, _}
                       <- meck:history(macula_client)] of
        [Ad | _] -> Ad;
        []       -> not_sent
    end.

advertise_count() ->
    length([1 || {_, {macula_client, advertise, _}, _}
                     <- meck:history(macula_client)]).

%% Placeholder for the find stub's unexpected-key branch; never read.
-define(PLACEHOLDER, placeholder).
