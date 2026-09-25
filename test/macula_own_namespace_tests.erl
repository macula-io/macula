%% EUnit tests for a node's own namespace (D25 item 6, revised 2026-09-24): a procedure named
%% `~<node_id as 64 lowercase hex>/<name>' is authorized by the advertisement's own signature. It carries no
%% authorization, and a verifier accepts it only when the advertisement's signer is the node the namespace names.
%% An org-less node (an agent, a CLI) serves this way, with no org and no human per node.
-module(macula_own_namespace_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<7:256>>).

own_namespace_test_() ->
    {setup, fun keys/0, fun(_) -> ok end,
     fun(Keys) ->
         [{Name, fun() -> Case(Keys) end}
          || {Name, Case} <- [{"a node's own namespace verifies with no authorization and no realm key",
                               fun an_own_namespace_advertisement_verifies_without_a_realm_key/1},
                              {"another node's namespace is refused",
                               fun another_nodes_namespace_is_refused/1},
                              {"an authorization attached to an own-namespace advertisement is refused",
                               fun an_own_namespace_advertisement_carries_no_authorization/1},
                              {"a namespace that is not exactly 64 lowercase hex is malformed",
                               fun a_namespace_that_is_not_a_node_id_is_malformed/1},
                              {"an org procedure is not an own namespace",
                               fun an_org_procedure_is_not_an_own_namespace/1}]]
     end}.

an_own_namespace_advertisement_verifies_without_a_realm_key(#{node := Node} = Keys) ->
    Ad = signed(Keys, own(Node, <<"ring">>), #{}),
    ?assertEqual(ok, macula_record:own_namespace(Ad)),
    ?assertEqual(ok, macula_record:verify_authorization(Ad, trust(), now_ms())).

another_nodes_namespace_is_refused(Keys) ->
    Ad = signed(Keys, own(<<1:256>>, <<"ring">>), #{}),
    ?assertEqual({error, not_own_namespace}, macula_record:own_namespace(Ad)),
    ?assertEqual({error, not_own_namespace}, macula_record:verify_authorization(Ad, trust(), now_ms())).

an_own_namespace_advertisement_carries_no_authorization(#{node := Node} = Keys) ->
    Ad = signed(Keys, own(Node, <<"ring">>),
                #{authorization => #{org_directory => <<"d">>, procedure_delegation => <<"p">>}}),
    ?assertEqual({error, authorization_not_allowed}, macula_record:own_namespace(Ad)),
    ?assertEqual({error, authorization_not_allowed},
                 macula_record:verify_authorization(Ad, trust(), now_ms())).

a_namespace_that_is_not_a_node_id_is_malformed(#{node := Node} = Keys) ->
    Upper = binary:encode_hex(Node, uppercase),
    Short = binary:part(binary:encode_hex(Node, lowercase), 0, 62),
    [begin
         Ad = signed(Keys, <<"~", Bad/binary, "/ring">>, #{}),
         ?assertEqual({error, malformed}, macula_record:own_namespace(Ad)),
         ?assertEqual({error, malformed}, macula_record:verify_authorization(Ad, trust(), now_ms()))
     end || Bad <- [Upper, Short, <<>>, <<"zz", (binary:part(Upper, 0, 62))/binary>>]].

an_org_procedure_is_not_an_own_namespace(Keys) ->
    [?assertEqual({error, not_own_namespace}, macula_record:own_namespace(signed(Keys, Procedure, #{})))
     || Procedure <- [<<"acme/count_v1">>, <<"plain">>, <<"_/internal">>]].

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

keys() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    {ok, Node} = macula_node_keys:node_id(Key),
    #{key => Key, node => Node}.

own(Node, Name) ->
    <<"~", (binary:encode_hex(Node, lowercase))/binary, "/", Name/binary>>.

signed(#{key := Key, node := Node}, Procedure, Opts) ->
    macula_record:refresh(macula_record:procedure_advertisement(Node, ?REALM, Procedure, <<9:256>>, Opts), Key).

trust() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    #{profile => Profile}.

now_ms() ->
    erlang:system_time(millisecond).
