%% EUnit tests: the distribution pool reads its tunnel RPC payload fields through the D26 facade accessors. A text key
%% comes before an atom key of the same name, and a binary key handed over in process is read too.
-module(macula_dist_pool_field_tests).

-include_lib("eunit/include/eunit.hrl").

tunnel_reply_prefers_the_text_key_test() ->
    Reply = #{{text, <<"tunnel_id">>} => {text, <<"from-text">>}, tunnel_id => <<"from-atom">>},
    ?assertEqual({tunnel, <<"from-text">>}, macula_dist_pool:tunnel_reply(Reply)).

tunnel_reply_reads_a_binary_key_handed_over_in_process_test() ->
    ?assertEqual({tunnel, <<"t1">>}, macula_dist_pool:tunnel_reply(#{<<"tunnel_id">> => <<"t1">>})).

tunnel_request_from_node_prefers_the_text_key_test() ->
    Args = #{{text, <<"from_node">>} => {text, <<"text@host">>}, from_node => <<"atom@host">>},
    ?assertEqual(<<"text@host">>, macula_dist_pool:tunnel_request_from_node(Args)).

tunnel_request_from_node_reads_a_binary_key_handed_over_in_process_test() ->
    ?assertEqual(<<"bin@host">>, macula_dist_pool:tunnel_request_from_node(#{<<"from_node">> => <<"bin@host">>})).
