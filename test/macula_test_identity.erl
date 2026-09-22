%% Identities for tests: a newly generated pq_pure identity key, or its node_id (D5), for a test that needs a peer, a
%% station, a subscriber or a provider that is some other node.
-module(macula_test_identity).

-export([key/0, node_id/0]).

%% @doc A new pq_pure identity key, unground: a test that needs the puzzle solved generates its key with
%% macula_node_keys:generate/3.
-spec key() -> macula_node_keys:node_key().
key() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    Key.

%% @doc The node_id of a new identity key from key/0.
-spec node_id() -> macula_node_keys:node_id().
node_id() ->
    {ok, NodeId} = macula_node_keys:node_id(key()),
    NodeId.
