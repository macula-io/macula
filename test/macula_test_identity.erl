%% Identities for tests: a newly generated pq_pure identity key, or its node_id (D5), for a test that needs a peer, a
%% station, a subscriber or a provider that is some other node; and a TLS key's seed, for a listener's certificate
%% (D12).
-module(macula_test_identity).

-export([key/0, node_id/0, tls_seed/0]).

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

%% @doc The 32-byte seed of a new TLS key, for `macula_quic:generate_self_signed_cert/2'.
-spec tls_seed() -> <<_:256>>.
tls_seed() ->
    {ok, #{components := [#{private := Seed}]}} = macula_node_keys:generate(tls, pq_pure),
    Seed.
