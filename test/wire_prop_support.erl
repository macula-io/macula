%% What the wire property modules share: a started macula with a crypto profile, and one key per purpose for the run.
%% A key is made once, not once per case: generating an ML-DSA-87 key costs more than everything a record or a token
%% property checks with it, and property runs are hundreds of cases.
-module(wire_prop_support).

-export([profile/0, key/1, node_id/1, started/0]).

%% The application, once: a property module may be the first thing in the node to need a profile or a NIF.
started() ->
    {ok, _Started} = application:ensure_all_started(macula),
    ok.

profile() ->
    started(),
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.

%% The key for a purpose, made on first ask and kept for the run.
key(Purpose) ->
    case persistent_term:get({?MODULE, Purpose}, undefined) of
        undefined -> made(Purpose);
        Key -> Key
    end.

made(Purpose) ->
    {ok, Key} = macula_node_keys:generate(Purpose, profile()),
    persistent_term:put({?MODULE, Purpose}, Key),
    Key.

%% The node_id an identity key's records and tokens name.
node_id(Purpose) ->
    {ok, NodeId} = macula_node_keys:node_id(key(Purpose)),
    NodeId.
