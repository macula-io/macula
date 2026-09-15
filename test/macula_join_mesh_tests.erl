%% EUnit tests for the relays of macula:join_mesh/1. Every relay names the node_id it expects, as every dial does (D16):
%% a relay without one refuses the join by name and starts no pool, and the pool the join starts dials each relay with
%% that relay's own pin.
-module(macula_join_mesh_tests).

-include_lib("eunit/include/eunit.hrl").

a_relay_without_its_node_id_refuses_the_join_and_starts_no_pool_test_() ->
    {timeout, 60, fun() ->
        {ok, _} = application:ensure_all_started(macula),
        Before = issuers(),
        Unpinned = [[<<"https://relay-a.example:4433">>],
                    [#{host => <<"relay-a.example">>, port => 4433}],
                    [#{host => <<"relay-a.example">>, port => 4433, expected_node_id => <<1:256>>},
                     #{host => <<"relay-b.example">>, port => 4433}]],
        Refused = [macula:join_mesh(#{relays => Relays}) || Relays <- Unpinned],
        ?assertEqual([{error, {relays, expected_node_id_required}} || _ <- Unpinned], Refused),
        ?assertEqual(Before, issuers())
    end}.

%% The pool the join starts gets the relays as its seeds, each with its own pin, and the node identity key if given.
the_pool_dials_each_relay_with_its_own_pin_test() ->
    Relays = [#{host => <<"relay-a.example">>, port => 4433, expected_node_id => <<1:256>>},
              #{host => <<"relay-b.example">>, port => 4433, expected_node_id => <<2:256>>}],
    Loader = {?MODULE, no_key, []},
    ?assertEqual({ok, Relays, #{}}, macula:join_pool_args(#{relays => Relays})),
    ?assertEqual({ok, Relays, #{node_identity => Loader}},
                 macula:join_pool_args(#{relays => Relays, node_identity => Loader, realm => <<"ignored">>})).

%% The statement issuers running under macula_statement_issuer_sup, one for each pool.
issuers() ->
    lists:sort([Pid || {_Id, Pid, _Type, _Modules} <- supervisor:which_children(macula_statement_issuer_sup),
                       is_pid(Pid)]).
