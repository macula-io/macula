%%% @doc Tests for macula_key_leak_sample: a depth-limited print of a whole key map and a printed decoded RSA private key
%%% each show a form the sample finds, and the public halves alone show none.
%%%
%%% NOT named macula_key_leak_sample_tests, which is what it tests. eunit pairs a
%%% module with a `<module>_tests' sibling, and both this and macula_key_leak_sample
%%% live under test/, so the pair made `rebar3 eunit' run these three tests TWICE:
%%% once by walking the helper and picking up its sibling, once by walking the
%%% sibling directly. The suite's headline count was 3 higher than the number of
%%% distinct tests. Breaking the name pairing is what stops that.
-module(macula_key_leak_tests).

-include_lib("eunit/include/eunit.hrl").

%% A pq_hybrid key carries an RSA-4096 half, which takes up to about a second to generate.
-define(EU_TIMEOUT, 120).

%% A log handler that prints a whole key map at a limited depth shows only the private half's first bytes. The sample
%% finds a form there, where a tail slice alone never would.
a_depth_limited_print_of_a_key_map_is_found_test() ->
    Key = key(pq_pure),
    ?assertNotEqual([], macula_key_leak_sample:found([logged(Key, 60)], Key)).

%% A decoded RSAPrivateKey prints its private exponent as a decimal integer, and the sample finds it.
a_printed_decoded_rsa_private_key_is_found_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        #{components := [_, #{algorithm := rsa_pss, private := Der}]} = Key = key(pq_hybrid),
        Printed = iolist_to_binary(io_lib:format("~p", [public_key:der_decode('RSAPrivateKey', Der)])),
        ?assertNotEqual([], macula_key_leak_sample:found([Printed], Key))
    end}.

%% The public halves alone, raw or printed whole, show no form in either profile, so public bytes set nothing off.
the_public_halves_show_no_form_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        [begin
             #{components := Components} = Key = key(Profile),
             Publics = [Public || #{public := Public} <- Components],
             ?assertEqual({Profile, []},
                          {Profile, macula_key_leak_sample:found([term_to_binary(Publics), logged(Publics, unlimited)],
                                                                 Key)})
         end || Profile <- [pq_pure, pq_hybrid]]
    end}.

key(Profile) ->
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    Key.

%% `Term' as a log handler formats it on one line, printed at most `Depth' deep.
logged(Term, Depth) ->
    Event = #{level => warning, msg => {"~p", [Term]}, meta => #{time => logger:timestamp()}},
    unicode:characters_to_binary(logger_formatter:format(Event, #{single_line => true, legacy_header => false,
                                                                  depth => Depth, chars_limit => unlimited})).
