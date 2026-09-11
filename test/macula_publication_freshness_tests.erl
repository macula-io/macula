%% EUnit tests for sized freshness refusals (DESIGN_PQ_DHT_SLOTS_AND_BUDGET.md, 3.1 and 3.4). A publication refused for
%% its time says how far past the moment its rule starts refusing it is, in milliseconds, and a freshness refusal is
%% charged only when it is more than 10 minutes past that moment, on either side.
-module(macula_publication_freshness_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NOW, 1789000000000).
-define(MINUTE, 60000).

a_freshness_refusal_is_charged_only_beyond_10_minutes_test_() ->
    [?_assertNot(macula_frame:charged_refusal({expired, 10 * ?MINUTE})),
     ?_assert(macula_frame:charged_refusal({expired, 10 * ?MINUTE + 1})),
     ?_assertNot(macula_frame:charged_refusal({not_yet_valid, 10 * ?MINUTE})),
     ?_assert(macula_frame:charged_refusal({not_yet_valid, 10 * ?MINUTE + 1}))].

%% The design's example: a publication at expires_at plus 11 minutes costs 1, and at plus 1 minute does not.
a_publication_11_minutes_past_its_expiry_is_charged_and_1_minute_past_is_not_test() ->
    Frame = publication(?NOW, #{ttl_ms => ?MINUTE}),
    {ok, #{expires_at := ExpiresAt}} = macula_frame:verify_publication(Frame, pq_pure, ?NOW),
    {error, Late} = macula_frame:verify_publication(Frame, pq_pure, ExpiresAt + 11 * ?MINUTE),
    {error, Recent} = macula_frame:verify_publication(Frame, pq_pure, ExpiresAt + ?MINUTE),
    ?assertEqual({{expired, 11 * ?MINUTE}, true}, {Late, macula_frame:charged_refusal(Late)}),
    ?assertEqual({{expired, ?MINUTE}, false}, {Recent, macula_frame:charged_refusal(Recent)}).

a_publication_far_ahead_of_the_tolerance_is_charged_test() ->
    Far = publication(?NOW + 16 * ?MINUTE, #{}),
    Near = publication(?NOW + 6 * ?MINUTE, #{}),
    {error, FarAhead} = macula_frame:verify_publication(Far, pq_pure, ?NOW),
    {error, NearAhead} = macula_frame:verify_publication(Near, pq_pure, ?NOW),
    ?assertEqual({{not_yet_valid, 11 * ?MINUTE}, true}, {FarAhead, macula_frame:charged_refusal(FarAhead)}),
    ?assertEqual({{not_yet_valid, ?MINUTE}, false}, {NearAhead, macula_frame:charged_refusal(NearAhead)}).

%% A PUBLISH signed by a fresh publisher, as a peer receives it.
publication(PublishedAt, Extra) ->
    {ok, Publisher} = macula_node_keys:generate(identity, pq_pure),
    Spec = maps:merge(#{realm => <<1:256>>, topic => <<"news">>, seq => 1, published_at => PublishedAt,
                        payload => <<"p">>}, Extra),
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(macula_frame:publish(Spec, Publisher))),
    Decoded.
