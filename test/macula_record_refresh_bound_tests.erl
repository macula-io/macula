%% EUnit tests for refreshing a record under a bound on its expiry: macula_record:refresh/3.
%%
%% A caller bounds a record's expiry to cap it against something else, a delegation's own expiry being the plain
%% case. The bound is an ABSOLUTE time, so the record must end at it, whatever else the signing does and however
%% long the record has been sitting around unsigned.
%%
%% ⚠ The defect this pins, found by Mars on 2026-09-22 by running one module on its own: the bound used to be
%% written into the record as `expires_at' and then re-anchored by refresh/2, which reads the clock a SECOND time
%% and keeps the record's LIFETIME, `expires_at - created_at', against the original `created_at'. The record then
%% ended at the bound plus its own age at signing time. On an idle box building and signing back to back that was
%% one millisecond; for a record built a minute before it is signed it is a minute past the bound, and nothing
%% bounds the gap. One clock read now decides both ends.
-module(macula_record_refresh_bound_tests).

-include_lib("eunit/include/eunit.hrl").

%% A record built a while ago and refreshed under a bound ends AT the bound, not at the bound plus its age. The
%% record is aged by hand rather than by sleeping: what the defect turned into milliseconds is the record's age,
%% so an hour-old record makes it an hour.
a_bound_is_not_pushed_out_by_the_records_age_test() ->
    Key = key(),
    Aged = aged(advertisement(), 3_600_000),
    NotAfter = erlang:system_time(millisecond) + 60_000,
    {ok, Signed} = macula_record:refresh(Aged, Key, NotAfter),
    ?assertEqual(NotAfter, macula_record:expires_at(Signed)).

%% The same, for a record built now: the case the client test pins, at the millisecond scale.
a_bound_within_the_lifetime_ends_the_record_at_the_bound_test() ->
    Key = key(),
    Built = advertisement(),
    NotAfter = erlang:system_time(millisecond) + lifetime(Built) div 2,
    {ok, Signed} = macula_record:refresh(Built, Key, NotAfter),
    ?assertEqual(NotAfter, macula_record:expires_at(Signed)).

%% A bound after the lifetime leaves the lifetime alone: the record is stamped now and keeps the lifetime it was
%% built with, exactly as an unbounded refresh does.
a_bound_after_the_lifetime_keeps_the_lifetime_test() ->
    Key = key(),
    Built = advertisement(),
    Lifetime = lifetime(Built),
    Before = erlang:system_time(millisecond),
    {ok, Signed} = macula_record:refresh(Built, Key, Before + 10 * Lifetime),
    ?assert(macula_record:created_at(Signed) >= Before),
    ?assertEqual(Lifetime, lifetime(Signed)).

%% A bound already passed signs nothing: the refusal is by name, and it is judged on the clock read that stamps the
%% record, so a bound that passes while the call runs cannot slip through.
a_bound_already_passed_is_refused_test() ->
    Key = key(),
    Record = advertisement(),
    %% ⚠ Read the clock into a variable first. Written as an argument beside `advertisement()' this was fragile:
    %% argument evaluation order is not specified, so under load the bound could be read before the work and end
    %% up AFTER the clock refresh then reads, which is not the case under test.
    Now = erlang:system_time(millisecond),
    ?assertEqual({error, not_after_passed}, macula_record:refresh(Record, Key, Now - 1_000)),
    ?assertEqual({error, not_after_passed}, macula_record:refresh(Record, Key, Now)).

%% A refreshed record still verifies, bound or not: the bound changes what it says, not whether it is signed.
a_bounded_refresh_still_verifies_test() ->
    Key = key(),
    NotAfter = erlang:system_time(millisecond) + 60_000,
    {ok, Signed} = macula_record:refresh(advertisement(), Key, NotAfter),
    ?assertMatch({ok, #{}}, macula_record:verify(macula_record:encode(Signed), pq_pure)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% The record as it would be if it had been built Ms milliseconds ago, with the same lifetime.
aged(#{created_at := Created, expires_at := Expires} = Record, Ms) ->
    Record#{created_at => Created - Ms, expires_at => Expires - Ms}.

advertisement() ->
    macula_record:procedure_advertisement(node_id(), <<1:256>>, <<"acme/echo_v1">>, <<7:256>>).

lifetime(Record) ->
    macula_record:expires_at(Record) - macula_record:created_at(Record).

key() ->
    persistent_term:get({?MODULE, key}, undefined) =/= undefined orelse
        persistent_term:put({?MODULE, key}, macula_test_identity:key()),
    persistent_term:get({?MODULE, key}).

node_id() ->
    {ok, NodeId} = macula_node_keys:node_id(key()),
    NodeId.
