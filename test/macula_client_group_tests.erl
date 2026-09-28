%%%-------------------------------------------------------------------
%%% @doc Sealed groups through a live, link-less pool (plans/DESIGN_E2E_SEALED_PUBSUB.md §6, §7): subscribing and
%%% publishing with `group => Prefix' join the group, a sealed event is opened for a group's subscriber or reported
%%% unopened, a node holding `required' refuses clear events under the prefix, and a publish under a held prefix names
%%% its group. The pool's keyring pulls from the real distributor (macula_group_keys) through a wire that answers as
%%% macula:call/6 does.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_client_group_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<7:256>>).
-define(PREFIX, <<"io.macula/acme/chat">>).
-define(TOPIC, <<"io.macula/acme/chat/room/said_v1">>).
-define(ELSEWHERE, <<"io.macula/other/news/said_v1">>).
-define(PUB, <<4:256>>).
-define(TOKEN, <<"org-grant">>).

group_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun subscribing_under_a_group_pulls_its_key_with_the_org_grant/1,
      fun a_sealed_event_opens_for_the_groups_subscriber/1,
      fun a_clear_event_under_a_required_group_is_refused/1,
      fun a_clear_event_elsewhere_is_delivered_as_clear/1,
      fun a_sealed_event_without_a_group_is_reported_no_group/1,
      fun a_sealed_event_that_does_not_open_is_reported_tag_invalid/1,
      fun a_sealed_event_under_an_unknown_epoch_is_reported/1,
      fun a_subscribe_the_distributor_refuses_fails_closed/1,
      fun a_publish_under_a_group_pulls_its_key_first/1,
      fun a_clear_publish_under_a_held_prefix_names_the_group/1,
      fun a_topic_outside_its_group_is_an_invalid_option/1,
      fun a_callback_subscription_logs_an_unopened_event_and_keeps_no_mail/1]}.

setup() ->
    {ok, _} = application:ensure_all_started(macula),
    Log = ets:new(log, [public, bag]),
    Script = ets:new(script, [public, ordered_set]),
    {ok, Distributor} = macula_group_keys:start_link(#{org => <<"acme">>, policy => required,
                                                       membership => fun(_Caller) -> ok end}),
    Handler = macula_group_keys:handler(Distributor),
    Call = fun(Realm, Procedure, Payload, Opts) ->
               ets:insert(Log, {call, erlang:unique_integer([monotonic]), {Realm, Procedure, Payload, Opts}}),
               answer(Script, Handler, Payload)
           end,
    {ok, Pool} = macula_client:connect([], #{group_keyring => #{call => Call}}),
    #{pool => Pool, distributor => Distributor, handler => Handler, log => Log, script => Script}.

cleanup(#{pool := Pool, distributor := Distributor, log := Log, script := Script}) ->
    ok = macula_client:close(Pool),
    unlink(Distributor),
    exit(Distributor, shutdown),
    ets:delete(Log),
    ets:delete(Script).

answer(Script, Handler, Payload) ->
    scripted(ets:first(Script), Script, Handler, Payload).

scripted('$end_of_table', _Script, Handler, Payload) ->
    on_the_wire(Handler(Payload#{caller => <<5:256>>}));
scripted(Key, Script, _Handler, _Payload) ->
    [{_, Answer}] = ets:lookup(Script, Key),
    ets:delete(Script, Key),
    Answer.

on_the_wire({error, Reason}) -> {error, atom_to_binary(Reason)};
on_the_wire(Reply) -> {ok, Reply, #{sealed => 1, provider => <<9:256>>}}.

calls(#{log := Log}) -> [C || {_, C} <- lists:keysort(1, [{I, C} || {call, I, C} <- ets:tab2list(Log)])].

subscribe(#{pool := Pool}, Topic, Opts) ->
    macula_client:subscribe(Pool, ?REALM, Topic, self(), Opts).

group_subscribe(W) ->
    {ok, Ref} = subscribe(W, ?TOPIC, #{group => ?PREFIX, ucan_token => ?TOKEN}),
    Ref.

%% The epoch the distributor hands out now, as a member pulling it would get it.
current_epoch(#{handler := Handler}) ->
    #{epochs := [Epoch | _]} = Handler(#{{text, <<"prefix">>} => {text, ?PREFIX},
                                         {text, <<"epoch">>} => {text, <<"current">>}, caller => <<6:256>>}),
    Epoch.

fields(Topic, Seq) ->
    #{realm => ?REALM, topic => Topic, publisher => ?PUB, seq => Seq, published_at => erlang:system_time(millisecond)}.

%% A verified publication as a link hands it to the pool: sealed events carry their seal in the meta, no payload.
inject_sealed(#{pool := Pool}, Topic, Epoch, Seq, Payload) ->
    Fields = fields(Topic, Seq),
    {ok, Sealed} = macula_group_event:seal(Epoch, Fields, Payload),
    inject(Pool, Topic, undefined, (maps:remove(topic, Fields))#{sealed => Sealed}),
    Sealed.

inject_clear(#{pool := Pool}, Topic, Seq, Payload) ->
    inject(Pool, Topic, Payload, maps:remove(topic, fields(Topic, Seq))).

inject(Pool, Topic, Payload, Meta) ->
    Pool ! {macula_event, make_ref(), Topic, Payload,
            Meta#{delivered_via => direct, publisher_verified => true,
                  publication_hash => crypto:strong_rand_bytes(48),
                  expires_at => erlang:system_time(millisecond) + 60_000}},
    ok.

next_message(Ref) ->
    receive
        {macula_event, Ref, _, _, _} = Event -> Event;
        {macula_event_unopened, Ref, _, _} = Unopened -> Unopened
    after 1_000 -> none
    end.

%%--------------------------------------------------------------------

subscribing_under_a_group_pulls_its_key_with_the_org_grant(W) ->
    ?_test(begin
        _Ref = group_subscribe(W),
        [{?REALM, <<"acme/group_keys_v1">>, _Payload, Opts}] = calls(W),
        ?assertMatch(#{confidential := required, ucan_token := ?TOKEN}, Opts)
    end).

a_sealed_event_opens_for_the_groups_subscriber(W) ->
    ?_test(begin
        Ref = group_subscribe(W),
        #{id := Id} = Epoch = current_epoch(W),
        _ = inject_sealed(W, ?TOPIC, Epoch, 1, <<"hello">>),
        {macula_event, Ref, ?TOPIC, Payload, Meta} = next_message(Ref),
        ?assertEqual(<<"hello">>, Payload),
        ?assertMatch(#{sealed := 1, seal_key_id := Id, publisher := ?PUB, seq := 1}, Meta)
    end).

a_clear_event_under_a_required_group_is_refused(W) ->
    ?_test(begin
        Ref = group_subscribe(W),
        {ok, Plain} = subscribe(W, ?TOPIC, #{}),
        ok = inject_clear(W, ?TOPIC, 1, <<"clear">>),
        ?assertEqual(none, next_message(Ref)),
        ?assertEqual(none, next_message(Plain))
    end).

a_clear_event_elsewhere_is_delivered_as_clear(W) ->
    ?_test(begin
        _ = group_subscribe(W),
        {ok, Ref} = subscribe(W, ?ELSEWHERE, #{}),
        ok = inject_clear(W, ?ELSEWHERE, 1, <<"news">>),
        ?assertMatch({macula_event, Ref, ?ELSEWHERE, <<"news">>, #{sealed := 0}}, next_message(Ref))
    end).

a_sealed_event_without_a_group_is_reported_no_group(W) ->
    ?_test(begin
        {ok, Ref} = subscribe(W, ?ELSEWHERE, #{}),
        #{key_id := Id} = inject_sealed(W, ?ELSEWHERE, current_epoch(W), 1, <<"x">>),
        ?assertEqual({macula_event_unopened, Ref, ?ELSEWHERE, #{publisher => ?PUB, seal_key_id => Id, reason => no_group}},
                     next_message(Ref))
    end).

a_sealed_event_that_does_not_open_is_reported_tag_invalid(#{pool := Pool} = W) ->
    ?_test(begin
        Ref = group_subscribe(W),
        #{id := Id} = Epoch = current_epoch(W),
        Fields = fields(?TOPIC, 1),
        {ok, #{ct := Ct} = Sealed} = macula_group_event:seal(Epoch, Fields, <<"x">>),
        <<First, Rest/binary>> = Ct,
        inject(Pool, ?TOPIC, undefined, (maps:remove(topic, Fields))#{sealed => Sealed#{ct := <<(First bxor 1), Rest/binary>>}}),
        ?assertEqual({macula_event_unopened, Ref, ?TOPIC, #{publisher => ?PUB, seal_key_id => Id, reason => tag_invalid}},
                     next_message(Ref))
    end).

a_sealed_event_under_an_unknown_epoch_is_reported(W) ->
    ?_test(begin
        Ref = group_subscribe(W),
        #{id := Id} = Stranger = macula_group_epoch:new(erlang:system_time(millisecond), 15 * 60000),
        _ = inject_sealed(W, ?TOPIC, Stranger, 1, <<"x">>),
        ?assertEqual({macula_event_unopened, Ref, ?TOPIC, #{publisher => ?PUB, seal_key_id => Id, reason => unknown_epoch}},
                     next_message(Ref))
    end).

a_subscribe_the_distributor_refuses_fails_closed(#{script := Script} = W) ->
    ?_test(begin
        ets:insert(Script, {1, {error, <<"not_a_member">>}}),
        ?assertEqual({error, {group, not_a_member}}, subscribe(W, ?TOPIC, #{group => ?PREFIX}))
    end).

a_publish_under_a_group_pulls_its_key_first(#{pool := Pool} = W) ->
    ?_test(begin
        %% A link-less pool has no station to send to: the publish gets that far, sealed.
        ?assertEqual({error, {transient, no_healthy_station}},
                     macula_client:publish(Pool, ?REALM, ?TOPIC, <<"x">>, #{group => ?PREFIX, ucan_token => ?TOKEN})),
        ?assertMatch([{?REALM, <<"acme/group_keys_v1">>, _, #{ucan_token := ?TOKEN}}], calls(W))
    end).

a_clear_publish_under_a_held_prefix_names_the_group(#{pool := Pool} = W) ->
    ?_test(begin
        _ = group_subscribe(W),
        ?assertEqual({error, {confidentiality, {group_held, ?PREFIX}}},
                     macula_client:publish(Pool, ?REALM, ?TOPIC, <<"x">>, #{}))
    end).

a_topic_outside_its_group_is_an_invalid_option(#{pool := Pool} = W) ->
    ?_test(begin
        Outside = <<"io.macula/acme/chatter/said_v1">>,
        ?assertEqual({error, {invalid_option, group}}, subscribe(W, Outside, #{group => ?PREFIX})),
        ?assertEqual({error, {invalid_option, group}},
                     macula_client:publish(Pool, ?REALM, Outside, <<"x">>, #{group => ?PREFIX})),
        ?assertEqual({error, {invalid_option, group}}, subscribe(W, ?TOPIC, #{group => <<"io.macula">>})),
        ?assertEqual([], calls(W))
    end).

%% subscribe_callback's receiver answers only macula_event with the callback: an unopened event is logged and
%% dropped, never left in its mailbox, where every one would stay for the subscription's life.
a_callback_subscription_logs_an_unopened_event_and_keeps_no_mail(#{pool := Pool} = W) ->
    ?_test(begin
        Test = self(),
        {ok, _Ref} = macula_pubsub:subscribe_callback(Pool, ?REALM, ?ELSEWHERE,
                                                      fun(Topic, Payload, _Meta) -> Test ! {called, Topic, Payload} end),
        _ = inject_sealed(W, ?ELSEWHERE, current_epoch(W), 1, <<"sealed">>),
        ok = inject_clear(W, ?ELSEWHERE, 2, <<"clear">>),
        receive {called, ?ELSEWHERE, <<"clear">>} -> ok after 2000 -> error(no_clear_event) end,
        [Receiver] = [P || P <- erlang:processes(),
                           process_info(P, current_function) =:= {current_function, {macula_pubsub, receiver_loop, 3}}],
        ?assertEqual({message_queue_len, 0}, process_info(Receiver, message_queue_len))
    end).
