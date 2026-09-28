%% @doc A node's keyring for the sealed groups it joined (plans/DESIGN_E2E_SEALED_PUBSUB.md §4, §6, §7).
%%
%% A group is a topic prefix whose second segment is its org. Joining it pulls the group's epochs from the org's
%% distributor, `<org>/group_keys_v1', over a call sealed to the distributor's KEM key (`confidential => required':
%% a keyless advertisement is never a distributor, since a clear pull would hand the key to every station on the
%% path) and carrying the org's UCAN as the call's own `ucan_token'. The first trusted distributor that answers is
%% kept for the group, or the one `distributor' pins.
%%
%% Every held group is pulled again at a uniformly random instant in its newest epoch's ahead window, event or no
%% event, so the rotation is spread over a third of an epoch and the policy a node holds is at most one epoch old. A
%% pull that fails is retried at once and then with backoff, doubling from 1 second up to a third of an epoch, until
%% one succeeds. The policy is monotonic per node run: once `preferred' or `required' has been seen for a prefix,
%% `off' is ignored.
%%
%% A publisher seals under the newest held epoch that has started and has not stopped publishing; with none, the
%% group is pulled at once, and the publish fails closed with that pull's error. A subscriber opens an event under a
%% held epoch until its acceptance ends; an unknown id is pulled by id, at most three unknown ids per publisher per
%% epoch, and an `unknown_epoch' answer is remembered until the id could no longer be accepted anyway.
%%
%% Only a pull goes through the keyring's process. What it holds is stored in a table it owns, read through a handle
%% (`handle/1'): publishing under a current epoch, opening under a held one and reading a group's policy never wait on
%% a pull, which can take the distributor's whole deadline, for this group or another.
%%
%% Refusals and failures are named by the closed set of §7: `not_a_member', `membership_unknown', `unknown_epoch',
%% `epoch_expired', and `no_distributor' for a distributor that cannot be found, reached or understood. Epoch keys are
%% held in memory only.
-module(macula_group_keyring).
-behaviour(gen_server).

-export([start_link/1, handle/1, pid/1, join/4, leave/3, publish_epoch/3, open_epoch/5, policy/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-type policy() :: required | preferred | off.
-type reason() :: not_a_member | membership_unknown | unknown_epoch | epoch_expired | no_distributor.
-type options() :: #{pool => pid(), now => fun(() -> integer()),
                     call => fun((binary(), binary(), map(), map()) -> {ok, term(), map()} | {error, term()}),
                     schedule => fun((non_neg_integer(), term()) -> term()),
                     cancel => fun((term()) -> term()),
                     uniform => fun(() -> float())}.
-type join_options() :: #{ucan_token => binary(), distributor => <<_:256>>}.
-opaque keyring() :: #{pid := pid(), table := ets:tid(), now := fun(() -> integer())}.
-export_type([options/0, join_options/0, policy/0, reason/0, keyring/0]).

-define(PULL_TIMEOUT_MS, 15000).
-define(FIRST_BACKOFF_MS, 1000).
%% The longest a publication lives, as macula_frame verifies one: an epoch's acceptance is exactly this past its
%% publishing, and an unknown id is remembered this long.
-define(EVENT_LIFE_MS, 65 * 60000).
-define(UNKNOWN_IDS_PER_PUBLISHER, 3).

%% @doc Start a keyring over `pool'. `call', `now', `schedule' and `uniform' replace the pull (as
%% `macula:call/6'), the clock (milliseconds), the timer and the jitter draw.
-spec start_link(options()) -> {ok, pid()} | {error, term()}.
start_link(Opts) when is_map(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc The handle every other function takes: the keyring's process, and the table it stores its groups in.
-spec handle(pid()) -> keyring().
handle(Pid) ->
    gen_server:call(Pid, handle).

%% @doc The keyring's process, for its owner to end it.
-spec pid(keyring()) -> pid().
pid(#{pid := Pid}) -> Pid.

%% @doc Join the group `Prefix' in `Realm': pull its current epoch and hold it. A group already held is not pulled
%% again. `ucan_token' is the org's grant, `distributor' pins the distributor's node id.
-spec join(keyring(), binary(), binary(), join_options()) ->
          {ok, policy()} | {error, reason() | {invalid_option, group}}.
join(#{pid := Pid}, Realm, Prefix, Opts) ->
    gen_server:call(Pid, {join, Realm, Prefix, Opts}, infinity).

%% @doc Stop holding the group, and erase its keys.
-spec leave(keyring(), binary(), binary()) -> ok.
leave(#{pid := Pid}, Realm, Prefix) ->
    gen_server:call(Pid, {leave, Realm, Prefix}, infinity).

%% @doc The epoch a publisher seals under now: read from the table while one is current; otherwise the keyring pulls.
-spec publish_epoch(keyring(), binary(), binary()) ->
          {ok, macula_group_epoch:epoch()} | {error, reason() | not_joined}.
publish_epoch(#{pid := Pid, table := Table, now := Now}, Realm, Prefix) ->
    publish_from_table(stored_epochs(Table, {Realm, Prefix}), Now(), Pid, Realm, Prefix).

publish_from_table({ok, Epochs}, Now, Pid, Realm, Prefix) ->
    current_or_pulled(macula_group_epoch:for_publish(Epochs, Now), Pid, Realm, Prefix);
publish_from_table(not_joined, _Now, _Pid, _Realm, _Prefix) ->
    {error, not_joined}.

current_or_pulled({ok, Epoch}, _Pid, _Realm, _Prefix) -> {ok, Epoch};
current_or_pulled({error, no_current_epoch}, Pid, Realm, Prefix) ->
    gen_server:call(Pid, {publish_epoch, Realm, Prefix}, infinity).

%% @doc The epoch `Id' an event from `Publisher' was sealed under, if it may be opened now: read from the table when it
%% is held; otherwise the keyring pulls it by id, within its bounds.
-spec open_epoch(keyring(), binary(), binary(), <<_:64>>, <<_:256>>) ->
          {ok, macula_group_epoch:epoch()} | {error, reason() | not_joined}.
open_epoch(#{pid := Pid, table := Table, now := Now}, Realm, Prefix, Id, Publisher) ->
    open_from_table(stored_epochs(Table, {Realm, Prefix}), Now(), Pid, {Realm, Prefix, Id, Publisher}).

open_from_table({ok, Epochs}, Now, Pid, {_Realm, _Prefix, Id, _Publisher} = Ask) ->
    held_or_pulled(lists:search(fun(#{id := E}) -> E =:= Id end, Epochs), Now, Pid, Ask);
open_from_table(not_joined, _Now, _Pid, _Ask) ->
    {error, not_joined}.

held_or_pulled({value, Epoch}, Now, _Pid, _Ask) ->
    acceptable(macula_group_epoch:acceptable(Epoch, Now), Epoch);
held_or_pulled(false, _Now, Pid, {Realm, Prefix, Id, Publisher}) ->
    gen_server:call(Pid, {open_epoch, Realm, Prefix, Id, Publisher}, infinity).

%% @doc The joined group covering `Topic' (its longest joined prefix) and that group's policy, or `none'.
-spec policy(keyring(), binary(), binary()) -> {ok, binary(), policy()} | none.
policy(#{table := Table}, Realm, Topic) ->
    covering([{byte_size(P), P, Policy} || {{_R, P}, Policy, _Epochs} <- ets:match_object(Table, {{Realm, '_'}, '_', '_'}),
                                           under(Topic, P)]).

stored_epochs(Table, Key) ->
    stored_row(ets:lookup(Table, Key)).

stored_row([{_Key, _Policy, Epochs}]) -> {ok, Epochs};
stored_row([]) -> not_joined.

%% @private
init(Opts) ->
    {ok, #{now => maps:get(now, Opts, fun() -> erlang:system_time(millisecond) end),
           call => maps:get(call, Opts, fun(Realm, Procedure, Payload, CallOpts) ->
                                               macula:call(maps:get(pool, Opts), Realm, Procedure, Payload,
                                                           ?PULL_TIMEOUT_MS, CallOpts)
                                       end),
           schedule => maps:get(schedule, Opts, fun(DelayMs, Msg) -> erlang:send_after(DelayMs, self(), Msg) end),
           cancel => maps:get(cancel, Opts, fun erlang:cancel_timer/1),
           uniform => maps:get(uniform, Opts, fun rand:uniform/0),
           table => ets:new(?MODULE, [protected, set, {read_concurrency, true}]),
           groups => #{}}}.

%% @private
handle_call(handle, _From, #{table := Table, now := Now} = State) ->
    {reply, #{pid => self(), table => Table, now => Now}, State};
handle_call({join, Realm, Prefix, Opts}, _From, State) ->
    joined(org_of(Prefix), maps:find({Realm, Prefix}, maps:get(groups, State)), Realm, Prefix, Opts, State);
handle_call({leave, Realm, Prefix}, _From, #{groups := Groups, table := Table} = State) ->
    true = ets:delete(Table, {Realm, Prefix}),
    _ = timer_cancelled(maps:get({Realm, Prefix}, Groups, #{}), State),
    {reply, ok, State#{groups := maps:remove({Realm, Prefix}, Groups)}};
handle_call({publish_epoch, Realm, Prefix}, _From, State) ->
    with_group({Realm, Prefix}, State, fun(Group) -> publishing(Group, now(State), {Realm, Prefix}, State) end);
handle_call({open_epoch, Realm, Prefix, Id, Publisher}, _From, State) ->
    with_group({Realm, Prefix}, State,
               fun(Group) -> opening(Id, Publisher, Group, now(State), {Realm, Prefix}, State) end).

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
handle_info({repull, Key, Token}, #{groups := Groups} = State) ->
    {noreply, repulled(current_wake(maps:find(Key, Groups), Token), Key, State)};
handle_info(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Joining
%%--------------------------------------------------------------------

joined({error, _} = Invalid, _Held, _Realm, _Prefix, _Opts, State) ->
    {reply, Invalid, State};
joined({ok, _Org}, {ok, #{policy := Policy}}, _Realm, _Prefix, _Opts, State) ->
    {reply, {ok, Policy}, State};
joined({ok, Org}, error, Realm, Prefix, Opts, State) ->
    options_valid(join_options_checked(Opts), Org, {Realm, Prefix}, Opts, State).

%% A join's options, checked before anything is sent: a pinned distributor is a node id, a grant is bytes.
join_options_checked(#{distributor := Node}) when not (is_binary(Node) andalso byte_size(Node) =:= 32) ->
    {error, {invalid_option, distributor}};
join_options_checked(#{ucan_token := Token}) when not is_binary(Token) ->
    {error, {invalid_option, ucan_token}};
join_options_checked(_Valid) ->
    ok.

options_valid({error, _} = Invalid, _Org, _Key, _Opts, State) ->
    {reply, Invalid, State};
options_valid(ok, Org, Key, Opts, State) ->
    Group = #{org => Org, ucan => maps:get(ucan_token, Opts, none), pin => pin(maps:find(distributor, Opts)),
              epochs => [], unknown => #{}, budget => #{}, backoff => ?FIRST_BACKOFF_MS, timer => none},
    first_pull(pull(<<"current">>, Group, Key, State), Key, State).

pin({ok, <<_:256>> = Node}) -> {user, Node};
pin(error) -> none.

first_pull({ok, #{policy := Policy} = Group}, Key, State) ->
    {reply, {ok, Policy}, scheduled(Group, Key, State)};
first_pull({error, Reason, _Group}, _Key, State) ->
    {reply, {error, Reason}, State}.

%% A prefix a group can be: at least two non-empty segments, the second its org.
org_of(Prefix) when is_binary(Prefix) ->
    org_of_segments(binary:split(Prefix, <<"/">>, [global]));
org_of(_NotAPrefix) ->
    {error, {invalid_option, group}}.

org_of_segments([Realm, Org | Rest] = Segments) when Realm =/= <<>>, Org =/= <<>> ->
    no_empty(lists:member(<<>>, Segments), Org, Rest);
org_of_segments(_Segments) ->
    {error, {invalid_option, group}}.

no_empty(false, Org, _Rest) -> {ok, Org};
no_empty(true, _Org, _Rest) -> {error, {invalid_option, group}}.

%%--------------------------------------------------------------------
%% Pulling
%%--------------------------------------------------------------------

%% One pull of `current' or a past epoch by id. Answers `{ok, Group}' with the reply's epochs and policy merged in,
%% or `{error, Reason, Group}' with the group as it was, save a dropped automatic pin.
pull(Epoch, #{org := Org} = Group, {Realm, Prefix}, #{call := Call}) ->
    Payload = #{{text, <<"prefix">>} => {text, Prefix}, {text, <<"epoch">>} => epoch_wire(Epoch)},
    pulled((Call)(Realm, <<Org/binary, "/group_keys_v1">>, Payload, call_opts(Group)), Prefix, Group).

epoch_wire(<<"current">>) -> {text, <<"current">>};
epoch_wire(<<_:64>> = Id) -> Id.

call_opts(#{ucan := Ucan, pin := Pin}) ->
    maps:merge(maps:merge(#{confidential => required, report => true}, ucan_opt(Ucan)), pin_opt(Pin)).

ucan_opt(none) -> #{};
ucan_opt(Token) -> #{ucan_token => Token}.

pin_opt({_Kind, Node}) -> #{provider => Node};
pin_opt(none) -> #{}.

pulled({ok, Reply, Report}, Prefix, Group) ->
    merged(parsed(Prefix, Reply), maps:get(provider, Report, undefined), Group);
pulled({error, {unresolved, provider_not_advertised}}, _Prefix, #{pin := {auto, _}} = Group) ->
    {error, no_distributor, Group#{pin := none}};
pulled({error, Reason}, _Prefix, Group) ->
    {error, refusal(Reason), Group}.

refusal(<<"not_a_member">>) -> not_a_member;
refusal(<<"membership_unknown">>) -> membership_unknown;
refusal(<<"unknown_epoch">>) -> unknown_epoch;
refusal(<<"epoch_expired">>) -> epoch_expired;
refusal(_CannotBeFoundReachedOrUnderstood) -> no_distributor.

merged({ok, Policy, [First | _] = Epochs}, Provider, #{epochs := Held} = Group) ->
    {ok, Group#{epochs := union(Held, Epochs), policy => monotonic(maps:get(policy, Group, undefined), Policy),
                rotation => rotation(First), pin := kept(maps:get(pin, Group), Provider)}};
merged({error, malformed_reply}, _Provider, Group) ->
    {error, no_distributor, Group}.

kept(none, <<_:256>> = Provider) -> {auto, Provider};
kept(Pin, _Provider) -> Pin.

union(Held, New) ->
    Ids = [Id || #{id := Id} <- Held],
    lists:sort(fun(#{issued_at := A}, #{issued_at := B}) -> A =< B end,
               Held ++ [E || #{id := Id} = E <- New, not lists:member(Id, Ids)]).

monotonic(Seen, off) when Seen =:= preferred; Seen =:= required -> Seen;
monotonic(_Seen, Policy) -> Policy.

%% The reply, read as the protocol has it: the asked prefix echoed, a policy, and epochs each with an 8-byte id, a
%% 32-byte key and the protocol's times. Anything else is the distributor's fault, and nothing of it is held.
parsed(Prefix, Reply) when is_map(Reply) ->
    reply_parts(macula_record:payload_field(Reply, <<"prefix">>), policy_of(macula_record:payload_field(Reply, <<"policy">>)),
                epochs_of(macula_record:payload_field(Reply, <<"epochs">>)), Prefix);
parsed(_Prefix, _NotAMap) ->
    {error, malformed_reply}.

reply_parts(Prefix, {ok, Policy}, {ok, [_ | _] = Epochs}, Prefix) -> {ok, Policy, Epochs};
reply_parts(_Echoed, _Policy, _Epochs, _Prefix) -> {error, malformed_reply}.

policy_of(<<"required">>) -> {ok, required};
policy_of(<<"preferred">>) -> {ok, preferred};
policy_of(<<"off">>) -> {ok, off};
policy_of(_Other) -> error.

epochs_of(List) when is_list(List) ->
    all_ok([epoch_of(E) || E <- List]);
epochs_of(_NotAList) ->
    error.

all_ok(Results) ->
    all_epochs([E || {ok, E} <- Results], length(Results)).

all_epochs(Epochs, Count) when length(Epochs) =:= Count -> {ok, Epochs};
all_epochs(_Epochs, _Count) -> error.

epoch_of(E) when is_map(E) ->
    epoch_fields([macula_record:payload_field(E, F)
                  || F <- [<<"id">>, <<"key">>, <<"issued_at">>, <<"publish_until">>, <<"accept_until">>]]);
epoch_of(_NotAMap) ->
    error.

epoch_fields([<<_:64>> = Id, <<_:256>> = Key, I, P, A])
  when is_integer(I), is_integer(P), is_integer(A), I < P, A =:= P + ?EVENT_LIFE_MS ->
    {ok, #{id => Id, key => Key, issued_at => I, publish_until => P, accept_until => A}};
epoch_fields(_Fields) ->
    error.

%%--------------------------------------------------------------------
%% Re-pulling
%%--------------------------------------------------------------------

repulled({ok, Group}, Key, State) ->
    after_repull(pull(<<"current">>, live(Group, now(State)), Key, State), Key, State);
repulled(error, _Key, State) ->
    State.

after_repull({ok, Group}, Key, State) ->
    scheduled(Group#{backoff := ?FIRST_BACKOFF_MS}, Key, State);
after_repull({error, _Reason, Group}, Key, State) ->
    backed_off(Group, Key, State).

%% The next re-pull: a random instant in the newest epoch's ahead window, or at once if that window has passed.
scheduled(#{epochs := Epochs, rotation := R} = Group, Key, #{uniform := Uniform} = State) ->
    At = macula_group_epoch:repull_at(lists:last(Epochs), R, Uniform()),
    stored(woken_in(max(0, At - now(State)), Group, Key, State), Key, State).

backed_off(#{backoff := Backoff, rotation := R} = Group, Key, State) ->
    stored((woken_in(Backoff, Group, Key, State))#{backoff := min(Backoff * 2, max(R div 3, ?FIRST_BACKOFF_MS))},
           Key, State).

%% A group has one re-pull pending, whatever asked for it: a new one replaces the pending one, whose wake-up, if it
%% is already on its way, names a token that is no longer the group's and does nothing (current_wake/2).
woken_in(DelayMs, Group, Key, #{schedule := Schedule} = State) ->
    _ = timer_cancelled(Group, State),
    Token = make_ref(),
    Group#{timer => {Token, Schedule(DelayMs, {repull, Key, Token})}}.

timer_cancelled(#{timer := {_Token, Timer}}, #{cancel := Cancel}) -> Cancel(Timer);
timer_cancelled(_NoTimer, _State) -> ok.

current_wake({ok, #{timer := {Token, _Timer}} = Group}, Token) -> {ok, Group};
current_wake(_GoneOrReplaced, _Token) -> error.

rotation(#{issued_at := I, publish_until := P}) -> P - I.

%% Every change to a group is stored in the table the handle reads, as it is kept here.
stored(#{policy := Policy, epochs := Epochs} = Group, Key, #{groups := Groups, table := Table} = State) ->
    true = ets:insert(Table, {Key, Policy, Epochs}),
    State#{groups := Groups#{Key => Group}}.

%%--------------------------------------------------------------------
%% Publishing and opening
%%--------------------------------------------------------------------

with_group(Key, #{groups := Groups} = State, Fun) ->
    group_found(maps:find(Key, Groups), State, Fun).

group_found({ok, Group}, _State, Fun) -> Fun(Group);
group_found(error, State, _Fun) -> {reply, {error, not_joined}, State}.

publishing(#{epochs := Epochs} = Group, Now, Key, State) ->
    publishing_from(macula_group_epoch:for_publish(Epochs, Now), Group, Now, Key, State).

publishing_from({ok, Epoch}, _Group, _Now, _Key, State) ->
    {reply, {ok, Epoch}, State};
publishing_from({error, no_current_epoch}, Group, Now, Key, State) ->
    pulled_to_publish(pull(<<"current">>, live(Group, Now), Key, State), Now, Key, State).

pulled_to_publish({ok, #{epochs := Epochs} = Group}, Now, Key, State) ->
    {reply, closed(macula_group_epoch:for_publish(Epochs, Now)), scheduled(Group, Key, State)};
pulled_to_publish({error, Reason, Group}, _Now, Key, State) ->
    {reply, {error, Reason}, backed_off(Group, Key, State)}.

closed({ok, Epoch}) -> {ok, Epoch};
closed({error, no_current_epoch}) -> {error, no_distributor}.

opening(Id, Publisher, #{epochs := Epochs} = Group, Now, Key, State) ->
    held(lists:search(fun(#{id := E}) -> E =:= Id end, Epochs), Id, Publisher, Group, Now, Key, State).

held({value, Epoch}, _Id, _Publisher, _Group, Now, _Key, State) ->
    {reply, acceptable(macula_group_epoch:acceptable(Epoch, Now), Epoch), State};
held(false, Id, Publisher, #{unknown := Unknown} = Group, Now, Key, State) ->
    remembered(maps:get(Id, Unknown, 0) > Now, Id, Publisher, Group, Now, Key, State).

%% A reply to a pull by id that holds no epoch of that id is the distributor's fault.
answered_by_id({value, Epoch}, Now) -> acceptable(macula_group_epoch:acceptable(Epoch, Now), Epoch);
answered_by_id(false, _Now) -> {error, no_distributor}.

acceptable(true, Epoch) -> {ok, Epoch};
acceptable(false, _Epoch) -> {error, epoch_expired}.

remembered(true, _Id, _Publisher, _Group, _Now, _Key, State) ->
    {reply, {error, unknown_epoch}, State};
remembered(false, Id, Publisher, Group, Now, Key, State) ->
    budgeted(spend(Publisher, Group, Now), Id, Now, Key, State).

%% At most three unknown-id pulls per publisher per rotation.
spend(Publisher, #{budget := Budget, rotation := Window} = Group, Now) ->
    spent(maps:get(Publisher, Budget, {Now, 0}), Window, Publisher, Group, Now).

spent({Since, _Count}, Window, Publisher, Group, Now) when Now - Since >= Window ->
    {ok, charged(Publisher, {Now, 1}, Group)};
spent({_Since, Count}, _Window, _Publisher, _Group, _Now) when Count >= ?UNKNOWN_IDS_PER_PUBLISHER ->
    exhausted;
spent({Since, Count}, _Window, Publisher, Group, _Now) ->
    {ok, charged(Publisher, {Since, Count + 1}, Group)}.

charged(Publisher, Entry, #{budget := Budget} = Group) ->
    Group#{budget := Budget#{Publisher => Entry}}.

budgeted(exhausted, _Id, _Now, _Key, State) ->
    {reply, {error, unknown_epoch}, State};
budgeted({ok, Group}, Id, Now, Key, State) ->
    by_id(pull(Id, Group, Key, State), Id, Now, Key, State).

by_id({ok, #{epochs := Epochs} = Group}, Id, Now, Key, State) ->
    {reply, answered_by_id(lists:search(fun(#{id := E}) -> E =:= Id end, Epochs), Now), stored(Group, Key, State)};
by_id({error, unknown_epoch, #{unknown := Unknown} = Group}, Id, Now, Key, State) ->
    {reply, {error, unknown_epoch}, stored(Group#{unknown := Unknown#{Id => Now + ?EVENT_LIFE_MS}}, Key, State)};
by_id({error, Reason, Group}, _Id, _Now, Key, State) ->
    {reply, {error, Reason}, stored(Group, Key, State)}.

%% The held epochs still of use; the others' keys are erased.
live(#{epochs := Epochs} = Group, Now) ->
    Group#{epochs := macula_group_epoch:live(Epochs, Now)}.

%%--------------------------------------------------------------------
%% Covering prefixes
%%--------------------------------------------------------------------

under(Topic, Prefix) ->
    Size = byte_size(Prefix),
    Topic =:= Prefix orelse (byte_size(Topic) > Size andalso binary:part(Topic, 0, Size + 1) =:= <<Prefix/binary, "/">>).

covering([]) -> none;
covering(Groups) -> {_Size, Prefix, Policy} = lists:max(Groups), {ok, Prefix, Policy}.

now(#{now := Now}) -> Now().
