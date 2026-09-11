%% @doc A node's status statement issuer (DESIGN_PQ_HANDSHAKE_FRAMES.md, Binding and Status statement; D22).
%%
%% It holds the identity key, the node's CONNECT keys and the TLS leaves its owner registers, a binding for each and a
%% fresh status statement for each binding.
%%
%% Every 15 minutes it issues a statement valid for 1 hour for each binding whose not_after has not passed, and sends it
%% to the processes subscribed to that binding. Every 5 days it rotates the CONNECT key: the new binding and its
%% statement exist before connect_material/1 hands the key out, and the rotated-out binding keeps its statements until
%% its not_after. Every 5 days it also tells its owner a TLS rotation is due. The owner makes the TLS key and
%% certificate, and register_tls_leaf/3 returns only once the leaf's binding and statement exist, so the owner reloads
%% its listener after that. When no newer TLS binding exists 24 hours before the newest one's not_after, the issuer
%% emits a warning diagnostic at every check until one does.
%%
%% With a key directory, CONNECT keys and registered TLS keys are saved with their bindings, and a restarted issuer
%% reloads the live ones and keeps issuing statements for them. Without one they live in memory only, and a restarted
%% issuer starts with a new CONNECT key and binding. The identity key is never written.
-module(macula_statement_issuer).
-behaviour(gen_server).

-export([start_link/1, connect_material/1, register_tls_leaf/3, tls_material/2, subscribe/2, tick/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([options/0, connect_material/0, tls_material/0]).

-define(STATEMENT_EVERY_MS, 15 * 60000).
-define(STATEMENT_VALID_MS, 60 * 60000).
-define(BINDING_VALID_MS, 7 * 86400000).
-define(ROTATE_EVERY_MS, 5 * 86400000).
-define(OVERDUE_WITHIN_MS, 86400000).
-define(OVERDUE_EVENT, <<"_macula.statement_issuer.tls_rotation_overdue">>).

%% `owner' receives `{macula_tls_rotation_due, Issuer}'. `key_dir' keeps CONNECT and TLS keys with their bindings across
%% restarts. `clock' gives wall-clock milliseconds, for tests.
-type options() :: #{identity_key := macula_node_keys:node_key(),
                     owner := pid(),
                     key_dir => file:name_all(),
                     clock => fun(() -> non_neg_integer())}.
-type connect_material() :: #{connect_key := macula_node_keys:node_key(), connect_binding := map(),
                              connect_status := map()}.
-type tls_material() :: #{tls_binding := map(), tls_status := map()}.

%%====================================================================
%% API
%%====================================================================

-spec start_link(options()) -> {ok, pid()} | {error, term()}.
start_link(#{identity_key := #{purpose := identity}, owner := Owner} = Options) when is_pid(Owner) ->
    gen_server:start_link(?MODULE, Options, []).

%% @doc The current CONNECT key with its binding and a fresh status statement, for a new dial.
-spec connect_material(pid()) -> connect_material().
connect_material(Issuer) ->
    gen_server:call(Issuer, connect_material).

%% @doc Bind a TLS leaf the owner made, with its TLS key. Returns once the binding and its statement exist, so the owner
%% can have its listener present the leaf after that.
-spec register_tls_leaf(pid(), binary(), macula_node_keys:node_key()) -> ok.
register_tls_leaf(Issuer, LeafDer, #{purpose := tls} = TlsKey) when is_binary(LeafDer) ->
    gen_server:call(Issuer, {register_tls_leaf, LeafDer, TlsKey}).

%% @doc The binding and fresh status statement for the leaf whose SHA-384 is `LeafHash'.
-spec tls_material(pid(), <<_:384>>) -> {ok, tls_material()} | {error, unknown_leaf}.
tls_material(Issuer, <<_:384>> = LeafHash) ->
    gen_server:call(Issuer, {tls_material, LeafHash}).

%% @doc Send the caller `{macula_statement, Issuer, BindingHash, Statement}' at every reissue for the binding whose tbs
%% hashes to `BindingHash', until the caller exits or the binding's not_after passes.
-spec subscribe(pid(), <<_:384>>) -> ok | {error, unknown_binding}.
subscribe(Issuer, <<_:384>> = BindingHash) ->
    gen_server:call(Issuer, {subscribe, BindingHash}).

%% @doc Run the periodic work now, at the issuer's clock: drop expired bindings, reissue statements, rotate the CONNECT
%% key when due, and check the TLS rotation.
-spec tick(pid()) -> ok.
tick(Issuer) ->
    gen_server:call(Issuer, tick).

%%====================================================================
%% gen_server
%%====================================================================

init(#{identity_key := #{profile := Profile} = Identity, owner := Owner} = Options) ->
    S0 = #{identity => Identity, profile => Profile, owner => Owner, key_dir => maps:get(key_dir, Options, none),
           clock => maps:get(clock, Options, fun wall_clock_ms/0), connect => none, bindings => #{}, leaves => #{},
           subscribers => #{}, tls_newest => none, notified => none},
    S1 = ensure_connect(loaded(S0)),
    erlang:send_after(?STATEMENT_EVERY_MS, self(), tick),
    {ok, S1}.

handle_call(connect_material, _From, #{connect := Hash, bindings := Bindings} = S) ->
    #{key := Key, binding := Binding, statement := Statement} = maps:get(Hash, Bindings),
    {reply, #{connect_key => Key, connect_binding => Binding, connect_status => Statement}, S};
handle_call({register_tls_leaf, LeafDer, TlsKey}, _From, #{leaves := Leaves} = S) ->
    {reply, ok, registered(maps:is_key(crypto:hash(sha384, LeafDer), Leaves), LeafDer, TlsKey, S)};
handle_call({tls_material, LeafHash}, _From, #{leaves := Leaves} = S) ->
    {reply, material(maps:find(LeafHash, Leaves), S), S};
handle_call({subscribe, Hash}, {Pid, _Tag}, #{bindings := Bindings} = S) ->
    {reply, subscription(maps:is_key(Hash, Bindings)), subscribed(maps:is_key(Hash, Bindings), Hash, Pid, S)};
handle_call(tick, _From, S) ->
    {reply, ok, ticked(S)}.

handle_cast(_Message, S) ->
    {noreply, S}.

handle_info(tick, S) ->
    erlang:send_after(?STATEMENT_EVERY_MS, self(), tick),
    {noreply, ticked(S)};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, #{subscribers := Subscribers} = S) ->
    {noreply, S#{subscribers := maps:map(without_pid(Pid), Subscribers)}};
handle_info(_Message, S) ->
    {noreply, S}.

%%====================================================================
%% Bindings
%%====================================================================

ensure_connect(#{connect := none} = S) ->
    rotated_connect(S);
ensure_connect(S) ->
    S.

rotated_connect(#{profile := Profile} = S) ->
    Now = now_ms(S),
    {ok, Key} = macula_node_keys:generate(connect, Profile),
    Binding = macula_key_bindings:connect_binding(identity(S), macula_node_keys:public_key(Key), Now,
                                                  Now + ?BINDING_VALID_MS),
    added(#{use => connect, key => Key, binding => Binding, issued_at => Now, not_after => Now + ?BINDING_VALID_MS},
          S).

registered(true, _LeafDer, _TlsKey, S) ->
    S;
registered(false, LeafDer, TlsKey, S) ->
    Now = now_ms(S),
    Binding = macula_key_bindings:tls_binding(identity(S), LeafDer, Now, Now + ?BINDING_VALID_MS),
    added(#{use => tls, key => TlsKey, leaf => LeafDer, binding => Binding, issued_at => Now,
            not_after => Now + ?BINDING_VALID_MS}, S).

%% A binding enters with its statement, and is saved when there is a key directory.
added(#{binding := #{tbs := Tbs}} = Entry, #{key_dir := Dir} = S) ->
    Hash = crypto:hash(sha384, Tbs),
    Stated = Entry#{statement => statement(Entry, now_ms(S), S)},
    ok = save(Dir, Hash, Stated),
    placed(Stated, Hash, S).

placed(#{use := connect} = Entry, Hash, #{bindings := Bindings} = S) ->
    S#{bindings := Bindings#{Hash => Entry}, connect := Hash};
placed(#{use := tls, leaf := LeafDer} = Entry, Hash, #{bindings := Bindings, leaves := Leaves} = S) ->
    S#{bindings := Bindings#{Hash => Entry}, leaves := Leaves#{crypto:hash(sha384, LeafDer) => Hash},
       tls_newest := newer(maps:get(tls_newest, S), Hash, Entry)}.

newer(#{issued_at := Newest} = Current, _Hash, #{issued_at := IssuedAt}) when Newest >= IssuedAt ->
    Current;
newer(_Current, Hash, #{issued_at := IssuedAt, not_after := NotAfter}) ->
    #{hash => Hash, issued_at => IssuedAt, not_after => NotAfter}.

statement(#{binding := Binding}, Now, S) ->
    macula_key_bindings:status_statement(identity(S), Binding, Now, Now + ?STATEMENT_VALID_MS).

material(error, _S) ->
    {error, unknown_leaf};
material({ok, Hash}, #{bindings := Bindings}) ->
    #{binding := Binding, statement := Statement} = maps:get(Hash, Bindings),
    {ok, #{tls_binding => Binding, tls_status => Statement}}.

subscription(true)  -> ok;
subscription(false) -> {error, unknown_binding}.

subscribed(false, _Hash, _Pid, S) ->
    S;
subscribed(true, Hash, Pid, #{subscribers := Subscribers} = S) ->
    _ = erlang:monitor(process, Pid),
    S#{subscribers := Subscribers#{Hash => [Pid | lists:delete(Pid, maps:get(Hash, Subscribers, []))]}}.

without_pid(Pid) ->
    fun(_Hash, Pids) -> lists:delete(Pid, Pids) end.

%%====================================================================
%% The periodic work
%%====================================================================

ticked(S) ->
    Now = now_ms(S),
    overdue_checked(rotation_noticed(connect_rotated(reissued(expired_dropped(S, Now), Now), Now), Now), Now).

expired_dropped(#{bindings := Bindings, leaves := Leaves, subscribers := Subscribers, key_dir := Dir} = S, Now) ->
    Live = maps:filter(live_at(Now), Bindings),
    Expired = maps:without(maps:keys(Live), Bindings),
    ok = maps:foreach(forgotten_in(Dir), Expired),
    S#{bindings := Live, leaves := maps:filter(bound_to(Live), Leaves),
       subscribers := maps:without(maps:keys(Expired), Subscribers)}.

live_at(Now) ->
    fun(_Hash, #{not_after := NotAfter}) -> NotAfter >= Now end.

bound_to(Live) ->
    fun(_LeafHash, Hash) -> maps:is_key(Hash, Live) end.

reissued(#{bindings := Bindings, subscribers := Subscribers} = S, Now) ->
    Reissued = maps:map(restated(Now, S), Bindings),
    ok = maps:foreach(pushed_from(Reissued), Subscribers),
    S#{bindings := Reissued}.

restated(Now, S) ->
    fun(_Hash, Entry) -> Entry#{statement => statement(Entry, Now, S)} end.

pushed_from(Bindings) ->
    fun(Hash, Pids) -> push(maps:find(Hash, Bindings), Hash, Pids) end.

push({ok, #{statement := Statement}}, Hash, Pids) ->
    _ = [Pid ! {macula_statement, self(), Hash, Statement} || Pid <- Pids],
    ok;
push(error, _Hash, _Pids) ->
    ok.

connect_rotated(#{connect := Hash, bindings := Bindings} = S, Now) ->
    #{issued_at := IssuedAt} = maps:get(Hash, Bindings),
    rotate_when(Now >= IssuedAt + ?ROTATE_EVERY_MS, S).

rotate_when(true, S)  -> rotated_connect(S);
rotate_when(false, S) -> S.

rotation_noticed(#{tls_newest := #{hash := Hash, issued_at := IssuedAt}, notified := Notified, owner := Owner} = S,
                 Now) when Now >= IssuedAt + ?ROTATE_EVERY_MS, Notified =/= Hash ->
    Owner ! {macula_tls_rotation_due, self()},
    S#{notified := Hash};
rotation_noticed(S, _Now) ->
    S.

overdue_checked(#{tls_newest := #{hash := Hash, not_after := NotAfter}} = S, Now)
  when Now >= NotAfter - ?OVERDUE_WITHIN_MS ->
    ok = macula_diagnostics:event(warning, ?OVERDUE_EVENT, #{binding_hash => binary:encode_hex(Hash, lowercase),
                                                             not_after => NotAfter, now => Now}),
    S;
overdue_checked(S, _Now) ->
    S.

%%====================================================================
%% The key directory
%%====================================================================

save(none, _Hash, _Entry) ->
    ok;
save(Dir, Hash, #{use := Use, key := Key, binding := Binding} = Entry) ->
    Base = base(Dir, Use, Hash),
    ok = macula_node_keys:save(<<Base/binary, ".key">>, Key),
    ok = write_owner_only(<<Base/binary, ".binding">>, term_to_binary(Binding)),
    save_leaf(Entry, Base).

save_leaf(#{use := tls, leaf := LeafDer}, Base) ->
    write_owner_only(<<Base/binary, ".leaf">>, LeafDer);
save_leaf(_Entry, _Base) ->
    ok.

forgotten_in(none) ->
    fun(_Hash, _Entry) -> ok end;
forgotten_in(Dir) ->
    fun(Hash, #{use := Use}) -> forget(base(Dir, Use, Hash)) end.

forget(Base) ->
    _ = [file:delete(<<Base/binary, Extension/binary>>) || Extension <- [<<".key">>, <<".binding">>, <<".leaf">>]],
    ok.

base(Dir, Use, Hash) ->
    Name = iolist_to_binary([atom_to_list(Use), "-", binary:encode_hex(Hash, lowercase)]),
    iolist_to_binary(filename:join(Dir, Name)).

write_owner_only(Path, Bytes) ->
    Tmp = <<Path/binary, ".tmp">>,
    ok = file:write_file(Tmp, <<>>),
    ok = file:change_mode(Tmp, 8#600),
    ok = file:write_file(Tmp, Bytes),
    file:rename(Tmp, Path).

%% A restarted issuer takes back every stored binding that still verifies, with its key, and restates it.
loaded(#{key_dir := none} = S) ->
    S;
loaded(#{key_dir := Dir} = S) ->
    Stored = filelib:wildcard("*.binding", as_list(Dir)),
    Reloaded = lists:foldl(fun reloaded/2, S, Stored),
    Reloaded#{connect := newest_connect(maps:get(bindings, Reloaded))}.

reloaded("connect-" ++ Rest, #{key_dir := Dir} = S) ->
    Base = iolist_to_binary(filename:join(Dir, "connect-" ++ filename:rootname(Rest))),
    take_back(connect_entry(Base, S), Base, S);
reloaded("tls-" ++ Rest, #{key_dir := Dir} = S) ->
    Base = iolist_to_binary(filename:join(Dir, "tls-" ++ filename:rootname(Rest))),
    take_back(tls_entry(Base, S), Base, S);
reloaded(_Other, S) ->
    S.

connect_entry(Base, #{profile := Profile} = S) ->
    with_key(macula_node_keys:load(<<Base/binary, ".key">>, connect, Profile), Base, S).

with_key({ok, Key}, Base, S) ->
    checked(stored_binding(Base), connect_verifier(Key, S), #{use => connect, key => Key});
with_key({error, _} = Refusal, _Base, _S) ->
    Refusal.

tls_entry(Base, #{profile := Profile} = S) ->
    tls_with_key(macula_node_keys:load(<<Base/binary, ".key">>, tls, Profile),
                 file:read_file(<<Base/binary, ".leaf">>), Base, S).

tls_with_key({ok, Key}, {ok, LeafDer}, Base, S) ->
    checked(stored_binding(Base), tls_verifier(LeafDer, S), #{use => tls, key => Key, leaf => LeafDer});
tls_with_key(_Key, _Leaf, _Base, _S) ->
    {error, unreadable}.

connect_verifier(Key, #{profile := Profile} = S) ->
    Public = macula_node_keys:public_key(Key),
    IdentityPublic = identity_public(S),
    Now = now_ms(S),
    fun(Binding) -> macula_key_bindings:verify_connect_binding(Binding, IdentityPublic, Profile, Public, Now) end.

tls_verifier(LeafDer, #{profile := Profile} = S) ->
    IdentityPublic = identity_public(S),
    Now = now_ms(S),
    fun(Binding) -> macula_key_bindings:verify_tls_binding(Binding, IdentityPublic, Profile, LeafDer, Now) end.

checked({ok, Binding}, Verify, Entry) ->
    verified(Verify(Binding), Entry#{binding => Binding});
checked({error, _} = Refusal, _Verify, _Entry) ->
    Refusal.

verified({ok, #{not_after := NotAfter}}, Entry) ->
    {ok, Entry#{issued_at => NotAfter - ?BINDING_VALID_MS, not_after => NotAfter}};
verified({error, _} = Refusal, _Entry) ->
    Refusal.

stored_binding(Base) ->
    binding_term(file:read_file(<<Base/binary, ".binding">>)).

binding_term({ok, Bytes}) ->
    try binary_to_term(Bytes, [safe]) of
        #{tbs := Tbs, signature := Signature} = Binding
          when map_size(Binding) =:= 2, is_binary(Tbs), is_binary(Signature) -> {ok, Binding};
        _Other -> {error, bad_binding_file}
    catch
        error:badarg -> {error, bad_binding_file}
    end;
binding_term({error, _} = Refusal) ->
    Refusal.

%% A stored binding that no longer verifies, or whose key is unreadable, is forgotten.
take_back({ok, Entry}, _Base, S) ->
    restored(Entry, S);
take_back({error, _}, Base, S) ->
    ok = forget(Base),
    S.

restored(#{binding := #{tbs := Tbs}} = Entry, S) ->
    placed(Entry#{statement => statement(Entry, now_ms(S), S)}, crypto:hash(sha384, Tbs), S).

newest_connect(Bindings) ->
    {Newest, _IssuedAt} = maps:fold(fun newer_connect/3, {none, -1}, Bindings),
    Newest.

newer_connect(Hash, #{use := connect, issued_at := IssuedAt}, {_Best, BestAt}) when IssuedAt > BestAt ->
    {Hash, IssuedAt};
newer_connect(_Hash, _Entry, Best) ->
    Best.

%%====================================================================
%% Helpers
%%====================================================================

identity(#{identity := Identity}) ->
    Identity.

identity_public(#{identity := Identity}) ->
    macula_node_keys:public_key(Identity).

as_list(Dir) when is_binary(Dir) ->
    binary_to_list(Dir);
as_list(Dir) ->
    Dir.

now_ms(#{clock := Clock}) ->
    Clock().

wall_clock_ms() ->
    erlang:system_time(millisecond).
