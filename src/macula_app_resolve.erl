%%%-------------------------------------------------------------------
%%% @doc Where an app runs, from its MRI (macula#75): the app record the org
%%% signed for it, then each of its services' procedures through the same
%%% provider lookup a call uses.
%%%
%%% The app record is looked up under `macula_record:app_key/3' of the MRI's
%%% realm, org and app, and is trusted only when `macula_record:verify_app/3'
%%% passes against the realm key the pool pinned for that realm: the record
%%% carries the realm-signed org directory, so the check needs no second
%%% lookup. Of several records that pass, the newest is used. A procedure no
%%% provider serves right now is listed with no providers and the reason, so
%%% an app that serves nothing at the moment still resolves.
%%%
%%% The DHT lookup, the pinned realm key and the provider lookup come from
%%% the `resolve_io/0' seam (each entry defaults to the real function), so a
%%% test stubs them here rather than replacing a module other processes call.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_app_resolve).

-export([resolve/3, resolve/4]).

-define(TYPE_APP_RECORD, 16#17).

-type resolved() :: #{app := #{realm_id := <<_:256>>, org_name := binary(), app_name := binary(),
                               version := binary()},
                      services := [#{name := binary(),
                                     procedures := [#{procedure := binary(),
                                                      providers := [#{provider := <<_:256>>,
                                                                      station := <<_:256>>}],
                                                      unresolved => term()}]}]}.
-type resolve_io() :: #{find_records => fun((pid(), <<_:256>>, pos_integer()) ->
                                                   {ok, [macula_record:m_record()]} | {error, term()}),
                        realm_key => fun((pid(), <<_:256>>) -> {ok, binary()} | none),
                        providers => fun((pid(), <<_:256>>, binary(), pos_integer()) ->
                                                {ok, [map()]} | {error, term()})}.
-export_type([resolved/0, resolve_io/0]).

%% @doc Resolve the app `AppMri' (`mri:app:<realm>/<org>/<app>') to its
%% services and the providers serving each of their procedures, within
%% `TimeoutMs' in all. `{error, {invalid_app_mri, Reason}}' for anything but a
%% valid app MRI; `{error, {unresolved, app_not_registered}}' when no record
%% is stored under it; `{error, {unresolved, {no_trusted_app_record,
%% Refusals}}}' when none passes `verify_app/3', naming each refusal.
-spec resolve(pid(), binary(), pos_integer()) -> {ok, resolved()} | {error, term()}.
resolve(Pool, AppMri, TimeoutMs) ->
    resolve(Pool, AppMri, TimeoutMs, #{}).

%% @doc As `resolve/3', with the lookups taken from `Io' (see `resolve_io/0').
-spec resolve(pid(), binary(), pos_integer(), resolve_io()) -> {ok, resolved()} | {error, term()}.
resolve(Pool, AppMri, TimeoutMs, Io) when is_binary(AppMri), is_integer(TimeoutMs), TimeoutMs > 0 ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    app_parts(macula_mri:validate(AppMri), macula_mri:parse(AppMri), {Pool, resolve_io(Io)}, Deadline).

resolve_io(Io) ->
    #{find_records => maps:get(find_records, Io, fun macula:find_records/3),
      realm_key => maps:get(realm_key, Io, fun macula_client:realm_key/2),
      providers => maps:get(providers, Io, fun macula_direct_dial:providers/4)}.

app_parts(ok, {ok, #{type := app, realm := RealmName, path := [OrgName, AppName]}},
          {Pool, #{find_records := FindRecords}} = Lookup, Deadline) ->
    RealmId = macula_realm:id(RealmName),
    Records = FindRecords(Pool, macula_record:app_key(RealmId, OrgName, AppName), remaining(Deadline)),
    app_record(Records, {RealmId, OrgName, AppName}, trust(Lookup, RealmId), Lookup, Deadline);
app_parts(ok, {ok, _NotAnApp}, _Lookup, _Deadline) ->
    {error, {invalid_app_mri, not_an_app}};
app_parts({error, Reason}, _Parsed, _Lookup, _Deadline) ->
    {error, {invalid_app_mri, Reason}}.

app_record({ok, Records}, Named, Trust, Lookup, Deadline) ->
    Apps = [R || R <- Records, macula_record:type(R) =:= ?TYPE_APP_RECORD, names(R, Named)],
    Checked = [{R, macula_record:verify_app(R, Trust, erlang:system_time(millisecond))} || R <- Apps],
    trusted([R || {R, ok} <- Checked], [Why || {_R, {error, Why}} <- Checked], Lookup, Deadline);
app_record({error, not_found}, _Named, _Trust, _Lookup, _Deadline) ->
    {error, {unresolved, app_not_registered}};
app_record({error, Reason}, _Named, _Trust, _Lookup, _Deadline) ->
    {error, {unresolved, Reason}}.

%% The slot is a multiset: a record stored there must still name this app.
names(Record, {RealmId, OrgName, AppName}) ->
    #{realm_id := R, org_name := O, app_name := A} = macula_record:read_app_record(Record),
    {R, O, A} =:= {RealmId, OrgName, AppName}.

trusted([], [], _Lookup, _Deadline) ->
    {error, {unresolved, app_not_registered}};
trusted([], Refusals, _Lookup, _Deadline) ->
    {error, {unresolved, {no_trusted_app_record, lists:usort(Refusals)}}};
trusted(Trusted, _Refusals, Lookup, Deadline) ->
    Newest = lists:last(lists:sort(fun(A, B) -> macula_record:created_at(A) =< macula_record:created_at(B) end,
                                   Trusted)),
    #{realm_id := RealmId, org_name := Org, app_name := App, version := Version, services := Services} =
        macula_record:read_app_record(Newest),
    {ok, #{app => #{realm_id => RealmId, org_name => Org, app_name => App, version => Version},
           services => [#{name => Name, procedures => [served(Lookup, RealmId, P, Deadline) || P <- Procedures]}
                        || #{name := Name, procedures := Procedures} <- Services]}}.

served({Pool, #{providers := Providers}}, RealmId, Procedure, Deadline) ->
    with_providers(Procedure, Providers(Pool, RealmId, Procedure, remaining(Deadline))).

with_providers(Procedure, {ok, Providers}) ->
    #{procedure => Procedure, providers => Providers};
with_providers(Procedure, {error, {unresolved, Reason}}) ->
    #{procedure => Procedure, providers => [], unresolved => Reason};
with_providers(Procedure, {error, Reason}) ->
    #{procedure => Procedure, providers => [], unresolved => Reason}.

%% What an app record is checked against: the node's crypto profile and the
%% realm key the pool pinned for the realm, as for a call's advertisements.
trust({Pool, #{realm_key := RealmKey}}, RealmId) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    with_realm_key(RealmKey(Pool, RealmId), #{profile => Profile}).

with_realm_key({ok, RealmKey}, Trust) -> Trust#{realm_key => RealmKey};
with_realm_key(none, Trust) -> Trust.

%% What is left of the deadline, at least 1 ms so a late lookup still answers.
remaining(Deadline) ->
    max(1, Deadline - erlang:monotonic_time(millisecond)).
