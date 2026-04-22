%%%-------------------------------------------------------------------
%%% @doc Mesh topic construction and validation.
%%%
%%% Every mesh topic follows a strict 5-segment structure:
%%%
%%%   {realm}/{publisher}/{publisher}/{domain}/{name}_v{N}
%%%
%%% The two publisher slots carry different values depending on the
%%% topic's ownership tier. Three tiers exist:
%%%
%%%   realm: {realm}/_realm/_realm/{domain}/{name}_v{N}
%%%   org:   {realm}/{org}/_org/{domain}/{name}_v{N}
%%%   app:   {realm}/{org}/{app}/{domain}/{name}_v{N}
%%%
%%% Tier answers: who owns the topic's schema and authority?
%%%
%%%   realm — realm authority (membership, identity, ban)
%%%   org   — org-spanning concept (licensing, billing)
%%%   app   — app-internal (game state, RPCs, app events)
%%%
%%% Use realm_fact / org_fact / app_fact for pub/sub topics
%%% (past tense names — something happened).
%%%
%%% Use realm_hope / org_hope / app_hope for RPC procedure names
%%% (present tense names — we want something to happen).
%%%
%%% System topics (with _mesh. prefix) are infrastructure-owned,
%%% dot-separated, and exempt from this 5-segment structure.
%%%
%%% Full guide: docs/guides/TOPIC_NAMING_GUIDE.md
%%% @end
%%%-------------------------------------------------------------------
-module(macula_topic).

-export([
    realm_fact/4, realm_hope/4,
    org_fact/5,   org_hope/5,
    app_fact/6,   app_hope/6,
    build/6,
    parse/1,
    validate/1,
    is_system_topic/1
]).

-type tier()    :: realm | org | app.
-type realm()   :: binary().
-type org()     :: binary().
-type app()     :: binary().
-type domain()  :: binary().
-type name()    :: binary().
-type version() :: pos_integer().
-type topic()   :: binary().

-export_type([tier/0, realm/0, org/0, app/0, domain/0, name/0, version/0, topic/0]).

-define(SENTINEL_REALM, <<"_realm">>).
-define(SENTINEL_ORG,   <<"_org">>).

%%%===================================================================
%%% Builders — realm tier
%%%===================================================================

%% @doc Build a realm-tier fact (pub/sub) topic.
%% Realm authority owns the schema. Publisher and subscriber slots
%% carry the _realm sentinel.
-spec realm_fact(realm(), domain(), name(), version()) -> topic().
realm_fact(Realm, Domain, Name, Version) ->
    build(Realm, ?SENTINEL_REALM, ?SENTINEL_REALM, Domain, Name, Version).

%% @doc Build a realm-tier hope (RPC) procedure name.
-spec realm_hope(realm(), domain(), name(), version()) -> topic().
realm_hope(Realm, Domain, Name, Version) ->
    build(Realm, ?SENTINEL_REALM, ?SENTINEL_REALM, Domain, Name, Version).

%%%===================================================================
%%% Builders — org tier
%%%===================================================================

%% @doc Build an org-tier fact topic.
%% An org owns the schema across multiple of its apps.
%% The app slot carries the _org sentinel.
-spec org_fact(realm(), org(), domain(), name(), version()) -> topic().
org_fact(Realm, Org, Domain, Name, Version) ->
    build(Realm, Org, ?SENTINEL_ORG, Domain, Name, Version).

%% @doc Build an org-tier hope procedure name.
-spec org_hope(realm(), org(), domain(), name(), version()) -> topic().
org_hope(Realm, Org, Domain, Name, Version) ->
    build(Realm, Org, ?SENTINEL_ORG, Domain, Name, Version).

%%%===================================================================
%%% Builders — app tier
%%%===================================================================

%% @doc Build an app-tier fact topic.
%% A specific app owns the schema. All slots carry real values.
-spec app_fact(realm(), org(), app(), domain(), name(), version()) -> topic().
app_fact(Realm, Org, App, Domain, Name, Version) ->
    build(Realm, Org, App, Domain, Name, Version).

%% @doc Build an app-tier hope procedure name.
-spec app_hope(realm(), org(), app(), domain(), name(), version()) -> topic().
app_hope(Realm, Org, App, Domain, Name, Version) ->
    build(Realm, Org, App, Domain, Name, Version).

%%%===================================================================
%%% Lower-level
%%%===================================================================

%% @doc Build a topic from explicit segments.
%% Validates each segment and the publisher-slot tier combination.
%% Prefer the tier-specific builders (realm_fact, org_fact, app_fact).
-spec build(realm(), org() | binary(), app() | binary(),
            domain(), name(), version()) -> topic().
build(Realm, Org, App, Domain, Name, Version)
  when is_binary(Realm), is_binary(Org), is_binary(App),
       is_binary(Domain), is_binary(Name),
       is_integer(Version), Version > 0 ->
    validate_segment(realm, Realm),
    validate_publisher_slots(Org, App),
    validate_segment(domain, Domain),
    validate_segment(name, Name),
    Vsn = integer_to_binary(Version),
    <<Realm/binary, "/", Org/binary, "/", App/binary, "/",
      Domain/binary, "/", Name/binary, "_v", Vsn/binary>>.

%% @doc Parse a topic into its constituent parts and tier.
%% Returns an error tuple for any non-canonical topic. System topics
%% (with _mesh. prefix) are not canonical and will return an error.
-spec parse(topic()) -> {ok, map()} | {error, term()}.
parse(Topic) when is_binary(Topic) ->
    parse_segments(binary:split(Topic, <<"/">>, [global]), Topic).

%% @doc Validate a topic string. Accepts canonical 5-segment topics
%% AND system topics (_mesh. prefix infrastructure events).
-spec validate(topic()) -> ok | {error, term()}.
validate(Topic) when is_binary(Topic) ->
    validate_dispatch(is_system_topic(Topic), Topic).

%% @doc Check if a topic is a system topic. System topics use
%% underscore prefix and dot separator (e.g. _mesh.node.up).
%% They are infrastructure-owned and exempt from the canonical
%% 5-segment structure.
-spec is_system_topic(binary()) -> boolean().
is_system_topic(<<"_mesh.", _/binary>>) -> true;
is_system_topic(_) -> false.

%%%===================================================================
%%% Internal — parse
%%%===================================================================

parse_segments([Realm, Org, App, Domain, NameVsn], Topic) ->
    parse_name_version(Realm, Org, App, Domain, NameVsn, Topic);
parse_segments(_Other, Topic) ->
    {error, {invalid_structure, Topic}}.

parse_name_version(Realm, Org, App, Domain, NameVsn, Topic) ->
    parse_with_version(parse_version_suffix(NameVsn),
                       Realm, Org, App, Domain, NameVsn, Topic).

parse_with_version({ok, Name, Version}, Realm, Org, App, Domain, NameVsn, _Topic) ->
    build_parsed(infer_tier(Org, App), Realm, Org, App, Domain, Name, NameVsn, Version);
parse_with_version({error, _} = Err, _Realm, _Org, _App, _Domain, _NameVsn, _Topic) ->
    Err.

build_parsed({ok, Tier}, Realm, Org, App, Domain, Name, NameVsn, Version) ->
    {ok, build_parsed_map(Tier, Realm, Org, App, Domain, Name, NameVsn, Version)};
build_parsed({error, _} = Err, _R, _O, _A, _D, _N, _NV, _V) ->
    Err.

build_parsed_map(realm, Realm, _Org, _App, Domain, Name, NameVsn, Version) ->
    #{
        tier    => realm,
        realm   => Realm,
        domain  => Domain,
        name    => Name,
        version => Version,
        topic   => rebuild_topic(Realm, ?SENTINEL_REALM, ?SENTINEL_REALM, Domain, NameVsn)
    };
build_parsed_map(org, Realm, Org, _App, Domain, Name, NameVsn, Version) ->
    #{
        tier    => org,
        realm   => Realm,
        org     => Org,
        domain  => Domain,
        name    => Name,
        version => Version,
        topic   => rebuild_topic(Realm, Org, ?SENTINEL_ORG, Domain, NameVsn)
    };
build_parsed_map(app, Realm, Org, App, Domain, Name, NameVsn, Version) ->
    #{
        tier    => app,
        realm   => Realm,
        org     => Org,
        app     => App,
        domain  => Domain,
        name    => Name,
        version => Version,
        topic   => rebuild_topic(Realm, Org, App, Domain, NameVsn)
    }.

rebuild_topic(Realm, Org, App, Domain, NameVsn) ->
    <<Realm/binary, "/", Org/binary, "/", App/binary, "/",
      Domain/binary, "/", NameVsn/binary>>.

parse_version_suffix(NameVsn) ->
    suffix_match(re:run(NameVsn, <<"^(.+)_v([0-9]+)$">>, [{capture, [1, 2], binary}]),
                 NameVsn).

suffix_match({match, [Name, VsnBin]}, _NameVsn) ->
    {ok, Name, binary_to_integer(VsnBin)};
suffix_match(nomatch, NameVsn) ->
    {error, {missing_version_suffix, NameVsn}}.

%% Infer tier from publisher slots. Used by parse/1.
%% Mismatched sentinel combinations are rejected.
infer_tier(?SENTINEL_REALM, ?SENTINEL_REALM) -> {ok, realm};
infer_tier(?SENTINEL_REALM, _App)            -> {error, {mismatched_realm_sentinel, app_must_be_realm_too}};
infer_tier(_Org, ?SENTINEL_REALM)            -> {error, {mismatched_realm_sentinel, org_must_be_realm_too}};
infer_tier(?SENTINEL_ORG, _App)              -> {error, {misplaced_org_sentinel, only_in_app_slot}};
infer_tier(Org, ?SENTINEL_ORG)               -> validate_org_for_tier(Org, org);
infer_tier(Org, App)                         -> validate_org_app_for_tier(Org, App).

validate_org_for_tier(Org, Tier) ->
    case is_valid_segment(Org) of
        true  -> {ok, Tier};
        false -> {error, {invalid_segment, org, Org}}
    end.

validate_org_app_for_tier(Org, App) ->
    case {is_valid_segment(Org), is_valid_segment(App)} of
        {true, true}  -> {ok, app};
        {false, _}    -> {error, {invalid_segment, org, Org}};
        {true, false} -> {error, {invalid_segment, app, App}}
    end.

%%%===================================================================
%%% Internal — validate
%%%===================================================================

validate_dispatch(true, _Topic)   -> ok;
validate_dispatch(false, Topic)   -> validate_canonical(parse(Topic)).

validate_canonical({ok, _})       -> ok;
validate_canonical({error, _} = E) -> E.

%%%===================================================================
%%% Internal — segment validation (build path)
%%%===================================================================

%% Publisher-slot validation enforces tier sentinel rules at build time.
%% Pattern-match order matters: sentinel combinations first, then real.
validate_publisher_slots(?SENTINEL_REALM, ?SENTINEL_REALM) -> ok;
validate_publisher_slots(?SENTINEL_REALM, _) ->
    error({invalid_tier_combination, app_must_be_realm_when_org_realm});
validate_publisher_slots(_, ?SENTINEL_REALM) ->
    error({invalid_tier_combination, org_must_be_realm_when_app_realm});
validate_publisher_slots(?SENTINEL_ORG, _) ->
    error({invalid_tier_combination, org_sentinel_only_in_app_slot});
validate_publisher_slots(Org, ?SENTINEL_ORG) ->
    validate_segment(org, Org);
validate_publisher_slots(Org, App) ->
    validate_segment(org, Org),
    validate_segment(app, App).

validate_segment(Label, Segment) ->
    case is_valid_segment(Segment) of
        true  -> ok;
        false -> error({invalid_segment, Label, Segment})
    end.

is_valid_segment(Segment) when is_binary(Segment) ->
    case re:run(Segment, <<"^[a-z0-9][a-z0-9._-]*$">>) of
        {match, _} -> true;
        nomatch    -> false
    end;
is_valid_segment(_) ->
    false.
