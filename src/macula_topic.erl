%%%-------------------------------------------------------------------
%%% @doc Mesh topic construction and validation.
%%%
%%% Every mesh topic follows a strict 5-segment structure:
%%%
%%%   {realm}/{org}/{app}/{domain}/{name}_v{N}
%%%
%%% Facts (past tense) are published via pub/sub.
%%% Hopes (present tense) are advertised via RPC.
%%%
%%% The verb tense determines intent:
%%%   lobby_opened_v1    → fact (something happened)
%%%   join_game_v1       → hope (we want something to happen)
%%%
%%% System topics (_mesh.node.up) are exempt from this structure.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_topic).

-export([
    fact/5,
    hope/5,
    build/5,
    parse/1,
    validate/1,
    is_system_topic/1
]).

-type realm()   :: binary().
-type app_id()  :: binary().  %% "org/app" format
-type domain()  :: binary().
-type name()    :: binary().
-type version() :: pos_integer().

-export_type([realm/0, app_id/0, domain/0, name/0, version/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Build a fact topic (pub/sub — something that happened).
%% Name should be past tense: subject_past_verb.
-spec fact(realm(), app_id(), domain(), name(), version()) -> binary().
fact(Realm, AppId, Domain, Name, Version) ->
    build(Realm, AppId, Domain, Name, Version).

%% @doc Build a hope topic (RPC — something we want to happen).
%% Name should be present tense: verb_subject.
-spec hope(realm(), app_id(), domain(), name(), version()) -> binary().
hope(Realm, AppId, Domain, Name, Version) ->
    build(Realm, AppId, Domain, Name, Version).

%% @doc Build a topic from parts.
%% AppId must be "org/app" format (exactly one slash).
-spec build(realm(), app_id(), domain(), name(), version()) -> binary().
build(Realm, AppId, Domain, Name, Version)
  when is_binary(Realm), is_binary(AppId),
       is_binary(Domain), is_binary(Name),
       is_integer(Version), Version > 0 ->
    validate_segment(realm, Realm),
    validate_app_id(AppId),
    validate_segment(domain, Domain),
    validate_segment(name, Name),
    Vsn = integer_to_binary(Version),
    <<Realm/binary, "/", AppId/binary, "/",
      Domain/binary, "/", Name/binary, "_v", Vsn/binary>>.

%% @doc Parse a topic into its constituent parts.
-spec parse(binary()) -> {ok, map()} | {error, term()}.
parse(Topic) when is_binary(Topic) ->
    case binary:split(Topic, <<"/">>, [global]) of
        [Realm, Org, App, Domain, NameVsn] ->
            parse_name_version(Realm, Org, App, Domain, NameVsn);
        _ ->
            {error, {invalid_structure, Topic}}
    end.

%% @doc Validate a topic string.
-spec validate(binary()) -> ok | {error, term()}.
validate(Topic) ->
    case is_system_topic(Topic) of
        true -> ok;
        false ->
            case parse(Topic) of
                {ok, _} -> ok;
                {error, _} = Err -> Err
            end
    end.

%% @doc Check if a topic is a system topic (underscore prefix).
-spec is_system_topic(binary()) -> boolean().
is_system_topic(<<"_", _/binary>>) -> true;
is_system_topic(_) -> false.

%%%===================================================================
%%% Internal
%%%===================================================================

parse_name_version(Realm, Org, App, Domain, NameVsn) ->
    case parse_version_suffix(NameVsn) of
        {ok, Name, Version} ->
            {ok, #{
                realm => Realm,
                org => Org,
                app => App,
                app_id => <<Org/binary, "/", App/binary>>,
                domain => Domain,
                name => Name,
                version => Version,
                topic => <<Realm/binary, "/", Org/binary, "/", App/binary, "/",
                           Domain/binary, "/", NameVsn/binary>>
            }};
        {error, _} = Err ->
            Err
    end.

parse_version_suffix(NameVsn) ->
    case re:run(NameVsn, <<"^(.+)_v([0-9]+)$">>, [{capture, [1, 2], binary}]) of
        {match, [Name, VsnBin]} ->
            {ok, Name, binary_to_integer(VsnBin)};
        nomatch ->
            {error, {missing_version_suffix, NameVsn}}
    end.

validate_app_id(AppId) ->
    case binary:split(AppId, <<"/">>) of
        [Org, App] when byte_size(Org) > 0, byte_size(App) > 0 ->
            validate_segment(org, Org),
            validate_segment(app, App),
            ok;
        _ ->
            error({invalid_app_id, AppId})
    end.

validate_segment(Label, Segment) ->
    case re:run(Segment, <<"^[a-z0-9][a-z0-9._-]*$">>) of
        {match, _} -> ok;
        nomatch -> error({invalid_segment, Label, Segment})
    end.
