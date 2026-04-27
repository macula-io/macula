%%%-------------------------------------------------------------------
%%% @doc MRI Storage Behaviour
%%%
%%% Defines the interface for MRI storage adapters.
%%% Implementations must provide registration, lookup, and hierarchy queries.
%%%
%%% Example adapters:
%%% - macula_mri_khepri (distributed Raft-consensus via Khepri)
%%% - macula_mri_ets (in-memory, for development/testing)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_mri_store).

-compile({no_auto_import, [register/2]}).

%% Behaviour definition
-callback register(MRI :: macula_mri:mri(), Metadata :: map()) ->
    ok | {error, term()}.

-callback lookup(MRI :: macula_mri:mri()) ->
    {ok, Metadata :: map()} | {error, not_found | term()}.

-callback update(MRI :: macula_mri:mri(), Metadata :: map()) ->
    ok | {error, term()}.

-callback delete(MRI :: macula_mri:mri()) ->
    ok | {error, term()}.

-callback exists(MRI :: macula_mri:mri()) ->
    boolean().

-callback list_children(MRI :: macula_mri:mri()) ->
    [macula_mri:mri()].

-callback list_descendants(MRI :: macula_mri:mri()) ->
    [macula_mri:mri()].

-callback list_by_type(Type :: macula_mri:mri_type(), Realm :: macula_mri:realm()) ->
    [macula_mri:mri()].

-callback list_by_realm(Realm :: macula_mri:realm()) ->
    [macula_mri:mri()].

%% Optional callbacks for bulk operations
-callback import(Entries :: [{macula_mri:mri(), map()}]) ->
    ok | {error, term()}.

-callback export() ->
    {ok, [{macula_mri:mri(), map()}]} | {error, term()}.

-optional_callbacks([import/1, export/0]).

%% API for working with configured adapter
-export([adapter/0, set_adapter/1]).
-export([register/2, lookup/1, update/2, delete/1, exists/1]).
-export([list_children/1, list_descendants/1, list_by_type/2, list_by_realm/1]).
-export([import/1, export/0]).

-define(ADAPTER_KEY, {?MODULE, adapter}).
-define(DEFAULT_ADAPTER, macula_mri_ets).

%%===================================================================
%% Adapter Management
%%===================================================================

%% @doc Get the configured storage adapter module.
-spec adapter() -> module().
adapter() ->
    case persistent_term:get(?ADAPTER_KEY, undefined) of
        undefined -> ?DEFAULT_ADAPTER;
        Adapter -> Adapter
    end.

%% @doc Set the storage adapter module.
-spec set_adapter(module()) -> ok.
set_adapter(Module) when is_atom(Module) ->
    persistent_term:put(?ADAPTER_KEY, Module),
    ok.

%%===================================================================
%% Delegating API
%%===================================================================

%% @doc Register an MRI with metadata.
-spec register(macula_mri:mri(), map()) -> ok | {error, term()}.
register(MRI, Metadata) ->
    (adapter()):register(MRI, Metadata).

%% @doc Look up an MRI's metadata.
-spec lookup(macula_mri:mri()) -> {ok, map()} | {error, not_found | term()}.
lookup(MRI) ->
    (adapter()):lookup(MRI).

%% @doc Update an MRI's metadata.
-spec update(macula_mri:mri(), map()) -> ok | {error, term()}.
update(MRI, Metadata) ->
    (adapter()):update(MRI, Metadata).

%% @doc Delete an MRI.
-spec delete(macula_mri:mri()) -> ok | {error, term()}.
delete(MRI) ->
    (adapter()):delete(MRI).

%% @doc Check if an MRI exists.
-spec exists(macula_mri:mri()) -> boolean().
exists(MRI) ->
    (adapter()):exists(MRI).

%% @doc List direct children of an MRI.
-spec list_children(macula_mri:mri()) -> [macula_mri:mri()].
list_children(MRI) ->
    (adapter()):list_children(MRI).

%% @doc List all descendants of an MRI.
-spec list_descendants(macula_mri:mri()) -> [macula_mri:mri()].
list_descendants(MRI) ->
    (adapter()):list_descendants(MRI).

%% @doc List all MRIs of a given type in a realm.
-spec list_by_type(macula_mri:mri_type(), macula_mri:realm()) -> [macula_mri:mri()].
list_by_type(Type, Realm) ->
    (adapter()):list_by_type(Type, Realm).

%% @doc List all MRIs in a realm.
-spec list_by_realm(macula_mri:realm()) -> [macula_mri:mri()].
list_by_realm(Realm) ->
    (adapter()):list_by_realm(Realm).

%% @doc Import multiple MRIs (bulk operation).
-spec import([{macula_mri:mri(), map()}]) -> ok | {error, term()}.
import(Entries) ->
    Adapter = adapter(),
    case erlang:function_exported(Adapter, import, 1) of
        true -> Adapter:import(Entries);
        false -> import_fallback(Entries)
    end.

%% @doc Export all MRIs (bulk operation).
-spec export() -> {ok, [{macula_mri:mri(), map()}]} | {error, term()}.
export() ->
    Adapter = adapter(),
    case erlang:function_exported(Adapter, export, 0) of
        true -> Adapter:export();
        false -> {error, not_implemented}
    end.

%%===================================================================
%% Internal
%%===================================================================

import_fallback(Entries) ->
    lists:foreach(
        fun({MRI, Metadata}) ->
            register(MRI, Metadata)
        end,
        Entries
    ),
    ok.
