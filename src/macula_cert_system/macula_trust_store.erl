%%%-------------------------------------------------------------------
%%% @doc Macula Certificate Trust Store
%%%
%%% ETS-based storage for trusted realm certificates.
%%% Manages trust decisions for self-sovereign certificates.
%%%
%%% Trust Model:
%%% - Realm certificates are added to the trust store explicitly
%%% - Instance certificates are verified against their realm's certificate
%%% - Trust-on-first-use (TOFU) can be enabled for automatic trust
%%%
%%% Example usage:
%%% ```
%%% %% Start the trust store
%%% {ok, Pid} = macula_trust_store:start_link(),
%%%
%%% %% Add a trusted realm certificate
%%% ok = macula_trust_store:add_trusted_realm(RealmDID, RealmCert),
%%%
%%% %% Check if a realm is trusted
%%% true = macula_trust_store:is_trusted(RealmDID),
%%%
%%% %% Get the certificate for verification
%%% {ok, RealmCert} = macula_trust_store:get_realm_cert(RealmDID),
%%%
%%% %% Verify an instance certificate
%%% ok = macula_trust_store:verify_instance_cert(InstanceCert).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_trust_store).

-behaviour(gen_server).

-include("macula_cert.hrl").

%% API
-export([start_link/0, start_link/1]).
-export([add_trusted_realm/2, add_trusted_realm/3]).
-export([remove_trusted_realm/1]).
-export([is_trusted/1]).
-export([get_realm_cert/1]).
-export([list_trusted/0]).
-export([verify_instance_cert/1]).
-export([clear_all/0]).
-export([count/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% ETS table name
-define(TABLE, macula_trust_store).

%% Default options
-define(DEFAULT_OPTS, #{
    tofu => false,           %% Trust-on-first-use disabled by default
    persist => false         %% No persistence by default
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the trust store with default options
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(?DEFAULT_OPTS).

%% @doc Start the trust store with custom options
-spec start_link(Opts :: map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Add a realm certificate to the trust store
-spec add_trusted_realm(RealmDID :: binary(), Cert :: macula_cert()) ->
    ok | {error, term()}.
add_trusted_realm(RealmDID, Cert) ->
    add_trusted_realm(RealmDID, Cert, <<>>).

%% @doc Add a realm certificate with optional notes
-spec add_trusted_realm(RealmDID :: binary(), Cert :: macula_cert(), Notes :: binary()) ->
    ok | {error, term()}.
add_trusted_realm(RealmDID, Cert, Notes) ->
    gen_server:call(?MODULE, {add_trusted, RealmDID, Cert, Notes}).

%% @doc Remove a realm from the trust store
-spec remove_trusted_realm(RealmDID :: binary()) -> ok | {error, not_found}.
remove_trusted_realm(RealmDID) ->
    gen_server:call(?MODULE, {remove_trusted, RealmDID}).

%% @doc Check if a realm DID is in the trust store
-spec is_trusted(RealmDID :: binary()) -> boolean().
is_trusted(RealmDID) ->
    case ets:lookup(?TABLE, RealmDID) of
        [_] -> true;
        [] -> false
    end.

%% @doc Get the certificate for a trusted realm
-spec get_realm_cert(RealmDID :: binary()) -> {ok, macula_cert()} | {error, not_found}.
get_realm_cert(RealmDID) ->
    case ets:lookup(?TABLE, RealmDID) of
        [{_, Entry}] ->
            {ok, Entry#trust_entry.cert};
        [] ->
            {error, not_found}
    end.

%% @doc List all trusted realm DIDs
-spec list_trusted() -> [binary()].
list_trusted() ->
    ets:foldl(
        fun({DID, _Entry}, Acc) -> [DID | Acc] end,
        [],
        ?TABLE
    ).

%% @doc Verify an instance certificate against the trust store
%% Looks up the issuer's realm certificate and verifies the chain.
-spec verify_instance_cert(Cert :: macula_cert()) -> ok | {error, term()}.
verify_instance_cert(Cert) ->
    #macula_cert{issuer_did = IssuerDID} = Cert,
    case get_realm_cert(IssuerDID) of
        {ok, RealmCert} ->
            macula_cert:verify_cert(Cert, RealmCert);
        {error, not_found} ->
            {error, {issuer_not_trusted, IssuerDID}}
    end.

%% @doc Clear all trusted realms (use with caution!)
-spec clear_all() -> ok.
clear_all() ->
    gen_server:call(?MODULE, clear_all).

%% @doc Get the count of trusted realms
-spec count() -> non_neg_integer().
count() ->
    ets:info(?TABLE, size).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init(Opts) ->
    %% Create ETS table
    ?TABLE = ets:new(?TABLE, [
        named_table,
        public,
        {read_concurrency, true},
        {keypos, 1}
    ]),

    %% Load persisted trust entries if enabled
    case maps:get(persist, Opts, false) of
        true ->
            load_persisted_entries(Opts);
        false ->
            ok
    end,

    {ok, #{opts => Opts}}.

%% @private
handle_call({add_trusted, RealmDID, Cert, Notes}, _From, State) ->
    Result = do_add_trusted(RealmDID, Cert, Notes),
    {reply, Result, State};

handle_call({remove_trusted, RealmDID}, _From, State) ->
    Result = case ets:lookup(?TABLE, RealmDID) of
        [_] ->
            ets:delete(?TABLE, RealmDID),
            ok;
        [] ->
            {error, not_found}
    end,
    {reply, Result, State};

handle_call(clear_all, _From, State) ->
    ets:delete_all_objects(?TABLE),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Add a trusted realm after validation
-spec do_add_trusted(RealmDID :: binary(), Cert :: macula_cert(), Notes :: binary()) ->
    ok | {error, term()}.
do_add_trusted(RealmDID, Cert, Notes) ->
    #macula_cert{subject_did = SubjectDID} = Cert,

    %% Verify the certificate is for the claimed realm
    case SubjectDID =:= RealmDID of
        true ->
            %% Verify it's a valid self-signed certificate
            case macula_cert:verify_self_signed(Cert) of
                ok ->
                    Entry = #trust_entry{
                        realm_did = RealmDID,
                        cert = Cert,
                        added_at = erlang:system_time(second),
                        verified = true,
                        notes = Notes
                    },
                    ets:insert(?TABLE, {RealmDID, Entry}),
                    ok;
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, {did_mismatch, RealmDID, SubjectDID}}
    end.

%% @private Load persisted trust entries from disk
-spec load_persisted_entries(Opts :: map()) -> ok.
load_persisted_entries(Opts) ->
    case maps:get(persist_path, Opts, undefined) of
        undefined ->
            ok;
        Path ->
            case file:read_file(Path) of
                {ok, Binary} ->
                    try
                        Entries = binary_to_term(Binary, [safe]),
                        lists:foreach(
                            fun({DID, Entry}) ->
                                ets:insert(?TABLE, {DID, Entry})
                            end,
                            Entries
                        )
                    catch
                        _:_ -> ok
                    end;
                {error, _} ->
                    ok
            end
    end.
