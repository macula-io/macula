%%%-------------------------------------------------------------------
%%% @doc Macula Certificate System Supervisor
%%%
%%% Supervises the certificate-related processes:
%%% - macula_trust_store: ETS-based trust store for realm certificates
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_cert_system).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).
-export([get_trust_store/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the certificate system with default options
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the certificate system with options
-spec start_link(Opts :: map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).

%% @doc Get the trust store process
-spec get_trust_store() -> pid() | undefined.
get_trust_store() ->
    whereis(macula_trust_store).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init(Opts) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    TrustStoreSpec = #{
        id => macula_trust_store,
        start => {macula_trust_store, start_link, [Opts]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [macula_trust_store]
    },

    {ok, {SupFlags, [TrustStoreSpec]}}.
