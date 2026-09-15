%%%-------------------------------------------------------------------
%%% @doc The statement issuers of this node's pools, one for each pool.
%%%
%%% A pool starts its own macula_statement_issuer here, for the node identity
%%% key it holds, with itself as the owner. A child is temporary: it ends with
%%% its owner and this supervisor never restarts it, so a pool that loses its
%%% issuer starts a new one itself. The identity key reaches the issuer as a
%%% function that returns it, so no start argument this supervisor keeps, and
%%% no report of a refused start, holds the key.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_statement_issuer_sup).

-behaviour(supervisor).

-export([start_link/0, start_issuer/2]).
-export([init/1]).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Start an issuer for the identity key `Identity' returns, owned by `Owner'.
-spec start_issuer(fun(() -> macula_node_keys:node_key()), pid()) -> {ok, pid()} | {error, term()}.
start_issuer(Identity, Owner) when is_function(Identity, 0), is_pid(Owner) ->
    supervisor:start_child(?MODULE, [#{identity => Identity, owner => Owner}]).

init([]) ->
    {ok, {#{strategy => simple_one_for_one, intensity => 10, period => 5},
          [#{id => macula_statement_issuer,
             start => {macula_statement_issuer, start_link, []},
             restart => temporary,
             shutdown => 5000,
             type => worker,
             modules => [macula_statement_issuer]}]}}.
