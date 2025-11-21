%% @doc Behaviour defining the client API contract for Macula connections
%%
%% This behaviour defines the standard interface that all client implementations
%% must provide, whether connecting via QUIC (macula_client) or local process
%% communication (macula_local_client).
%%
%% By defining this as a behaviour, we ensure compile-time verification that
%% both implementations maintain API compatibility.
%%
%% @end
-module(macula_client_behaviour).

%% Connection management callbacks
-callback connect(Opts :: map(), EventHandler :: pid()) ->
    {ok, pid()} | {error, term()}.

-callback connect_local(Opts :: map()) ->
    {ok, pid()} | {error, term()}.

-callback disconnect(Client :: pid()) ->
    ok.

%% Pub/Sub callbacks
-callback publish(Client :: pid(), Topic :: binary(), Payload :: map()) ->
    ok | {error, term()}.

-callback publish(Client :: pid(), Topic :: binary(), Payload :: map(), Opts :: map()) ->
    ok | {error, term()}.

-callback subscribe(Client :: pid(), Topic :: binary(), Handler :: pid()) ->
    {ok, reference()} | {error, term()}.

-callback unsubscribe(Client :: pid(), SubRef :: reference()) ->
    ok | {error, term()}.

-callback discover_subscribers(Client :: pid(), Topic :: binary()) ->
    {ok, [binary()]} | {error, term()}.

%% RPC callbacks
-callback call(Client :: pid(), Procedure :: binary(), Args :: list()) ->
    {ok, term()} | {error, term()}.

-callback call(Client :: pid(), Procedure :: binary(), Args :: list(), Opts :: map()) ->
    {ok, term()} | {error, term()}.

-callback advertise(Client :: pid(), Procedure :: binary(), Handler :: fun()) ->
    ok | {error, term()}.

-callback advertise(Client :: pid(), Procedure :: binary(), Handler :: fun(), Opts :: map()) ->
    ok | {error, term()}.

-callback unadvertise(Client :: pid(), Procedure :: binary()) ->
    ok | {error, term()}.

%% Utility callbacks
-callback get_node_id(Client :: pid()) ->
    {ok, binary()} | {error, term()}.
