%%%-------------------------------------------------------------------
%%% @doc
%%% Macula application-wide configuration constants.
%%%
%%% This header provides a single source of truth for timeouts,
%%% limits, and default values used across the Macula codebase.
%%%
%%% To use: -include("macula_config.hrl").
%%% @end
%%%-------------------------------------------------------------------

%%%===================================================================
%%% Network Timeouts (milliseconds)
%%%===================================================================

%% General network operation timeout
-define(NETWORK_TIMEOUT_MS, 5000).        % 5 seconds

%% RPC-specific timeouts
-define(RPC_CALL_TIMEOUT_MS, 5000).       % 5 seconds - default RPC call timeout
-define(RPC_LONG_CALL_TIMEOUT_MS, 30000). % 30 seconds - long-running RPC calls

%% DHT operation timeouts
-define(DHT_QUERY_TIMEOUT_MS, 5000).      % 5 seconds - DHT lookup operations

%% QoS acknowledgment timeout
-define(PUBACK_TIMEOUT_MS, 5000).         % 5 seconds - QoS 1 acknowledgment

%% Connection timeout
-define(CONNECTION_TIMEOUT_MS, 10000).    % 10 seconds - initial connection

%%%===================================================================
%%% Retry Configuration
%%%===================================================================

-define(PUBACK_MAX_RETRIES, 3).           % Maximum retries for QoS 1 messages
-define(RPC_MAX_FAILOVER_ATTEMPTS, 3).   % Maximum RPC failover attempts

%%%===================================================================
%%% DHT Configuration
%%%===================================================================

-define(DHT_TTL_SECONDS, 300).            % 5 minutes - DHT advertisement TTL
-define(DHT_CACHE_TTL_SECONDS, 60).       % 1 minute - local cache TTL
-define(DHT_K_PARAMETER, 20).             % Kademlia K (bucket size)
-define(DHT_ALPHA_PARAMETER, 3).          % Concurrent DHT queries
-define(DHT_MAX_HOPS, 10).                % Maximum routing hops

%%%===================================================================
%%% Port Defaults
%%%===================================================================

-define(DEFAULT_GATEWAY_PORT, 9443).      % Main gateway QUIC port
-define(DEFAULT_HEALTH_PORT, 8080).       % Health check HTTP port

%%%===================================================================
%%% Application Defaults
%%%===================================================================

-define(DEFAULT_REALM, <<"macula.default">>).  % Default realm name

%%%===================================================================
%%% Legacy Aliases (for backward compatibility during migration)
%%%===================================================================

%% These aliases point to the canonical definitions above.
%% Use the canonical names in new code.

-define(DEFAULT_TIMEOUT, ?NETWORK_TIMEOUT_MS).           % Legacy alias
-define(DEFAULT_CALL_TIMEOUT, ?RPC_CALL_TIMEOUT_MS).     % Legacy alias
-define(CALL_TIMEOUT, ?RPC_LONG_CALL_TIMEOUT_MS).        % Legacy alias
-define(PUBACK_TIMEOUT, ?PUBACK_TIMEOUT_MS).             % Legacy alias
-define(DHT_QUERY_TIMEOUT, ?DHT_QUERY_TIMEOUT_MS).       % Legacy alias
-define(DEFAULT_TTL, ?DHT_TTL_SECONDS).                  % Legacy alias
-define(CACHE_TTL, ?DHT_CACHE_TTL_SECONDS).              % Legacy alias
-define(DEFAULT_MAX_HOPS, ?DHT_MAX_HOPS).                % Legacy alias
-define(DEFAULT_PORT, ?DEFAULT_GATEWAY_PORT).            % Legacy alias
