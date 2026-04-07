%%%-------------------------------------------------------------------
%%% @doc
%%% Shared record definition for macula_connection and its dispatch module.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(MACULA_CONNECTION_HRL).
-define(MACULA_CONNECTION_HRL, true).

-record(state, {
    url :: binary(),
    opts :: map(),
    node_id :: binary(),
    realm :: binary(),
    peer_id :: integer(),  %% Unique peer system identifier for gproc lookups
    connection :: pid() | undefined,
    stream :: pid() | undefined,
    status = connecting :: connecting | connected | disconnected | error,
    recv_buffer = <<>> :: binary(),
    keepalive_timer :: reference() | undefined,
    connect_in_flight = false :: boolean(),
    retry_delay_ms = 2000 :: pos_integer()
}).

-endif.
