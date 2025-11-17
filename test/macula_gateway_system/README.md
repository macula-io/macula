# Macula Gateway Subsystem Tests

This directory contains all tests for the gateway subsystem.

## Test Files

```
- macula_gateway_clients_endpoint_tests.erl
- macula_gateway_clients_tests.erl
- macula_gateway_connection_tests.erl
- macula_gateway_dht_tests.erl
- macula_gateway_endpoint_tests.erl
- macula_gateway_keepalive_tests.erl
- macula_gateway_mesh_tests.erl
- macula_gateway_pubsub_dht_tests.erl
- macula_gateway_pubsub_regression_test.erl
- macula_gateway_pubsub_router_tests.erl
- macula_gateway_pubsub_tests.erl
- macula_gateway_quic_server_tests.erl
- macula_gateway_rpc_router_tests.erl
- macula_gateway_rpc_tests.erl
- macula_gateway_system_tests.erl
- macula_gateway_tests.erl
```

## Running Tests

```bash
# Run all gateway tests
rebar3 eunit --dir=test/macula_gateway_system

# Run specific test
rebar3 eunit --module=macula_gateway_<module>_tests
```
