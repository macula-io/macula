# Macula Rpc Subsystem Tests

This directory contains all tests for the rpc subsystem.

## Test Files

```
- macula_rpc_handler_tests.erl
- macula_rpc_routing_tests.erl
- macula_rpc_server_tests.erl
```

## Running Tests

```bash
# Run all rpc tests
rebar3 eunit --dir=test/macula_rpc_system

# Run specific test
rebar3 eunit --module=macula_rpc_<module>_tests
```
