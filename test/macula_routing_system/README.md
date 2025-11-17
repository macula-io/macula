# Macula Routing Subsystem Tests

This directory contains all tests for the routing subsystem.

## Test Files

```
- macula_routing_bucket_tests.erl
- macula_routing_dht_tests.erl
- macula_routing_nodeid_tests.erl
- macula_routing_server_tests.erl
- macula_routing_table_tests.erl
```

## Running Tests

```bash
# Run all routing tests
rebar3 eunit --dir=test/macula_routing_system

# Run specific test
rebar3 eunit --module=macula_routing_<module>_tests
```
