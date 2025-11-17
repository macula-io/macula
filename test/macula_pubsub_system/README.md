# Macula Pubsub Subsystem Tests

This directory contains all tests for the pubsub subsystem.

## Test Files

```
- macula_pubsub_dht_tests.erl
- macula_pubsub_handler_comprehensive_tests.erl
- macula_pubsub_handler_tests.erl
- macula_pubsub_qos_tests.erl
- macula_pubsub_routing_tests.erl
- macula_pubsub_server_tests.erl
- macula_pubsub_subscription_tests.erl
- macula_pubsub_topic_tests.erl
```

## Running Tests

```bash
# Run all pubsub tests
rebar3 eunit --dir=test/macula_pubsub_system

# Run specific test
rebar3 eunit --module=macula_pubsub_<module>_tests
```
