# Macula Peer Subsystem Tests

This directory contains all tests for the peer subsystem.

## Test Files

```
- macula_peer_system_tests.erl
- macula_peer_tests.erl
```

## Running Tests

```bash
# Run all peer tests
rebar3 eunit --dir=test/macula_peer_system

# Run specific test
rebar3 eunit --module=macula_peer_<module>_tests
```
