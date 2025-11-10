#!/usr/bin/env bash
cd "$(dirname "$0")/.."
echo "Starting Bob's chat client..."
echo "Connecting to gateway at https://localhost:9443"
echo
erl -pa _build/default/lib/*/ebin \
    -eval "
    application:ensure_all_started(ssl),
    io:format(\"Bob's Client Ready!~n\"),
    io:format(\"Type messages to chat with Alice~n~n\"),
    shell:start_interactive()
    " \
    -noshell
