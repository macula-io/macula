#!/usr/bin/env escript
%%% @doc
%%% Simple test script to call the Macula Gateway diagnostics procedures
%%%
%%% Usage:
%%%   ./test_diagnostics.erl
%%% @end

main(_Args) ->
    io:format("~n=== Macula Gateway Diagnostics Test ===~n~n"),

    io:format("This demonstrates how to call the diagnostics procedures:~n"),
    io:format("  - com.macula.diagnostics.hello~n"),
    io:format("  - com.macula.diagnostics.echo~n"),
    io:format("  - com.macula.diagnostics.info~n~n"),

    io:format("To test with the Macula SDK:~n~n"),

    io:format("1. Start the gateway:~n"),
    io:format("   docker run -p 9443:9443 -p 8080:8080 macula/macula-gateway:latest~n~n"),

    io:format("2. Connect a client (Elixir example):~n"),
    io:format("   {:ok, client} = MaculaSdk.Client.start_link(~n"),
    io:format("     url: \"https://localhost:9443\",~n"),
    io:format("     realm: \"be.cortexiq.energy\",~n"),
    io:format("     node_id: \"test-client\"~n"),
    io:format("   )~n~n"),

    io:format("3. Call the hello procedure:~n"),
    io:format("   {:ok, result} = MaculaSdk.Client.call(~n"),
    io:format("     client,~n"),
    io:format("     \"com.macula.diagnostics.hello\",~n"),
    io:format("     %{}~n"),
    io:format("   )~n~n"),

    io:format("   Expected result:~n"),
    io:format("   %{~n"),
    io:format("     \"message\" => \"Hello from Macula Gateway!\",~n"),
    io:format("     \"gateway\" => \"macula@127.0.0.1\",~n"),
    io:format("     \"realm\" => \"be.cortexiq.energy\",~n"),
    io:format("     \"uptime_seconds\" => 42,~n"),
    io:format("     \"timestamp\" => 1699612800,~n"),
    io:format("     \"version\" => \"0.1.0\",~n"),
    io:format("     \"protocol\" => \"HTTP/3 (QUIC)\"~n"),
    io:format("   }~n~n"),

    io:format("4. Call the echo procedure:~n"),
    io:format("   {:ok, result} = MaculaSdk.Client.call(~n"),
    io:format("     client,~n"),
    io:format("     \"com.macula.diagnostics.echo\",~n"),
    io:format("     %{\"message\" => \"test\", \"count\" => 123}~n"),
    io:format("   )~n~n"),

    io:format("   Expected result:~n"),
    io:format("   %{~n"),
    io:format("     \"echo\" => %{\"message\" => \"test\", \"count\" => 123},~n"),
    io:format("     \"timestamp\" => 1699612800~n"),
    io:format("   }~n~n"),

    io:format("5. Call the info procedure:~n"),
    io:format("   {:ok, result} = MaculaSdk.Client.call(~n"),
    io:format("     client,~n"),
    io:format("     \"com.macula.diagnostics.info\",~n"),
    io:format("     %{}~n"),
    io:format("   )~n~n"),

    io:format("   Expected result:~n"),
    io:format("   %{~n"),
    io:format("     \"gateway\" => %{~n"),
    io:format("       \"node\" => \"macula@127.0.0.1\",~n"),
    io:format("       \"realm\" => \"be.cortexiq.energy\",~n"),
    io:format("       \"uptime_seconds\" => 42,~n"),
    io:format("       \"version\" => \"0.1.0\"~n"),
    io:format("     },~n"),
    io:format("     \"system\" => %{~n"),
    io:format("       \"otp_version\" => \"27\",~n"),
    io:format("       \"erts_version\" => \"15.2.7.2\",~n"),
    io:format("       \"process_count\" => 123,~n"),
    io:format("       \"memory_bytes\" => 12345678,~n"),
    io:format("       \"process_memory_bytes\" => 1234567~n"),
    io:format("     },~n"),
    io:format("     \"timestamp\" => 1699612800~n"),
    io:format("   }~n~n"),

    io:format("=== End of Diagnostics Test ===~n~n").
