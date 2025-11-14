#!/bin/sh
set -e

# Macula Docker Entrypoint Script
# Handles different node types: registry, provider, client

NODE_TYPE="${NODE_TYPE:-registry}"
NODE_NAME="${NODE_NAME:-node}"
NODE_HOST="${NODE_HOST:-localhost}"
COOKIE="${ERLANG_COOKIE:-macula_test}"
REGISTRY_ENDPOINT="${REGISTRY_ENDPOINT:-https://registry.macula.test:9443}"
PROVIDER_MULTIPLIER="${PROVIDER_MULTIPLIER:-1}"

echo "==> Starting Macula node: $NODE_TYPE"
echo "    Node name: ${NODE_NAME}@${NODE_HOST}"
echo "    Cookie: $COOKIE"

case "$NODE_TYPE" in
  registry)
    echo "==> Starting registry node..."
    exec erl \
      -pa _build/default/lib/*/ebin \
      -name "${NODE_NAME}@${NODE_HOST}" \
      -setcookie "$COOKIE" \
      -noshell \
      -eval "
        io:format(\"Starting Macula application...~n\"),
        case application:ensure_all_started(macula) of
          {ok, Started} ->
            io:format(\"Registry started with apps: ~p~n\", [Started]),
            io:format(\"Registry node running~n\"),
            receive after infinity -> ok end;
          {error, Reason} ->
            io:format(\"Failed to start: ~p~n\", [Reason]),
            halt(1)
        end
      "
    ;;

  provider)
    echo "==> Starting provider node..."
    echo "    Registry: $REGISTRY_ENDPOINT"
    echo "    Multiplier: $PROVIDER_MULTIPLIER"
    echo "    Provider endpoint: https://${NODE_HOST}:9443"

    # Wait for registry to be ready
    sleep 5

    exec erl \
      -pa _build/default/lib/*/ebin \
      -name "${NODE_NAME}@${NODE_HOST}" \
      -setcookie "$COOKIE" \
      -noshell \
      -eval "
        io:format(\"Starting provider: ~s~n\", [\"$NODE_NAME\"]),
        %% Enable gateway for peer-to-peer - providers run their own QUIC servers
        application:set_env(macula, start_gateway, true),
        application:set_env(macula, gateway_port, 9443),
        application:ensure_all_started(macula),

        %% Wait for gateway to start
        timer:sleep(2000),

        ConnOpts = #{
          realm => <<\"com.example.realm\">>,
          node_id => <<\"$NODE_NAME\">>
        },

        case macula_client:connect(<<\"$REGISTRY_ENDPOINT\">>, ConnOpts) of
          {ok, Pid} ->
            io:format(\"[~s] Connected to registry~n\", [\"$NODE_NAME\"]),

            Handler = fun(Args) ->
              io:format(\"[~s] Handling call: ~p~n\", [\"$NODE_NAME\", Args]),
              X = maps:get(<<\"x\">>, Args, 1),
              Result = X * $PROVIDER_MULTIPLIER,
              {ok, #{
                provider => <<\"$NODE_NAME\">>,
                result => Result,
                timestamp => erlang:system_time(second)
              }}
            end,

            %% Advertise with own endpoint for peer-to-peer connections
            ProviderEndpoint = <<\"https://${NODE_HOST}:9443\">>,
            case macula_client:advertise(Pid, <<\"test.calculator\">>, Handler, #{
              ttl => 300,
              advertise_endpoint => ProviderEndpoint
            }) of
              {ok, _Ref} ->
                io:format(\"[~s] Service advertised at ~s~n\", [\"$NODE_NAME\", ProviderEndpoint]),
                receive after infinity -> ok end;
              {error, AdvReason} ->
                io:format(\"[~s] Advertise failed: ~p~n\", [\"$NODE_NAME\", AdvReason]),
                halt(1)
            end;
          {error, ConnReason} ->
            io:format(\"[~s] Connection failed: ~p~n\", [\"$NODE_NAME\", ConnReason]),
            halt(1)
        end
      "
    ;;

  client)
    echo "==> Starting client node..."
    echo "    Registry: $REGISTRY_ENDPOINT"

    # Wait for providers to be ready
    sleep 15

    exec erl \
      -pa _build/default/lib/*/ebin \
      -name "${NODE_NAME}@${NODE_HOST}" \
      -setcookie "$COOKIE" \
      -noshell \
      -eval "
        io:format(\"~n==> Starting RPC test client...~n\"),
        %% Disable gateway auto-start for client nodes (they are clients, not gateways)
        application:set_env(macula, start_gateway, false),
        application:ensure_all_started(macula),

        ClientOpts = #{
          realm => <<\"com.example.realm\">>,
          provider_selection_strategy => round_robin,
          node_id => <<\"client\">>
        },

        {ok, Client} = macula_client:connect(<<\"$REGISTRY_ENDPOINT\">>, ClientOpts),
        io:format(\"~n==> Connected to registry~n\"),

        io:format(\"~n==> Waiting for service discovery...~n\"),
        timer:sleep(5000),

        io:format(\"~n==> TEST 1: Round-robin distribution (6 calls)~n\"),
        lists:foreach(fun(I) ->
          io:format(\"~nCall #~p:~n\", [I]),
          case macula_client:call(Client, <<\"test.calculator\">>, #{<<\"x\">> => I}) of
            {ok, Result} ->
              io:format(\"  Success: ~p~n\", [Result]);
            {error, Reason} ->
              io:format(\"  Error: ~p~n\", [Reason])
          end,
          timer:sleep(2000)
        end, lists:seq(1, 6)),

        io:format(\"~n==> All tests complete!~n\"),
        timer:sleep(5000),
        init:stop()
      "
    ;;

  publisher)
    echo "==> Starting publisher node (full Macula)..."
    echo "    Registry: $REGISTRY_ENDPOINT"
    echo "    Publisher endpoint: https://${NODE_HOST}:9443"

    # Wait for registry and subscribers
    sleep 10

    exec erl \
      -pa _build/default/lib/*/ebin \
      -name "${NODE_NAME}@${NODE_HOST}" \
      -setcookie "$COOKIE" \
      -noshell \
      -eval "
        io:format(\"Starting publisher: ~s~n\", [\"$NODE_NAME\"]),
        %% Enable gateway for peer-to-peer
        application:set_env(macula, start_gateway, true),
        application:set_env(macula, gateway_port, 9443),
        application:ensure_all_started(macula),

        %% Wait for gateway to start
        timer:sleep(2000),

        ConnOpts = #{
          realm => <<\"com.example.realm\">>,
          node_id => <<\"$NODE_NAME\">>
        },

        %% Define publish loop as anonymous recursive function (Y-combinator style)
        PublishLoop = fun(Self, Client, Topic, Counter) ->
          Msg = #{
            event => <<\"sensor.reading\">>,
            value => 20 + (Counter rem 30),
            sensor_id => <<\"temp1\">>,
            timestamp => erlang:system_time(second),
            sequence => Counter
          },

          io:format(\"[~s] Publishing message #~p: ~p~n\",
                    [calendar:system_time_to_rfc3339(erlang:system_time(second)),
                     Counter, Msg]),

          case macula_client:publish(Client, Topic, Msg) of
            ok ->
              io:format(\"  -> Published successfully~n\");
            {error, Reason} ->
              io:format(\"  -> ERROR: ~p~n\", [Reason])
          end,

          timer:sleep(3000),
          Self(Self, Client, Topic, Counter + 1)
        end,

        case macula_client:connect(<<\"$REGISTRY_ENDPOINT\">>, ConnOpts) of
          {ok, Pid} ->
            io:format(\"[~s] Connected to registry~n\", [\"$NODE_NAME\"]),

            %% Start publishing loop
            Topic = <<\"test.events\">>,
            PublishLoop(PublishLoop, Pid, Topic, 1);
          {error, ConnReason} ->
            io:format(\"[~s] Connection failed: ~p~n\", [\"$NODE_NAME\", ConnReason]),
            halt(1)
        end
      "
    ;;

  subscriber)
    echo "==> Starting subscriber node (full Macula)..."
    echo "    Registry: $REGISTRY_ENDPOINT"
    echo "    Subscriber endpoint: https://${NODE_HOST}:9443"

    # Wait for registry to be ready
    sleep 8

    exec erl \
      -pa _build/default/lib/*/ebin \
      -name "${NODE_NAME}@${NODE_HOST}" \
      -setcookie "$COOKIE" \
      -noshell \
      -eval "
        io:format(\"Starting subscriber: ~s~n\", [\"$NODE_NAME\"]),
        %% Enable gateway for peer-to-peer - subscribers run their own QUIC servers
        application:set_env(macula, start_gateway, true),
        application:set_env(macula, gateway_port, 9443),
        application:ensure_all_started(macula),

        %% Wait for gateway to start
        timer:sleep(2000),

        ConnOpts = #{
          realm => <<\"com.example.realm\">>,
          node_id => <<\"$NODE_NAME\">>
        },

        case macula_client:connect(<<\"$REGISTRY_ENDPOINT\">>, ConnOpts) of
          {ok, Pid} ->
            io:format(\"[~s] Connected to registry~n\", [\"$NODE_NAME\"]),

            %% Subscribe to test topic
            Topic = <<\"test.events\">>,
            Callback = fun(Msg) ->
              io:format(\"[~s] [~s] RECEIVED MESSAGE: ~p~n\",
                        [calendar:system_time_to_rfc3339(erlang:system_time(second)),
                         \"$NODE_NAME\", Msg]),
              ok
            end,

            case macula_client:subscribe(Pid, Topic, Callback) of
              {ok, SubRef} ->
                io:format(\"[~s] Subscribed to topic: ~s (ref: ~p)~n\",
                          [\"$NODE_NAME\", Topic, SubRef]),
                io:format(\"[~s] Listening for messages...~n\", [\"$NODE_NAME\"]),
                receive after infinity -> ok end;
              {error, SubReason} ->
                io:format(\"[~s] Subscribe failed: ~p~n\", [\"$NODE_NAME\", SubReason]),
                halt(1)
            end;
          {error, ConnReason} ->
            io:format(\"[~s] Connection failed: ~p~n\", [\"$NODE_NAME\", ConnReason]),
            halt(1)
        end
      "
    ;;

  # Wildcard pub/sub test node types
  publisher_wildcard)
    echo "==> Starting wildcard publisher node..."
    echo "    Registry: $REGISTRY_ENDPOINT"
    sleep 10
    exec /macula/docker/test_publisher_wildcard.erl
    ;;

  subscriber_exact)
    echo "==> Starting exact-match subscriber node..."
    echo "    Registry: $REGISTRY_ENDPOINT"
    echo "    Subscribe topic: $SUBSCRIBE_TOPIC"
    sleep 8
    exec /macula/docker/test_subscriber_wildcard.erl "$SUBSCRIBE_TOPIC" "exact"
    ;;

  subscriber_single_wildcard)
    echo "==> Starting single-wildcard subscriber node..."
    echo "    Registry: $REGISTRY_ENDPOINT"
    echo "    Subscribe topic: $SUBSCRIBE_TOPIC"
    sleep 8
    exec /macula/docker/test_subscriber_wildcard.erl "$SUBSCRIBE_TOPIC" "single-wildcard"
    ;;

  subscriber_multi_wildcard)
    echo "==> Starting multi-wildcard subscriber node..."
    echo "    Registry: $REGISTRY_ENDPOINT"
    echo "    Subscribe topic: $SUBSCRIBE_TOPIC"
    sleep 8
    exec /macula/docker/test_subscriber_wildcard.erl "$SUBSCRIBE_TOPIC" "multi-wildcard"
    ;;

  *)
    echo "ERROR: Unknown NODE_TYPE: $NODE_TYPE"
    echo "Valid types: registry, provider, client, publisher, subscriber,"
    echo "             publisher_wildcard, subscriber_exact, subscriber_single_wildcard, subscriber_multi_wildcard"
    exit 1
    ;;
esac
