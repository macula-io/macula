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

    # Wait for registry to be ready
    sleep 5

    exec erl \
      -pa _build/default/lib/*/ebin \
      -name "${NODE_NAME}@${NODE_HOST}" \
      -setcookie "$COOKIE" \
      -noshell \
      -eval "
        io:format(\"Starting provider: ~s~n\", [\"$NODE_NAME\"]),
        application:ensure_all_started(macula),

        ConnOpts = #{
          realm => <<\"com.test\">>,
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

            case macula_client:advertise(Pid, <<\"test.calculator\">>, Handler, #{ttl => 300}) of
              {ok, _Ref} ->
                io:format(\"[~s] Service advertised~n\", [\"$NODE_NAME\"]),
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
        application:ensure_all_started(macula),

        ClientOpts = #{
          realm => <<\"com.test\">>,
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

  *)
    echo "ERROR: Unknown NODE_TYPE: $NODE_TYPE"
    echo "Valid types: registry, provider, client"
    exit 1
    ;;
esac
