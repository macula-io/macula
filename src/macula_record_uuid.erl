%% @doc UUIDv7 generator (RFC 9562 §5.7).
%%
%% Bit layout (128 bits / 16 bytes):
%% <pre>
%%  48b unix_ts_ms | 4b ver=7 | 12b rand_a | 2b var=10 | 62b rand_b
%% </pre>
-module(macula_record_uuid).

-export([v7/0, v7/1]).

-spec v7() -> <<_:128>>.
v7() ->
    v7(erlang:system_time(millisecond)).

-spec v7(non_neg_integer()) -> <<_:128>>.
v7(Ms) when is_integer(Ms), Ms >= 0 ->
    <<RandA:12, RandB:62, _Pad:6>> = crypto:strong_rand_bytes(10),
    <<Ms:48, 7:4, RandA:12, 2:2, RandB:62>>.
