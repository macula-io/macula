%% Calls a consumer makes, written as a consumer writes them, for dialyzer to check against macula's own specs. Nothing
%% runs them: `rebar3 as consumer_contracts dialyzer' analyses this directory (the profile in rebar.config), and a spec
%% that refuses what the code accepts and the docs describe fails the dialyzer check here, before a consumer's own
%% dialyzer finds it.
%%
%% Every mcl service dials its stations by pin, a seed map naming the node_id the station must prove. `seed()' once
%% left that key out, so each such connect/2 broke the contract in the consumer and dialyzer marked everything after it
%% unreachable (13.0.1).
-module(macula_consumer_contracts).

-export([connect_to_a_pinned_seed/1, pool_of_a_pinned_seed/1, call_through_a_pinned_seed/1,
         pool_with_its_ordering_bounds/0, a_required_call/1, a_reported_call/1, a_reported_station_call/1,
         a_streams_report/1]).

-define(STATION, <<7:256>>).

connect_to_a_pinned_seed(Opts) ->
    macula:connect([pinned_seed()], Opts).

pool_of_a_pinned_seed(Opts) ->
    macula_client:connect([pinned_seed(), <<"quic://[::1]:4433">>], Opts#{expected_node_id => ?STATION}).

%% A pool's reorder bounds for `ordered' subscriptions, which the pool reads and `opts()' once did not name (13.0.1).
pool_with_its_ordering_bounds() ->
    macula:connect([pinned_seed()], #{order_timeout_ms => 500, order_max_buffer => 4096}).

%% A resolved call that fails closed without a key, which call/6's spec once did not name (13.0.1).
a_required_call(Pool) ->
    macula:call(Pool, <<1:256>>, <<"acme/echo_v1">>, #{}, 5_000, #{confidential => required}).

%% A call that asks for its seal report and reads it (13.1.0): the option and the 3-tuple must both be in call/6's spec,
%% or the call breaks its contract and the match can never succeed.
a_reported_call(Pool) ->
    {ok, _Result, #{sealed := Sealed, provider := <<_:256>>}} =
        macula:call(Pool, <<1:256>>, <<"acme/echo_v1">>, #{}, 5_000, #{report => true}),
    Sealed.

%% The same through an explicit station, which honours `report' because the pool's own direct dial calls through it.
a_reported_station_call(Pool) ->
    {ok, _Result, #{sealed := Sealed}} =
        macula:call_station(Pool, pinned_seed(), <<9:256>>, <<1:256>>, <<"acme/echo_v1">>, #{}, 5_000,
                            #{confidential => required, report => true}),
    Sealed.

%% A stream's seal report, once it has settled.
a_streams_report(Stream) ->
    {ok, #{sealed := Sealed, provider := <<_:256>>}} = macula:stream_report(Stream),
    Sealed.

call_through_a_pinned_seed(Pool) ->
    macula:call_station(Pool, pinned_seed(), <<9:256>>, <<1:256>>, <<"acme/echo_v1">>, #{}, 5_000).

pinned_seed() ->
    #{host => <<"station.example">>, port => 4433, expected_node_id => ?STATION}.
