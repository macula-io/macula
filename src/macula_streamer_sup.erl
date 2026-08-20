%%%-------------------------------------------------------------------
%%% @doc Factory supervisor for `macula_streamer' children.
%%%
%%% One instance is started internally by `macula_streamer:advertise/5,6'
%%% per advertised streaming procedure. Each inbound STREAM_OPEN starts
%%% one `temporary' child here — a crashed or completed streamer is
%%% never restarted, since it represents exactly one already-ended (or
%%% already-dead) stream.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_streamer_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link(?MODULE, []).

%% @private
init([]) ->
    ChildSpec = #{
        id => macula_streamer,
        start => {macula_streamer, start_link, []},
        restart => temporary,
        shutdown => 5_000,
        type => worker,
        modules => [macula_streamer]
    },
    {ok, {#{strategy => simple_one_for_one, intensity => 10, period => 10},
          [ChildSpec]}}.
