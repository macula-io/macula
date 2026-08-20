%%%-------------------------------------------------------------------
%%% @doc Factory supervisor for `macula_responder' children.
%%%
%%% One instance is started internally by `macula_responder:advertise/5,6'
%%% per advertised procedure. Each inbound call starts one `temporary'
%%% child here — a crashed or completed responder is never restarted,
%%% since it represents exactly one already-handled (or already-dead)
%%% request.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_responder_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link(?MODULE, []).

%% @private
init([]) ->
    ChildSpec = #{
        id => macula_responder,
        start => {macula_responder, start_link, []},
        restart => temporary,
        shutdown => 5_000,
        type => worker,
        modules => [macula_responder]
    },
    {ok, {#{strategy => simple_one_for_one, intensity => 10, period => 10},
          [ChildSpec]}}.
