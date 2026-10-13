%% A stand-in for the stations a test pool's links would reach: it accepts every registration the pool waits on
%% (macula#12), as a station answers with a REGISTER_ACK, through the first link the registration went to. For a test
%% about something else, whose links never connect, so that its macula:advertise/5, advertise_stream/6 and
%% subscribe/4,5 return as they would against a station that accepted them. macula_client_register_ack_tests holds
%% the waiting itself to real acks.
-module(macula_test_station_acks).

-export([start/1, stop/1]).

%% @doc Accept, from now on, every registration `Pool' waits on, until stop/1 or the pool ends.
-spec start(pid()) -> pid().
start(Pool) ->
    spawn_link(fun() -> Mon = erlang:monitor(process, Pool), accepting(Pool, Mon) end).

-spec stop(pid()) -> ok.
stop(Acks) ->
    unlink(Acks),
    exit(Acks, shutdown),
    ok.

accepting(Pool, Mon) ->
    receive
        {'DOWN', Mon, process, Pool, _} -> ok
    after 10 ->
        _ = [Pool ! {macula_register_ack, Link, undefined, Key, accepted} || {Key, Link} <- pending(Pool)],
        accepting(Pool, Mon)
    end.

pending(Pool) ->
    pending_in(catch sys:get_state(Pool)).

pending_in({'EXIT', _PoolGone}) ->
    [];
pending_in(State) ->
    Registrations = element(macula_client:state_field_index(registrations), State),
    [{Key, first(Awaited)} || Key := #{awaited := Awaited} <- Registrations].

%% A registration handed to no link (every link respawning) is accepted as its replay would be.
first([Link | _]) -> Link;
first([]) -> undefined.
