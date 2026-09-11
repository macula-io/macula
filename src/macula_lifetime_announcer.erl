%%%-------------------------------------------------------------------
%%% @doc Announces a supervised wrapper's start fact and its end fact
%%% from a process of its own.
%%%
%%% A wrapper, such as `macula_stream_sink', calls `start/2' once its
%%% operation has begun. The announcer monitors the wrapper, publishes
%%% the start fact, and waits for the wrapper to hand over its end
%%% payload with `announce_end/2', which it publishes under the end
%%% topic. A wrapper that goes down before it hands over its end, for
%%% example because it was killed, has its end fact published for it,
%%% with the end fields plus outcome `failed' and the reason it went
%%% down for.
%%%
%%% Publishing never fails, holds up or ends the wrapper: a publish that
%%% raises or returns anything but `ok' is logged, and the announcer goes
%%% on. The announcer touches nothing but its publish function, and it
%%% ends once it has published the end fact.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_lifetime_announcer).

-include_lib("kernel/include/logger.hrl").

-export([start/2, announce_end/2]).

-export_type([publish/0, facts/0]).

-type publish() :: fun((macula:pool(), macula:realm(), macula:topic(), map()) -> term()).
-type facts() :: #{publish := publish(),
                   pool := macula:pool(),
                   realm := macula:realm(),
                   started := {macula:topic(), map()},
                   ended := {macula:topic(), map()}}.

%% @doc With `Announce' true, starts the announcer of the calling
%% process and returns its pid once it monitors the caller; the
%% announcer then publishes the start fact. With `Announce' false
%% nothing starts and `undefined' is returned.
-spec start(boolean(), facts()) -> pid() | undefined.
start(false, _Facts) ->
    undefined;
start(true, #{publish := Publish, pool := Pool, realm := Realm,
              started := {_, _} = Started, ended := {_, _} = Ended}) ->
    Wrapper = self(),
    Announcer = spawn(fun() -> announce(Wrapper, Publish, Pool, Realm, Started, Ended) end),
    receive
        {announcer_watching, Announcer} -> Announcer
    end.

%% @doc Hands the announcer the wrapper's end payload, to publish under
%% the end topic. Returns at once; with `undefined' it does nothing.
-spec announce_end(pid() | undefined, map()) -> ok.
announce_end(undefined, _Payload) ->
    ok;
announce_end(Announcer, Payload) when is_pid(Announcer), is_map(Payload) ->
    Announcer ! {wrapper_ended, Payload},
    ok.

announce(Wrapper, Publish, Pool, Realm, {StartTopic, StartFields}, {EndTopic, EndFields}) ->
    WrapperRef = monitor(process, Wrapper),
    Wrapper ! {announcer_watching, self()},
    publish(Publish, Pool, Realm, StartTopic, StartFields),
    receive
        {wrapper_ended, Payload} ->
            publish(Publish, Pool, Realm, EndTopic, Payload);
        {'DOWN', WrapperRef, process, Wrapper, Reason} ->
            publish(Publish, Pool, Realm, EndTopic,
                    EndFields#{outcome => failed, reason => Reason})
    end.

publish(Publish, Pool, Realm, Topic, Payload) ->
    log_unpublished(Topic, Payload, try_publish(Publish, Pool, Realm, Topic, Payload)).

try_publish(Publish, Pool, Realm, Topic, Payload) ->
    try
        Publish(Pool, Realm, Topic, Payload)
    catch
        Class:Reason -> {Class, Reason}
    end.

log_unpublished(_Topic, _Payload, ok) ->
    ok;
log_unpublished(Topic, Payload, Failure) ->
    ?LOG_WARNING("[macula_lifetime_announcer] ~ts not published: ~p; payload ~p",
                 [Topic, Failure, Payload]).
