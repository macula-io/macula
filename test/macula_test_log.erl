%%%-------------------------------------------------------------------
%%% @doc Captures what this node logs while a test runs, as text, so a
%%% test can check what reached the log and how much of it did.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_test_log).

-export([capture/0, wait_text/2, release/1]).
-export([log/2]).

%% @doc Starts sending the calling process the text of every event this
%% node logs. Returns the id to hand to release/1.
-spec capture() -> logger:handler_id().
capture() ->
    Id = list_to_atom(?MODULE_STRING ++ "_" ++
                          integer_to_list(erlang:unique_integer([positive]))),
    ok = logger:add_handler(Id, ?MODULE, #{level => all, config => #{to => self()}}),
    Id.

%% @doc The first captured text that contains Part, waiting at most
%% TimeoutMs for each event.
-spec wait_text(binary(), timeout()) -> binary().
wait_text(Part, TimeoutMs) ->
    receive
        {?MODULE, Text} ->
            text_containing(binary:match(Text, Part), Text, Part, TimeoutMs)
    after TimeoutMs ->
        error({not_logged, Part})
    end.

%% @doc Stops capturing and drops what was captured and not read.
-spec release(logger:handler_id()) -> ok.
release(Id) ->
    ok = logger:remove_handler(Id),
    drop_captured().

%% @private The logger handler callback.
log(#{msg := Msg}, #{config := #{to := To}}) ->
    To ! {?MODULE, text(Msg)},
    ok.

text({string, Chars}) -> unicode:characters_to_binary(Chars);
text({report, Report}) -> unicode:characters_to_binary(io_lib:format("~p", [Report]));
text({Format, Args}) -> unicode:characters_to_binary(io_lib:format(Format, Args)).

text_containing(nomatch, _Text, Part, TimeoutMs) -> wait_text(Part, TimeoutMs);
text_containing(_Found, Text, _Part, _TimeoutMs) -> Text.

drop_captured() ->
    receive
        {?MODULE, _} -> drop_captured()
    after 0 ->
        ok
    end.
