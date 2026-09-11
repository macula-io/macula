%%%-------------------------------------------------------------------
%%% @doc What a reason is called when it leaves this node, and how much
%%% of it the local log prints.
%%%
%%% A peer, a remote caller or a published fact is told a reason's name
%%% and none of its terms. The name is the atom at the reason's head,
%%% such as `killed', `shutdown', `timeout' or `badmatch', looking
%%% through `{error, Reason}' and at most three tuples deep. A reason
%%% with no such atom, or whose atom's name is longer than 64 bytes, has
%%% no name.
%%%
%%% An error a handler answers its caller with may carry the handler's
%%% own text: a binary, or a list whose first 257 elements, or all of them
%%% if fewer, are printable Unicode characters. Only those elements of a
%%% list are looked at, however long it is. The text crosses as at most
%%% 256 bytes of valid UTF-8, cut on a character boundary.
%%%
%%% The whole reason stays on this node, where `logged/2' prints it for
%%% the log to at most 4,096 characters however large it is.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_reason_name).

-export([text/1, reply_text/1, logged/2]).

%% The longest name a reason goes by.
-define(NAME_BYTES, 64).
%% How many tuples deep a reason is looked into for its name.
-define(NAME_DEPTH, 3).
%% The most bytes of a handler's own text that cross to its caller.
-define(REPLY_TEXT_BYTES, 256).
%% What ends a text that was cut.
-define(CUT, <<"...">>).
%% The most characters the log prints of a reason and what came with it.
-define(LOGGED_CHARS, 4096).

%% @doc The reason's name, such as `<<"timeout">>', or `<<"crashed">>'
%% when it has none. This is what an end fact or an abort message says
%% about why something ended.
-spec text(term()) -> binary().
text(Reason) ->
    name_or_crashed(name(Reason)).

%% @doc The text a caller is told for an error a handler answered with:
%% the handler's own text when the reason is a binary or a list whose
%% first 257 elements are printable Unicode characters, as at most 256
%% bytes of valid UTF-8; otherwise the reason's name; `error' when the
%% reason has neither.
-spec reply_text(term()) -> {ok, binary()} | error.
reply_text(Text) when is_binary(Text) ->
    {ok, within_reply_bytes(Text)};
reply_text(Reason) when is_list(Reason) ->
    charlist_text(list_start(Reason, ?REPLY_TEXT_BYTES + 1, []));
reply_text(Reason) ->
    name(Reason).

%% @doc `Format' and `Args' printed for the local log, to at most 4,096
%% characters however large the terms in `Args' are. The print stops
%% early on large terms, and what it gives is cut to the bound.
-spec logged(io:format(), [term()]) -> unicode:chardata().
logged(Format, Args) ->
    Print = io_lib:format(Format, Args, [{chars_limit, ?LOGGED_CHARS}]),
    string:slice(Print, 0, ?LOGGED_CHARS).

name_or_crashed({ok, Name}) -> Name;
name_or_crashed(error) -> <<"crashed">>.

charlist_text({ok, Start}) ->
    printable_text(io_lib:printable_unicode_list(Start), Start);
charlist_text(error) ->
    error.

printable_text(true, Chars) ->
    {ok, within_reply_bytes(unicode:characters_to_binary(Chars))};
printable_text(false, _List) ->
    error.

%% A character takes at least one byte, so a list's first 257 elements
%% hold all of its text that can cross and show whether there was more.
%% A list that ends improperly among them is not text.
list_start([Elem | Rest], Left, Start) when Left > 0 ->
    list_start(Rest, Left - 1, [Elem | Start]);
list_start([], _Left, Start) ->
    {ok, lists:reverse(Start)};
list_start([_ | _], 0, Start) ->
    {ok, lists:reverse(Start)};
list_start(_Improper, _Left, _Start) ->
    error.

name(Reason) ->
    name_text(head_atom(Reason, ?NAME_DEPTH)).

head_atom(Name, _Depth) when is_atom(Name) ->
    {ok, Name};
head_atom({error, Reason}, Depth) when Depth > 0 ->
    head_atom(Reason, Depth - 1);
head_atom(Reason, Depth) when is_tuple(Reason), tuple_size(Reason) > 0, Depth > 0 ->
    head_atom(element(1, Reason), Depth - 1);
head_atom(_Reason, _Depth) ->
    error.

name_text({ok, Name}) -> within_name_bytes(atom_to_binary(Name, utf8));
name_text(error) -> error.

within_name_bytes(Text) when byte_size(Text) =< ?NAME_BYTES -> {ok, Text};
within_name_bytes(_Text) -> error.

%% The text whole when it is valid UTF-8 and fits, otherwise its longest
%% valid start that fits with the cut mark after it.
within_reply_bytes(Text) ->
    whole_or_cut(Text, valid_start(Text)).

whole_or_cut(Text, Text) when byte_size(Text) =< ?REPLY_TEXT_BYTES ->
    Text;
whole_or_cut(Text, _ValidStart) ->
    Room = ?REPLY_TEXT_BYTES - byte_size(?CUT),
    Start = valid_start(binary:part(Text, 0, min(Room, byte_size(Text)))),
    <<Start/binary, ?CUT/binary>>.

valid_start(Bytes) ->
    valid(unicode:characters_to_binary(Bytes)).

valid(Valid) when is_binary(Valid) -> Valid;
valid({incomplete, Valid, _Rest}) -> Valid;
valid({error, Valid, _Rest}) -> Valid.
