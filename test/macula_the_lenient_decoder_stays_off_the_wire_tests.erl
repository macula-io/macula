%% No lenient NIF decoder may read a peer's bytes (macula#154).
%%
%% Two kinds of CBOR decoder ship in this project and they are not interchangeable. `macula_record_cbor:decode_strict/1'
%% is the strict one: it refuses a duplicate map key, text that is not UTF-8, a key that is neither text nor an integer,
%% an integer past 2^63, and nesting past 64. The NIF exposes lenient ones - `macula_cbor_nif:unpack/1' and
%% `macula_cbor_nif:unpack_deterministic/1,2' - which refuse none of those, and bound nesting at 128 rather than 64.
%%
%% That is fine as long as no lenient one is on the wire. Nothing in the build enforced it, and the reasons to reach for
%% one are obvious ones - it is a NIF, it is quicker, and one of them is even called "deterministic" - so this test is
%% what makes the difference visible the moment somebody takes that shortcut. A red run here is not a bug in the NIF; it
%% is a decision that needs a threat model, because a lenient reader on a peer's bytes accepts encodings the strict one
%% exists to refuse.
%%
%% Every NIF decoder entry point is matched, not just the one this issue was about: matching the prefix
%% `macula_cbor_nif:unpack' covers `unpack/1', `unpack_deterministic/1' and `unpack_deterministic/2', and a decoder added
%% later that is named `unpack_something' is covered without this file being remembered.
%%
%% THE ONE EXEMPTION. `macula_dist_system/macula_dist_relay_protocol.erl' reads the relay's control frames with
%% `macula_cbor_nif:unpack/1', which is a lenient decoder on peer bytes. It is a real defect and it is not this package's
%% to fix: macula-io/macula#160 carries it. A call is allowed through only when its own line carries the marker
%% `lenient-decoder-ok:' naming the issue that removes it, so the exemption sits at the call rather than in a list here
%% that could drift from it, and #160 deletes both the call and the marker.
%%
%% What this does NOT cover: a call reached through `apply/3' or built at runtime, and a call in a test or a script,
%% which is what makes the NIF's own test surface legitimate.
-module(macula_the_lenient_decoder_stays_off_the_wire_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LENIENT, <<"macula_cbor_nif:unpack">>).

%% The marker that exempts one call line, and what a marker must say to be read as one at all: the issue it names.
-define(MARKER, <<"lenient-decoder-ok:">>).
-define(MARKER_NAMES_AN_ISSUE, <<"lenient-decoder-ok: macula-io/macula#">>).

%% The decoder's own module implements the wrappers, so it is the one file that may name them.
-define(WRAPPER, "macula_cbor_nif.erl").

no_src_module_reads_peer_bytes_with_a_lenient_decoder_test() ->
    Sources = source_files(),
    %% A check that searched nothing would pass for the wrong reason, so the tree is asserted to be the real one first.
    ?assert(length(Sources) > 50, {too_few_sources, length(Sources)}),
    ?assert(lists:any(fun(F) -> filename:basename(F) =:= "macula_record_cbor.erl" end, Sources)),
    ?assertEqual([], lists:append([calls_in(F) || F <- Sources])).

%% Where src lives depends on how the tests were started, so both the project root and one level up are tried; an empty
%% list is never accepted as "nothing to check".
source_files() ->
    case lists:append([sources_at(Dir) || Dir <- ["src", filename:join("..", "src")]]) of
        [] -> ?assert(false);
        Found -> Found
    end.

sources_at(Dir) ->
    [F || F <- filelib:wildcard(filename:join([Dir, "**", "*.erl"])),
          filename:basename(F) =/= ?WRAPPER].

calls_in(File) ->
    {ok, Bin} = file:read_file(File),
    Lines = binary:split(Bin, <<"\n">>, [global]),
    [{File, Line, Text} || {Line, Text} <- numbered(Lines), calls(Text), not exempted(Text)].

numbered(Lines) ->
    lists:zip(lists:seq(1, length(Lines)), Lines).

%% Only the code on a line counts. A comment naming the function is documentation, and this module's own subject would
%% otherwise be flagged by the very comment that explains why it is forbidden.
calls(Line) ->
    binary:match(code_of(Line), ?LENIENT) =/= nomatch.

%% Read off the raw line, because the marker lives in the comment that `code_of/1' strips. A marker without an issue
%% number behind it is not an exemption: it is a word somebody typed.
exempted(Line) ->
    binary:match(Line, ?MARKER) =/= nomatch andalso binary:match(Line, ?MARKER_NAMES_AN_ISSUE) =/= nomatch.

code_of(Line) ->
    hd(binary:split(Line, <<"%">>)).
