%% EUnit tests for macula_frame:parse_for_relay/2, the reader a relay uses on bytes it received on a stream. Each whole
%% frame that passes the reader's checks comes back with a unit holding exactly the bytes received for it, and only such
%% a unit is ever written on by a relay. A frame whose fields its type refuses comes back refused, with no unit; a length
%% header over the cap, or bytes that are not a frame, end the parse, and nothing after them yields a unit. The unit is
%% tagged, and no module other than macula_frame builds or reads that tag.
-module(macula_frame_relay_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CAP, 65536).

relay_test_() ->
    {setup, fun keys/0, fun cases/1}.

cases(Keys) ->
    [{case_name(Case), fun() -> Case(Keys) end}
     || Case <- [fun a_frame_that_passes_its_checks_yields_a_unit_of_its_exact_bytes/1,
                 fun whole_frames_yield_units_and_a_partial_frame_stays_in_the_tail/1,
                 fun a_frame_its_type_refuses_yields_no_unit/1,
                 fun a_length_header_over_the_cap_yields_no_unit_and_ends_the_parse/1,
                 fun bytes_that_are_not_a_frame_yield_no_unit_and_end_the_parse/1,
                 fun no_other_module_builds_or_reads_the_received_frame_tag/1]].

a_frame_that_passes_its_checks_yields_a_unit_of_its_exact_bytes(Keys) ->
    Bytes = provider_frame(Keys),
    {ok, [{Frame, Unit}], <<>>} = macula_frame:parse_for_relay(Bytes, ?CAP),
    ?assertEqual(Bytes, macula_frame:relayed_bytes(Unit)),
    ?assertEqual({ok, Frame, <<>>}, macula_frame:decode(Bytes)).

whole_frames_yield_units_and_a_partial_frame_stays_in_the_tail(Keys) ->
    [First, Second] = [provider_frame(Keys), caller_frame(Keys)],
    Partial = binary:part(First, 0, 10),
    {ok, Items, Tail} = macula_frame:parse_for_relay(<<First/binary, Second/binary, Partial/binary>>, ?CAP),
    ?assertEqual([First, Second], [macula_frame:relayed_bytes(Unit) || {_Frame, Unit} <- Items]),
    ?assertEqual(Partial, Tail).

%% A refused frame is named once, in its place, and the frames around it still yield their units.
a_frame_its_type_refuses_yields_no_unit(Keys) ->
    [Good, Refused, Other] = [provider_frame(Keys), refused_frame(Keys), caller_frame(Keys)],
    {ok, Items, <<>>} = macula_frame:parse_for_relay(<<Good/binary, Refused/binary, Other/binary>>, ?CAP),
    ?assertMatch([{_, _}, {refused, {invalid_frame, stream_data, stream}}, {_, _}], Items),
    ?assertEqual([Good, Other], [macula_frame:relayed_bytes(Unit) || {_Frame, Unit} <- Items, is_map(_Frame)]).

a_length_header_over_the_cap_yields_no_unit_and_ends_the_parse(Keys) ->
    Good = provider_frame(Keys),
    {malformed, Items, frame_too_large} =
        macula_frame:parse_for_relay(<<Good/binary, (?CAP + 1):32/big, (provider_frame(Keys))/binary>>, ?CAP),
    ?assertEqual([Good], [macula_frame:relayed_bytes(Unit) || {_Frame, Unit} <- Items]).

bytes_that_are_not_a_frame_yield_no_unit_and_end_the_parse(Keys) ->
    Good = provider_frame(Keys),
    {malformed, Items, bad_frame} =
        macula_frame:parse_for_relay(<<Good/binary, 4:32/big, "junk", (provider_frame(Keys))/binary>>, ?CAP),
    ?assertEqual([Good], [macula_frame:relayed_bytes(Unit) || {_Frame, Unit} <- Items]).

%% The tag that marks bytes received through the reader appears in no other module, in a construction or a match, so no
%% other module can relay bytes as if the reader had accepted them.
no_other_module_builds_or_reads_the_received_frame_tag(_Keys) ->
    {ok, Modules} = application:get_key(macula, modules),
    %% The search must find the tag where it is, or finding it nowhere else proves nothing.
    ?assert(mentions_tag(macula_frame, macula_received_frame)),
    ?assertEqual([], [Module || Module <- Modules, Module =/= macula_frame,
                                mentions_tag(Module, macula_received_frame)]).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

keys() ->
    _ = application:load(macula),
    Generate = fun() -> {ok, Key} = macula_node_keys:generate(identity, pq_pure), Key end,
    Keys = #{caller => Generate(), provider => Generate()},
    Keys#{open => verified_open(Keys)}.

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).

verified_open(#{caller := Caller, provider := Provider}) ->
    Spec = #{request_id => <<7:128>>, realm => <<1:256>>, procedure => <<"acme/count_v1">>,
             target => macula_node_keys:key_id(Provider), deadline => 1789000600000, payload => #{}, mode => bidi},
    {ok, Open, <<>>} = macula_frame:decode(macula_frame:encode(macula_frame:stream_open(Spec, Caller))),
    {ok, Request} = macula_frame:verify_request(Open, pq_pure),
    Request.

chunk(Seq) ->
    #{frame_type => stream_data, seq => Seq, encoding => raw, body => <<"chunk">>}.

provider_frame(#{provider := Provider, open := Open}) ->
    macula_frame:encode(macula_frame:provider_stream(chunk(0), Provider, Open)).

caller_frame(#{caller := Caller, open := Open}) ->
    macula_frame:encode(macula_frame:caller_stream(chunk(0), Caller, Open)).

%% A provider stream frame without its signed object: its bytes decode, and its type refuses its fields.
refused_frame(#{provider := Provider, open := Open}) ->
    macula_frame:encode(maps:remove(stream, macula_frame:provider_stream(chunk(1), Provider, Open))).

%% Whether a module's abstract code holds a tuple whose first element is Tag, anywhere. The beam is read from the
%% application's ebin, since a cover-compiled module has no beam file of its own.
mentions_tag(Module, Tag) ->
    Beam = filename:join([code:lib_dir(macula), "ebin", atom_to_list(Module) ++ ".beam"]),
    {ok, {Module, [{debug_info, {debug_info_v1, Backend, Data}}]}} = beam_lib:chunks(Beam, [debug_info]),
    {ok, Forms} = Backend:debug_info(erlang_v1, Module, Data, []),
    holds_tag_tuple(Forms, Tag).

holds_tag_tuple({tuple, _Anno, [{atom, _, Tag} | _]}, Tag) -> true;
holds_tag_tuple(Term, Tag) when is_tuple(Term)            -> holds_tag_tuple(tuple_to_list(Term), Tag);
holds_tag_tuple([Head | Tail], Tag)                        -> holds_tag_tuple(Head, Tag) orelse holds_tag_tuple(Tail, Tag);
holds_tag_tuple(_Leaf, _Tag)                               -> false.
