%% The wire 1.0 signed frame objects that carry an extension inside their tbs (priv/vectors/wire_1_0/verify.json,
%% docs/design/WIRE_1_0.md section 3.2): a non-critical one this node does not know is ignored and counted by its
%% name, and a critical one refuses the object it sits in, extension_unsupported. Each case is put to the frame layer
%% as the frame that carries it — a CALL to verify_request/2, a RESULT or ERROR to verify_reply/3 against the request
%% the case names, a STREAM_ERROR to verify_provider_stream/3 against that request's open stream — so the vectors are
%% held to this implementation, not only to the stacks that vendor them.
-module(macula_frame_wire_1_0_ext_verify_conformance_tests).

-include_lib("eunit/include/eunit.hrl").

%% The extension every case carries, and the name the tolerance counter counts it under.
-define(EXT, <<"example.unknown">>).

%% The counters live in the tolerance table macula_peering_sup creates, so the application is started and the cases
%% run under it: each one asserts the counter its case names moved.
frame_ext_test_() ->
    {setup, fun() -> {ok, _} = application:ensure_all_started(macula) end, fun(_) -> ok end,
     [case_test(C) || C <- cases()]}.

case_test(#{<<"id">> := Id, <<"expect">> := Expect} = Case) ->
    {binary_to_list(Id), fun() -> ?assertEqual(expected(Expect), verdict(Case)) end}.

%% A non-critical extension that is ignored is counted by its name, so an accepted case asserts the counter moved. A
%% refusal inside a signed object counts nothing (macula_frame:non_critical/2: the signature covers the flag, so no
%% hop can have changed it), and the vector asks for the refusal alone.
expected(#{<<"outcome">> := <<"accept">>, <<"count">> := <<"wire_ext_ignored">>}) ->
    {accepted, ignored_counted};
expected(#{<<"outcome">> := <<"refuse">>, <<"reason">> := <<"extension_unsupported">>}) ->
    extension_unsupported.

verdict(#{<<"profile">> := P, <<"input">> := Input} = Case) ->
    Before = macula_wire_tolerance:counters(),
    case run(Input, binary_to_atom(P)) of
        accepted -> {accepted, counted(Case, Before)};
        Refusal -> Refusal
    end.

%% The case's object as the frame its own frame_type is carried in, verified the way the receiver verifies it.
run(Input, Profile) ->
    Object = object(maps:get(<<"object">>, Input)),
    case frame_type(Object) of
        call ->
            accepted(macula_frame:verify_request(#{version => 5, frame_type => call, request => Object}, Profile));
        stream_open ->
            accepted(macula_frame:verify_request(#{version => 5, frame_type => stream_open, request => Object},
                                                 Profile));
        Type when Type =:= result; Type =:= error ->
            Request = verified_request(maps:get(<<"request">>, Input), Profile),
            accepted(macula_frame:verify_reply(#{version => 5, frame_type => Type, reply => Object}, Request, Profile));
        stream_error ->
            Open = macula_frame:open_stream(verified_request(maps:get(<<"request">>, Input), Profile)),
            accepted(macula_frame:verify_provider_stream(#{version => 5, frame_type => stream_error, stream => Object},
                                                         Open, Profile))
    end.

verified_request(Hex, Profile) ->
    Object = object(Hex),
    Type = frame_type(Object),
    {ok, Request} = macula_frame:verify_request(#{version => 5, frame_type => Type, request => Object}, Profile),
    Request.

accepted({ok, _}) -> accepted;
accepted({ok, _, _}) -> accepted;
accepted({error, Reason}) -> Reason.

%% Whether the ignored counter moved for the case's extension.
counted(_Case, Before) ->
    Key = {wire_ext_ignored, ?EXT},
    case maps:get(Key, macula_wire_tolerance:counters(), 0) > maps:get(Key, Before, 0) of
        true -> ignored_counted;
        false -> not_counted
    end.

object(Hex) ->
    {ok, Object} = macula_signed_object:decode(binary:decode_hex(Hex)),
    Object.

frame_type(#{tbs := Tbs}) ->
    #{{text, <<"frame_type">>} := {text, Name}} = macula_record_cbor:decode(Tbs),
    binary_to_atom(Name).

%% The cases of this shape, and nothing else in the class.
cases() ->
    #{<<"cases">> := Cases} = vectors(<<"verify">>),
    [C || #{<<"id">> := <<"frame.", _/binary>>} = C <- Cases].

vectors(Class) ->
    {ok, Bytes} = file:read_file(filename:join([code:priv_dir(macula), "vectors", "wire_1_0",
                                                <<Class/binary, ".json">>])),
    json:decode(Bytes).
