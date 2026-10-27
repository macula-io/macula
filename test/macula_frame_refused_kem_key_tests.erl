%% A keyed provider's `sealed_required' refusal carries the KEM key it holds now as the non-critical signed extension
%% `seal.kem_key' (DESIGN_E2E_SEALED_BY_DEFAULT §3, release A0): an ERROR to a CALL, and a STREAM_ERROR to a
%% STREAM_OPEN. The extension travels inside the `tbs', so the signature covers it and a relay cannot strip it. A
%% reader of this release reads it, does not count it as ignored, and still ignores and counts a non-critical
%% extension it does not know.
-module(macula_frame_refused_kem_key_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, crypto:hash(sha256, <<"test">>)).
-define(PROCEDURE, <<"acme/echo_v1">>).

%% An ERROR to a CALL carrying the key builds, verifies against its request, and reads back the key as carried.
an_error_carries_the_providers_key_test() ->
    #{provider := Provider, request := Request, kem_key := KemKey} = fixture(),
    Frame = refusal(Request, KemKey, Provider),
    ?assertEqual({ok, KemKey}, macula_frame:refused_kem_key(verified_error(Frame, Request))).

%% A STREAM_ERROR to a STREAM_OPEN carries it the same way, past a verifier that reads the frame's own shape.
a_stream_error_carries_the_providers_key_test() ->
    #{provider := Provider, open_frame := OpenFrame, open := Open, kem_key := KemKey} = fixture(),
    Refusal = macula_frame:provider_stream(#{frame_type => stream_error, seq => 0, code => <<"sealed_required">>,
                                             message => <<>>, ext => #{<<"seal.kem_key">> =>
                                                                           #{critical => 0, value => KemKey}}},
                                           Provider, Open),
    ?assertEqual({ok, KemKey}, macula_frame:refused_kem_key(stream_error_read(Refusal, OpenFrame))).

%% The extension a refusal of this release carries is one this node reads: it is not counted among the ignored ones.
the_refusal_extension_is_not_counted_as_ignored_test() ->
    #{provider := Provider, request := Request, kem_key := KemKey} = fixture(),
    Before = ignored_count(<<"seal.kem_key">>),
    _ = verified_error(refusal(Request, KemKey, Provider), Request),
    ?assertEqual(Before, ignored_count(<<"seal.kem_key">>)).

%% Any other non-critical extension is still ignored and counted by its name: an A0 caller handed an extension it does
%% not know reads its refusal and counts the extension.
an_unknown_non_critical_extension_is_ignored_and_counted_test() ->
    #{provider := Provider, request := Request, kem_key := KemKey} = fixture(),
    Before = ignored_count(<<"seal.something_new">>),
    Frame = refusal(Request, KemKey, Provider, #{<<"seal.something_new">> => #{critical => 0, value => <<1>>}}),
    ?assertEqual({ok, KemKey}, macula_frame:refused_kem_key(verified_error(Frame, Request))),
    ?assertEqual(Before + 1, ignored_count(<<"seal.something_new">>)).

%% A refusal without the extension names no key, and one whose value is not bytes names none either: the error reaches
%% its caller as the provider's own.
a_refusal_without_a_usable_key_names_none_test() ->
    #{provider := Provider, request := Request} = fixture(),
    WithoutExt = macula_frame:provider_error(#{request => Request, code => <<"sealed_required">>}, Provider),
    ?assertEqual(none, macula_frame:refused_kem_key(verified_error(WithoutExt, Request))),
    NotBytes = refusal(Request, 42, Provider),
    ?assertEqual(none, macula_frame:refused_kem_key(verified_error(NotBytes, Request))).

%% A critical extension refuses the signed object it rides in, as any critical one does: a provider cannot make a
%% caller read a critical extension it does not know, and the signature covers the flag.
a_critical_extension_refuses_the_refusal_test() ->
    #{provider := Provider, request := Request, kem_key := KemKey} = fixture(),
    Frame = refusal(Request, KemKey, Provider, #{}, 1),
    ?assertMatch({error, _}, macula_frame:verify_reply(Frame, Request, profile())).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

fixture() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Provider} = macula_node_keys:generate(identity, profile()),
    {ok, Caller} = macula_node_keys:generate(identity, profile()),
    {Public, _Private} = macula_seal:generate_key(profile()),
    Request = #{
        request_id => crypto:strong_rand_bytes(16), realm => ?REALM, procedure => ?PROCEDURE,
        target => macula_node_keys:key_id(Provider), deadline => erlang:system_time(millisecond) + 5_000,
        payload => #{}},
    Frame = macula_frame:call(Request, Caller),
    OpenSpec = maps:merge(Request, #{mode => bidi}),
    OpenFrame = macula_frame:stream_bytes({stream_open, OpenSpec}, Caller),
    {ok, Bytes} = OpenFrame,
    {ok, OpenFrameDecoded, <<>>} = macula_frame:decode(macula_frame:written_bytes(Bytes)),
    {ok, Open} = macula_frame:verify_request(OpenFrameDecoded, profile()),
    #{provider => Provider, request => verified(Frame), open => Open, open_frame => OpenFrameDecoded,
      kem_key => macula_seal:key_as_carried(Public)}.

verified(Frame) ->
    {ok, Request} = macula_frame:verify_request(Frame, profile()),
    Request.

refusal(Request, KemKey, Provider) ->
    refusal(Request, KemKey, Provider, #{}, 0).

refusal(Request, KemKey, Provider, Extra) ->
    refusal(Request, KemKey, Provider, Extra, 0).

%% An ERROR to a CALL carrying `seal.kem_key' with the value given, at the criticality given, beside `Extra'.
refusal(Request, KemKey, Provider, Extra, Critical) ->
    Ext = maps:merge(Extra, #{<<"seal.kem_key">> => #{critical => Critical, value => KemKey}}),
    macula_frame:provider_error(#{request => Request, code => <<"sealed_required">>, ext => Ext}, Provider).

verified_error(Frame, Request) ->
    {ok, Fields} = macula_frame:verify_reply(Frame, Request, profile()),
    Fields.

%% The fields of a STREAM_ERROR as the open's caller verifies it.
stream_error_read(Frame, OpenFrame) ->
    Open = verified(OpenFrame),
    {ok, Fields, _Verifier} = macula_frame:verify_provider_stream(Frame, macula_frame:open_stream(Open), profile()),
    Fields.

ignored_count(Name) ->
    maps:get({wire_ext_ignored, Name}, macula_wire_tolerance:counters(), 0).

profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.
