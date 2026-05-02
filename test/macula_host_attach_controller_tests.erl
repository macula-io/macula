%%%-------------------------------------------------------------------
%%% @doc Tests for macula_host_attach_controller (Phase 3.5).
%%%
%%% Validates the four dispatch paths: attach_v1 -> attach_fn,
%%% data-for-hosted -> attach_send_fn, data-for-unhosted -> fallback_fn,
%%% garbage CBOR -> fallback_fn. Plus the wire-shape delegation helper
%%% and the address-binding-by-lookup discipline.
%%%
%%% Pluggable callbacks let every test drop its own captures into a
%%% process mailbox; no live transport, no live ETS table.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_host_attach_controller_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<16#11:256>>).
-define(SINK, macula_host_attach_controller_tests_sink).
-define(LOOKUP_TABLE, macula_host_attach_controller_tests_lookups).

%% =============================================================================
%% Fixtures
%% =============================================================================

new_actor() ->
    Kp   = macula_identity:generate(),
    Pk   = macula_identity:public(Kp),
    Addr = macula_address:derive(?REALM, Pk),
    #{kp => Kp, pk => Pk, addr => Addr}.

now_ms() -> erlang:system_time(millisecond).

setup() ->
    %% NB: do NOT register(?SINK, self()) here — eunit fixtures run
    %% setup and the test body in different processes. Registration
    %% must happen inside the test body (in start_controller/1).
    Host = new_actor(),
    #{host => Host}.

cleanup(_) ->
    catch macula_host_attach_controller:stop(),
    catch unregister(?SINK),
    drop_lookup_table(ets:info(?LOOKUP_TABLE)),
    flush().

drop_lookup_table(undefined) -> ok;
drop_lookup_table(_)         -> ets:delete(?LOOKUP_TABLE), ok.

ensure_lookup_table() ->
    create_lookup_table(ets:info(?LOOKUP_TABLE)).

create_lookup_table(undefined) ->
    _ = ets:new(?LOOKUP_TABLE, [named_table, public, set]),
    ok;
create_lookup_table(_) ->
    ok.

set_lookup(Dst, Reply) ->
    ensure_lookup_table(),
    true = ets:insert(?LOOKUP_TABLE, {Dst, Reply}),
    ok.

table_lookup([])           -> not_found;
table_lookup([{_, Reply}]) -> Reply.

flush() ->
    receive _ -> flush() after 0 -> ok end.

start_controller(#{host := #{pk := HPk}}) ->
    catch unregister(?SINK),
    true = register(?SINK, self()),
    AttachSendFn = fun(StreamRef, Frame) ->
                       ?SINK ! {sent, StreamRef, Frame}, ok
                   end,
    ensure_lookup_table(),
    LookupFn     = fun(Dst) -> table_lookup(ets:lookup(?LOOKUP_TABLE, Dst)) end,
    AttachFn     = fun(Addr, Pk, Delegation, StreamRef) ->
                       ?SINK ! {attach, Addr, Pk, Delegation, StreamRef},
                       ok
                   end,
    FallbackFn   = fun(Cbor) ->
                       ?SINK ! {fallback, Cbor}, ok
                   end,
    {ok, _} = macula_host_attach_controller:start_link(#{
                  realm_pubkey   => ?REALM,
                  host_pubkey    => HPk,
                  attach_send_fn => AttachSendFn,
                  lookup_fn      => LookupFn,
                  attach_fn      => AttachFn,
                  fallback_fn    => FallbackFn
              }).

await(Tag, Timeout) ->
    receive
        Msg when element(1, Msg) =:= Tag -> Msg
    after Timeout -> timeout
    end.

%% =============================================================================
%% Wire builders
%% =============================================================================

build_attach_frame(#{kp := DKp, pk := DPk, addr := DAddr}, #{pk := HPk}) ->
    Now = now_ms(),
    Signed = macula_record:sign_host_delegation(
               macula_record:host_delegation(DPk, HPk, ?REALM,
                                              Now, Now + 60_000),
               DKp),
    Wire = #{
        <<"d">>  => maps:get(daemon_pubkey, Signed),
        <<"h">>  => maps:get(host_pubkey,   Signed),
        <<"r">>  => maps:get(realm_pubkey,  Signed),
        <<"nb">> => maps:get(not_before_ms, Signed),
        <<"na">> => maps:get(not_after_ms,  Signed),
        <<"s">>  => maps:get(daemon_sig,    Signed)
    },
    macula_cbor_nif:pack(#{
        <<"type">>          => <<"macula_attach_v1">>,
        <<"daemon_pubkey">> => DPk,
        <<"daemon_addr">>   => DAddr,
        <<"delegation">>    => Wire
    }).

build_data_frame(SrcAddr, DstAddr, Payload) ->
    macula_cbor_nif:pack(#{
        <<"v">>       => 1,
        <<"type">>    => <<"data">>,
        <<"src">>     => SrcAddr,
        <<"dst">>     => DstAddr,
        <<"payload">> => Payload
    }).

%% =============================================================================
%% Tests
%% =============================================================================

controller_test_() ->
    {inorder, [
        {timeout, 5,
         {setup, fun setup/0, fun cleanup/1, fun attach_dispatches_to_attach_fn/1}},
        {timeout, 5,
         {setup, fun setup/0, fun cleanup/1, fun attach_passes_stream_ref_through/1}},
        {timeout, 5,
         {setup, fun setup/0, fun cleanup/1, fun attach_with_garbage_delegation_is_dropped/1}},
        {timeout, 5,
         {setup, fun setup/0, fun cleanup/1, fun data_for_hosted_dst_forwards_on_stored_stream/1}},
        {timeout, 5,
         {setup, fun setup/0, fun cleanup/1, fun data_for_unhosted_dst_falls_through/1}},
        {timeout, 5,
         {setup, fun setup/0, fun cleanup/1, fun garbage_cbor_falls_through/1}},
        {timeout, 5,
         {setup, fun setup/0, fun cleanup/1, fun unknown_type_falls_through/1}},
        {timeout, 5,
         {setup, fun setup/0, fun cleanup/1, fun delegation_from_wire_round_trips/1}}
    ]}.

attach_dispatches_to_attach_fn(Ctx) ->
    fun() ->
        start_controller(Ctx),
        Daemon = new_actor(),
        Frame  = build_attach_frame(Daemon, maps:get(host, Ctx)),
        StreamRef = make_ref(),
        ok = macula_host_attach_controller:handle(Frame, StreamRef),
        Got = await(attach, 1000),
        ?assertMatch({attach, _, _, _, StreamRef}, Got),
        {attach, Addr, Pk, _Del, _Ref} = Got,
        ?assertEqual(maps:get(addr, Daemon), Addr),
        ?assertEqual(maps:get(pk,   Daemon), Pk)
    end.

attach_passes_stream_ref_through(Ctx) ->
    fun() ->
        start_controller(Ctx),
        Daemon = new_actor(),
        Frame  = build_attach_frame(Daemon, maps:get(host, Ctx)),
        Ref1 = make_ref(),
        Ref2 = make_ref(),
        ok = macula_host_attach_controller:handle(Frame, Ref1),
        {attach, _, _, _, Got1} = await(attach, 1000),
        ?assertEqual(Ref1, Got1),
        ok = macula_host_attach_controller:handle(Frame, Ref2),
        {attach, _, _, _, Got2} = await(attach, 1000),
        ?assertEqual(Ref2, Got2)
    end.

attach_with_garbage_delegation_is_dropped(Ctx) ->
    fun() ->
        start_controller(Ctx),
        Daemon = new_actor(),
        Bad = macula_cbor_nif:pack(#{
            <<"type">>          => <<"macula_attach_v1">>,
            <<"daemon_pubkey">> => maps:get(pk, Daemon),
            <<"daemon_addr">>   => maps:get(addr, Daemon),
            <<"delegation">>    => #{<<"garbage">> => true}
        }),
        ok = macula_host_attach_controller:handle(Bad, make_ref()),
        %% Controller silently drops malformed delegations — neither
        %% attach_fn nor fallback_fn fires.
        ?assertEqual(timeout, await(attach, 100)),
        ?assertEqual(timeout, await(fallback, 100))
    end.

data_for_hosted_dst_forwards_on_stored_stream(Ctx) ->
    fun() ->
        start_controller(Ctx),
        Src   = new_actor(),
        Dst   = new_actor(),
        Frame = build_data_frame(maps:get(addr, Src),
                                  maps:get(addr, Dst),
                                  <<"hello">>),
        OutStream = make_ref(),
        InStream  = make_ref(),
        ok = set_lookup(maps:get(addr, Dst), {ok, OutStream}),
        ok = macula_host_attach_controller:handle(Frame, InStream),
        Got = await(sent, 2000),
        ?assertMatch({sent, OutStream, _Bytes}, Got),
        {sent, _, Bytes} = Got,
        %% Re-framed length-prefix is the original Cbor's size.
        Sz = byte_size(Frame),
        ?assertMatch(<<Sz:32/big, Frame/binary>>, Bytes),
        %% No fallback should fire when the dst was hosted.
        ?assertEqual(timeout, await(fallback, 100))
    end.

data_for_unhosted_dst_falls_through(Ctx) ->
    fun() ->
        start_controller(Ctx),
        Src   = new_actor(),
        Dst   = new_actor(),
        Frame = build_data_frame(maps:get(addr, Src),
                                  maps:get(addr, Dst),
                                  <<"hello">>),
        %% No set_lookup/2 call -> lookup returns not_found.
        ok = macula_host_attach_controller:handle(Frame, make_ref()),
        Got = await(fallback, 2000),
        ?assertEqual({fallback, Frame}, Got),
        ?assertEqual(timeout, await(sent, 100))
    end.

garbage_cbor_falls_through(Ctx) ->
    fun() ->
        start_controller(Ctx),
        Garbage = <<255, 255, 255, 255>>,
        ok = macula_host_attach_controller:handle(Garbage, make_ref()),
        Got = await(fallback, 1000),
        ?assertEqual({fallback, Garbage}, Got)
    end.

unknown_type_falls_through(Ctx) ->
    fun() ->
        start_controller(Ctx),
        Frame = macula_cbor_nif:pack(#{
            <<"type">> => <<"some_unknown_v1">>,
            <<"x">>    => 1
        }),
        ok = macula_host_attach_controller:handle(Frame, make_ref()),
        Got = await(fallback, 1000),
        ?assertEqual({fallback, Frame}, Got)
    end.

delegation_from_wire_round_trips(_Ctx) ->
    fun() ->
        Daemon = new_actor(),
        Host   = new_actor(),
        Now    = now_ms(),
        Signed = macula_record:sign_host_delegation(
                   macula_record:host_delegation(
                     maps:get(pk, Daemon), maps:get(pk, Host), ?REALM,
                     Now, Now + 60_000),
                   maps:get(kp, Daemon)),
        Wire = #{
            <<"d">>  => maps:get(daemon_pubkey, Signed),
            <<"h">>  => maps:get(host_pubkey,   Signed),
            <<"r">>  => maps:get(realm_pubkey,  Signed),
            <<"nb">> => maps:get(not_before_ms, Signed),
            <<"na">> => maps:get(not_after_ms,  Signed),
            <<"s">>  => maps:get(daemon_sig,    Signed)
        },
        Got = macula_host_attach_controller:delegation_from_wire(Wire),
        ?assertEqual(Signed, Got),
        ?assertEqual(error,
                     macula_host_attach_controller:delegation_from_wire(
                       #{<<"oops">> => true}))
    end.
