%% @doc BERT-encoded wire frames for Macula V2.
%%
%% A wire frame is a length-prefixed BERT term:
%% <pre>
%%   <<Length:32/big, Bert/binary>>
%% </pre>
%% where `Bert' is the deterministic external-term-format encoding of a single
%% map. The map carries the common header fields (`Part 6 §3') plus
%% type-specific fields and an Ed25519 signature.
%%
%% Phase 1 covers CONNECT / HELLO / GOODBYE only; CALL / PUBLISH / DHT /
%% SWIM frames land in later phases.
%%
%% Signatures are Ed25519 over `"macula-v2-frame\0" ++ canonical_bert(unsigned)'
%% where `canonical_bert' is `term_to_binary' with `[{minor_version, 2},
%% deterministic]' (OTP 27+).
-module(macula_frame).

-export([
    %% Constructors
    connect/1, hello/1, goodbye/2, goodbye/3,

    %% Sign / verify
    sign/2, verify/2,

    %% Wire codec — single frame
    encode/1, decode/1,

    %% Stream parser — drain frames from a buffer
    parse_stream/1,

    %% Accessors
    frame_type/1, frame_id/1, version/1, signature/1, sent_at_ms/1
]).

-export_type([
    frame/0,
    frame_type/0,
    connect_spec/0,
    hello_spec/0
]).

-define(SIG_DOMAIN, "macula-v2-frame\0").
-define(PROTOCOL_VERSION, 2).
-define(MAX_FRAME_BYTES, 16#FFFFFF).   %% 16 MiB cap (Part 6 §2.2).

-type frame_type() :: connect | hello | goodbye.

-type frame() :: map().

-type connect_spec() :: #{
    node_id          := macula_identity:pubkey(),
    station_id       := macula_identity:pubkey(),
    realms           := [macula_identity:pubkey()],
    capabilities     := non_neg_integer(),
    puzzle_evidence  := <<_:256>>,
    addresses        => [map()],
    site             => map() | undefined,
    endorsements     => [map()]
}.

-type hello_spec() :: #{
    node_id                 := macula_identity:pubkey(),
    station_id              := macula_identity:pubkey(),
    realms                  := [macula_identity:pubkey()],
    capabilities            := non_neg_integer(),
    accepted                := boolean(),
    negotiated_capabilities := non_neg_integer(),
    addresses               => [map()],
    site                    => map() | undefined,
    refusal_code            => non_neg_integer() | undefined
}.

%%------------------------------------------------------------------
%% Constructors
%%------------------------------------------------------------------

-spec connect(connect_spec()) -> frame().
connect(#{node_id := NodeId, station_id := StationId,
          realms := Realms, capabilities := Caps,
          puzzle_evidence := Puzzle} = Spec)
  when is_binary(NodeId), byte_size(NodeId) =:= 32,
       is_binary(StationId), byte_size(StationId) =:= 32,
       is_list(Realms),
       is_integer(Caps), Caps >= 0,
       is_binary(Puzzle), byte_size(Puzzle) =:= 32 ->
    Header = base(connect, Caps),
    Header#{
        node_id          => NodeId,
        station_id       => StationId,
        realms           => Realms,
        addresses        => maps:get(addresses, Spec, []),
        site             => maps:get(site, Spec, undefined),
        puzzle_evidence  => Puzzle,
        endorsements     => maps:get(endorsements, Spec, [])
    }.

-spec hello(hello_spec()) -> frame().
hello(#{node_id := NodeId, station_id := StationId,
        realms := Realms, capabilities := Caps,
        accepted := Accepted,
        negotiated_capabilities := Negotiated} = Spec)
  when is_binary(NodeId), byte_size(NodeId) =:= 32,
       is_binary(StationId), byte_size(StationId) =:= 32,
       is_list(Realms),
       is_integer(Caps), Caps >= 0,
       is_boolean(Accepted),
       is_integer(Negotiated), Negotiated >= 0 ->
    Header = base(hello, Caps),
    Header#{
        node_id                 => NodeId,
        station_id              => StationId,
        realms                  => Realms,
        addresses               => maps:get(addresses, Spec, []),
        site                    => maps:get(site, Spec, undefined),
        accepted                => Accepted,
        refusal_code            => maps:get(refusal_code, Spec, undefined),
        negotiated_capabilities => Negotiated
    }.

-spec goodbye(atom(), binary() | undefined) -> frame().
goodbye(Reason, Detail) ->
    goodbye(Reason, Detail, 0).

-spec goodbye(atom(), binary() | undefined, non_neg_integer()) -> frame().
goodbye(Reason, undefined, Caps) when is_atom(Reason), is_integer(Caps), Caps >= 0 ->
    do_goodbye(Reason, undefined, Caps);
goodbye(Reason, Detail, Caps)
  when is_atom(Reason), is_binary(Detail), is_integer(Caps), Caps >= 0 ->
    do_goodbye(Reason, Detail, Caps).

do_goodbye(Reason, Detail, Caps) ->
    Header = base(goodbye, Caps),
    Header#{reason => Reason, detail => Detail}.

%%------------------------------------------------------------------
%% Sign / verify
%%------------------------------------------------------------------

-spec sign(frame(), macula_identity:key_pair() | macula_identity:privkey()) ->
    frame().
sign(Frame, Identity) ->
    Bytes = canonical_unsigned(Frame),
    Sig = macula_identity:sign([?SIG_DOMAIN, Bytes], Identity),
    Frame#{signature => Sig}.

-spec verify(frame(), macula_identity:pubkey()) ->
    {ok, frame()} | {error, term()}.
verify(#{signature := Sig} = Frame, Pub)
  when is_binary(Sig), byte_size(Sig) =:= 64,
       is_binary(Pub), byte_size(Pub) =:= 32 ->
    Bytes = canonical_unsigned(Frame),
    verify_result(macula_identity:verify([?SIG_DOMAIN, Bytes], Sig, Pub),
                  Frame);
verify(_Frame, _Pub) ->
    {error, bad_frame}.

verify_result(true,  Frame) -> {ok, Frame};
verify_result(false, _Frame) -> {error, signature_invalid}.

%%------------------------------------------------------------------
%% Wire codec
%%------------------------------------------------------------------

-spec encode(frame()) -> binary().
encode(Frame) when is_map(Frame) ->
    Bytes = term_to_binary(Frame, [{minor_version, 2}, deterministic]),
    Len = byte_size(Bytes),
    encode_with_check(Len, Bytes).

encode_with_check(Len, _Bytes) when Len > ?MAX_FRAME_BYTES ->
    error({frame_too_large, Len});
encode_with_check(Len, Bytes) ->
    <<Len:32/big, Bytes/binary>>.

%% @doc Decode a single length-prefixed frame from the head of a buffer.
%% Returns `{ok, Frame, RestBuffer}', `{more, BytesNeeded}' if the buffer
%% is short, or `{error, Reason}' if the framing is malformed.
-spec decode(binary()) ->
    {ok, frame(), binary()}
  | {more, pos_integer()}
  | {error, term()}.
decode(<<Len:32/big, _Rest/binary>>) when Len > ?MAX_FRAME_BYTES ->
    {error, frame_too_large};
decode(<<Len:32/big, Bytes:Len/binary, Rest/binary>>) ->
    decode_term(Bytes, Rest);
decode(<<Len:32/big, Tail/binary>>) ->
    {more, Len - byte_size(Tail)};
decode(Buf) when is_binary(Buf), byte_size(Buf) < 4 ->
    {more, 4 - byte_size(Buf)}.

decode_term(Bytes, Rest) ->
    decode_safe(catch binary_to_term(Bytes, [safe]), Rest).

decode_safe({'EXIT', _}, _Rest) ->
    {error, bad_frame};
decode_safe(Term, Rest) when is_map(Term) ->
    {ok, Term, Rest};
decode_safe(_Term, _Rest) ->
    {error, bad_frame}.

%% @doc Drain all complete frames from a buffer. Returns the list of frames
%% (in order) and the remaining (incomplete) buffer.
-spec parse_stream(binary()) -> {[frame()], binary()}.
parse_stream(Buf) when is_binary(Buf) ->
    drain(Buf, []).

drain(Buf, Acc) ->
    drain_step(decode(Buf), Buf, Acc).

drain_step({ok, Frame, Rest}, _Buf, Acc) ->
    drain(Rest, [Frame | Acc]);
drain_step({more, _N}, Buf, Acc) ->
    {lists:reverse(Acc), Buf};
drain_step({error, _R}, Buf, Acc) ->
    %% Stop draining on first parse error; surface buffer as-is.
    {lists:reverse(Acc), Buf}.

%%------------------------------------------------------------------
%% Accessors
%%------------------------------------------------------------------

frame_type(#{frame_type := T}) -> T.
frame_id(#{frame_id := Id}) -> Id.
version(#{version := V}) -> V.
sent_at_ms(#{sent_at_ms := T}) -> T.
signature(#{signature := S}) -> S.

%%------------------------------------------------------------------
%% Internals
%%------------------------------------------------------------------

base(FrameType, Caps) ->
    #{
        version      => ?PROTOCOL_VERSION,
        frame_type   => FrameType,
        frame_id     => macula_record_uuid:v7(),
        sent_at_ms   => erlang:system_time(millisecond),
        capabilities => Caps,
        realm        => undefined,
        call_id      => undefined,
        source_route => undefined
    }.

canonical_unsigned(Frame) ->
    Unsigned = maps:without([signature], Frame),
    term_to_binary(Unsigned, [{minor_version, 2}, deterministic]).
