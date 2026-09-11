%% @doc CBOR-encoded wire frames for Macula V2 (Part 6 §3 canonical wire).
%%
%% A wire frame is a length-prefixed deterministic CBOR map:
%% <pre>
%%   &lt;&lt;Length:32/big, Cbor/binary&gt;&gt;
%% </pre>
%% where `Cbor' is the RFC 8949 §4.2.1 deterministic encoding of a single
%% map. The map carries the common header fields (`Part 6 §3') plus
%% type-specific fields and an Ed25519 signature.
%%
%% PLAN_WIRE_CBOR.md migrated this codec from BERT to CBOR so
%% hecate-station and the macula 3.x SDK share a wire format. Frame
%% schemas (atom-keyed maps in process memory) are unchanged. Atoms go out
%% as text; on decode a frame type's own fields come back through a fixed
%% table, and peer-supplied maps keep the one key form (D26).
%%
%% Phase 1 covers CONNECT / HELLO / GOODBYE. Phase 2 adds SWIM. Phase 3
%% (Session 3.4) adds the DHT operation frames from Part 6 §7:
%% PING / PONG, FIND_NODE / NODES, FIND_VALUE / VALUE and STORE / STORE_ACK.
%% CALL and STREAM_OPEN carry a request signed by the caller, RESULT and
%% ERROR a reply signed by the provider, and ERROR and STREAM_ERROR from a
%% station a relay error signed by that station (D25). PUBLISH frames land
%% later.
%%
%% Signatures are Ed25519 over `"macula-v2-frame\0" ++ canonical_cbor(unsigned)'
%% where `canonical_cbor' is `macula_cbor_nif:pack_deterministic/1' (RFC 8949
%% §4.2.1 deterministic — same rules `macula_record_cbor:encode/1' implements
%% in pure Erlang, kept as the differentially-tested reference; see
%% `test/macula_cbor_deterministic_diff_tests.erl').
-module(macula_frame).

-export([
    %% Constructors — handshake
    connect/1, hello/1, goodbye/2, goodbye/3,

    %% Constructors — SWIM
    swim_ping/1, swim_ack/1, swim_suspect/1, swim_confirm/1,
    swim_update/1,

    %% Constructors — DHT (Part 6 §7)
    ping/1, pong/1,
    find_node/1, nodes/1,
    find_value/1, value/1,
    store/1, store_ack/1,

    %% DHT helper — build and validate a station_ref entry
    station_ref/1,

    %% Constructors — CALL (Part 6 §5)
    call/2, result/2, provider_error/2, relay_error/2,
    verify_request/2, verify_reply/3, verify_relay_error/3,

    %% Constructors — HyParView (Part 3 §7.1)
    hyparview_join/1, hyparview_forward_join/1, hyparview_neighbor/1,
    hyparview_disconnect/1, hyparview_shuffle/1, hyparview_shuffle_reply/1,

    %% Constructors — Plumtree (Part 3 §7.2)
    plumtree_gossip/1, plumtree_ihave/1, plumtree_graft/1, plumtree_prune/1,

    %% Constructor — overlay relay envelope (Phase 3.5). Wraps an
    %% already-encoded HyParView/Plumtree frame with an explicit target
    %% peer NodeId so a station can forward it to whichever of its OTHER
    %% connections authenticates as that peer. Opaque `payload' — the
    %% station never decodes it.
    overlay_relay/1,

    %% Constructors — PubSub (Part 6 §6)
    publish/2, subscribe/1, unsubscribe/1, event/1, verify_publication/3,

    %% Constructors — RPC procedure advertise (connection-scoped,
    %% Part 6 §5.5). Companions to call/result/error: a peer connected
    %% to a station registers itself as the handler for a procedure
    %% URI in the station's per-connection routing table; the station
    %% forwards inbound CALL frames for that procedure back over the
    %% advertiser's connection. Tombstoned on UNADVERTISE or peer
    %% disconnect.
    advertise/1, unadvertise/1,

    %% Constructors — Streaming RPC (Part 6 §5.6)
    stream_open/2, open_stream/1, provider_stream/3, caller_stream/3,
    verify_provider_stream/3, verify_caller_stream/3, charged_refusal/1,

    %% Constructors — Content transfer (Part 6 §9)
    want/1, have/1, block/1,
    manifest_req/1, manifest_res/1, cancel/1,

    %% Neighbour signatures on control frames in pq_hybrid (D17)
    neighbour_signed/2, sign_neighbour/3, verify_neighbour/2,

    %% Sign / verify frame
    sign/2, verify/2,

    %% Wire codec — single frame
    encode/1, decode/1,

    %% Wire codec for the post-quantum handshake: frame CBOR bytes exactly as
    %% sent and received, which the connection proof hashes
    encode_bytes/1, parse_stream_bytes/1, read_wire/1,

    %% Sendability, checked before a frame is cast at a peering
    %% connection. Mirrors `to_wire/1' + `macula_record_cbor'.
    check_payload/1, check_frame/1, explain/1,

    %% Stream parser — drain frames from a buffer
    parse_stream/1,

    %% Accessors
    frame_type/1, frame_id/1, version/1, signature/1, sent_at_ms/1
]).

-export_type([
    frame/0,
    frame_type/0,
    connect_spec/0,
    hello_spec/0,
    swim_ping_spec/0,
    swim_ack_spec/0,
    swim_suspect_spec/0,
    swim_update/0,
    swim_update_spec/0,
    member_state/0,
    ping_spec/0, pong_spec/0,
    find_node_spec/0, nodes_spec/0,
    find_value_spec/0, value_spec/0,
    store_spec/0, store_ack_spec/0,
    station_ref/0, station_ref_spec/0,
    request_spec/0, verified_request/0,
    call_id/0,
    hyparview_join_spec/0, hyparview_forward_join_spec/0,
    hyparview_neighbor_spec/0, hyparview_disconnect_spec/0,
    hyparview_shuffle_spec/0, hyparview_shuffle_reply_spec/0,
    neighbor_priority/0,
    plumtree_gossip_spec/0, plumtree_ihave_spec/0,
    plumtree_graft_spec/0, plumtree_prune_spec/0,
    msg_id/0,
    overlay_relay_spec/0,
    publish_spec/0, subscribe_spec/0, unsubscribe_spec/0, event_spec/0, verified_publication/0,
    advertise_spec/0, unadvertise_spec/0,
    delivery_channel/0,
    stream_id/0, stream_mode/0, stream_encoding/0, stream_role/0,
    stream_spec/0, stream_state/0,
    mcid/0, want_priority/0, want_entry/0, have_entry/0,
    want_spec/0, have_spec/0, block_spec/0,
    manifest_req_spec/0, manifest_res_spec/0, cancel_spec/0
]).

-define(SIG_DOMAIN,        "macula-v2-frame\0").
-define(PROTOCOL_VERSION,   2).
-define(MAX_FRAME_BYTES,    16#FFFFFF).   %% 16 MiB cap (Part 6 §2.2).
%% A payload sits in the frame map, so a container at payload path length L
%% is at nesting depth L + 2, and the decoding rule allows depth 64.
-define(MAX_PAYLOAD_NESTING, 62).
%% A GOODBYE reason is text for people, bounded because it ends up in logs.
-define(MAX_GOODBYE_REASON_BYTES, 256).
%% A protocol integer in a signed structure stays below 2^53 (the decoding rule).
-define(MAX_PROTOCOL_INT, 1 bsl 53).
-define(REQUEST_LABEL, <<"MACULA-PQ-REQUEST-V1">>).
-define(REPLY_LABEL, <<"MACULA-PQ-REPLY-V1">>).
-define(RELAY_ERROR_LABEL, <<"MACULA-PQ-RELAY-ERROR-V1">>).
%% The relay error codes, a closed set disjoint from every provider code (D25 item 7).
-define(RELAY_CODES, [unknown_next_peer]).
-define(STREAM_LABEL, <<"MACULA-PQ-STREAM-V1">>).
-define(CALLER_STREAM_LABEL, <<"MACULA-PQ-CALLER-STREAM-V1">>).
-define(PUBLICATION_LABEL, <<"MACULA-PQ-PUBLICATION-V1">>).
%% A publication verifies from 5 minutes before its published_at until its ttl_ms, or 10 minutes without one, plus 5.
-define(PUBLICATION_TOLERANCE_MS, 5 * 60000).
-define(PUBLICATION_DEFAULT_TTL_MS, 10 * 60000).
-define(PUBLICATION_MAX_TTL_MS, 60 * 60000).
-define(NEIGHBOUR_LABEL, <<"MACULA-PQ-NEIGHBOUR-V1">>).
%% The control frames, which pq_hybrid neighbour-signs (D17). Data frames carry their own end-to-end signatures.
-define(NEIGHBOUR_SIGNED,
        [swim_ping, swim_ack, swim_suspect, swim_confirm, ping, pong, find_node, nodes, find_value, value,
         store, store_ack, advertise, unadvertise, subscribe, unsubscribe,
         overlay_relay, hyparview_join, hyparview_forward_join, hyparview_neighbor, hyparview_disconnect,
         hyparview_shuffle, hyparview_shuffle_reply, plumtree_ihave, plumtree_graft, plumtree_prune,
         goodbye]).

-type frame_type() :: connect | hello | goodbye
                    | swim_ping | swim_ack | swim_suspect | swim_confirm
                    | ping | pong
                    | find_node | nodes
                    | find_value | value
                    | store | store_ack
                    | call | result | error
                    | hyparview_join | hyparview_forward_join
                    | hyparview_neighbor | hyparview_disconnect
                    | hyparview_shuffle | hyparview_shuffle_reply
                    | plumtree_gossip | plumtree_ihave
                    | plumtree_graft | plumtree_prune
                    | overlay_relay
                    | publish | subscribe | unsubscribe | event
                    | advertise | unadvertise
                    | stream_open | stream_data | stream_end
                    | stream_error | stream_reply
                    | want | have | block
                    | manifest_req | manifest_res | cancel.

-type member_state() :: alive | suspect | confirmed_failed.

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

-type swim_update_spec() :: #{
    target      := id256(),
    state       := member_state(),
    incarnation := non_neg_integer(),
    observed_at := pos_integer(),
    by          := id256()
}.

-type swim_update() :: #{
    target      := id256(),
    state       := member_state(),
    incarnation := non_neg_integer(),
    observed_at := pos_integer(),
    by          := id256()
}.

-type swim_ping_spec() :: #{
    round       := non_neg_integer(),
    incarnation := non_neg_integer(),
    piggyback   => [swim_update()]
}.

-type swim_ack_spec() :: #{
    round       := non_neg_integer(),
    responder   := id256(),
    incarnation := non_neg_integer(),
    piggyback   => [swim_update()]
}.

-type swim_suspect_spec() :: #{
    target             := id256(),
    target_incarnation := non_neg_integer(),
    suspected_by       := id256(),
    ttl                := non_neg_integer()
}.

%%------------------------------------------------------------------
%% DHT frame specs (Part 6 §7)
%%
%% `key' and `origin' are 32-byte identifiers (NodeId / RealmId /
%% SHA-256 derivation per Part 3 §3.3). `country' is the 2-byte
%% ISO-3166-1 alpha-2 code. A `station_ref()' is the tier-diverse
%% routing-table payload returned in NODES responses.
%%------------------------------------------------------------------

-type id256() :: <<_:256>>.
-type nonce128() :: <<_:128>>.
-type tier() :: 0..4.
-type country() :: <<_:16>>.

-type station_ref_spec() :: #{
    node_id      := id256(),
    station_id   := id256(),
    addresses    => [map()],
    tier         := tier(),
    asn          => non_neg_integer() | undefined,
    country      := country(),
    last_seen_at := pos_integer()
}.

-type station_ref() :: #{
    node_id      := id256(),
    station_id   := id256(),
    addresses    := [map()],
    tier         := tier(),
    asn          := non_neg_integer() | undefined,
    country      := country(),
    last_seen_at := pos_integer()
}.

-type ping_spec()          :: #{nonce := nonce128()}.
-type pong_spec()          :: #{nonce := nonce128()}.

-type find_node_spec()     :: #{
    key    := id256(),
    origin := id256(),
    depth  := non_neg_integer()
}.

-type nodes_spec()         :: #{
    key   := id256(),
    nodes := [station_ref()]
}.

-type find_value_spec()    :: #{
    key    := id256(),
    origin := id256()
}.

-type value_spec()         :: #{
    key     := id256(),
    records := [binary()]
}.

-type store_spec()         :: #{record := binary()}.

-type store_ack_spec()     :: #{
    key    := id256(),
    stored := boolean()
}.

%%------------------------------------------------------------------
%% CALL frame specs (Part 6 §5)
%%------------------------------------------------------------------

-type call_id() :: <<_:128>>.

%% A request as its caller gives it to call/2 and stream_open/2 (D25). mode belongs to STREAM_OPEN only, and
%% source_route and retry_budget are routing fields outside the signature.
-type request_spec() :: #{
    request_id   := <<_:128>>,
    realm        := id256(),
    procedure    := binary(),
    target       := id256(),
    deadline     := non_neg_integer(),
    payload      := term(),
    mode         => stream_mode(),
    token        => binary(),
    source_route => binary(),
    retry_budget => non_neg_integer()
}.

%% A request that verified: its fields, the caller's carried key, and request_hash, the SHA-384 of its tbs.
-type verified_request() :: #{
    frame_type   := call | stream_open,
    key          := binary(),
    request_hash := <<_:384>>,
    caller       := id256(),
    request_id   := <<_:128>>,
    realm        := id256(),
    procedure    := binary(),
    target       := id256(),
    deadline     := non_neg_integer(),
    payload      := term(),
    mode         => stream_mode(),
    token        => binary()
}.

%%------------------------------------------------------------------
%% HyParView frame specs (Part 3 §7.1)
%%------------------------------------------------------------------

-type neighbor_priority() :: high | low.

-type hyparview_join_spec() :: #{
    realm      := id256(),
    new_member := id256(),
    %% Signed `realm_member_endorsement' macula_record (see
    %% hecate_overlay's hecate_realm_join module), proving the realm's
    %% admin authorised `new_member' to join. Optional at the type
    %% level since not every realm may require admission-gated JOIN,
    %% but any realm that does MUST reject a JOIN missing it. The record
    %% travels as its wire form, as received, and the receiver verifies it.
    record     => binary()
}.

-type hyparview_forward_join_spec() :: #{
    realm      := id256(),
    new_member := id256(),
    ttl        := non_neg_integer(),
    arwl       := non_neg_integer(),
    prwl       := non_neg_integer(),
    %% Carries the ORIGINAL JOIN's endorsement through the forward
    %% chain, so every peer that admits `new_member' off a
    %% FORWARD_JOIN verifies the same admission proof the original
    %% JOIN carried -- trust is never transitively assumed from
    %% "my neighbour forwarded this to me." See `record' above.
    record     => binary()
}.

-type hyparview_neighbor_spec() :: #{
    realm    := id256(),
    priority := neighbor_priority(),
    %% The SENDER's own signed `realm_member_endorsement', so the
    %% receiver can verify admission the same way it would for a
    %% fresh JOIN -- a NEIGHBOR is an admission event too (it can
    %% arrive unsolicited, e.g. shuffle-driven promotion, not only as
    %% an ack to a JOIN the receiver itself initiated), so trust is
    %% never assumed just because a frame is shaped like an ack. See
    %% `record' on `hyparview_join_spec()'.
    record   => binary()
}.

-type hyparview_disconnect_spec() :: #{
    realm := id256()
}.

-type hyparview_shuffle_spec() :: #{
    realm       := id256(),
    origin      := id256(),
    ttl         := non_neg_integer(),
    peer_sample := [id256()]
}.

-type hyparview_shuffle_reply_spec() :: #{
    realm       := id256(),
    peer_sample := [id256()]
}.

%%------------------------------------------------------------------
%% Plumtree frame specs (Part 3 §7.2)
%%------------------------------------------------------------------

%% A publication's id in Plumtree bookkeeping: the SHA-384 of its tbs.
-type msg_id() :: <<_:384>>.

-type plumtree_gossip_spec() :: #{
    publication := macula_signed_object:object(),
    round       := non_neg_integer()
}.

-type plumtree_ihave_spec() :: #{
    realm  := id256(),
    msg_id := msg_id(),
    round  := non_neg_integer()
}.

-type plumtree_graft_spec() :: #{
    realm  := id256(),
    msg_id := msg_id(),
    round  := non_neg_integer()
}.

-type plumtree_prune_spec() :: #{
    realm := id256()
}.

%%------------------------------------------------------------------
%% Overlay relay envelope spec (Phase 3.5)
%%
%% Wraps an already-`encode/1'd overlay frame (HyParView/Plumtree) so a
%% station can forward it to whichever of its OTHER connections
%% authenticates as `peer'. Carries no `realm' of its own — the wrapped
%% frame's own `realm' field (inside `payload') is what the receiving
%% end routes on after decoding.
%%------------------------------------------------------------------

-type overlay_relay_spec() :: #{
    peer    := id256(),
    payload := binary()
}.

%%------------------------------------------------------------------
%% PubSub frame specs (Part 6 §6)
%%------------------------------------------------------------------

-type delivery_channel() :: plumtree | direct.

%% A publication as its publisher gives it to publish/2.
-type publish_spec() :: #{
    realm        := id256(),
    topic        := binary(),
    seq          := non_neg_integer(),
    published_at := non_neg_integer(),
    payload      := term(),
    ttl_ms       => 0..3600000
}.

%% A publication that verified: its fields, the publisher's carried key, publication_hash, the SHA-384 of its tbs, which
%% deduplication and Plumtree bookkeeping key on, and expires_at, the last moment a verifier accepts it: published_at
%% plus its ttl_ms, or 10 minutes without one, plus 5 minutes. A subscriber keeps a delivered publication's hash until
%% then.
-type verified_publication() :: #{
    publisher        := id256(),
    realm            := id256(),
    topic            := binary(),
    seq              := non_neg_integer(),
    published_at     := non_neg_integer(),
    ttl_ms           => 0..3600000,
    payload          := term(),
    key              := binary(),
    publication_hash := msg_id(),
    expires_at       := non_neg_integer()
}.

-type subscribe_spec() :: #{
    topic      := binary(),
    realm      := id256(),
    subscriber := id256(),
    filter     => term() | undefined,
    options    => map()
}.

-type unsubscribe_spec() :: #{
    topic      := binary(),
    realm      := id256(),
    subscriber := id256()
}.

%% An EVENT carries the publication bytes of the PUBLISH it was made from, unchanged.
-type event_spec() :: #{
    publication   := macula_signed_object:object(),
    delivered_via := delivery_channel()
}.

%%------------------------------------------------------------------
%% RPC advertise frame specs (Part 6 §5.5)
%%
%% A peer connected to a station declares itself the handler for
%% `procedure' under `realm'. The station's per-connection
%% advertise registry routes inbound CALL frames for that procedure
%% back across the advertiser's QUIC connection. Tombstoned on
%% explicit UNADVERTISE or on peer disconnect (cleanup runs in the
%% peer_observer's terminate path).
%%------------------------------------------------------------------

%% ADVERTISE carries the provider's signed procedure advertisement record, and UNADVERTISE the tombstone that
%% withdraws it, each as record bytes that stations forward unchanged.
-type advertise_spec() :: #{advertisement := binary()}.

-type unadvertise_spec() :: #{withdrawal := binary()}.

%%------------------------------------------------------------------
%% Streaming RPC frame specs (Part 6 §5.6)
%%
%% A streaming RPC threads chunks across a single logical stream
%% identified by a 16-byte `stream_id'. STREAM_OPEN initiates;
%% STREAM_DATA carries chunks in either direction; STREAM_END
%% half-closes (`role = send') or fully closes (`role = both');
%% STREAM_ERROR aborts; STREAM_REPLY delivers the terminal value for
%% client-stream / bidi modes.
%%
%% Procedure registration reuses the unary `advertise' / `unadvertise'
%% frames; the receiving link tracks `mode' locally per procedure and
%% dispatches inbound STREAM_OPEN to the registered streaming handler.
%%------------------------------------------------------------------

-type stream_id() :: <<_:128>>.

-type stream_mode() :: server_stream | client_stream | bidi.

-type stream_encoding() :: raw | msgpack.

-type stream_role() :: send | both.

%% A stream frame as its sender gives it to provider_stream/3 or caller_stream/3: its type, the sender's own sequence
%% number and the fields of its type. STREAM_REPLY is the provider's only.
-type stream_spec() :: #{
    frame_type := stream_data | stream_end | stream_error | stream_reply,
    seq        := non_neg_integer(),
    encoding   => stream_encoding(),
    body       => term(),
    role       => stream_role(),
    code       => binary(),
    message    => binary(),
    payload    => term()
}.

%% What a verifier holds for one stream: the verified STREAM_OPEN and, per side, the next sequence number and whether
%% that side has ended; for the provider also the key and signer its first frame carried.
-type stream_state() :: #{
    request  := verified_request(),
    provider := #{next := non_neg_integer(), ended := boolean(), key => binary(), signer => id256()},
    caller   := #{next := non_neg_integer(), ended := boolean()}
}.

%%------------------------------------------------------------------
%% Content transfer frame specs (Part 6 §9)
%%
%% MCID, Macula Content IDentifier, 50 bytes:
%% &lt;&lt;Tag:8, Codec:8, Hash:48/binary&gt;&gt;, tag 2 for SHA-384 (D24). Block payloads carry
%% raw chunk bytes; manifest payloads carry the structured manifest
%% map. Frames are signed by the sender for accountability; the
%% recipient verifies the signature on top of the per-block /
%% per-manifest hash check.
%%------------------------------------------------------------------

-type mcid() :: <<_:400>>.

-type want_priority() :: 0..255.

-type want_entry() :: #{
    mcid     := mcid(),
    priority => want_priority()
}.

-type have_entry() :: #{
    mcid := mcid(),
    size := non_neg_integer()
}.

-type want_spec() :: #{
    blocks := [want_entry()]
}.

-type have_spec() :: #{
    blocks := [have_entry()]
}.

-type block_spec() :: #{
    mcid    := mcid(),
    payload := binary()
}.

-type manifest_req_spec() :: #{
    mcid := mcid()
}.

-type manifest_res_spec() :: #{
    mcid     := mcid(),
    manifest := map()
}.

-type cancel_spec() :: #{
    blocks := [mcid()]
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
    ok = reason_within_bound(byte_size(atom_to_binary(Reason)) =< ?MAX_GOODBYE_REASON_BYTES),
    Header = base(goodbye, Caps),
    Header#{reason => Reason, detail => Detail}.

reason_within_bound(true) -> ok.

%%------------------------------------------------------------------
%% SWIM frame constructors (Part 6 §8)
%%
%% Ping / Ack carry the sender's current `incarnation' and a list of
%% piggyback updates. Suspect / Confirm are the explicit dissemination
%% path; their `ttl' is decremented by each rebroadcaster.
%%------------------------------------------------------------------

-spec swim_ping(swim_ping_spec()) -> frame().
swim_ping(#{round := Round, incarnation := Inc} = Spec)
  when is_integer(Round), Round >= 0,
       is_integer(Inc), Inc >= 0 ->
    Header = base(swim_ping, 0),
    Header#{
        round       => Round,
        incarnation => Inc,
        piggyback   => maps:get(piggyback, Spec, [])
    }.

-spec swim_ack(swim_ack_spec()) -> frame().
swim_ack(#{round := Round, responder := Responder, incarnation := Inc} = Spec)
  when is_integer(Round), Round >= 0,
       is_binary(Responder), byte_size(Responder) =:= 32,
       is_integer(Inc), Inc >= 0 ->
    Header = base(swim_ack, 0),
    Header#{
        round       => Round,
        responder   => Responder,
        incarnation => Inc,
        piggyback   => maps:get(piggyback, Spec, [])
    }.

-spec swim_suspect(swim_suspect_spec()) -> frame().
swim_suspect(Spec) ->
    build_suspect_like(swim_suspect, Spec).

-spec swim_confirm(swim_suspect_spec()) -> frame().
swim_confirm(Spec) ->
    build_suspect_like(swim_confirm, Spec).

build_suspect_like(Type,
                   #{target := Target,
                     target_incarnation := Inc,
                     suspected_by := By,
                     ttl := Ttl})
  when is_binary(Target), byte_size(Target) =:= 32,
       is_integer(Inc), Inc >= 0,
       is_binary(By), byte_size(By) =:= 32,
       is_integer(Ttl), Ttl >= 0 ->
    Header = base(Type, 0),
    Header#{
        target             => Target,
        target_incarnation => Inc,
        suspected_by       => By,
        ttl                => Ttl
    }.

%%------------------------------------------------------------------
%% SWIM piggyback updates
%%
%% An update names its observer in `by' and travels as piggyback inside
%% PING and ACK frames.
%%------------------------------------------------------------------

-spec swim_update(swim_update_spec()) -> swim_update().
swim_update(#{target := T, state := St, incarnation := Inc,
              observed_at := Ts, by := By})
  when is_binary(T),  byte_size(T)  =:= 32,
       is_binary(By), byte_size(By) =:= 32,
       is_integer(Inc), Inc >= 0,
       is_integer(Ts),  Ts  > 0,
       (St =:= alive orelse St =:= suspect orelse St =:= confirmed_failed) ->
    #{
        target      => T,
        state       => St,
        incarnation => Inc,
        observed_at => Ts,
        by          => By
    }.

%%------------------------------------------------------------------
%% DHT frame constructors (Part 6 §7)
%%
%% Every DHT frame carries `capabilities => 0' (no capability
%% negotiation in-operation) and the standard header from `base/2'.
%% Request/response pairs share their `key' / `nonce' so a responder
%% can match queries to replies without a transaction table.
%%------------------------------------------------------------------

-spec ping(ping_spec()) -> frame().
ping(#{nonce := N}) when is_binary(N), byte_size(N) =:= 16 ->
    (base(ping, 0))#{nonce => N}.

-spec pong(pong_spec()) -> frame().
pong(#{nonce := N}) when is_binary(N), byte_size(N) =:= 16 ->
    (base(pong, 0))#{nonce => N}.

-spec find_node(find_node_spec()) -> frame().
find_node(#{key := K, origin := O, depth := D})
  when is_binary(K), byte_size(K) =:= 32,
       is_binary(O), byte_size(O) =:= 32,
       is_integer(D), D >= 0 ->
    (base(find_node, 0))#{key => K, origin => O, depth => D}.

-spec nodes(nodes_spec()) -> frame().
nodes(#{key := K, nodes := Ns})
  when is_binary(K), byte_size(K) =:= 32,
       is_list(Ns) ->
    Validated = [station_ref(Ref) || Ref <- Ns],
    (base(nodes, 0))#{key => K, nodes => Validated}.

-spec find_value(find_value_spec()) -> frame().
find_value(#{key := K, origin := O})
  when is_binary(K), byte_size(K) =:= 32,
       is_binary(O), byte_size(O) =:= 32 ->
    (base(find_value, 0))#{key => K, origin => O}.

-spec value(value_spec()) -> frame().
value(#{key := K, records := Rs})
  when is_binary(K), byte_size(K) =:= 32,
       is_list(Rs) ->
    lists:foreach(fun validate_record_bytes/1, Rs),
    (base(value, 0))#{key => K, records => Rs}.

-spec store(store_spec()) -> frame().
store(#{record := R}) when is_binary(R) ->
    (base(store, 0))#{record => R}.

%% A STORE_ACK carries no reason: nothing reads one, so a spec that brings one is refused.
-spec store_ack(store_ack_spec()) -> frame().
store_ack(#{key := K, stored := Stored} = Spec)
  when is_binary(K), byte_size(K) =:= 32,
       is_boolean(Stored), not is_map_key(reason, Spec) ->
    (base(store_ack, 0))#{key => K, stored => Stored}.

%%------------------------------------------------------------------
%% station_ref — validated payload for NODES responses
%%------------------------------------------------------------------

-spec station_ref(station_ref_spec()) -> station_ref().
station_ref(#{node_id := NodeId, station_id := StationId,
              tier := Tier, country := Country,
              last_seen_at := LastSeen} = Spec)
  when is_binary(NodeId),    byte_size(NodeId)    =:= 32,
       is_binary(StationId), byte_size(StationId) =:= 32,
       is_integer(Tier),     Tier >= 0, Tier =< 4,
       is_binary(Country),   byte_size(Country)   =:= 2,
       is_integer(LastSeen), LastSeen > 0 ->
    Addresses = maps:get(addresses, Spec, []),
    Asn       = maps:get(asn, Spec, undefined),
    validate_asn(Asn),
    validate_addresses(Addresses),
    #{
        node_id      => NodeId,
        station_id   => StationId,
        addresses    => Addresses,
        tier         => Tier,
        asn          => Asn,
        country      => Country,
        last_seen_at => LastSeen
    }.

-spec validate_asn(non_neg_integer() | undefined) -> ok.
validate_asn(undefined) -> ok;
validate_asn(N) when is_integer(N), N >= 0 -> ok.

-spec validate_addresses([map()]) -> ok.
validate_addresses([])                        -> ok;
validate_addresses([A | Rest]) when is_map(A) -> validate_addresses(Rest).

-spec validate_record_bytes(binary()) -> ok.
validate_record_bytes(Bytes) when is_binary(Bytes) ->
    ok.

%%------------------------------------------------------------------
%% Requests, replies and relay errors (D25)
%%
%% CALL and STREAM_OPEN are {version, frame_type, request}, with the routing
%% fields source_route and retry_budget. request is {key, tbs, signature}
%% under MACULA-PQ-REQUEST-V1, signed with the caller's identity key, and
%% its caller is that key's key id. RESULT and ERROR from a provider carry
%% reply under MACULA-PQ-REPLY-V1, with source_route_reverse; ERROR and
%% STREAM_ERROR from a station carry relay_error under
%% MACULA-PQ-RELAY-ERROR-V1, with source_route_partial. A reply and a relay
%% error name their request by request_id and request_hash. The checks that
%% belong to a provider or a station, such as its own node_id as target, the
%% deadline window, replays and tokens, stay with the caller of these
%% functions.
%%------------------------------------------------------------------

%% @doc Sign a CALL with the caller's identity key.
-spec call(request_spec(), macula_node_keys:node_key()) -> frame().
call(Spec, Key) when not is_map_key(mode, Spec) ->
    request(call, Spec, Key).

%% @doc Sign a STREAM_OPEN, which carries its stream mode, with the caller's identity key.
-spec stream_open(request_spec(), macula_node_keys:node_key()) -> frame().
stream_open(#{mode := Mode} = Spec, Key) when Mode =:= server_stream; Mode =:= client_stream; Mode =:= bidi ->
    request(stream_open, Spec, Key).

request(Type, #{request_id := RequestId, realm := Realm, procedure := Procedure, target := Target,
                deadline := Deadline, payload := Payload} = Spec, #{purpose := identity} = Key)
  when byte_size(RequestId) =:= 16, byte_size(Realm) =:= 32, is_binary(Procedure), byte_size(Target) =:= 32,
       is_integer(Deadline), Deadline >= 0, Deadline < ?MAX_PROTOCOL_INT ->
    ok = check_payload(Payload),
    Fields = optional_token(Spec, maps:merge(maps:with([mode], Spec),
                                             #{frame_type => Type, caller => macula_node_keys:key_id(Key),
                                               request_id => RequestId, realm => Realm, procedure => {text, Procedure},
                                               target => Target, deadline => Deadline, payload => Payload})),
    routed(#{version => ?PROTOCOL_VERSION, frame_type => Type,
             request => macula_signed_object:sign(?REQUEST_LABEL, to_wire(Fields), Key)},
           maps:with([source_route, retry_budget], Spec)).

optional_token(#{token := Token}, Fields) when is_binary(Token) -> Fields#{token => Token};
optional_token(Spec, Fields) when not is_map_key(token, Spec) -> Fields.

%% @doc Verify a received CALL or STREAM_OPEN under the connection's profile: the request's signature and fields, and
%% caller as the key id of its key. A station checks this before it routes, a provider before its own checks.
-spec verify_request(frame(), macula_crypto_profile:profile()) ->
        {ok, verified_request()} | {error, malformed_frame | signature_invalid | key_id_mismatch}.
verify_request(#{frame_type := Type, request := Signed} = Frame, Profile) when Type =:= call; Type =:= stream_open ->
    request_signed(only_fields(Frame, [version, frame_type, request, source_route, retry_budget]),
                   macula_signed_object:verify(?REQUEST_LABEL, Signed, Profile), Type, Profile);
verify_request(_Frame, _Profile) ->
    {error, malformed_frame}.

request_signed(true, {ok, #{key := Key, tbs := Tbs, fields := Fields}}, Type, Profile) ->
    request_read(read_fields(maps:to_list(Fields), request_table(Type), #{}), Key, Tbs, Type, Profile);
request_signed(true, {error, signature_invalid}, _Type, _Profile) ->
    {error, signature_invalid};
request_signed(_OnlyFields, _Verified, _Type, _Profile) ->
    {error, malformed_frame}.

request_read({ok, #{frame_type := Type, caller := Caller, request_id := _, realm := _, procedure := _, target := _,
                    deadline := _, payload := _} = Read}, Key, Tbs, Type, Profile) ->
    request_checked(is_map_key(mode, Read) =:= (Type =:= stream_open),
                    Caller =:= macula_node_keys:node_id(Key, Profile), Read, Key, Tbs);
request_read(_NotARequest, _Key, _Tbs, _Type, _Profile) ->
    {error, malformed_frame}.

request_checked(false, _CallerIsKey, _Read, _Key, _Tbs) ->
    {error, malformed_frame};
request_checked(true, false, _Read, _Key, _Tbs) ->
    {error, key_id_mismatch};
request_checked(true, true, Read, Key, Tbs) ->
    {ok, (maps:remove(alg, Read))#{key => Key, request_hash => crypto:hash(sha384, Tbs)}}.

request_table(Type) ->
    #{<<"frame_type">> => {frame_type, {enum, [Type]}},
      <<"alg">> => {alg, value},
      <<"caller">> => {caller, {bytes, 32}},
      <<"request_id">> => {request_id, {bytes, 16}},
      <<"realm">> => {realm, {bytes, 32}},
      <<"procedure">> => {procedure, text},
      <<"target">> => {target, {bytes, 32}},
      <<"deadline">> => {deadline, uint},
      <<"payload">> => {payload, value},
      <<"mode">> => {mode, {enum, [server_stream, client_stream, bidi]}},
      <<"token">> => {token, bytes}}.

%% @doc Sign a RESULT for a verified request with the provider's identity key.
-spec result(#{request := verified_request(), payload := term(), source_route_reverse => binary()},
             macula_node_keys:node_key()) -> frame().
result(#{request := Request, payload := Payload} = Spec, Key) ->
    ok = check_payload(Payload),
    reply(result, #{payload => Payload}, Request, Spec, Key).

%% @doc Sign a provider's ERROR for a verified request: a code and an optional detail, both text.
-spec provider_error(#{request := verified_request(), code := binary(), detail => binary(),
                       source_route_reverse => binary()}, macula_node_keys:node_key()) -> frame().
provider_error(#{request := Request, code := Code} = Spec, Key) when is_binary(Code) ->
    reply(error, optional_text(detail, Spec, #{code => {text, Code}}), Request, Spec, Key).

reply(Type, Fields, #{request_id := RequestId, request_hash := RequestHash}, Spec, #{purpose := identity} = Key) ->
    Tbs = Fields#{frame_type => Type, request_id => RequestId, request_hash => RequestHash,
                  responded_by => macula_node_keys:key_id(Key)},
    routed(#{version => ?PROTOCOL_VERSION, frame_type => Type,
             reply => macula_signed_object:sign(?REPLY_LABEL, to_wire(Tbs), Key)},
           maps:with([source_route_reverse], Spec)).

%% @doc Verify a received RESULT or provider ERROR for the request it answers: its signature and fields, responded_by
%% as the key id of its key, the request's request_id and request_hash, and responded_by as the request's target.
-spec verify_reply(frame(), verified_request(), macula_crypto_profile:profile()) ->
        {ok, map()} | {error, malformed_frame | signature_invalid | key_id_mismatch | request_mismatch
                                                                    | not_the_target}.
verify_reply(#{frame_type := Type, reply := Signed} = Frame, Request, Profile) when Type =:= result; Type =:= error ->
    reply_signed(only_fields(Frame, [version, frame_type, reply, source_route_reverse]),
                 macula_signed_object:verify(?REPLY_LABEL, Signed, Profile), Type, Request, Profile);
verify_reply(_Frame, _Request, _Profile) ->
    {error, malformed_frame}.

reply_signed(true, {ok, #{key := Key, fields := Fields}}, Type, Request, Profile) ->
    reply_read(read_fields(maps:to_list(Fields), reply_table(Type), #{}), Key, Type, Request, Profile);
reply_signed(true, {error, signature_invalid}, _Type, _Request, _Profile) ->
    {error, signature_invalid};
reply_signed(_OnlyFields, _Verified, _Type, _Request, _Profile) ->
    {error, malformed_frame}.

reply_read({ok, #{frame_type := Type, request_id := RequestId, request_hash := RequestHash,
                  responded_by := RespondedBy} = Read}, Key, Type, Request, Profile) ->
    reply_checked([reply_shape(Type, Read),
                   RespondedBy =:= macula_node_keys:node_id(Key, Profile),
                   {RequestId, RequestHash} =:= request_names(Request),
                   RespondedBy =:= maps:get(target, Request)], Read);
reply_read(_NotAReply, _Key, _Type, _Request, _Profile) ->
    {error, malformed_frame}.

reply_shape(result, Read) ->
    is_map_key(payload, Read) andalso not is_map_key(code, Read) andalso not is_map_key(detail, Read);
reply_shape(error, Read) ->
    is_map_key(code, Read) andalso not is_map_key(payload, Read).

reply_checked([false | _], _Read) -> {error, malformed_frame};
reply_checked([true, false | _], _Read) -> {error, key_id_mismatch};
reply_checked([true, true, false | _], _Read) -> {error, request_mismatch};
reply_checked([true, true, true, false], _Read) -> {error, not_the_target};
reply_checked([true, true, true, true], Read) -> {ok, maps:without([alg, request_id, request_hash], Read)}.

reply_table(Type) ->
    #{<<"frame_type">> => {frame_type, {enum, [Type]}},
      <<"alg">> => {alg, value},
      <<"request_id">> => {request_id, {bytes, 16}},
      <<"request_hash">> => {request_hash, {bytes, 48}},
      <<"responded_by">> => {responded_by, {bytes, 32}},
      <<"payload">> => {payload, value},
      <<"code">> => {code, text},
      <<"detail">> => {detail, text}}.

%% @doc Sign a station's relay error, an ERROR or STREAM_ERROR for a pending request, with the station's identity key.
-spec relay_error(#{frame_type := error | stream_error, request := verified_request(), code := unknown_next_peer,
                    detail => binary(), offending_hop => binary(), source_route_partial => binary()},
                  macula_node_keys:node_key()) -> frame().
relay_error(#{frame_type := Type, request := #{request_id := RequestId, request_hash := RequestHash},
              code := Code} = Spec, #{purpose := identity} = Key) when Type =:= error; Type =:= stream_error ->
    ok = relay_code(lists:member(Code, ?RELAY_CODES)),
    Tbs = optional_hop(Spec, optional_text(detail, Spec,
                                           #{frame_type => Type, request_id => RequestId, request_hash => RequestHash,
                                             reported_by => macula_node_keys:key_id(Key), code => Code})),
    routed(#{version => ?PROTOCOL_VERSION, frame_type => Type,
             relay_error => macula_signed_object:sign(?RELAY_ERROR_LABEL, to_wire(Tbs), Key)},
           maps:with([source_route_partial], Spec)).

relay_code(true) -> ok.

optional_hop(#{offending_hop := Hop}, Fields) when byte_size(Hop) =:= 32 -> Fields#{offending_hop => Hop};
optional_hop(Spec, Fields) when not is_map_key(offending_hop, Spec) -> Fields.

%% @doc Verify a received relay error for the pending request it names: its signature and fields, reported_by as the
%% key id of its key, and the request's request_id and request_hash.
-spec verify_relay_error(frame(), verified_request(), macula_crypto_profile:profile()) ->
        {ok, map()} | {error, malformed_frame | signature_invalid | key_id_mismatch | request_mismatch}.
verify_relay_error(#{frame_type := Type, relay_error := Signed} = Frame, Request, Profile)
  when Type =:= error; Type =:= stream_error ->
    relay_signed(only_fields(Frame, [version, frame_type, relay_error, source_route_partial]),
                 macula_signed_object:verify(?RELAY_ERROR_LABEL, Signed, Profile), Type, Request, Profile);
verify_relay_error(_Frame, _Request, _Profile) ->
    {error, malformed_frame}.

relay_signed(true, {ok, #{key := Key, fields := Fields}}, Type, Request, Profile) ->
    relay_read(read_fields(maps:to_list(Fields), relay_error_table(Type), #{}), Key, Type, Request, Profile);
relay_signed(true, {error, signature_invalid}, _Type, _Request, _Profile) ->
    {error, signature_invalid};
relay_signed(_OnlyFields, _Verified, _Type, _Request, _Profile) ->
    {error, malformed_frame}.

relay_read({ok, #{frame_type := Type, request_id := RequestId, request_hash := RequestHash, reported_by := ReportedBy,
                  code := _} = Read}, Key, Type, Request, Profile) ->
    relay_checked([ReportedBy =:= macula_node_keys:node_id(Key, Profile),
                   {RequestId, RequestHash} =:= request_names(Request)], Read);
relay_read(_NotARelayError, _Key, _Type, _Request, _Profile) ->
    {error, malformed_frame}.

relay_checked([false | _], _Read) -> {error, key_id_mismatch};
relay_checked([true, false], _Read) -> {error, request_mismatch};
relay_checked([true, true], Read) -> {ok, maps:without([alg, request_id, request_hash], Read)}.

relay_error_table(Type) ->
    #{<<"frame_type">> => {frame_type, {enum, [Type]}},
      <<"alg">> => {alg, value},
      <<"request_id">> => {request_id, {bytes, 16}},
      <<"request_hash">> => {request_hash, {bytes, 48}},
      <<"reported_by">> => {reported_by, {bytes, 32}},
      <<"code">> => {code, {enum, ?RELAY_CODES}},
      <<"detail">> => {detail, text},
      <<"offending_hop">> => {offending_hop, {bytes, 32}}}.

request_names(#{request_id := RequestId, request_hash := RequestHash}) ->
    {RequestId, RequestHash}.

optional_text(Name, Spec, Fields) ->
    text_field(maps:find(Name, Spec), Name, Fields).

text_field({ok, Text}, Name, Fields) when is_binary(Text) -> Fields#{Name => {text, Text}};
text_field(error, _Name, Fields) -> Fields.

%% The routing fields a frame carries outside its signed object: a source route as bytes, a retry budget as a
%% protocol integer.
routed(Frame, Routing) ->
    maps:merge(Frame, maps:map(fun routing_value/2, Routing)).

routing_value(retry_budget, Budget) when is_integer(Budget), Budget >= 0, Budget < ?MAX_PROTOCOL_INT -> Budget;
routing_value(_Route, Route) when is_binary(Route) -> Route.

only_fields(Frame, Allowed) ->
    map_size(maps:without(Allowed, Frame)) =:= 0.

%%------------------------------------------------------------------
%% HyParView constructors (Part 3 §7.1)
%%------------------------------------------------------------------

-spec hyparview_join(hyparview_join_spec()) -> frame().
hyparview_join(#{realm := R, new_member := M} = Spec)
  when is_binary(R), byte_size(R) =:= 32,
       is_binary(M), byte_size(M) =:= 32 ->
    with_endorsement(Spec, (base(hyparview_join, 0))#{realm => R, new_member => M}).

-spec hyparview_forward_join(hyparview_forward_join_spec()) -> frame().
hyparview_forward_join(#{realm := R, new_member := M,
                         ttl := Ttl, arwl := A, prwl := P} = Spec)
  when is_binary(R), byte_size(R) =:= 32,
       is_binary(M), byte_size(M) =:= 32,
       is_integer(Ttl), Ttl >= 0,
       is_integer(A),   A >= 0,
       is_integer(P),   P >= 0 ->
    with_endorsement(Spec, (base(hyparview_forward_join, 0))#{
        realm => R, new_member => M,
        ttl => Ttl, arwl => A, prwl => P
    }).

%% `record' is optional at the spec level (see the type's own doc) --
%% only set it on the outgoing frame when the caller actually supplied
%% one, so a realm that doesn't require admission-gated JOIN isn't
%% forced to carry an empty/dummy record.
with_endorsement(#{record := R}, Frame) when is_binary(R) -> Frame#{record => R};
with_endorsement(_Spec, Frame) -> Frame.

-spec hyparview_neighbor(hyparview_neighbor_spec()) -> frame().
hyparview_neighbor(#{realm := R, priority := P} = Spec)
  when is_binary(R), byte_size(R) =:= 32,
       (P =:= high orelse P =:= low) ->
    with_endorsement(Spec, (base(hyparview_neighbor, 0))#{realm => R, priority => P}).

-spec hyparview_disconnect(hyparview_disconnect_spec()) -> frame().
hyparview_disconnect(#{realm := R})
  when is_binary(R), byte_size(R) =:= 32 ->
    (base(hyparview_disconnect, 0))#{realm => R}.

-spec hyparview_shuffle(hyparview_shuffle_spec()) -> frame().
hyparview_shuffle(#{realm := R, origin := O,
                    ttl := Ttl, peer_sample := S})
  when is_binary(R), byte_size(R) =:= 32,
       is_binary(O), byte_size(O) =:= 32,
       is_integer(Ttl), Ttl >= 0,
       is_list(S) ->
    lists:foreach(fun validate_pubkey/1, S),
    (base(hyparview_shuffle, 0))#{
        realm => R, origin => O, ttl => Ttl, peer_sample => S
    }.

-spec hyparview_shuffle_reply(hyparview_shuffle_reply_spec()) -> frame().
hyparview_shuffle_reply(#{realm := R, peer_sample := S})
  when is_binary(R), byte_size(R) =:= 32,
       is_list(S) ->
    lists:foreach(fun validate_pubkey/1, S),
    (base(hyparview_shuffle_reply, 0))#{
        realm => R, peer_sample => S
    }.

-spec validate_pubkey(binary()) -> ok.
validate_pubkey(B) when is_binary(B), byte_size(B) =:= 32 -> ok.

%%------------------------------------------------------------------
%% Plumtree constructors (Part 3 §7.2)
%%------------------------------------------------------------------

%% GOSSIP carries a publication as received, so every node checks it end to end, and a round that stays unsigned.
-spec plumtree_gossip(plumtree_gossip_spec()) -> frame().
plumtree_gossip(#{publication := Publication, round := Round})
  when is_integer(Round), Round >= 0, Round < ?MAX_PROTOCOL_INT ->
    #{version => ?PROTOCOL_VERSION, frame_type => plumtree_gossip, publication => publication_object(Publication),
      round => Round}.

-spec plumtree_ihave(plumtree_ihave_spec()) -> frame().
plumtree_ihave(#{realm := R, msg_id := M, round := Rd})
  when is_binary(R), byte_size(R) =:= 32,
       is_binary(M), byte_size(M) =:= 48,
       is_integer(Rd), Rd >= 0 ->
    (base(plumtree_ihave, 0))#{realm => R, msg_id => M, round => Rd}.

-spec plumtree_graft(plumtree_graft_spec()) -> frame().
plumtree_graft(#{realm := R, msg_id := M, round := Rd})
  when is_binary(R), byte_size(R) =:= 32,
       is_binary(M), byte_size(M) =:= 48,
       is_integer(Rd), Rd >= 0 ->
    (base(plumtree_graft, 0))#{realm => R, msg_id => M, round => Rd}.

-spec plumtree_prune(plumtree_prune_spec()) -> frame().
plumtree_prune(#{realm := R})
  when is_binary(R), byte_size(R) =:= 32 ->
    (base(plumtree_prune, 0))#{realm => R}.

%%------------------------------------------------------------------
%% Overlay relay envelope constructor (Phase 3.5)
%%------------------------------------------------------------------

-spec overlay_relay(overlay_relay_spec()) -> frame().
overlay_relay(#{peer := P, payload := Bin})
  when is_binary(P), byte_size(P) =:= 32, is_binary(Bin) ->
    (base(overlay_relay, 0))#{peer => P, payload => Bin}.

%%------------------------------------------------------------------
%% PubSub constructors (Part 6 §6)
%%------------------------------------------------------------------

%% @doc Sign a publication with the publisher's identity key, as a PUBLISH. Its tbs holds no frame_type, because the
%% same bytes ride in every EVENT and GOSSIP made from it.
-spec publish(publish_spec(), macula_node_keys:node_key()) -> frame().
publish(#{realm := Realm, topic := Topic, seq := Seq, published_at := PublishedAt, payload := Payload} = Spec,
        #{purpose := identity} = Key)
  when byte_size(Realm) =:= 32, is_binary(Topic), is_integer(Seq), Seq >= 0, Seq < ?MAX_PROTOCOL_INT,
       is_integer(PublishedAt), PublishedAt >= 0, PublishedAt < ?MAX_PROTOCOL_INT ->
    ok = check_payload(Payload),
    Fields = optional_ttl(Spec, #{publisher => macula_node_keys:key_id(Key), realm => Realm, topic => {text, Topic},
                                  seq => Seq, published_at => PublishedAt, payload => Payload}),
    #{version => ?PROTOCOL_VERSION, frame_type => publish,
      publication => macula_signed_object:sign(?PUBLICATION_LABEL, to_wire(Fields), Key)}.

optional_ttl(#{ttl_ms := Ttl}, Fields) when is_integer(Ttl), Ttl >= 0, Ttl =< ?PUBLICATION_MAX_TTL_MS ->
    Fields#{ttl_ms => Ttl};
optional_ttl(Spec, Fields) when not is_map_key(ttl_ms, Spec) -> Fields.

-spec subscribe(subscribe_spec()) -> frame().
subscribe(#{topic := T, realm := R, subscriber := Sub} = Spec)
  when is_binary(T),
       is_binary(R),   byte_size(R)   =:= 32,
       is_binary(Sub), byte_size(Sub) =:= 32 ->
    Filter  = maps:get(filter,  Spec, undefined),
    Options = maps:get(options, Spec, #{}),
    validate_options(Options),
    (base(subscribe, 0))#{
        topic      => T,
        realm      => R,
        subscriber => Sub,
        filter     => Filter,
        options    => Options
    }.

-spec unsubscribe(unsubscribe_spec()) -> frame().
unsubscribe(#{topic := T, realm := R, subscriber := Sub})
  when is_binary(T),
       is_binary(R),   byte_size(R)   =:= 32,
       is_binary(Sub), byte_size(Sub) =:= 32 ->
    (base(unsubscribe, 0))#{
        topic      => T,
        realm      => R,
        subscriber => Sub
    }.

%% @doc An EVENT for a verified publication, carrying its bytes unchanged and how it was delivered.
-spec event(event_spec()) -> frame().
event(#{publication := Publication, delivered_via := Via}) when Via =:= plumtree; Via =:= direct ->
    #{version => ?PROTOCOL_VERSION, frame_type => event, publication => publication_object(Publication),
      delivered_via => Via}.

publication_object(#{key := Key, tbs := Tbs, signature := Signature} = Publication)
  when map_size(Publication) =:= 3, is_binary(Key), is_binary(Tbs), is_binary(Signature) ->
    Publication.

%% @doc Verify the publication a PUBLISH, EVENT or GOSSIP carries, under the connection's profile and a clock in
%% milliseconds: its signature and fields, a ttl_ms of at most one hour, publisher as the key id of its key, a
%% published_at no more than 5 minutes ahead, and not past published_at plus its ttl_ms, or 10 minutes without one,
%% plus 5 minutes. The origin station checks this before fan-out, and every subscriber before delivery.
-spec verify_publication(frame(), macula_crypto_profile:profile(), integer()) ->
        {ok, verified_publication()} | {error, malformed_frame | signature_invalid | key_id_mismatch | not_yet_valid
                                                                          | expired}.
verify_publication(#{frame_type := Type, publication := Signed} = Frame, Profile, Now)
  when (Type =:= publish orelse Type =:= event orelse Type =:= plumtree_gossip), is_integer(Now) ->
    publication_signed(only_fields(Frame, publication_frame_fields(Type)),
                       macula_signed_object:verify(?PUBLICATION_LABEL, Signed, Profile), Profile, Now);
verify_publication(_Frame, _Profile, _Now) ->
    {error, malformed_frame}.

publication_frame_fields(publish) -> [version, frame_type, publication];
publication_frame_fields(event) -> [version, frame_type, publication, delivered_via];
publication_frame_fields(plumtree_gossip) -> [version, frame_type, publication, round].

publication_signed(true, {ok, #{key := Key, tbs := Tbs, fields := Fields}}, Profile, Now) ->
    publication_read(read_fields(maps:to_list(Fields), publication_table(), #{}), Key, Tbs, Profile, Now);
publication_signed(true, {error, signature_invalid}, _Profile, _Now) ->
    {error, signature_invalid};
publication_signed(_OnlyFields, _Verified, _Profile, _Now) ->
    {error, malformed_frame}.

publication_read({ok, #{publisher := Publisher, realm := _, topic := _, seq := _, published_at := PublishedAt,
                        payload := _} = Read}, Key, Tbs, Profile, Now) ->
    Expiry = PublishedAt + maps:get(ttl_ms, Read, ?PUBLICATION_DEFAULT_TTL_MS) + ?PUBLICATION_TOLERANCE_MS,
    publication_checked([{maps:get(ttl_ms, Read, 0) =< ?PUBLICATION_MAX_TTL_MS, malformed_frame},
                         {Publisher =:= macula_node_keys:node_id(Key, Profile), key_id_mismatch},
                         {PublishedAt =< Now + ?PUBLICATION_TOLERANCE_MS, not_yet_valid},
                         {Now =< Expiry, expired}],
                        (maps:remove(alg, Read))#{key => Key, publication_hash => crypto:hash(sha384, Tbs),
                                                  expires_at => Expiry});
publication_read(_NotAPublication, _Key, _Tbs, _Profile, _Now) ->
    {error, malformed_frame}.

publication_checked([{true, _Refusal} | Checks], Verified) -> publication_checked(Checks, Verified);
publication_checked([{false, Refusal} | _Checks], _Verified) -> {error, Refusal};
publication_checked([], Verified) -> {ok, Verified}.

publication_table() ->
    #{<<"alg">> => {alg, value},
      <<"publisher">> => {publisher, {bytes, 32}},
      <<"realm">> => {realm, {bytes, 32}},
      <<"topic">> => {topic, text},
      <<"seq">> => {seq, uint},
      <<"published_at">> => {published_at, uint},
      <<"ttl_ms">> => {ttl_ms, uint},
      <<"payload">> => {payload, value}}.

-spec validate_options(map()) -> ok.
validate_options(M) when is_map(M) -> ok.

%%------------------------------------------------------------------
%% RPC advertise constructors (Part 6 §5.5)
%%------------------------------------------------------------------

-spec advertise(advertise_spec()) -> frame().
advertise(#{advertisement := Advertisement}) when is_binary(Advertisement) ->
    (base(advertise, 0))#{advertisement => Advertisement}.

-spec unadvertise(unadvertise_spec()) -> frame().
unadvertise(#{withdrawal := Withdrawal}) when is_binary(Withdrawal) ->
    (base(unadvertise, 0))#{withdrawal => Withdrawal}.

%%------------------------------------------------------------------
%% Streaming RPC constructors (Part 6 §5.6)
%%
%% STREAM_OPEN is a request, built by stream_open/2 with the rules of
%% requests above. A provider's STREAM_DATA, STREAM_END, STREAM_ERROR and
%% STREAM_REPLY carry stream under MACULA-PQ-STREAM-V1, with the provider's
%% key on its first frame only; a caller's STREAM_DATA, STREAM_END and
%% STREAM_ERROR carry caller_stream under MACULA-PQ-CALLER-STREAM-V1, which
%% verifies with the STREAM_OPEN's key. Each tbs names the stream by
%% request_id and request_hash, and each side numbers its own frames.
%%------------------------------------------------------------------

%% @doc The state a verifier starts a stream with: nothing seen from either side yet.
-spec open_stream(verified_request()) -> stream_state().
open_stream(#{frame_type := stream_open} = Open) ->
    #{request => Open, provider => #{next => 0, ended => false}, caller => #{next => 0, ended => false}}.

%% @doc Sign a provider's stream frame for a verified STREAM_OPEN with the provider's identity key. The first frame, seq
%% 0, carries the key; later frames do not.
-spec provider_stream(stream_spec(), macula_node_keys:node_key(), verified_request()) -> frame().
provider_stream(#{frame_type := Type, seq := Seq} = Spec, #{purpose := identity} = Key,
                #{frame_type := stream_open} = Open)
  when is_integer(Seq), Seq >= 0, Seq < ?MAX_PROTOCOL_INT ->
    Tbs = to_wire(stream_tbs(Type, Spec, Key, Open)),
    #{version => ?PROTOCOL_VERSION, frame_type => Type, stream => provider_signed(Seq, Tbs, Key)}.

provider_signed(0, Tbs, Key) -> macula_signed_object:sign(?STREAM_LABEL, Tbs, Key);
provider_signed(_Later, Tbs, Key) -> macula_signed_object:sign_held(?STREAM_LABEL, Tbs, Key).

%% @doc Sign a caller's stream frame for a verified STREAM_OPEN with the caller's identity key. A caller sends no
%% STREAM_REPLY, and no STREAM_DATA in a server_stream.
-spec caller_stream(stream_spec(), macula_node_keys:node_key(), verified_request()) -> frame().
caller_stream(#{frame_type := Type, seq := Seq} = Spec, #{purpose := identity} = Key,
              #{frame_type := stream_open, mode := Mode} = Open)
  when (Type =:= stream_end orelse Type =:= stream_error orelse (Type =:= stream_data andalso Mode =/= server_stream)),
       is_integer(Seq), Seq >= 0, Seq < ?MAX_PROTOCOL_INT ->
    Tbs = to_wire(stream_tbs(Type, Spec, Key, Open)),
    #{version => ?PROTOCOL_VERSION, frame_type => Type,
      caller_stream => macula_signed_object:sign_held(?CALLER_STREAM_LABEL, Tbs, Key)}.

stream_tbs(Type, #{seq := Seq} = Spec, Key, #{request_id := RequestId, request_hash := RequestHash}) ->
    (stream_fields(Type, Spec))#{frame_type => Type, request_id => RequestId, request_hash => RequestHash,
                                signer => macula_node_keys:key_id(Key), seq => Seq}.

stream_fields(stream_data, #{encoding := raw, body := Body}) when is_binary(Body) ->
    #{encoding => raw, body => Body};
stream_fields(stream_data, #{encoding := msgpack, body := Body}) ->
    ok = check_payload(Body),
    #{encoding => msgpack, body => Body};
stream_fields(stream_end, #{role := Role}) when Role =:= send; Role =:= both ->
    #{role => Role};
stream_fields(stream_error, #{code := Code, message := Message}) when is_binary(Code), is_binary(Message) ->
    #{code => {text, Code}, message => {text, Message}};
stream_fields(stream_reply, #{payload := Payload}) ->
    ok = check_payload(Payload),
    #{payload => Payload}.

%% @doc Verify a provider's stream frame against the stream's state, and return its fields and the next state.
%% Before the provider's first frame the verifier holds no provider key, so a frame without one is out of order. The
%% first frame's key must be the key id of its signer and the STREAM_OPEN's target; later frames verify with that key.
%% Each side's seq is the previous one plus one, and nothing follows its STREAM_END.
-spec verify_provider_stream(frame(), stream_state(), macula_crypto_profile:profile()) ->
        {ok, map(), stream_state()}
      | {error, malformed_frame | signature_invalid | key_id_mismatch | not_the_target | request_mismatch
                | seq_mismatch | stream_ended}.
verify_provider_stream(#{frame_type := Type, stream := Object} = Frame, #{provider := Side} = State, Profile)
  when Type =:= stream_data; Type =:= stream_end; Type =:= stream_error; Type =:= stream_reply ->
    provider_frame(only_fields(Frame, [version, frame_type, stream]), Side, Object, Type, State, Profile);
verify_provider_stream(_Frame, _State, _Profile) ->
    {error, malformed_frame}.

provider_frame(false, _Side, _Object, _Type, _State, _Profile) ->
    {error, malformed_frame};
provider_frame(true, #{ended := true}, _Object, _Type, _State, _Profile) ->
    {error, stream_ended};
provider_frame(true, #{key := HeldKey}, Object, Type, State, Profile) ->
    provider_later(provider_object(Object, HeldKey, Profile), Type, State);
provider_frame(true, _NoKeyYet, #{key := _} = Object, Type, State, Profile) ->
    provider_first(macula_signed_object:verify(?STREAM_LABEL, Object, Profile), Type, State, Profile);
provider_frame(true, _NoKeyYet, _WithoutKey, _Type, _State, _Profile) ->
    {error, seq_mismatch}.

%% A later frame verifies with the held key. One that still carries a key verifies with that key, which must be the
%% held one, and its shape is refused once its sequence number has been checked.
provider_object(#{key := _} = Object, HeldKey, Profile) ->
    carried_later(macula_signed_object:verify(?STREAM_LABEL, Object, Profile), HeldKey);
provider_object(Object, HeldKey, Profile) ->
    held_later(macula_signed_object:verify_held(?STREAM_LABEL, Object, HeldKey, Profile)).

carried_later({ok, #{key := HeldKey, fields := Fields}}, HeldKey) -> {ok, carried, Fields};
carried_later({ok, _OtherKey}, _HeldKey) -> {error, key_id_mismatch};
carried_later({error, _} = Refused, _HeldKey) -> Refused.

held_later({ok, #{fields := Fields}}) -> {ok, held, Fields};
held_later({error, _} = Refused) -> Refused.

provider_later({ok, Shape, Fields}, Type,
               #{request := Open, provider := #{next := Next, signer := Signer} = Side} = State) ->
    later_checked(stream_read(read_fields(maps:to_list(Fields), stream_table(Type), #{}), Type), Shape, Signer, Next,
                  Open, Side, State);
provider_later({error, Refusal}, _Type, _State) when Refusal =:= signature_invalid; Refusal =:= key_id_mismatch ->
    {error, Refusal};
provider_later({error, _MalformedOrAlgMismatch}, _Type, _State) ->
    {error, malformed_frame}.

later_checked({ok, #{signer := FrameSigner, seq := Seq} = Read}, Shape, Signer, Next, Open, Side, State) ->
    stream_result([{FrameSigner =:= Signer, key_id_mismatch},
                   {request_names(Read) =:= request_names(Open), request_mismatch},
                   {Seq =:= Next, seq_mismatch},
                   {Shape =:= held, malformed_frame}], Read, provider, Side, State);
later_checked(error, _Shape, _Signer, _Next, _Open, _Side, _State) ->
    {error, malformed_frame}.

provider_first({ok, #{key := Key, fields := Fields}}, Type, #{request := Open, provider := Side} = State, Profile) ->
    first_checked(stream_read(read_fields(maps:to_list(Fields), stream_table(Type), #{}), Type), Key, Open, Side,
                  State, Profile);
provider_first({error, signature_invalid}, _Type, _State, _Profile) ->
    {error, signature_invalid};
provider_first({error, _MalformedOrAlgMismatch}, _Type, _State, _Profile) ->
    {error, malformed_frame}.

first_checked({ok, #{signer := Signer, seq := Seq} = Read}, Key, Open, Side, State, Profile) ->
    stream_result([{Signer =:= macula_node_keys:node_id(Key, Profile), key_id_mismatch},
                   {request_names(Read) =:= request_names(Open), request_mismatch},
                   {Signer =:= maps:get(target, Open), not_the_target},
                   {Seq =:= 0, seq_mismatch}], Read, provider, Side#{key => Key, signer => Signer}, State);
first_checked(error, _Key, _Open, _Side, _State, _Profile) ->
    {error, malformed_frame}.

%% @doc Verify a caller's stream frame against the stream's state with the STREAM_OPEN's key, and return its fields
%% and the next state. The signer must be the STREAM_OPEN's caller, and in a server_stream a caller sends no
%% STREAM_DATA.
-spec verify_caller_stream(frame(), stream_state(), macula_crypto_profile:profile()) ->
        {ok, map(), stream_state()}
      | {error, malformed_frame | signature_invalid | key_id_mismatch | request_mismatch | seq_mismatch
                | stream_ended}.
verify_caller_stream(#{frame_type := Type, caller_stream := Object} = Frame, #{caller := Side} = State, Profile)
  when Type =:= stream_data; Type =:= stream_end; Type =:= stream_error ->
    caller_frame(only_fields(Frame, [version, frame_type, caller_stream]), Side, Object, Type, State, Profile);
verify_caller_stream(_Frame, _State, _Profile) ->
    {error, malformed_frame}.

%% @doc Whether a refusal of a signed object is charged to the connection that carried it
%% (DESIGN_PQ_DHT_SLOTS_AND_BUDGET.md, 3.1). A refusal every verifier reaches from the same bytes is charged: a
%% malformed shape, key, field or alg, a signature that does not verify, or a signer that is not its key. One that
%% depends on what the receiver holds is not: a sequence number, a stream that has ended, or a match with the
%% request or its target.
-spec charged_refusal(malformed_frame | signature_invalid | key_id_mismatch | seq_mismatch | stream_ended
                      | request_mismatch | not_the_target) -> boolean().
charged_refusal(malformed_frame) -> true;
charged_refusal(signature_invalid) -> true;
charged_refusal(key_id_mismatch) -> true;
charged_refusal(seq_mismatch) -> false;
charged_refusal(stream_ended) -> false;
charged_refusal(request_mismatch) -> false;
charged_refusal(not_the_target) -> false.

caller_frame(false, _Side, _Object, _Type, _State, _Profile) ->
    {error, malformed_frame};
caller_frame(true, #{ended := true}, _Object, _Type, _State, _Profile) ->
    {error, stream_ended};
caller_frame(true, Side, Object, Type, #{request := #{key := CallerKey}} = State, Profile) ->
    caller_verified(macula_signed_object:verify_held(?CALLER_STREAM_LABEL, Object, CallerKey, Profile), Type, Side,
                    State).

caller_verified({ok, #{fields := Fields}}, Type, Side, #{request := Open} = State) ->
    caller_checked(stream_read(read_fields(maps:to_list(Fields), stream_table(Type), #{}), Type), Type, Side, Open,
                   State);
caller_verified({error, signature_invalid}, _Type, _Side, _State) ->
    {error, signature_invalid};
caller_verified({error, _MalformedOrAlgMismatch}, _Type, _Side, _State) ->
    {error, malformed_frame}.

caller_checked({ok, #{signer := Signer, seq := Seq} = Read}, Type, #{next := Next} = Side, Open, State) ->
    stream_result([{not (Type =:= stream_data andalso maps:get(mode, Open) =:= server_stream), malformed_frame},
                   {Signer =:= maps:get(caller, Open), key_id_mismatch},
                   {request_names(Read) =:= request_names(Open), request_mismatch},
                   {Seq =:= Next, seq_mismatch}], Read, caller, Side, State);
caller_checked(error, _Type, _Side, _Open, _State) ->
    {error, malformed_frame}.

%% The fields of a stream frame's tbs, with exactly the fields its type carries.
stream_read({ok, #{frame_type := Type, request_id := _, request_hash := _, signer := _, seq := _} = Read}, Type) ->
    shaped(stream_shape(Type, Read), Read);
stream_read(_NotAStreamFrame, _Type) ->
    error.

stream_shape(stream_data, #{encoding := raw, body := Body} = Read) ->
    is_binary(Body) andalso only_type_fields(Read, [encoding, body]);
stream_shape(stream_data, #{encoding := msgpack, body := _} = Read) -> only_type_fields(Read, [encoding, body]);
stream_shape(stream_end, #{role := _} = Read) -> only_type_fields(Read, [role]);
stream_shape(stream_error, #{code := _, message := _} = Read) -> only_type_fields(Read, [code, message]);
stream_shape(stream_reply, #{payload := _} = Read) -> only_type_fields(Read, [payload]);
stream_shape(_Type, _Read) -> false.

only_type_fields(Read, TypeFields) ->
    only_fields(Read, [frame_type, alg, request_id, request_hash, signer, seq | TypeFields]).

shaped(true, Read) -> {ok, Read};
shaped(false, _Read) -> error.

%% Run the checks in order; the first that fails names the refusal. When all hold, the side moves to its next
%% sequence number and records whether it has ended.
stream_result([{true, _Refusal} | Checks], Read, SideName, Side, State) ->
    stream_result(Checks, Read, SideName, Side, State);
stream_result([{false, Refusal} | _Checks], _Read, _SideName, _Side, _State) ->
    {error, Refusal};
stream_result([], #{frame_type := Type, seq := Seq} = Read, SideName, Side, State) ->
    {ok, maps:without([alg, request_id, request_hash], Read),
     State#{SideName := Side#{next := Seq + 1, ended := Type =:= stream_end}}}.

stream_table(Type) ->
    #{<<"frame_type">> => {frame_type, {enum, [Type]}},
      <<"alg">> => {alg, value},
      <<"request_id">> => {request_id, {bytes, 16}},
      <<"request_hash">> => {request_hash, {bytes, 48}},
      <<"signer">> => {signer, {bytes, 32}},
      <<"seq">> => {seq, uint},
      <<"encoding">> => {encoding, {enum, [raw, msgpack]}},
      <<"body">> => {body, value},
      <<"role">> => {role, {enum, [send, both]}},
      <<"code">> => {code, text},
      <<"message">> => {message, text},
      <<"payload">> => {payload, value}}.

%%------------------------------------------------------------------
%% Content transfer constructors (Part 6 §9)
%%
%% Want / Have / Block / Manifest_req / Manifest_res / Cancel are the
%% bitswap-style exchange primitives. All carry the standard frame
%% header and are signed by the sender; payloads validated for size
%% invariants but the contents are application-opaque.
%%------------------------------------------------------------------

-spec want(want_spec()) -> frame().
want(#{blocks := Bs}) when is_list(Bs) ->
    Validated = [validate_want_entry(E) || E <- Bs],
    (base(want, 0))#{blocks => Validated}.

-spec have(have_spec()) -> frame().
have(#{blocks := Bs}) when is_list(Bs) ->
    Validated = [validate_have_entry(E) || E <- Bs],
    (base(have, 0))#{blocks => Validated}.

-spec block(block_spec()) -> frame().
block(#{mcid := M, payload := P}) when is_binary(P) ->
    validate_mcid(M),
    (base(block, 0))#{mcid => M, payload => P}.

-spec manifest_req(manifest_req_spec()) -> frame().
manifest_req(#{mcid := M}) ->
    validate_mcid(M),
    (base(manifest_req, 0))#{mcid => M}.

-spec manifest_res(manifest_res_spec()) -> frame().
manifest_res(#{mcid := M, manifest := Manifest}) ->
    validate_mcid(M),
    validate_manifest_payload(Manifest),
    (base(manifest_res, 0))#{mcid => M, manifest => Manifest}.

-spec cancel(cancel_spec()) -> frame().
cancel(#{blocks := Bs}) when is_list(Bs) ->
    lists:foreach(fun validate_mcid/1, Bs),
    (base(cancel, 0))#{blocks => Bs}.

-spec validate_mcid(mcid()) -> ok.
validate_mcid(<<2, _Codec:8, _Hash:48/binary>>) -> ok.

-spec validate_want_entry(want_entry()) -> want_entry().
validate_want_entry(#{mcid := M} = E) ->
    validate_mcid(M),
    Prio = maps:get(priority, E, 128),
    validate_priority(Prio),
    #{mcid => M, priority => Prio}.

-spec validate_priority(want_priority()) -> ok.
validate_priority(P) when is_integer(P), P >= 0, P =< 255 -> ok.

-spec validate_have_entry(have_entry()) -> have_entry().
validate_have_entry(#{mcid := M, size := S})
  when is_integer(S), S >= 0 ->
    validate_mcid(M),
    #{mcid => M, size => S}.

-spec validate_manifest_payload(map()) -> ok.
validate_manifest_payload(M) when is_map(M) -> ok.

%%------------------------------------------------------------------
%% Neighbour signatures (D17)
%%
%% In pq_hybrid a control frame travels as {version, frame_type, neighbour}. neighbour is {tbs, signature} under
%% MACULA-PQ-NEIGHBOUR-V1, signed with the sender's identity key, which the receiver holds from the connection's
%% handshake. Its tbs holds the frame's fields with frame_type and alg, the connection hash (the SHA-384 of the
%% challenge frame's bytes) and seq: 0 on the first neighbour-signed frame in each direction, one more on each after.
%% In pq_pure no frame carries one. The caller counts seq per direction and closes the connection on a refusal.
%%------------------------------------------------------------------

%% @doc Whether a profile neighbour-signs a frame type.
-spec neighbour_signed(macula_crypto_profile:profile(), frame_type()) -> boolean().
neighbour_signed(pq_hybrid, FrameType) -> lists:member(FrameType, ?NEIGHBOUR_SIGNED);
neighbour_signed(pq_pure, _FrameType) -> false.

%% @doc Neighbour-sign a control frame with the sender's identity key, for one connection and one seq.
-spec sign_neighbour(frame(), macula_node_keys:node_key(), #{connection := binary(), seq := non_neg_integer()}) ->
        frame().
sign_neighbour(#{frame_type := Type} = Frame, #{profile := Profile} = Key, #{connection := Connection, seq := Seq})
  when not is_map_key(neighbour, Frame), byte_size(Connection) =:= 48, is_integer(Seq), Seq >= 0 ->
    neighbour_signature(neighbour_signed(Profile, Type), Frame, Key, Connection, Seq).

neighbour_signature(true, #{version := Version, frame_type := Type} = Frame, Key, Connection, Seq) ->
    Fields = wire_form(maps:without([version, signature], Frame)),
    Tbs = Fields#{{text, <<"connection">>} => Connection, {text, <<"seq">>} => Seq},
    #{version => Version, frame_type => Type, neighbour => macula_signed_object:sign_held(?NEIGHBOUR_LABEL, Tbs, Key)}.

%% @doc Read a received frame under the connection's profile. A frame type the profile signs must be exactly
%% {version, frame_type, neighbour}, signed by the peer's identity key for this connection and the next seq, and comes
%% back as the frame its tbs holds. Any other frame must not carry neighbour and comes back as it is.
-spec verify_neighbour(frame(), #{profile := macula_crypto_profile:profile(), peer_key := binary(),
                                  connection := binary(), seq := non_neg_integer()}) ->
        {ok, frame()} | {error, malformed_frame | signature_invalid}.
verify_neighbour(#{frame_type := Type} = Frame, #{profile := Profile} = Opts) ->
    neighbour_read(neighbour_signed(Profile, Type), Frame, Opts).

neighbour_read(true, #{version := _, neighbour := Held} = Frame, Opts) when map_size(Frame) =:= 3 ->
    neighbour_opened(neighbour_held(Held, Opts), Frame, Opts);
neighbour_read(true, _Frame, _Opts) ->
    {error, malformed_frame};
neighbour_read(false, #{neighbour := _}, _Opts) ->
    {error, malformed_frame};
neighbour_read(false, Frame, _Opts) ->
    {ok, Frame}.

neighbour_held(Held, #{profile := Profile, peer_key := PeerKey}) ->
    macula_signed_object:verify_held(?NEIGHBOUR_LABEL, Held, PeerKey, Profile).

neighbour_opened({ok, #{fields := Fields}}, #{version := Version, frame_type := Type}, Opts) ->
    neighbour_frame(read_fields(maps:to_list(Fields), neighbour_tbs_table(Type), #{}), Version, Type, Opts);
neighbour_opened({error, signature_invalid}, _Frame, _Opts) ->
    {error, signature_invalid};
neighbour_opened({error, _MalformedOrAlgMismatch}, _Frame, _Opts) ->
    {error, malformed_frame}.

%% A neighbour tbs holds the frame type's own fields, without version, the per-hop signature and neighbour itself, and
%% adds alg, connection and seq.
neighbour_tbs_table(Type) ->
    (maps:without([<<"version">>, <<"signature">>, <<"neighbour">>], field_table(Type)))#{
        <<"alg">> => {alg, value}, <<"connection">> => {connection, value}, <<"seq">> => {seq, value}}.

neighbour_frame({ok, #{frame_type := Type, connection := Connection, seq := Seq} = Read}, Version, Type,
                #{connection := Connection, seq := Seq}) ->
    {ok, (maps:without([alg, connection, seq], Read))#{version => Version}};
neighbour_frame(_NotThisConnectionOrSeq, _Version, _Type, _Opts) ->
    {error, malformed_frame}.

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
%% Wire codec — CBOR (RFC 8949 §4.2.1 deterministic, Part 6 §3)
%%------------------------------------------------------------------
%%
%% A frame map is encoded with wire_form/1: a boolean field of the frame
%% type's table becomes 1 or 0, other atoms become text and undefined
%% becomes null. A frame is decoded under the decoding rule of
%% DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md, and a frame type's own fields
%% come back through a fixed table (D26): each field the table defines
%% takes its atom key, an enum value takes one of the atoms the table
%% lists, a boolean is 1 or 0, and everything else keeps the one key form
%% of peer-supplied maps. No atom is made or looked up from what a peer sent.

-spec encode(frame()) -> binary().
encode(Frame) when is_map(Frame) ->
    encode_bytes(macula_cbor_nif:pack_deterministic(wire_form(Frame))).

%% @doc Prefix frame CBOR bytes with their length, leaving the bytes as they
%% are. The handshake keeps the bytes it passes here, because the connection
%% proof hashes a frame's bytes without the prefix.
-spec encode_bytes(binary()) -> binary().
encode_bytes(Bytes) when is_binary(Bytes) ->
    encode_with_check(byte_size(Bytes), Bytes).

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
decode(Buf) when is_binary(Buf) ->
    decoded_frame(split_frame(Buf)).

decoded_frame({ok, Bytes, Rest}) -> decode_cbor(Bytes, Rest);
decoded_frame(NotAFrame) -> NotAFrame.

%% @doc Read a frame from its decoded CBOR value, the way decode/1 reads one after decoding its bytes. A connection
%% decodes each frame once to route it by its frame_type, and passes the value here.
-spec read_wire(term()) -> {ok, frame()} | {error, bad_frame}.
read_wire(Wire) ->
    whole_frame(frame_read({ok, Wire}, <<>>)).

whole_frame({ok, Frame, <<>>}) -> {ok, Frame};
whole_frame({error, bad_frame} = Refused) -> Refused.

%% The length prefix, shared by decode/1 and parse_stream_bytes/1.
split_frame(<<Len:32/big, _Rest/binary>>) when Len > ?MAX_FRAME_BYTES ->
    {error, frame_too_large};
split_frame(<<Len:32/big, Bytes:Len/binary, Rest/binary>>) ->
    {ok, Bytes, Rest};
split_frame(<<Len:32/big, Tail/binary>>) ->
    {more, Len - byte_size(Tail)};
split_frame(Buf) when byte_size(Buf) < 4 ->
    {more, 4 - byte_size(Buf)}.

decode_cbor(Bytes, Rest) ->
    frame_read(macula_record_cbor:decode_strict(Bytes), Rest).

frame_read({ok, #{{text, <<"frame_type">>} := {text, TypeName}} = Wire}, Rest) ->
    typed_frame(frame_type_named(TypeName), Wire, Rest);
frame_read(_NotAFrame, _Rest) ->
    {error, bad_frame}.

typed_frame({ok, Type}, Wire, Rest) ->
    read_frame(read_fields(maps:to_list(Wire), field_table(Type), #{}), Rest);
typed_frame(error, _Wire, _Rest) ->
    {error, bad_frame}.

read_frame({ok, Frame}, Rest) -> {ok, Frame, Rest};
read_frame(error, _Rest) -> {error, bad_frame}.

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

%% @doc Drain every complete frame from a buffer as its CBOR bytes, exactly as
%% received and without the length prefix, and return the incomplete rest.
%% The handshake reads frames this way, because the connection proof hashes
%% the challenge bytes as received. A length over the frame cap is refused.
-spec parse_stream_bytes(binary()) -> {ok, [binary()], binary()} | {error, frame_too_large}.
parse_stream_bytes(Buf) when is_binary(Buf) ->
    drain_bytes(split_frame(Buf), Buf, []).

drain_bytes({ok, Bytes, Rest}, _Buf, Acc) ->
    drain_bytes(split_frame(Rest), Rest, [Bytes | Acc]);
drain_bytes({more, _Needed}, Buf, Acc) ->
    {ok, lists:reverse(Acc), Buf};
drain_bytes({error, frame_too_large} = Error, _Buf, _Acc) ->
    Error.

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
    macula_cbor_nif:pack_deterministic(wire_form(Unsigned)).

%%------------------------------------------------------------------
%% Atom <-> wire-binary translation
%%
%% The CBOR codec ships text strings as `{text, Bin}' tuples and byte
%% strings as plain binaries (per `macula_record_cbor'). Atoms in the
%% in-process frame map are converted to `{text, atom_to_binary(A)}'
%% before encoding, except envelope booleans, which travel as 1 or 0.
%% On the decode path envelope fields come back through the frame
%% type's table, and payload text stays `{text, Bin}'. Binaries
%% (signatures, node ids, payloads, nonces) stay as binaries on the wire.
%%------------------------------------------------------------------

%% @doc Is this term admissible as a frame payload?
%%
%% Returns `ok', or `{error, {unsupported_payload_type, Type, Path}}'
%% where `Path' locates the offending value inside the term (map keys
%% and zero-based list indices, outermost first).
%%
%% WHY THIS EXISTS. `macula_peering:send_frame/2' is a cast, so the
%% frame is encoded later, in the shared peering connection process,
%% with no try/catch around it. A term the codec cannot represent
%% therefore does not fail the publisher — it kills the connection,
%% taking every other producer's in-flight traffic with it, while the
%% publisher was told `ok'. Checking here, in the caller's process
%% before the cast, is what makes that `ok' mean something.
%%
%% This function must agree exactly with `to_wire/1' followed by
%% `macula_cbor_nif:pack_deterministic/1'. That is why it lives beside them
%% rather than in a validation module: the two cannot drift apart
%% without the agreement property test in `macula_frame_tests' going
%% red.
%%
%% FLOATS ARE CARRIED, as of the float support in `macula_record_cbor'.
%% They used to be rejected here, and before that `to_wire/1' silently
%% rewrote them as six-decimal text. Both were workarounds for the canonical
%% encoder lacking a float clause, which was never a CBOR limitation: RFC
%% 8949 major type 7 is floats. The encoder now emits binary64, so callers
%% no longer scale to integers to get a number across.
-spec check_payload(term()) ->
    ok | {error, {unsupported_payload_type, atom(), [term()]}}.
check_payload(Payload) ->
    sized(check_value(Payload, []), Payload).

%% @doc Is this whole frame sendable? Used by `macula_peering:send_frame/2',
%% which is the single seam every producer passes through. Records travel
%% as their wire bytes, so a record-bearing frame is judged like any other.
-spec check_frame(frame()) ->
    ok | {error, {unsupported_payload_type, atom(), [term()]}}.
check_frame(Frame) when is_map(Frame) ->
    check_payload(Frame).

%% @doc Render a rejection as a sentence, with the remedy where there is
%% one. The operator reading a log at 03:00 is not reading edoc.
-spec explain(term()) -> unicode:chardata().
explain({unsupported_payload_type, duplicate_wire_key, Path}) ->
    ["two keys in the map at ", fmt_path(Path),
     " collapse to the same wire key: an atom, a binary and a "
     "{text, Binary} of the same name are one key on the wire, so one "
     "would silently overwrite the other"];
explain({unsupported_payload_type, payload_too_large, Path}) ->
    ["value at ", fmt_path(Path), " exceeds the ",
     integer_to_list(?MAX_FRAME_BYTES), "-byte frame cap"];
explain({unsupported_payload_type, Type, Path}) ->
    [atom_to_list(Type), " at ", fmt_path(Path), " cannot be encoded"];
explain(Other) ->
    io_lib:format("~p", [Other]).

fmt_path([])   -> "the payload root";
fmt_path(Path) -> lists:join(".", [fmt_seg(S) || S <- Path]).

fmt_seg(S) when is_binary(S)  -> S;
fmt_seg(S) when is_atom(S)    -> atom_to_list(S);
fmt_seg(S) when is_integer(S) -> ["[", integer_to_list(S), "]"];
fmt_seg(S)                    -> io_lib:format("~p", [S]).

%% Size is checked only after the structure is known good, so the walk
%% below can assume proper lists and encodable scalars.
sized(ok, Payload)             -> within_cap(byte_floor(Payload));
sized({error, _} = Error, _P)  -> Error.

within_cap(Floor) when Floor > ?MAX_FRAME_BYTES ->
    unsupported(payload_too_large, []);
within_cap(_Floor) ->
    ok.

%% A LOWER bound on encoded size: every binary and text string costs at
%% least its own bytes, and the CBOR head only adds to that. Sound for
%% rejection — if the floor is over the cap the real encoding certainly
%% is — and deliberately not an upper bound, so a payload sitting just
%% under still reaches the connection, where `encode_or_drop/2' now
%% turns `frame_too_large' into a dropped frame instead of a dead link.
byte_floor(B) when is_binary(B)          -> byte_size(B);
byte_floor({text, B}) when is_binary(B)  -> byte_size(B);
byte_floor(L) when is_list(L)            -> sum_floor(L, 0);
byte_floor(M) when is_map(M)             ->
    maps:fold(fun(K, V, Acc) -> Acc + byte_floor(K) + byte_floor(V) end, 0, M);
byte_floor(_Scalar)                      -> 1.

sum_floor([], Acc)      -> Acc;
sum_floor([H | T], Acc) -> sum_floor(T, Acc + byte_floor(H)).

%% Integers: the decoding rule refuses an integer below -2^63 or above
%% 2^63-1, so a payload integer must lie between them. Both CBOR encoders
%% render that whole range, so no integer outside it reaches them.
check_value(I, Path) when is_integer(I) ->
    int_ok(I >= -(1 bsl 63) andalso I < 1 bsl 63, Path);
check_value(F, _Path) when is_float(F) ->
    ok;
check_value(B, _Path) when is_binary(B) ->
    ok;
%% `{text, Binary}' is the codec's own major-3 marker, not a user tuple. Its
%% bytes must be valid UTF-8, as the decoding rule requires of text.
check_value({text, B}, Path) when is_binary(B) ->
    text_ok(valid_utf8(B), Path);
%% Every atom survives: `undefined' becomes null, the rest become text
%% and are restored via `binary_to_existing_atom'.
check_value(A, _Path) when is_atom(A) ->
    ok;
check_value(Container, Path)
  when (is_list(Container) orelse is_map(Container)), length(Path) > ?MAX_PAYLOAD_NESTING ->
    unsupported(too_deep, Path);
check_value(L, Path) when is_list(L) ->
    check_list(L, 0, Path);
check_value(M, Path) when is_map(M) ->
    then_keys_distinct(check_map(maps:to_list(M), Path), M, Path);
check_value(T, Path) when is_tuple(T) ->
    unsupported(tuple, Path);
%% Pids, refs, funs, ports.
check_value(_Other, Path) ->
    unsupported(unsupported_term, Path).

%% An improper tail matches neither `[]' nor `[H | T]' as a list, and
%% `encode_array/1' calls `length/1', which would crash on it.
check_list([], _Index, _Path) ->
    ok;
check_list([H | T], Index, Path) ->
    check_next(check_value(H, [Index | Path]), T, Index + 1, Path);
check_list(_Improper, _Index, Path) ->
    unsupported(improper_list, Path).

check_next(ok, T, Index, Path) ->
    check_list(T, Index, Path);
check_next({error, _} = Error, _T, _Index, _Path) ->
    Error.

check_map([], _Path) ->
    ok;
check_map([{K, V} | T], Path) ->
    check_pair(check_key(K, Path), K, V, T, Path).

check_pair(ok, K, V, T, Path) ->
    check_map_tail(check_value(V, [K | Path]), T, Path);
check_pair({error, _} = Error, _K, _V, _T, _Path) ->
    Error.

check_map_tail(ok, T, Path) ->
    check_map(T, Path);
check_map_tail({error, _} = Error, _T, _Path) ->
    Error.

%% `wire_key/1' accepts atoms, `{text, _}', binaries and integers of
%% either sign, and nothing else — a float or nested key crashes it.
check_key(A, _Path) when is_atom(A) ->
    ok;
check_key({text, B}, Path) when is_binary(B) ->
    text_ok(valid_utf8(B), Path);
check_key(B, Path) when is_binary(B) ->
    text_ok(valid_utf8(B), Path);
check_key(I, Path) when is_integer(I) ->
    check_value(I, Path);
check_key(_Other, Path) ->
    unsupported(unsupported_map_key, Path).

int_ok(true, _Path)  -> ok;
int_ok(false, Path)  -> unsupported(integer_out_of_range, Path).

text_ok(true, _Path)  -> ok;
text_ok(false, Path)  -> unsupported(invalid_text, Path).

valid_utf8(Bin) ->
    unicode:characters_to_binary(Bin, utf8, utf8) =:= Bin.

%% `to_wire/1' projects an atom, a binary and a `{text, Binary}' of the
%% same name onto ONE wire key, and folds them into one map. Two distinct
%% Erlang keys therefore ship as a single pair and the loser vanishes,
%% silently, chosen by sort order. That is the same class of failure as
%% the float rewrite, so it is rejected the same way rather than
%% certified.
then_keys_distinct(ok, M, Path) ->
    distinct(length(lists:usort([wire_key(K) || K <- maps:keys(M)])) =:= maps:size(M),
             Path);
then_keys_distinct({error, _} = Error, _M, _Path) ->
    Error.

distinct(true, _Path) -> ok;
distinct(false, Path) -> unsupported(duplicate_wire_key, Path).

unsupported(Type, Path) ->
    {error, {unsupported_payload_type, Type, lists:reverse(Path)}}.

%% @private The wire form of a frame map: the boolean fields its frame
%% type's table names become 1 or 0, then to_wire/1 applies.
wire_form(#{frame_type := Type} = Frame) when is_atom(Type) ->
    booleans_on_wire(frame_type_named(atom_to_binary(Type)), Frame);
wire_form(Frame) ->
    to_wire(Frame).

booleans_on_wire({ok, Type}, Frame) ->
    Booleans = [Field || {Field, boolean} <- maps:values(field_table(Type))],
    to_wire(maps:map(fun(Field, Value) -> boolean_on_wire(lists:member(Field, Booleans), Value) end, Frame));
booleans_on_wire(error, Frame) ->
    to_wire(Frame).

boolean_on_wire(true, true) -> 1;
boolean_on_wire(true, false) -> 0;
boolean_on_wire(_Boolean, Value) -> Value.

%% @private Convert a frame map (atom keys, atom values where used)
%% into the shape the CBOR encoders understand: atoms, including true and
%% false outside an envelope boolean field, become text, undefined becomes
%% null, and binaries, integers and floats stay as they are.
to_wire(M) when is_map(M) ->
    maps:fold(fun(K, V, Acc) ->
                  Acc#{wire_key(K) => to_wire(V)}
              end, #{}, M);
to_wire(L) when is_list(L) ->
    [to_wire(E) || E <- L];
to_wire(undefined) -> null;
to_wire(A) when is_atom(A) ->
    {text, atom_to_binary(A, utf8)};
to_wire({text, B}) when is_binary(B) -> {text, B};
to_wire(B) when is_binary(B) -> B;
to_wire(I) when is_integer(I) -> I;
to_wire(Other) -> Other.

wire_key(A) when is_atom(A)   -> {text, atom_to_binary(A, utf8)};
wire_key({text, B})           -> {text, B};
wire_key(B) when is_binary(B) -> {text, B};
%% Integer keys (any sign) pass through to the CBOR encoder as-is,
%% which renders them as major 0 or major 1 — both are valid CBOR map
%% keys. Required for payloads whose nested maps are indexed by integer
%% (e.g. per-wall sub-maps in mpong game state).
wire_key(I) when is_integer(I) -> I.

%%------------------------------------------------------------------
%% Decoding through a fixed table (D26)
%%------------------------------------------------------------------

%% A frame type from its wire name. Only the frame types below exist.
frame_type_named(<<"connect">>) -> {ok, connect};
frame_type_named(<<"hello">>) -> {ok, hello};
frame_type_named(<<"goodbye">>) -> {ok, goodbye};
frame_type_named(<<"swim_ping">>) -> {ok, swim_ping};
frame_type_named(<<"swim_ack">>) -> {ok, swim_ack};
frame_type_named(<<"swim_suspect">>) -> {ok, swim_suspect};
frame_type_named(<<"swim_confirm">>) -> {ok, swim_confirm};
frame_type_named(<<"ping">>) -> {ok, ping};
frame_type_named(<<"pong">>) -> {ok, pong};
frame_type_named(<<"find_node">>) -> {ok, find_node};
frame_type_named(<<"nodes">>) -> {ok, nodes};
frame_type_named(<<"find_value">>) -> {ok, find_value};
frame_type_named(<<"value">>) -> {ok, value};
frame_type_named(<<"store">>) -> {ok, store};
frame_type_named(<<"store_ack">>) -> {ok, store_ack};
frame_type_named(<<"call">>) -> {ok, call};
frame_type_named(<<"result">>) -> {ok, result};
frame_type_named(<<"error">>) -> {ok, error};
frame_type_named(<<"hyparview_join">>) -> {ok, hyparview_join};
frame_type_named(<<"hyparview_forward_join">>) -> {ok, hyparview_forward_join};
frame_type_named(<<"hyparview_neighbor">>) -> {ok, hyparview_neighbor};
frame_type_named(<<"hyparview_disconnect">>) -> {ok, hyparview_disconnect};
frame_type_named(<<"hyparview_shuffle">>) -> {ok, hyparview_shuffle};
frame_type_named(<<"hyparview_shuffle_reply">>) -> {ok, hyparview_shuffle_reply};
frame_type_named(<<"plumtree_gossip">>) -> {ok, plumtree_gossip};
frame_type_named(<<"plumtree_ihave">>) -> {ok, plumtree_ihave};
frame_type_named(<<"plumtree_graft">>) -> {ok, plumtree_graft};
frame_type_named(<<"plumtree_prune">>) -> {ok, plumtree_prune};
frame_type_named(<<"overlay_relay">>) -> {ok, overlay_relay};
frame_type_named(<<"publish">>) -> {ok, publish};
frame_type_named(<<"subscribe">>) -> {ok, subscribe};
frame_type_named(<<"unsubscribe">>) -> {ok, unsubscribe};
frame_type_named(<<"event">>) -> {ok, event};
frame_type_named(<<"advertise">>) -> {ok, advertise};
frame_type_named(<<"unadvertise">>) -> {ok, unadvertise};
frame_type_named(<<"stream_open">>) -> {ok, stream_open};
frame_type_named(<<"stream_data">>) -> {ok, stream_data};
frame_type_named(<<"stream_end">>) -> {ok, stream_end};
frame_type_named(<<"stream_error">>) -> {ok, stream_error};
frame_type_named(<<"stream_reply">>) -> {ok, stream_reply};
frame_type_named(<<"want">>) -> {ok, want};
frame_type_named(<<"have">>) -> {ok, have};
frame_type_named(<<"block">>) -> {ok, block};
frame_type_named(<<"manifest_req">>) -> {ok, manifest_req};
frame_type_named(<<"manifest_res">>) -> {ok, manifest_res};
frame_type_named(<<"cancel">>) -> {ok, cancel};
frame_type_named(_Other) -> error.

%% The fields each frame type defines, by wire name: the atom key a field
%% decodes to, and how its value is read. `value' keeps a value in the one
%% key form of peer-supplied maps; an enum takes only the atoms it lists; a
%% reason takes its listed atoms and keeps any other reason as text; a list
%% of entries reads each entry through its own table.
field_table(connect) ->
    #{<<"version">> => {version, value},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"node_id">> => {node_id, value},
      <<"station_id">> => {station_id, value},
      <<"realms">> => {realms, value},
      <<"addresses">> => {addresses, value},
      <<"site">> => {site, value},
      <<"puzzle_evidence">> => {puzzle_evidence, value},
      <<"endorsements">> => {endorsements, value}};
field_table(hello) ->
    #{<<"version">> => {version, value},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"node_id">> => {node_id, value},
      <<"station_id">> => {station_id, value},
      <<"realms">> => {realms, value},
      <<"addresses">> => {addresses, value},
      <<"site">> => {site, value},
      <<"accepted">> => {accepted, boolean},
      <<"refusal_code">> => {refusal_code, value},
      <<"negotiated_capabilities">> => {negotiated_capabilities, value}};
field_table(goodbye) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"reason">> => {reason, {bounded_text, ?MAX_GOODBYE_REASON_BYTES}},
      <<"detail">> => {detail, value}};
field_table(swim_ping) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"round">> => {round, uint},
      <<"incarnation">> => {incarnation, uint},
      <<"piggyback">> => {piggyback, {list_of, #{<<"target">> => {target, {bytes, 32}},
          <<"state">> => {state, {enum, [alive, suspect, confirmed_failed]}},
          <<"incarnation">> => {incarnation, uint},
          <<"observed_at">> => {observed_at, uint},
          <<"by">> => {by, {bytes, 32}}}}}};
field_table(swim_ack) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"round">> => {round, uint},
      <<"responder">> => {responder, {bytes, 32}},
      <<"incarnation">> => {incarnation, uint},
      <<"piggyback">> => {piggyback, {list_of, #{<<"target">> => {target, {bytes, 32}},
          <<"state">> => {state, {enum, [alive, suspect, confirmed_failed]}},
          <<"incarnation">> => {incarnation, uint},
          <<"observed_at">> => {observed_at, uint},
          <<"by">> => {by, {bytes, 32}}}}}};
field_table(swim_suspect) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"target">> => {target, {bytes, 32}},
      <<"target_incarnation">> => {target_incarnation, uint},
      <<"suspected_by">> => {suspected_by, {bytes, 32}},
      <<"ttl">> => {ttl, uint}};
field_table(swim_confirm) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"target">> => {target, {bytes, 32}},
      <<"target_incarnation">> => {target_incarnation, uint},
      <<"suspected_by">> => {suspected_by, {bytes, 32}},
      <<"ttl">> => {ttl, uint}};
field_table(ping) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"nonce">> => {nonce, value}};
field_table(pong) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"nonce">> => {nonce, value}};
field_table(find_node) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"key">> => {key, value},
      <<"origin">> => {origin, {bytes, 32}},
      <<"depth">> => {depth, uint}};
field_table(nodes) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"key">> => {key, value},
      <<"nodes">> => {nodes, {list_of, #{<<"node_id">> => {node_id, {bytes, 32}},
          <<"station_id">> => {station_id, {bytes, 32}},
          <<"addresses">> => {addresses, value},
          <<"tier">> => {tier, uint},
          <<"asn">> => {asn, {optional, uint}},
          <<"country">> => {country, {bytes, 2}},
          <<"last_seen_at">> => {last_seen_at, uint}}}}};
field_table(find_value) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"key">> => {key, value},
      <<"origin">> => {origin, {bytes, 32}}};
field_table(value) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"key">> => {key, value},
      <<"records">> => {records, value}};
field_table(store) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"record">> => {record, value}};
field_table(store_ack) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"key">> => {key, value},
      <<"stored">> => {stored, boolean}};
field_table(call) ->
    #{<<"version">> => {version, value},
      <<"frame_type">> => {frame_type, frame_type},
      <<"request">> => {request, signed_object},
      <<"source_route">> => {source_route, bytes},
      <<"retry_budget">> => {retry_budget, uint}};
field_table(result) ->
    #{<<"version">> => {version, value},
      <<"frame_type">> => {frame_type, frame_type},
      <<"reply">> => {reply, signed_object},
      <<"source_route_reverse">> => {source_route_reverse, bytes}};
field_table(error) ->
    #{<<"version">> => {version, value},
      <<"frame_type">> => {frame_type, frame_type},
      <<"reply">> => {reply, signed_object},
      <<"relay_error">> => {relay_error, signed_object},
      <<"source_route_reverse">> => {source_route_reverse, bytes},
      <<"source_route_partial">> => {source_route_partial, bytes}};
field_table(hyparview_join) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"new_member">> => {new_member, {bytes, 32}},
      <<"record">> => {record, value}};
field_table(hyparview_forward_join) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"new_member">> => {new_member, {bytes, 32}},
      <<"ttl">> => {ttl, uint},
      <<"arwl">> => {arwl, uint},
      <<"prwl">> => {prwl, uint},
      <<"record">> => {record, value}};
field_table(hyparview_neighbor) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"priority">> => {priority, {enum, [high, low]}},
      <<"record">> => {record, value}};
field_table(hyparview_disconnect) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value}};
field_table(hyparview_shuffle) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"origin">> => {origin, {bytes, 32}},
      <<"ttl">> => {ttl, uint},
      <<"peer_sample">> => {peer_sample, {list_of_bytes, 32}}};
field_table(hyparview_shuffle_reply) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"peer_sample">> => {peer_sample, {list_of_bytes, 32}}};
field_table(plumtree_gossip) ->
    #{<<"version">> => {version, value},
      <<"frame_type">> => {frame_type, frame_type},
      <<"publication">> => {publication, signed_object},
      <<"round">> => {round, uint}};
field_table(plumtree_ihave) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"msg_id">> => {msg_id, {bytes, 48}},
      <<"round">> => {round, uint}};
field_table(plumtree_graft) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"msg_id">> => {msg_id, {bytes, 48}},
      <<"round">> => {round, uint}};
field_table(plumtree_prune) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value}};
field_table(overlay_relay) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"peer">> => {peer, {bytes, 32}},
      <<"payload">> => {payload, value}};
field_table(publish) ->
    #{<<"version">> => {version, value},
      <<"frame_type">> => {frame_type, frame_type},
      <<"publication">> => {publication, signed_object}};
field_table(subscribe) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"topic">> => {topic, value},
      <<"subscriber">> => {subscriber, {bytes, 32}},
      <<"filter">> => {filter, value},
      <<"options">> => {options, value}};
field_table(unsubscribe) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"topic">> => {topic, value},
      <<"subscriber">> => {subscriber, {bytes, 32}}};
field_table(event) ->
    #{<<"version">> => {version, value},
      <<"frame_type">> => {frame_type, frame_type},
      <<"publication">> => {publication, signed_object},
      <<"delivered_via">> => {delivered_via, {enum, [plumtree, direct]}}};
field_table(advertise) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"advertisement">> => {advertisement, bytes}};
field_table(unadvertise) ->
    #{<<"version">> => {version, value},
      <<"neighbour">> => {neighbour, held_object},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"withdrawal">> => {withdrawal, bytes}};
field_table(stream_open) ->
    #{<<"version">> => {version, value},
      <<"frame_type">> => {frame_type, frame_type},
      <<"request">> => {request, signed_object},
      <<"source_route">> => {source_route, bytes},
      <<"retry_budget">> => {retry_budget, uint}};
field_table(stream_data) ->
    #{<<"version">> => {version, value},
      <<"frame_type">> => {frame_type, frame_type},
      <<"stream">> => {stream, stream_object},
      <<"caller_stream">> => {caller_stream, held_object}};
field_table(stream_end) ->
    #{<<"version">> => {version, value},
      <<"frame_type">> => {frame_type, frame_type},
      <<"stream">> => {stream, stream_object},
      <<"caller_stream">> => {caller_stream, held_object}};
field_table(stream_error) ->
    #{<<"version">> => {version, value},
      <<"frame_type">> => {frame_type, frame_type},
      <<"stream">> => {stream, stream_object},
      <<"caller_stream">> => {caller_stream, held_object},
      <<"relay_error">> => {relay_error, signed_object},
      <<"source_route_partial">> => {source_route_partial, bytes}};
field_table(stream_reply) ->
    #{<<"version">> => {version, value},
      <<"frame_type">> => {frame_type, frame_type},
      <<"stream">> => {stream, stream_object}};
field_table(want) ->
    #{<<"version">> => {version, value},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"blocks">> => {blocks, {list_of, #{<<"mcid">> => {mcid, value},
          <<"priority">> => {priority, value}}}}};
field_table(have) ->
    #{<<"version">> => {version, value},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"blocks">> => {blocks, {list_of, #{<<"mcid">> => {mcid, value},
          <<"size">> => {size, value}}}}};
field_table(block) ->
    #{<<"version">> => {version, value},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"mcid">> => {mcid, value},
      <<"payload">> => {payload, value}};
field_table(manifest_req) ->
    #{<<"version">> => {version, value},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"mcid">> => {mcid, value}};
field_table(manifest_res) ->
    #{<<"version">> => {version, value},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"mcid">> => {mcid, value},
      <<"manifest">> => {manifest, manifest}};
field_table(cancel) ->
    #{<<"version">> => {version, value},
      <<"frame_type">> => {frame_type, frame_type},
      <<"frame_id">> => {frame_id, value},
      <<"sent_at_ms">> => {sent_at_ms, uint},
      <<"capabilities">> => {capabilities, uint},
      <<"realm">> => {realm, value},
      <<"call_id">> => {call_id, value},
      <<"source_route">> => {source_route, value},
      <<"signature">> => {signature, value},
      <<"blocks">> => {blocks, value}}.

read_fields([{{text, Name}, Value} | Rest], Table, Frame) ->
    field_read(maps:get(Name, Table, undefined), Value, Rest, Table, Frame);
read_fields([], _Table, Frame) ->
    {ok, Frame};
read_fields(_KeyTheTableCannotName, _Table, _Frame) ->
    error.

field_read({Field, Kind}, Value, Rest, Table, Frame) ->
    field_value(read_value(Kind, Value), Field, Rest, Table, Frame);
field_read(undefined, _Value, _Rest, _Table, _Frame) ->
    error.

field_value({ok, Read}, Field, Rest, Table, Frame) -> read_fields(Rest, Table, Frame#{Field => Read});
field_value(error, _Field, _Rest, _Table, _Frame) -> error.

read_value(value, Value) -> {ok, peer_value(Value)};
read_value(frame_type, {text, Name}) -> frame_type_named(Name);
read_value({enum, Atoms}, {text, Name}) -> enum_value(Name, Atoms);
read_value({bounded_text, Max}, {text, Bin} = Text) when byte_size(Bin) =< Max -> {ok, Text};
read_value(boolean, 1) -> {ok, true};
read_value(boolean, 0) -> {ok, false};
read_value({list_of, Table}, Entries) when is_list(Entries) -> entries_read(Entries, Table, []);
read_value(manifest, Manifest) when is_map(Manifest) -> {ok, peer_value(Manifest)};
read_value(signed_object, #{{text, <<"key">>} := Key, {text, <<"tbs">>} := Tbs,
                            {text, <<"signature">>} := Signature} = Object)
  when map_size(Object) =:= 3, is_binary(Key), is_binary(Tbs), is_binary(Signature) ->
    {ok, #{key => Key, tbs => Tbs, signature => Signature}};
read_value(bytes, Bytes) when is_binary(Bytes) -> {ok, Bytes};
read_value({bytes, Size}, Bytes) when byte_size(Bytes) =:= Size -> {ok, Bytes};
read_value(text, {text, Text}) -> {ok, Text};
read_value(uint, N) when is_integer(N), N >= 0, N < ?MAX_PROTOCOL_INT -> {ok, N};
read_value(held_object, #{{text, <<"tbs">>} := Tbs, {text, <<"signature">>} := Signature} = Held)
  when map_size(Held) =:= 2, is_binary(Tbs), is_binary(Signature) ->
    {ok, #{tbs => Tbs, signature => Signature}};
read_value(stream_object, Object) -> either_object(read_value(signed_object, Object), Object);
read_value({list_of_bytes, Size}, Items) when is_list(Items) ->
    sized_items([Item || Item <- Items, is_binary(Item), byte_size(Item) =:= Size], Items);
read_value({optional, _Kind}, null) -> {ok, undefined};
read_value({optional, Kind}, Value) -> read_value(Kind, Value);
read_value(_Kind, _Value) -> error.

sized_items(Items, Items) -> {ok, Items};
sized_items(_Sized, _Items) -> error.

%% A provider's stream object carries its key on the first frame and not after, so either shape reads.
either_object({ok, _Carried} = Read, _Object) -> Read;
either_object(error, Object) -> read_value(held_object, Object).

enum_value(Name, [Atom | Atoms]) -> enum_match(atom_to_binary(Atom) =:= Name, Atom, Name, Atoms);
enum_value(_Name, []) -> error.

enum_match(true, Atom, _Name, _Atoms) -> {ok, Atom};
enum_match(false, _Atom, Name, Atoms) -> enum_value(Name, Atoms).

entries_read([Entry | Rest], Table, Acc) when is_map(Entry) ->
    entry_read(read_fields(maps:to_list(Entry), Table, #{}), Rest, Table, Acc);
entries_read([], _Table, Acc) ->
    {ok, lists:reverse(Acc)};
entries_read(_NotEntries, _Table, _Acc) ->
    error.

entry_read({ok, Entry}, Rest, Table, Acc) -> entries_read(Rest, Table, [Entry | Acc]);
entry_read(error, _Rest, _Table, _Acc) -> error.

%% A peer-supplied value in the one key form (D26): text stays `{text, Bin}',
%% byte strings stay binaries, null reads as undefined, and no atom is made.
peer_value(null) -> undefined;
peer_value(List) when is_list(List) -> [peer_value(Element) || Element <- List];
peer_value(Map) when is_map(Map) -> maps:map(fun(_Key, Value) -> peer_value(Value) end, Map);
peer_value(Other) -> Other.
