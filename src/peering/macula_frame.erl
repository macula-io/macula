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
%% schemas (atom-keyed maps in process memory) are unchanged; the codec
%% transparently round-trips atom keys/values to text strings on the wire
%% and reconstitutes them via `binary_to_existing_atom' on decode.
%%
%% Phase 1 covers CONNECT / HELLO / GOODBYE. Phase 2 adds SWIM. Phase 3
%% (Session 3.4) adds the DHT operation frames from Part 6 §7:
%% PING / PONG, FIND_NODE / NODES, FIND_VALUE / VALUE, STORE / STORE_ACK,
%% REPLICATE / REPLICATE_ACK. Phase 4 (Session 4.1) adds the CALL /
%% RESULT / ERROR frames from Part 6 §5 plus the BOLT#4 error
%% taxonomy in `hecate_bolt4'. PUBLISH frames land later.
%%
%% Signatures are Ed25519 over `"macula-v2-frame\0" ++ canonical_cbor(unsigned)'
%% where `canonical_cbor' is `macula_record_cbor:encode/1' (RFC 8949 §4.2.1
%% deterministic).
-module(macula_frame).

-export([
    %% Constructors — handshake
    connect/1, hello/1, goodbye/2, goodbye/3,

    %% Constructors — SWIM
    swim_ping/1, swim_ack/1, swim_suspect/1, swim_confirm/1,
    swim_update/1,
    sign_swim_update/2, verify_swim_update/1,

    %% Constructors — DHT (Part 6 §7)
    ping/1, pong/1,
    find_node/1, nodes/1,
    find_value/1, value/1,
    store/1, store_ack/1,
    replicate/1, replicate_ack/1,

    %% DHT helper — build and validate a station_ref entry
    station_ref/1,

    %% Constructors — CALL (Part 6 §5)
    call/1, result/1, call_error/1,

    %% Constructors — HyParView (Part 3 §7.1)
    hyparview_join/1, hyparview_forward_join/1, hyparview_neighbor/1,
    hyparview_disconnect/1, hyparview_shuffle/1, hyparview_shuffle_reply/1,

    %% Constructors — Plumtree (Part 3 §7.2)
    plumtree_gossip/1, plumtree_ihave/1, plumtree_graft/1, plumtree_prune/1,

    %% Constructors — PubSub (Part 6 §6)
    publish/1, subscribe/1, unsubscribe/1, event/1,

    %% Constructors — RPC procedure advertise (connection-scoped,
    %% Part 6 §5.5). Companions to call/result/error: a peer connected
    %% to a station registers itself as the handler for a procedure
    %% URI in the station's per-connection routing table; the station
    %% forwards inbound CALL frames for that procedure back over the
    %% advertiser's connection. Tombstoned on UNADVERTISE or peer
    %% disconnect.
    advertise/1, unadvertise/1,

    %% Constructors — Content transfer (Part 6 §9)
    want/1, have/1, block/1,
    manifest_req/1, manifest_res/1, cancel/1,

    %% Sign / verify frame
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
    replicate_spec/0, replicate_ack_spec/0,
    station_ref/0, station_ref_spec/0,
    call_spec/0, result_spec/0, call_error_spec/0,
    call_id/0,
    hyparview_join_spec/0, hyparview_forward_join_spec/0,
    hyparview_neighbor_spec/0, hyparview_disconnect_spec/0,
    hyparview_shuffle_spec/0, hyparview_shuffle_reply_spec/0,
    neighbor_priority/0,
    plumtree_gossip_spec/0, plumtree_ihave_spec/0,
    plumtree_graft_spec/0, plumtree_prune_spec/0,
    msg_id/0,
    publish_spec/0, subscribe_spec/0, unsubscribe_spec/0, event_spec/0,
    advertise_spec/0, unadvertise_spec/0,
    delivery_channel/0,
    mcid/0, want_priority/0, want_entry/0, have_entry/0,
    want_spec/0, have_spec/0, block_spec/0,
    manifest_req_spec/0, manifest_res_spec/0, cancel_spec/0
]).

-define(SIG_DOMAIN,        "macula-v2-frame\0").
-define(SWIM_UPDATE_DOMAIN, "macula-v2-swim-update\0").
-define(PROTOCOL_VERSION,   2).
-define(MAX_FRAME_BYTES,    16#FFFFFF).   %% 16 MiB cap (Part 6 §2.2).

-type frame_type() :: connect | hello | goodbye
                    | swim_ping | swim_ack | swim_suspect | swim_confirm
                    | ping | pong
                    | find_node | nodes
                    | find_value | value
                    | store | store_ack
                    | replicate | replicate_ack
                    | call | result | error
                    | hyparview_join | hyparview_forward_join
                    | hyparview_neighbor | hyparview_disconnect
                    | hyparview_shuffle | hyparview_shuffle_reply
                    | plumtree_gossip | plumtree_ihave
                    | plumtree_graft | plumtree_prune
                    | publish | subscribe | unsubscribe | event
                    | advertise | unadvertise
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
    target      := macula_identity:pubkey(),
    state       := member_state(),
    incarnation := non_neg_integer(),
    observed_at := pos_integer(),
    by          := macula_identity:pubkey()
}.

-type swim_update() :: #{
    target      := macula_identity:pubkey(),
    state       := member_state(),
    incarnation := non_neg_integer(),
    observed_at := pos_integer(),
    by          := macula_identity:pubkey(),
    signature   => <<_:512>>
}.

-type swim_ping_spec() :: #{
    round       := non_neg_integer(),
    incarnation := non_neg_integer(),
    piggyback   => [swim_update()]
}.

-type swim_ack_spec() :: #{
    round       := non_neg_integer(),
    responder   := macula_identity:pubkey(),
    incarnation := non_neg_integer(),
    piggyback   => [swim_update()]
}.

-type swim_suspect_spec() :: #{
    target             := macula_identity:pubkey(),
    target_incarnation := non_neg_integer(),
    suspected_by       := macula_identity:pubkey(),
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
    node_id      := macula_identity:pubkey(),
    station_id   := macula_identity:pubkey(),
    addresses    => [map()],
    tier         := tier(),
    asn          => non_neg_integer() | undefined,
    country      := country(),
    last_seen_at := pos_integer()
}.

-type station_ref() :: #{
    node_id      := macula_identity:pubkey(),
    station_id   := macula_identity:pubkey(),
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
    origin := macula_identity:pubkey(),
    depth  := non_neg_integer()
}.

-type nodes_spec()         :: #{
    key   := id256(),
    nodes := [station_ref()]
}.

-type find_value_spec()    :: #{
    key    := id256(),
    origin := macula_identity:pubkey()
}.

-type value_spec()         :: #{
    key     := id256(),
    records := [macula_record:record()]
}.

-type store_spec()         :: #{record := macula_record:record()}.

-type store_ack_spec()     :: #{
    key    := id256(),
    stored := boolean(),
    reason => atom() | undefined
}.

-type replicate_spec()     :: #{
    record        := macula_record:record(),
    new_custodian := boolean()
}.

-type replicate_ack_spec() :: #{
    key      := id256(),
    accepted := boolean()
}.

%%------------------------------------------------------------------
%% CALL frame specs (Part 6 §5)
%%------------------------------------------------------------------

-type call_id() :: <<_:128>>.

-type call_spec() :: #{
    call_id      := call_id(),
    procedure    := binary(),
    realm        := id256(),
    payload      := term(),
    deadline_ms  := integer(),
    caller       := macula_identity:pubkey(),
    source_route => binary(),
    retry_budget => non_neg_integer()
}.

-type result_spec() :: #{
    call_id              := call_id(),
    payload              := term(),
    responded_by         := macula_identity:pubkey(),
    source_route_reverse => binary()
}.

-type call_error_spec() :: #{
    call_id              := call_id(),
    code                 := macula_bolt4:code(),
    reported_by          := macula_identity:pubkey(),
    detail               => binary() | undefined,
    offending_hop        => macula_identity:pubkey() | undefined,
    source_route_partial => binary()
}.

%%------------------------------------------------------------------
%% HyParView frame specs (Part 3 §7.1)
%%------------------------------------------------------------------

-type neighbor_priority() :: high | low.

-type hyparview_join_spec() :: #{
    realm      := id256(),
    new_member := macula_identity:pubkey()
}.

-type hyparview_forward_join_spec() :: #{
    realm      := id256(),
    new_member := macula_identity:pubkey(),
    ttl        := non_neg_integer(),
    arwl       := non_neg_integer(),
    prwl       := non_neg_integer()
}.

-type hyparview_neighbor_spec() :: #{
    realm    := id256(),
    priority := neighbor_priority()
}.

-type hyparview_disconnect_spec() :: #{
    realm := id256()
}.

-type hyparview_shuffle_spec() :: #{
    realm       := id256(),
    origin      := macula_identity:pubkey(),
    ttl         := non_neg_integer(),
    peer_sample := [macula_identity:pubkey()]
}.

-type hyparview_shuffle_reply_spec() :: #{
    realm       := id256(),
    peer_sample := [macula_identity:pubkey()]
}.

%%------------------------------------------------------------------
%% Plumtree frame specs (Part 3 §7.2)
%%------------------------------------------------------------------

-type msg_id() :: <<_:128>>.

-type plumtree_gossip_spec() :: #{
    realm   := id256(),
    msg_id  := msg_id(),
    round   := non_neg_integer(),
    payload := term()
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
%% PubSub frame specs (Part 6 §6)
%%------------------------------------------------------------------

-type delivery_channel() :: plumtree | dht | direct.

-type publish_spec() :: #{
    topic           := binary(),
    realm           := id256(),
    publisher       := macula_identity:pubkey(),
    seq             := non_neg_integer(),
    payload         := term(),
    published_at_ms := non_neg_integer(),
    ttl_ms          => non_neg_integer() | undefined
}.

-type subscribe_spec() :: #{
    topic      := binary(),
    realm      := id256(),
    subscriber := macula_identity:pubkey(),
    filter     => term() | undefined,
    options    => map()
}.

-type unsubscribe_spec() :: #{
    topic      := binary(),
    realm      := id256(),
    subscriber := macula_identity:pubkey()
}.

-type event_spec() :: #{
    topic         := binary(),
    realm         := id256(),
    publisher     := macula_identity:pubkey(),
    seq           := non_neg_integer(),
    payload       := term(),
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

-type advertise_spec() :: #{
    realm      := id256(),
    procedure  := binary(),
    advertiser := macula_identity:pubkey(),
    options    => map()
}.

-type unadvertise_spec() :: #{
    realm      := id256(),
    procedure  := binary(),
    advertiser := macula_identity:pubkey()
}.

%%------------------------------------------------------------------
%% Content transfer frame specs (Part 6 §9)
%%
%% MCID — Macula Content IDentifier — 34 bytes:
%% &lt;&lt;Version:8, Codec:8, Hash:32/binary&gt;&gt;. Block payloads carry
%% raw chunk bytes; manifest payloads carry the structured manifest
%% map. Frames are signed by the sender for accountability; the
%% recipient verifies the signature on top of the per-block /
%% per-manifest hash check.
%%------------------------------------------------------------------

-type mcid() :: <<_:272>>.

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
    manifest := map() | not_found
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
    Header = base(goodbye, Caps),
    Header#{reason => Reason, detail => Detail}.

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
%% Updates are individually signed by the observer (`by') so piggyback
%% propagation can be verified end-to-end. Domain separator differs
%% from the frame signature (`macula-v2-swim-update\0').
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

-spec sign_swim_update(swim_update(),
                       macula_identity:key_pair() | macula_identity:privkey()) ->
    swim_update().
sign_swim_update(Update, Identity) ->
    Bytes = canonical_swim_update(Update),
    Sig = macula_identity:sign([?SWIM_UPDATE_DOMAIN, Bytes], Identity),
    Update#{signature => Sig}.

-spec verify_swim_update(swim_update()) -> {ok, swim_update()} | {error, term()}.
verify_swim_update(#{signature := Sig, by := By} = Update)
  when is_binary(Sig), byte_size(Sig) =:= 64,
       is_binary(By),  byte_size(By)  =:= 32 ->
    Bytes = canonical_swim_update(Update),
    verify_update_result(
        macula_identity:verify([?SWIM_UPDATE_DOMAIN, Bytes], Sig, By),
        Update);
verify_swim_update(_Update) ->
    {error, bad_swim_update}.

verify_update_result(true,  Update) -> {ok, Update};
verify_update_result(false, _Update) -> {error, signature_invalid}.

canonical_swim_update(Update) ->
    macula_record_cbor:encode(to_wire(maps:without([signature], Update))).

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
    lists:foreach(fun validate_record/1, Rs),
    (base(value, 0))#{key => K, records => Rs}.

-spec store(store_spec()) -> frame().
store(#{record := R}) ->
    validate_record(R),
    (base(store, 0))#{record => R}.

-spec store_ack(store_ack_spec()) -> frame().
store_ack(#{key := K, stored := Stored} = Spec)
  when is_binary(K), byte_size(K) =:= 32,
       is_boolean(Stored) ->
    Reason = maps:get(reason, Spec, undefined),
    validate_optional_reason(Reason),
    (base(store_ack, 0))#{key => K, stored => Stored, reason => Reason}.

-spec replicate(replicate_spec()) -> frame().
replicate(#{record := R, new_custodian := NC})
  when is_boolean(NC) ->
    validate_record(R),
    (base(replicate, 0))#{record => R, new_custodian => NC}.

-spec replicate_ack(replicate_ack_spec()) -> frame().
replicate_ack(#{key := K, accepted := A})
  when is_binary(K), byte_size(K) =:= 32,
       is_boolean(A) ->
    (base(replicate_ack, 0))#{key => K, accepted => A}.

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

-spec validate_record(macula_record:record()) -> ok.
validate_record(#{type := _, key := <<_:256>>, payload := P}) when is_map(P) ->
    ok.

-spec validate_optional_reason(atom() | undefined) -> ok.
validate_optional_reason(undefined)                      -> ok;
validate_optional_reason(R) when is_atom(R), R =/= true, R =/= false -> ok.

%%------------------------------------------------------------------
%% CALL / RESULT / ERROR constructors (Part 6 §5)
%%
%% CALL is the request envelope; RESULT is the success response;
%% `call_error/1' (avoids clashing with the auto-imported
%% `error/1' BIF) builds a structured BOLT#4 failure.
%%
%% Source-route fields are accepted as opaque binaries here —
%% encoding/decoding the source-route header lands in Phase 4
%% Session 4.2 against `macula_routing'.
%%------------------------------------------------------------------

-spec call(call_spec()) -> frame().
call(#{call_id := CallId, procedure := Proc, realm := Realm,
       payload := Payload, deadline_ms := DeadlineMs,
       caller := Caller} = Spec)
  when is_binary(CallId),  byte_size(CallId) =:= 16,
       is_binary(Proc),
       is_binary(Realm),   byte_size(Realm)  =:= 32,
       is_integer(DeadlineMs),
       is_binary(Caller),  byte_size(Caller) =:= 32 ->
    SourceRoute = maps:get(source_route, Spec, <<>>),
    RetryBudget = maps:get(retry_budget, Spec, 0),
    validate_source_route(SourceRoute),
    validate_retry_budget(RetryBudget),
    Header = base(call, 0),
    Header#{
        call_id      => CallId,
        procedure    => Proc,
        realm        => Realm,
        payload      => Payload,
        deadline_ms  => DeadlineMs,
        caller       => Caller,
        source_route => SourceRoute,
        retry_budget => RetryBudget
    }.

-spec result(result_spec()) -> frame().
result(#{call_id := CallId, payload := Payload,
         responded_by := RespondedBy} = Spec)
  when is_binary(CallId),       byte_size(CallId) =:= 16,
       is_binary(RespondedBy),  byte_size(RespondedBy) =:= 32 ->
    Reverse = maps:get(source_route_reverse, Spec, <<>>),
    validate_source_route(Reverse),
    Header = base(result, 0),
    Header#{
        call_id              => CallId,
        payload              => Payload,
        responded_by         => RespondedBy,
        source_route_reverse => Reverse
    }.

-spec call_error(call_error_spec()) -> frame().
call_error(#{call_id := CallId, code := Code,
             reported_by := ReportedBy} = Spec)
  when is_binary(CallId),       byte_size(CallId) =:= 16,
       is_integer(Code),        Code >= 0, Code =< 255,
       is_binary(ReportedBy),   byte_size(ReportedBy) =:= 32 ->
    Name      = macula_bolt4:name(Code),
    Detail    = maps:get(detail, Spec, undefined),
    Hop       = maps:get(offending_hop, Spec, undefined),
    Partial   = maps:get(source_route_partial, Spec, <<>>),
    validate_optional_detail(Detail),
    validate_optional_hop(Hop),
    validate_source_route(Partial),
    Header = base(error, 0),
    Header#{
        call_id              => CallId,
        code                 => Code,
        name                 => Name,
        reported_by          => ReportedBy,
        detail               => Detail,
        offending_hop        => Hop,
        source_route_partial => Partial
    }.

-spec validate_source_route(binary()) -> ok.
validate_source_route(B) when is_binary(B) -> ok.

-spec validate_retry_budget(non_neg_integer()) -> ok.
validate_retry_budget(N) when is_integer(N), N >= 0 -> ok.

-spec validate_optional_detail(binary() | undefined) -> ok.
validate_optional_detail(undefined)                  -> ok;
validate_optional_detail(B) when is_binary(B)        -> ok.

-spec validate_optional_hop(macula_identity:pubkey() | undefined) -> ok.
validate_optional_hop(undefined)                              -> ok;
validate_optional_hop(B) when is_binary(B), byte_size(B) =:= 32 -> ok.

%%------------------------------------------------------------------
%% HyParView constructors (Part 3 §7.1)
%%------------------------------------------------------------------

-spec hyparview_join(hyparview_join_spec()) -> frame().
hyparview_join(#{realm := R, new_member := M})
  when is_binary(R), byte_size(R) =:= 32,
       is_binary(M), byte_size(M) =:= 32 ->
    (base(hyparview_join, 0))#{realm => R, new_member => M}.

-spec hyparview_forward_join(hyparview_forward_join_spec()) -> frame().
hyparview_forward_join(#{realm := R, new_member := M,
                         ttl := Ttl, arwl := A, prwl := P})
  when is_binary(R), byte_size(R) =:= 32,
       is_binary(M), byte_size(M) =:= 32,
       is_integer(Ttl), Ttl >= 0,
       is_integer(A),   A >= 0,
       is_integer(P),   P >= 0 ->
    (base(hyparview_forward_join, 0))#{
        realm => R, new_member => M,
        ttl => Ttl, arwl => A, prwl => P
    }.

-spec hyparview_neighbor(hyparview_neighbor_spec()) -> frame().
hyparview_neighbor(#{realm := R, priority := P})
  when is_binary(R), byte_size(R) =:= 32,
       (P =:= high orelse P =:= low) ->
    (base(hyparview_neighbor, 0))#{realm => R, priority => P}.

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

-spec plumtree_gossip(plumtree_gossip_spec()) -> frame().
plumtree_gossip(#{realm := R, msg_id := M, round := Rd,
                  payload := Payload})
  when is_binary(R), byte_size(R) =:= 32,
       is_binary(M), byte_size(M) =:= 16,
       is_integer(Rd), Rd >= 0 ->
    (base(plumtree_gossip, 0))#{
        realm   => R,
        msg_id  => M,
        round   => Rd,
        payload => Payload
    }.

-spec plumtree_ihave(plumtree_ihave_spec()) -> frame().
plumtree_ihave(#{realm := R, msg_id := M, round := Rd})
  when is_binary(R), byte_size(R) =:= 32,
       is_binary(M), byte_size(M) =:= 16,
       is_integer(Rd), Rd >= 0 ->
    (base(plumtree_ihave, 0))#{realm => R, msg_id => M, round => Rd}.

-spec plumtree_graft(plumtree_graft_spec()) -> frame().
plumtree_graft(#{realm := R, msg_id := M, round := Rd})
  when is_binary(R), byte_size(R) =:= 32,
       is_binary(M), byte_size(M) =:= 16,
       is_integer(Rd), Rd >= 0 ->
    (base(plumtree_graft, 0))#{realm => R, msg_id => M, round => Rd}.

-spec plumtree_prune(plumtree_prune_spec()) -> frame().
plumtree_prune(#{realm := R})
  when is_binary(R), byte_size(R) =:= 32 ->
    (base(plumtree_prune, 0))#{realm => R}.

%%------------------------------------------------------------------
%% PubSub constructors (Part 6 §6)
%%------------------------------------------------------------------

-spec publish(publish_spec()) -> frame().
publish(#{topic := T, realm := R, publisher := Pub, seq := Seq,
          payload := Payload, published_at_ms := PubAt} = Spec)
  when is_binary(T),
       is_binary(R),   byte_size(R)   =:= 32,
       is_binary(Pub), byte_size(Pub) =:= 32,
       is_integer(Seq),   Seq   >= 0,
       is_integer(PubAt), PubAt >= 0 ->
    Ttl = maps:get(ttl_ms, Spec, undefined),
    validate_optional_ttl(Ttl),
    (base(publish, 0))#{
        topic           => T,
        realm           => R,
        publisher       => Pub,
        seq             => Seq,
        payload         => Payload,
        published_at_ms => PubAt,
        ttl_ms          => Ttl
    }.

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

-spec event(event_spec()) -> frame().
event(#{topic := T, realm := R, publisher := Pub, seq := Seq,
        payload := Payload, delivered_via := Via})
  when is_binary(T),
       is_binary(R),   byte_size(R)   =:= 32,
       is_binary(Pub), byte_size(Pub) =:= 32,
       is_integer(Seq), Seq >= 0,
       (Via =:= plumtree orelse Via =:= dht orelse Via =:= direct) ->
    (base(event, 0))#{
        topic         => T,
        realm         => R,
        publisher     => Pub,
        seq           => Seq,
        payload       => Payload,
        delivered_via => Via
    }.

-spec validate_optional_ttl(non_neg_integer() | undefined) -> ok.
validate_optional_ttl(undefined)                             -> ok;
validate_optional_ttl(N) when is_integer(N), N >= 0          -> ok.

-spec validate_options(map()) -> ok.
validate_options(M) when is_map(M) -> ok.

%%------------------------------------------------------------------
%% RPC advertise constructors (Part 6 §5.5)
%%------------------------------------------------------------------

-spec advertise(advertise_spec()) -> frame().
advertise(#{realm := R, procedure := Proc, advertiser := Adv} = Spec)
  when is_binary(R),    byte_size(R)   =:= 32,
       is_binary(Proc),
       is_binary(Adv),  byte_size(Adv) =:= 32 ->
    Options = maps:get(options, Spec, #{}),
    validate_options(Options),
    (base(advertise, 0))#{
        realm      => R,
        procedure  => Proc,
        advertiser => Adv,
        options    => Options
    }.

-spec unadvertise(unadvertise_spec()) -> frame().
unadvertise(#{realm := R, procedure := Proc, advertiser := Adv})
  when is_binary(R),    byte_size(R)   =:= 32,
       is_binary(Proc),
       is_binary(Adv),  byte_size(Adv) =:= 32 ->
    (base(unadvertise, 0))#{
        realm      => R,
        procedure  => Proc,
        advertiser => Adv
    }.

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
validate_mcid(<<_:272>>) -> ok.

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

-spec validate_manifest_payload(map() | not_found) -> ok.
validate_manifest_payload(not_found)              -> ok;
validate_manifest_payload(M) when is_map(M)       -> ok.

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
%% Atom-keyed maps round-trip via two helpers:
%%   to_wire/1   — atoms → `{text, atom_to_binary(A)}', recursing into
%%                 nested maps and lists.
%%   from_wire/1 — `{text, Bin}' values whose binary is a known atom
%%                 spelling become atoms again; binary keys whose name
%%                 matches an existing atom are restored.
%%
%% `binary_to_existing_atom' is safe: the codec never creates new atoms
%% from untrusted wire input, so a malicious peer cannot exhaust the
%% atom table.

-spec encode(frame()) -> binary().
encode(Frame) when is_map(Frame) ->
    Bytes = macula_record_cbor:encode(to_wire(prepare_records(Frame))),
    Len = byte_size(Bytes),
    encode_with_check(Len, Bytes).

%% Records (`record', `records' fields) are delegated to
%% `macula_record:encode/1' so the SDK's canonical CBOR shape is
%% preserved verbatim. The frame map carries the resulting opaque
%% binary blob; on decode `restore_records/1' inflates it back to a
%% record map. This keeps the macula_record payload's `{text, Bin}'
%% keys from being mistaken for frame-envelope atoms.
prepare_records(F = #{record := R}) when is_map(R) ->
    F#{record := macula_record:encode(R)};
prepare_records(F = #{records := L}) when is_list(L) ->
    F#{records := [macula_record:encode(R) || R <- L, is_map(R)]};
prepare_records(F) -> F.

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
    decode_cbor(Bytes, Rest);
decode(<<Len:32/big, Tail/binary>>) ->
    {more, Len - byte_size(Tail)};
decode(Buf) when is_binary(Buf), byte_size(Buf) < 4 ->
    {more, 4 - byte_size(Buf)}.

decode_cbor(Bytes, Rest) ->
    try macula_record_cbor:decode(Bytes) of
        Term when is_map(Term) ->
            Frame = from_wire_envelope(Term),
            {ok, restore_records(Frame), Rest};
        _Other ->
            {error, bad_frame}
    catch
        _:_ -> {error, bad_frame}
    end.

%% Inverse of `prepare_records/1' — opaque binary blobs in `record' /
%% `records' fields are decoded via `macula_record:decode/1' so the
%% frame map exposes record values in their natural map shape.
restore_records(F = #{record := B}) when is_binary(B) ->
    case macula_record:decode(B) of
        {ok, R} -> F#{record := R};
        _       -> F
    end;
restore_records(F = #{records := L}) when is_list(L) ->
    Decoded = [decode_record_or_keep(E) || E <- L],
    F#{records := Decoded};
restore_records(F) -> F.

decode_record_or_keep(B) when is_binary(B) ->
    case macula_record:decode(B) of
        {ok, R} -> R;
        _       -> B
    end;
decode_record_or_keep(Other) -> Other.

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
    macula_record_cbor:encode(to_wire(prepare_records(Unsigned))).

%%------------------------------------------------------------------
%% Atom <-> wire-binary translation
%%
%% The CBOR codec ships text strings as `{text, Bin}' tuples and byte
%% strings as plain binaries (per `macula_record_cbor'). Atoms in the
%% in-process frame map are converted to `{text, atom_to_binary(A)}'
%% before encoding, and reconstructed via `binary_to_existing_atom'
%% on the decode path. Binaries (signatures, node ids, payloads,
%% nonces) stay as binaries on the wire.
%%------------------------------------------------------------------

%% @private Convert a frame map (atom keys, atom values where used)
%% into the shape `macula_record_cbor:encode/1' understands. Booleans
%% (`true' / `false') are atoms in Erlang and round-trip the same way
%% as any other atom — encoded as text strings, decoded via
%% `binary_to_existing_atom'. Floats are stringified compactly so the
%% canonical-byte derivation is independent of platform float encoding.
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
to_wire(I) when is_integer(I), I >= 0 -> I;
to_wire(F) when is_float(F) ->
    {text, float_to_binary(F, [{decimals, 6}, compact])};
to_wire(Other) -> Other.

wire_key(A) when is_atom(A)   -> {text, atom_to_binary(A, utf8)};
wire_key({text, B})           -> {text, B};
wire_key(B) when is_binary(B) -> {text, B}.

%% @private Walk the decoded CBOR term and restore atom keys/values
%% via `binary_to_existing_atom'. Records (`record' / `records'
%% fields) are pre-encoded as opaque CBOR binaries by `prepare_records'
%% on the encode path and re-decoded by `restore_records' after this
%% walk; their internal `{text, Bin}' payload keys never reach this
%% function, so unconditional atom restoration is safe.
%%
%% `binary_to_existing_atom' raises `badarg' on names that are not
%% already in the runtime atom table — that is the safety guarantee
%% against atom-table exhaustion. Names hecate-station never declared
%% (e.g. a peer-supplied custom field) come back as `{text, Bin}'
%% (text string) or plain binary (byte string).
from_wire_envelope(M) when is_map(M) ->
    maps:fold(fun(K, V, Acc) ->
                  Acc#{envelope_key(K) => from_wire_envelope(V)}
              end, #{}, M);
from_wire_envelope(L) when is_list(L) ->
    [from_wire_envelope(E) || E <- L];
from_wire_envelope(null)  -> undefined;
from_wire_envelope({text, B}) when is_binary(B) -> maybe_atom(B, {text, B});
from_wire_envelope(B) when is_binary(B) -> B;
from_wire_envelope(I) when is_integer(I) -> I;
from_wire_envelope(Other) -> Other.

envelope_key({text, B}) when is_binary(B) -> maybe_atom(B, {text, B});
envelope_key(B) when is_binary(B)         -> maybe_atom(B, B);
envelope_key(K)                            -> K.

maybe_atom(B, Default) ->
    try binary_to_existing_atom(B, utf8)
    catch error:badarg -> Default
    end.
