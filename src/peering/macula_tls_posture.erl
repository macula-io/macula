%% @doc The TLS posture handshake v5 depends on, checked before peering starts
%% (docs/design/DESIGN_NEIGHBOUR_CHANNEL_BINDING.md sections 3 and 6). v5 lets QUIC's AEAD authenticate every frame after
%% the session proofs, which holds only if the session's keys come from a hybrid ML-KEM exchange and no frame travels
%% in replayable 0-RTT. So both ends offer exactly SecP384r1MLKEM1024 then SecP256r1MLKEM768, neither offers nor
%% accepts early data or sends tickets, a second handshake between the same configurations is a full one, and the
%% dialler's own setting holds too: against a listener that does issue tickets, its second handshake is also full.
%%
%% This proves the posture the configurations have, as macula_quic:tls_posture/0 reads it from the NIF, not the group
%% any one connection negotiated; since only hybrid groups are offered, no handshake can negotiate another.
-module(macula_tls_posture).

-export([ensure/0, check/1]).

%% IANA TLS Supported Groups: SecP384r1MLKEM1024, then SecP256r1MLKEM768.
-define(GROUPS, [16#11ED, 16#11EB]).

-type posture() :: #{dialler_second_handshake := full | resumed, client_groups := [non_neg_integer()], server_groups := [non_neg_integer()],
                     client_early_data := 0 | 1, server_max_early_data := non_neg_integer(),
                     server_tickets := non_neg_integer(), second_handshake := full | resumed}.

%% @doc Check this build's posture, and raise naming the first departure. Called where peering starts, so a node
%% with another posture does not run.
-spec ensure() -> ok.
ensure() ->
    ensured(read(macula_quic:tls_posture())).

read({ok, Posture}) -> check(Posture);
read({error, Reason}) -> {error, {tls_posture, unreadable, Reason}}.

ensured(ok) ->
    ok;
ensured({error, {tls_posture, Field, Got}} = Refusal) ->
    logger:error("[macula_tls_posture] refusing to start peering: TLS ~p is ~0p, which handshake v5 cannot rely on",
                 [Field, Got]),
    error(Refusal).

%% @doc The first field of Posture that departs from the one handshake v5 depends on, or ok.
-spec check(posture()) -> ok | {error, {tls_posture, atom(), term()}}.
check(Posture) ->
    first_departure([{client_groups, ?GROUPS}, {server_groups, ?GROUPS}, {client_early_data, 0},
                     {server_max_early_data, 0}, {server_tickets, 0}, {second_handshake, full},
                     {dialler_second_handshake, full}], Posture).

first_departure([], _Posture) ->
    ok;
first_departure([{Field, Expected} | Rest], Posture) ->
    departed(maps:get(Field, Posture), Expected, Field, Rest, Posture).

departed(Expected, Expected, _Field, Rest, Posture) -> first_departure(Rest, Posture);
departed(Got, _Expected, Field, _Rest, _Posture) -> {error, {tls_posture, Field, Got}}.
