%% Property tests for signed records (macula#151): a record a key signs is read back off the wire as the same record,
%% its bytes are what a verifier holds it to, and a record that changed by one bit — or names a key that did not sign
%% it — is refused whatever its fields were. The fields are generated; the key is made once for the run.
-module(prop_record).

-include_lib("proper/include/proper.hrl").

%%------------------------------------------------------------------
%% Properties
%%------------------------------------------------------------------

%% A signed record survives its own wire form: what the verifier reads back encodes to the bytes it verified. This is
%% the whole contract — the signature covers the tbs, the tbs is the record, and the key travels with it.
prop_a_signed_record_survives_its_wire_form() ->
    ?FORALL({Realms, Capabilities}, {realms(), capabilities()},
            begin
                Profile = wire_prop_support:profile(),
                Record = node_record(Realms, Capabilities),
                Signed = macula_record:sign(Record, wire_prop_support:key(identity)),
                Bytes = macula_record:encode(Signed),
                case macula_record:verify(Bytes, Profile) of
                    {ok, Verified} ->
                        macula_record:encode(Verified) =:= Bytes
                            andalso macula_record:type(Verified) =:= macula_record:type(Record);
                    {error, _Refusal} ->
                        false
                end
            end).

%% One bit of the signature changed and the record is not the record: the verifier refuses it, and the refusal it names
%% is the signature's. The fields and the key are the record's own, so a refusal for any other reason would mean the
%% record never reached the check this property is about.
prop_a_record_with_a_changed_signature_is_refused() ->
    ?FORALL({Realms, Capabilities, Where, Bit}, {realms(), capabilities(), where(), choose(0, 7)},
            begin
                Profile = wire_prop_support:profile(),
                Signed = macula_record:sign(node_record(Realms, Capabilities), wire_prop_support:key(identity)),
                Signature = maps:get(signature, Signed),
                Tampered = #{key => maps:get(key, Signed), tbs => maps:get(tbs, Signed),
                             signature => flipped(Signature, Where, Bit)},
                macula_record:verify(Tampered, Profile) =:= {error, signature_invalid}
            end).

%% A record naming a signer other than the key that signs it is refused before it is signed, whatever else it holds:
%% the signer a record names is a binding, not a claim.
prop_a_record_naming_another_signer_cannot_be_signed() ->
    ?FORALL({Realms, Capabilities}, {realms(), capabilities()},
            sign_refused(macula_record:node_record(<<3:256>>, Realms, Capabilities),
                         wire_prop_support:key(identity))).

%%------------------------------------------------------------------
%% Generators
%%------------------------------------------------------------------

node_record(Realms, Capabilities) ->
    macula_record:node_record(wire_prop_support:node_id(identity), Realms, Capabilities).

%% The realms a node record names: 32-byte ids, as its payload takes them.
realms() ->
    ?LET(N, choose(0, 3), ?LET(Ids, vector(N, id()), lists:usort(Ids))).

id() ->
    ?LET(B, binary(32), B).

capabilities() ->
    oneof([0, 1, 255, 256, 65536, choose(0, 16#FFFFFFFF)]).

%% Which byte of the signature is turned over: its first, its last, or one in the middle.
where() ->
    oneof([first, last, middle]).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

sign_refused(Record, Key) ->
    try macula_record:sign(Record, Key) of
        _Signed -> false
    catch
        error:{key_id_mismatch, _} -> true
    end.

flipped(Signature, Where, Bit) ->
    Last = byte_size(Signature) - 1,
    Position = case Where of
                   first -> 0;
                   last -> Last;
                   middle -> Last div 2
               end,
    <<Before:Position/binary, Octet, After/binary>> = Signature,
    <<Before/binary, (Octet bxor (1 bsl Bit)), After/binary>>.
