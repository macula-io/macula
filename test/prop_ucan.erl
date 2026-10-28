%% Property tests for UCAN tokens (macula#151): a token a key mints authorizes exactly its own audience, until its own
%% exp and no longer, and a key's did:key carries that key and nothing else. The claims are generated; the keys are
%% made once for the run.
-module(prop_ucan).

-include_lib("proper/include/proper.hrl").

%%------------------------------------------------------------------
%% Properties
%%------------------------------------------------------------------

%% What a key minted comes back out of the token: the audience, the expiry and the capabilities it was given, verified
%% under the issuer's own node_id.
prop_a_minted_token_authorizes_its_own_grant() ->
    ?FORALL({Audience, With, Can, Lifetime}, {id(), grant_target(), can(), choose(1, 3600)},
            begin
                {Token, Exp, Now} = minted(Audience, With, Can, Lifetime),
                case authorize(Token, Audience, Now) of
                    {ok, Claims} ->
                        maps:get(<<"exp">>, Claims) =:= Exp
                            andalso maps:get(<<"aud">>, Claims) =:= binary:encode_hex(Audience, lowercase)
                            andalso maps:get(<<"cap">>, Claims) =:= [#{<<"with">> => With, <<"can">> => Can}];
                    {error, _Refusal} ->
                        false
                end
            end).

%% A token is for its audience alone: the same token, offered by a caller it does not name, is refused, and the refusal
%% says so rather than being some other failure.
prop_a_token_is_not_for_another_audience() ->
    ?FORALL({Audience, With, Can}, {id(), grant_target(), can()},
            begin
                {Token, _Exp, Now} = minted(Audience, With, Can, 600),
                <<First, Rest/binary>> = Audience,
                authorize(Token, <<(First bxor 1), Rest/binary>>, Now) =:= {error, not_the_audience}
            end).

%% A token expires when it says it does: at its own exp it is refused as expired, whatever its caps.
prop_a_token_is_refused_at_its_expiry() ->
    ?FORALL({Audience, With, Can, Late}, {id(), grant_target(), can(), choose(0, 1_000_000)},
            begin
                {Token, Exp, _Now} = minted(Audience, With, Can, 60),
                authorize(Token, Audience, Exp + Late) =:= {error, expired}
            end).

%% A key's did:key decodes back to that key, for every purpose a node holds a key for.
prop_a_did_key_carries_its_key() ->
    ?FORALL(Purpose, elements([identity, realm, org]),
            begin
                Profile = wire_prop_support:profile(),
                Carried = macula_node_keys:public_key(wire_prop_support:key(Purpose)),
                DidKey = macula_ucan:did_key(Carried, Profile),
                macula_ucan:carried_key(DidKey, Profile) =:= {ok, Carried}
            end).

%%------------------------------------------------------------------
%% Generators
%%------------------------------------------------------------------

id() ->
    ?LET(B, binary(32), B).

%% A capability's `with': what a grant names, in the shape the mesh writes it.
grant_target() ->
    ?LET({Kind, Name}, {grant_kind(), name()},
         <<"mri:", Kind/binary, ":io.macula/", Name/binary>>).

%% A choice among fixed strings is drawn as atoms and written out: PropEr's type system has no literal binary, and a
%% binary wrapped in a generator of its own breaks shrinking, which is how a failing property loses its counterexample.
grant_kind() ->
    ?LET(Atom, oneof([topic, proc]), atom_to_binary(Atom, utf8)).

name() ->
    ?LET(Segments, ?LET(N, choose(1, 3), vector(N, segment())), iolist_to_binary(lists:join(<<"/">>, Segments))).

segment() ->
    ?LET(Chars, ?LET(N, choose(1, 8), vector(N, choose($a, $z))), list_to_binary(Chars)).

can() ->
    ?LET(Atom, oneof([call, publish, subscribe, serve]), atom_to_binary(Atom, utf8)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

minted(Audience, With, Can, Lifetime) ->
    Now = erlang:system_time(second),
    Exp = Now + Lifetime,
    Token = macula_ucan:create(wire_prop_support:key(identity), Audience,
                               [#{with => With, can => Can}], #{exp => Exp}),
    {Token, Exp, Now}.

authorize(Token, Caller, Now) ->
    macula_ucan:authorize(Token, {ucan_required, wire_prop_support:node_id(identity)},
                          #{caller => Caller, profile => wire_prop_support:profile(), now => Now}).
