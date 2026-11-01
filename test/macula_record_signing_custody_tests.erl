%% EUnit test for record signing custody. Within the macula application, a record a node signs about itself is signed
%% only in its pool's process, with the node identity key the pool holds: macula_record:sign/2 is called only by
%% macula_record:refreshed/4, which both refresh/2 and refresh/3 stamp through, and the pool's tombstone signing; the
%% two refresh doors are called only by the pool's record signing. The calls are read with xref from the application's
%% compiled modules, so a new caller fails this test until it signs through the pool or is named here with its reason.
%% One level down, only macula_record, macula_frame and the audit chain's checkpoint signer call macula_signed_object's
%% signing functions.
-module(macula_record_signing_custody_tests).

-include_lib("eunit/include/eunit.hrl").

only_the_pool_signs_records_test_() ->
    {timeout, 60, fun() ->
        ?assertEqual(#{{macula_record, sign, 2} => lists:sort([{macula_client, withdrawable, 5},
                                                              {macula_record, refreshed, 4}]),
                       {macula_record, refresh, 2} => [{macula_client, domain_record_signed, 3},
                                                       {macula_client, node_record_signed, 3}],
                       {macula_record, refresh, 3} => [{macula_client, node_record_signed_bounded, 4}]},
                     callers([{macula_record, sign, 2},
                              {macula_record, refresh, 2},
                              {macula_record, refresh, 3}]))
    end}.

%% One level down, a module could sign under the record label by calling macula_signed_object directly, past
%% macula_record:sign/2. Only macula_record, which signs records, macula_frame, which signs frames, and
%% macula_audit_checkpoint, which signs an audit chain's checkpoints (macula#175 WP1), call its signing functions, and
%% only macula_frame signs a held object.
%%
%% The checkpoint signer is named here rather than routed through macula_record because a checkpoint is not a record:
%% it is never stored or replicated, it carries no payload, and it says nothing about the node's own state — it is a
%% statement about a chain, signed under MACULA-AUDIT-CHECKPOINT-V1, which is what keeps a checkpoint signature from
%% ever being read as a record or a frame.
only_the_three_signing_doors_call_the_object_signers_test_() ->
    {timeout, 60, fun() ->
        ?assertEqual(#{{macula_signed_object, sign, 3} => [macula_audit_checkpoint, macula_frame, macula_record],
                       {macula_signed_object, sign_held, 3} => [macula_frame]},
                     calling_modules([{macula_signed_object, sign, 3}, {macula_signed_object, sign_held, 3}]))
    end}.

%% The functions of the macula application's own modules that call each of Functions, read with xref from the
%% application's beams, sorted.
callers(Functions) ->
    ok = loaded(application:load(macula)),
    {ok, Modules} = application:get_key(macula, modules),
    {ok, Xref} = xref:start([{xref_mode, functions}]),
    try
        ok = xref:set_default(Xref, [{warnings, false}, {verbose, false}]),
        {ok, _Added} = xref:add_directory(Xref, filename:join(code:lib_dir(macula), "ebin")),
        maps:from_list([{Function, application_callers(xref:analyze(Xref, {use, Function}), Modules)}
                        || Function <- Functions])
    after
        xref:stop(Xref)
    end.

%% The macula application's modules, other than the function's own, that call each of Functions. Within
%% macula_signed_object, sign_held/3 signs through sign/3, which is no way in from outside it.
calling_modules(Functions) ->
    maps:map(fun({Own, _Name, _Arity}, Callers) ->
                     lists:usort([Module || {Module, _Caller, _CallerArity} <- Callers, Module =/= Own])
             end,
             callers(Functions)).

loaded(ok) -> ok;
loaded({error, {already_loaded, macula}}) -> ok.

application_callers({ok, Callers}, Modules) ->
    lists:sort([Caller || {Module, _Name, _Arity} = Caller <- Callers, lists:member(Module, Modules)]).
