%% EUnit test for record signing custody. Within the macula application, a record a node signs about itself is signed
%% only in its pool's process, with the node identity key the pool holds: macula_record:sign/2 is called only by
%% macula_record:refresh/2 and the pool's tombstone signing, and refresh/2 only by the pool's node record signing. The
%% calls are read with xref from the application's compiled modules, so a new caller fails this test until it signs
%% through the pool or is named here with its reason.
-module(macula_record_signing_custody_tests).

-include_lib("eunit/include/eunit.hrl").

%% macula_direct_dial:publish_advertisement/5 signs an advertisement, in on_links/6, with a key its caller passes. It
%% is named here until P2 moves that signing into the pool.
-define(UNTIL_P2, [{macula_direct_dial, on_links, 6}]).

only_the_pool_signs_records_test_() ->
    {timeout, 60, fun() ->
        ?assertEqual(#{{macula_record, sign, 2} => lists:sort([{macula_client, withdrawable, 5},
                                                              {macula_record, refresh, 2} | ?UNTIL_P2]),
                       {macula_record, refresh, 2} => [{macula_client, node_record_signed, 3}]},
                     callers([{macula_record, sign, 2}, {macula_record, refresh, 2}]))
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

loaded(ok) -> ok;
loaded({error, {already_loaded, macula}}) -> ok.

application_callers({ok, Callers}, Modules) ->
    lists:sort([Caller || {Module, _Name, _Arity} = Caller <- Callers, lists:member(Module, Modules)]).
