%% EUnit tests for the facade's node-served content (D27): share_content/3,4 keeps and serves content through one
%% sharer per pool, unshare_content/3 stops, get_content/3,4 fetches from a sharer. The pool here has no station, so
%% nothing is announced or found; what is under test is the facade's wiring.
-module(macula_content_facade_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<7:256>>).

facade_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(macula),
              {ok, Pool} = macula:connect([], #{}), Pool end,
     fun(Pool) -> macula:close(Pool) end,
     fun(Pool) ->
         [{"sharing on a pool keeps the content in its one sharer",
           fun() ->
               {ok, MCID} = macula:share_content(Pool, ?REALM, <<"hello">>),
               {ok, Sharer} = macula_content_sharer_sup:sharer(Pool),
               ?assertEqual({ok, Sharer}, macula_content_sharer_sup:sharer(Pool)),
               ?assertMatch({ok, #{kind := block}}, macula_content_sharer:lookup(Sharer, ?REALM, root, MCID)),
               ok = macula:unshare_content(Pool, ?REALM, MCID),
               ?assertEqual(not_found, macula_content_sharer:lookup(Sharer, ?REALM, root, MCID))
           end},
          {"content nobody announces is not_shared",
           fun() -> ?assertEqual({error, not_shared}, macula:get_content(Pool, ?REALM, <<2, 16#55, 0:384>>)) end},
          {"a content id that is not tag 2 is refused",
           fun() -> ?assertEqual({error, invalid_mcid}, macula:get_content(Pool, ?REALM, <<1, 2, 3>>)) end}]
     end}.
