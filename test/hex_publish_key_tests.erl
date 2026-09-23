%%% scripts/is_hex_publish_key_live.sh against a fake hex API on localhost.
%%%
%%% The preflight asked `/users/me', which answers 404 for a key with no user
%%% behind it, an organisation key. mcl_om publishes with one, so the check
%%% refused a key that had published three versions the same day, and mcl_om
%%% 0.26.4 did not go out. The same script is in every publishing library;
%%% this test is identical in each. The question publishing needs answered is whether the key
%%% may WRITE, which hex answers at `/auth?domain=api&resource=write': 204 yes,
%%% 401 not a key it accepts, 403 a key without write permission.
-module(hex_publish_key_tests).
-include_lib("eunit/include/eunit.hrl").

-define(KEY, "not-a-real-key-0123456789abcdef").

asks_whether_the_key_may_write_test() ->
    {Status, _Out, Path} = run_against(204),
    ?assertEqual(0, Status),
    ?assertEqual("/api/auth?domain=api&resource=write", Path).

a_key_that_may_write_is_accepted_test_() ->
    [fun() ->
         {Status, Out, _Path} = run_against(Code),
         ?assertEqual(0, Status),
         ?assertEqual(nomatch, string:find(Out, ?KEY))
     end || Code <- [204, 200]].

a_key_hex_does_not_accept_is_refused_test() ->
    ?assertMatch({1, _, _}, run_against(401)).

a_key_without_write_permission_is_refused_test() ->
    ?assertMatch({1, _, _}, run_against(403)).

anything_else_is_refused_test_() ->
    [?_assertMatch({1, _, _}, run_against(Code)) || Code <- [404, 500]].

an_empty_key_is_refused_before_asking_test() ->
    Script = script(),
    Port = open_port({spawn_executable, "/usr/bin/env"},
                     [{args, ["bash", Script]}, {env, [{"HEX_API_KEY", ""}]},
                      exit_status, stderr_to_stdout, binary]),
    ?assertMatch({1, _}, collect(Port, <<>>)).

%%% A one-shot HTTP server answering `Code', reporting the request path.

run_against(Code) ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true},
                                      {ip, {127, 0, 0, 1}}]),
    {ok, PortNo} = inet:port(Listen),
    Self = self(),
    _ = spawn_link(fun() -> serve_once(Listen, Code, Self) end),
    Url = "http://127.0.0.1:" ++ integer_to_list(PortNo) ++ "/api",
    Port = open_port({spawn_executable, "/usr/bin/env"},
                     [{args, ["bash", script()]},
                      {env, [{"HEX_API_KEY", ?KEY}, {"HEX_API_URL", Url}]},
                      exit_status, stderr_to_stdout, binary]),
    {Status, Out} = collect(Port, <<>>),
    Path = receive {path, P} -> P after 5000 -> none end,
    gen_tcp:close(Listen),
    {Status, binary_to_list(Out), Path}.

serve_once(Listen, Code, Parent) ->
    {ok, Sock} = gen_tcp:accept(Listen, 10000),
    {ok, Req} = gen_tcp:recv(Sock, 0, 5000),
    [RequestLine | _] = binary:split(Req, <<"\r\n">>),
    [_Method, Path | _] = binary:split(RequestLine, <<" ">>, [global]),
    Parent ! {path, binary_to_list(Path)},
    ok = gen_tcp:send(Sock, [<<"HTTP/1.1 ">>, integer_to_binary(Code),
                             <<" X\r\ncontent-length: 0\r\nconnection: close\r\n\r\n">>]),
    gen_tcp:close(Sock).

collect(Port, Acc) ->
    receive
        {Port, {data, Bin}}      -> collect(Port, <<Acc/binary, Bin/binary>>);
        {Port, {exit_status, N}} -> {N, Acc}
    after 30000 ->
        error({timeout, Acc})
    end.

%% The repository's scripts/, found by walking up from this file, so the test
%% is the same whether it lives in test/ or test/unit/, and whether rebar3
%% compiles it in place or from a copy under _build/.
script() ->
    find_script(filename:dirname(?FILE)).

find_script("/") ->
    error(no_scripts_is_hex_publish_key_live_sh_above_this_test);
find_script(Dir) ->
    Candidate = filename:join([Dir, "scripts", "is_hex_publish_key_live.sh"]),
    case filelib:is_regular(Candidate) of
        true -> Candidate;
        false -> find_script(filename:dirname(Dir))
    end.
