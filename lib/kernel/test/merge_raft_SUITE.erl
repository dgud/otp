%%% % @format

-module(merge_raft_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([
    suite/0,
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
         basic/1,
         kv/1
]).

-define(WAIT_UNTIL(Condition, TimeLimitMs),
    (fun() ->
        ___EndTime = erlang:system_time(millisecond) + TimeLimitMs,
        (fun ___RecFn() ->
            try
                Condition
            catch
                ___Type:___Err:___Stack ->
                    case erlang:system_time(millisecond) >= ___EndTime of
                        true ->
                            erlang:raise(___Type, ___Err, ___Stack);
                        _ ->
                            timer:sleep(20),
                            ___RecFn()
                    end
            end
        end)()
    end)()
).

-define(WAIT_UNTIL(Condition), ?WAIT_UNTIL(Condition, 5000)).

suite() ->
    [
        {timetrap, {seconds, 100}},
        {auto_meckanic, #{enable_autoclean => true}},
        {appatic, #{enable_autoclean => true}}
    ].

init_per_suite(Config) ->
    net_kernel:start([list_to_atom(?CT_PEER_NAME(?MODULE))]),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(TestCase, Config) ->
    NeedPeers = [kv],
    case lists:member(TestCase, NeedPeers) of
        true ->
            Self = self(),
            [
             spawn_link(
               fun() ->
                       Opts = #{
                                name => atom_to_list(TestCase) ++ "_" ++ integer_to_list(NodeNR),
                                connection => 0,
                                args => ["-connect_all", "false", "-kernel", "+S", "4:4"]
                               },
                       {ok, Peer, Node} = ?CT_PEER(Opts),
                       ok = peer:call(Peer, code, add_pathsa, [code:get_path()]),
                       Self ! {self(), Peer, Node},
                       timer:sleep(infinity)
               end
              )
             || NodeNR <- lists:seq(1, 5)
            ],
            Pids = [
                    receive
                        {Pid, Peer, Node} -> {Pid, Peer, Node}
                    end
                    || _ <- lists:seq(1, 5)
                   ],
            Peers = #{Peer => Node || {_Pid, Peer, Node} <- Pids},
            [{peers, Peers} | Config];
        false ->
            Config
    end.

end_per_testcase(_TestCase, Config) ->
    dbg:stop(),
    catch [peer:stop(Peer) || Peer := _ <- proplists:get_value(peers, Config)],
    ok.

all() ->
    [{group, basic}].

groups() ->
    [
        {basic, [], %% [parallel],
         [basic, kv]
        }
    ].

%%--------------------------------------------------------------------
%% TEST CASES

kv(Config) ->
    Peers = proplists:get_value(peers, Config),
    Mons =
        [
            monitor(process, Pid)
         || Peer := _ <- Peers,
            {ok, Pid} <- [peer:call(Peer, merge_raft_kv, start, [?FUNCTION_NAME])]
        ],
    [P1, P2, P3, P4, P5] = maps:keys(Peers),
    #{P1 := N1, P2 := N2, P3 := N3, P4 := N4, P5 := _N5} = Peers,
    {ok, ok} = peer:call(P1, merge_raft_kv, sync_put, [?FUNCTION_NAME, a, 1]),
    {ok, ok} = peer:call(P2, merge_raft_kv, sync_put, [?FUNCTION_NAME, b, 2]),
    true = peer:call(P2, net_kernel, connect_node, [N1]),
    true = peer:call(P4, net_kernel, connect_node, [N3]),
    true = peer:call(P3, net_kernel, connect_node, [N2]),
    true = peer:call(P5, net_kernel, connect_node, [N4]),

    timer:sleep(3000),

    {ok, 1} = peer:call(P1, merge_raft_kv, sync_get, [?FUNCTION_NAME, a]),
    {ok, 2} = peer:call(P2, merge_raft_kv, sync_get, [?FUNCTION_NAME, b]),
    io:format("~w: a ~w~n",[?LINE, [{Node, erpc:call(Node, merge_raft_kv, sync_get, [?FUNCTION_NAME, a])}
                                    || _ := Node <- Peers]]),
    io:format("~w: a ~w~n",[?LINE, [{Node, erpc:call(Node, merge_raft_kv, sync_get, [?FUNCTION_NAME, b])}
                                    || _ := Node <- Peers]]),

    ?WAIT_UNTIL({ok, 1} = peer:call(P5, merge_raft_kv, async_get, [?FUNCTION_NAME, a])),
    ?WAIT_UNTIL({ok, 2} = peer:call(P5, merge_raft_kv, async_get, [?FUNCTION_NAME, b])),
    {ok, 1} = peer:call(P5, merge_raft_kv, sync_get, [?FUNCTION_NAME, a]),
    {ok, 2} = peer:call(P5, merge_raft_kv, sync_get, [?FUNCTION_NAME, b]),
    [
        receive
            {'DOWN', Mon, process, Pid, Reason} ->
                error({Pid, Reason})
        after 0 ->
            ok
        end
     || Mon <- Mons
    ],
    ok.

basic(_Config) ->
    Pids = [Pid || _ <- lists:seq(1,5), {ok, Pid} <- [mr_cb_test:start()]],
    Mons = [monitor(process, Pid) || Pid <- Pids],
    [Pid1, Pid2, Pid3, Pid4, Pid5] = Pids,
    io:format("Network Pids: ~w~n", [Pids]),
    %% mr_cb_test:trace(#{ps => [Pid1,Pid2, Pid3], fs => all}),
    timer:sleep(200),

    {ok, ok} = mr_cb_test:put(Pid1, a, 1),
    {ok, ok} = mr_cb_test:put(Pid2, b, 2),

    [Pid2] = lists:sort(mr_cb_test:connect(Pid2, [Pid1])),
    [Pid4] = lists:sort(mr_cb_test:connect(Pid4, [Pid3])),
    [Pid3] = lists:sort(mr_cb_test:connect(Pid3, [Pid2])),
    [Pid5] = lists:sort(mr_cb_test:connect(Pid5, [Pid4])),

    timer:sleep(500),
    [ct:log("~tw~n", [merge_raft:get_info(Pid)]) || Pid <- Pids],
    timer:sleep(3000),
    [ct:log("~tw~n", [merge_raft:get_info(Pid)]) || Pid <- Pids],

    Verify = fun(Pid) ->
                     maybe
                         {ok, 1} ?= mr_cb_test:leader_get(Pid, a),
                         {ok, 1} ?= mr_cb_test:get(Pid, a),
                         {ok, 2} ?= mr_cb_test:leader_get(Pid, b),
                         {ok, 2} ?= mr_cb_test:get(Pid, b),
                         ct:log("Checked pid ~w~n",[Pid]),
                         false
                     else Reason ->
                             {true, {Pid, Reason}}
                     end
             end,

    [] = lists:filtermap(Verify, Pids),

    [
     receive
         {'DOWN', Mon, process, Pid, Reason} ->
             error({Pid, Reason})
     after 0 ->
             ok
     end
     || Mon <- Mons
    ],
    ok.
