%%%-------------------------------------------------------------------
%%% @author Zandra Hird
%%% @copyright (C) 2016, Zandra Hird
%%% @doc
%%%
%%% @end
%%% Created : 2016-09-26 by Zandra Hird
%%%-------------------------------------------------------------------
-module(kademlia_test).

-include("kademlia.hrl").

-export([test/0, basic_join/0, many_joins/0, basic_nodes_leaving/0,
         many_nodes_leaving/0]).

test() ->
    io:format("Basic Joins: ~p~n", [basic_join()]),
    io:format("Many Joins: ~p~n", [many_joins()]),
    io:format("Basic Nodes Leaving: ~p~n", [basic_nodes_leaving()]),
    io:format("Many Nodes Leaving: ~p~n", [many_nodes_leaving()]).

basic_join() ->
    {ok, P1} = kademlia:start_link([]),
    check_some_lookups([P1]),
    ok = check_states([id(P1)]),
    {ok, P2} = kademlia:start_link([{gates, [P1]}]),
    check_some_lookups([P1, P2]),
    ok = check_states([id(P1), id(P2)]),
    {ok, P3} = kademlia:start_link([{gates, [P1]}]),
    {ok, P4} = kademlia:start_link([{gates, [P1]}]),
    {ok, P5} = kademlia:start_link([{gates, [P1]}]),
    {ok, P6} = kademlia:start_link([{gates, [P1]}]),
    All = [P1, P2, P3, P4, P5, P6],
    check_some_lookups(All),
    ok = check_states([id(P) || P <- All]),
%    [kademlia:print_state(P) || P <- All],
    kill(All),
    ok.

many_joins() ->
    All = start_nodes(500),
    check_some_lookups(All),
    ok = check_states([id(P) || P <- All]),
%    [kademlia:print_state(P) || P <- All],
    kill(All),
    ok.

basic_nodes_leaving() ->
    All = start_nodes(200),
    check_some_lookups(All),
    check_states([id(P) || P <- All]),
    {Leaving, Left} = lists:split(5, All),
    kill(Leaving),
    check_some_lookups(Left),
    check_states([id(P) || P <- Left]),
    kill(Left),
    ok.

many_nodes_leaving() ->
    All = start_nodes(500),
    check_some_lookups(All),
    {ok, Left0} = test_leaving(250, All),
    {ok, Left1} = test_leaving(200, Left0),
    {ok, Left2} = test_leaving(25, Left1),
    {ok, Left3} = test_leaving(15, Left2),
    {ok, Left4} = test_leaving(5, Left3),
    {ok, Left5} = test_leaving(3, Left4),
    kill(Left5),
    ok.

test_leaving(N, All) ->
    {Leaving, Left} = lists:split(N, All),
    kill(Leaving),
    check_some_lookups(Left),
    check_states([id(P) || P <- Left]),
    {ok, Left}.

kill(Leaving) ->
    [begin unlink(P), exit(P, die) end || P <- Leaving].

start_nodes(N) ->
    {ok, P1} = kademlia:start_link([]),
    [P1|[begin {ok, P} = kademlia:start_link([{gates, [P1]}]), P end || _ <- lists:seq(1, N-1)]].

id(P) ->
    #id{key = kademlia:key(P, 32), pid=P}.

check_some_lookups(All) ->
    timer:sleep(1000),
    [check(random_key(), All) || _ <- lists:seq(1, 100)].

check(Key, All) ->
    X = kademlia:find_node(id(random_member(All)), Key),
    [X = kademlia:find_node(id(random_member(All)), Key) || _ <- lists:seq(1, 100)].

random_key() ->
    rand:uniform(?KEY_SIZE(32)).

random_member(List) ->
    lists:nth(rand:uniform(length(List)), List).

check_states(Nodes) ->
    check_states(Nodes, Nodes).

check_states([], _Nodes) ->
    ok;
check_states([#id{key = Key, pid = Pid} = Node|Nodes], AllNodes) ->
    {state, #routing_table{self = Node,
                           closest_neighbours = Neighbours,
                           k_buckets = KBuckets,
                           neighbour_size = NSZ}, _} = sys:get_state(Pid),
    ExpectedKBuckets = get_expected_k_buckets(Key, AllNodes, maps:new()),
    ExpectedNeighbours = get_expected_neighbours(Key, AllNodes, NSZ),
    case check_k_buckets(ExpectedKBuckets, KBuckets) of
        true ->
            true = check_monitors(Pid, Neighbours, KBuckets, maps:keys(ExpectedKBuckets), ExpectedNeighbours),
            check_states(Nodes, AllNodes);
        false ->
            io:format("~p: KBuckets incorrect!~n   Expected: ~p~n   Acutal: ~p~n",
                       [Node, ExpectedKBuckets, KBuckets]),
            ok = retry_check_state(Node, ExpectedKBuckets, 3),%probably stabalizing
            check_states(Nodes, AllNodes)
    end.

get_expected_neighbours(Key, AllNodes, NSZ) ->
    tl(lists:sublist(routing_tables:sort(Key, AllNodes), NSZ + 1)).

retry_check_state(_Node, _, 0) ->
    fail;
retry_check_state(#id{pid = Pid} = Node, ExpectedKBuckets, N) ->
    {state, #routing_table{self = Node, k_buckets = KBuckets}, _} = sys:get_state(Pid),
    case check_k_buckets(ExpectedKBuckets, KBuckets) of
        true -> ok;
        false -> retry_check_state(Node, ExpectedKBuckets, N-1)
    end.

check_monitors(Pid, Neighbours, KBuckets, ExpectedKs, ExpectedNeighbours) ->
    Ms = get_monitors(Neighbours ++ maps:values(KBuckets), []),
    {monitors, Mons} = process_info(Pid, monitors),
    [true = lists:member(P, Ms) || {process, P} <- Mons],
    L = length(Ms),
    L = length(ExpectedKs ++ ExpectedNeighbours),
    true.

get_monitors([], Monitors) ->
    Monitors;
get_monitors([{{_, #id{pid=Pid}}, _}|KBuckets], Monitors) ->
    get_monitors(KBuckets, [Pid | Monitors]);
get_monitors([{_, #id{pid=Pid}}|KBuckets], Monitors) ->
    get_monitors(KBuckets, [Pid | Monitors]).

get_expected_k_buckets(_Key, [], Expected) ->
    Expected;
get_expected_k_buckets(Key, [#id{key=Key}|Nodes], Expected) ->
    get_expected_k_buckets(Key, Nodes, Expected);
get_expected_k_buckets(Key, [#id{key=Key0}=Node|Nodes], Expected) ->
    K = get_k(Key, Key0),
    KBucket = lists:sort([Node|maps:get(K, Expected, [])]),
    get_expected_k_buckets(Key, Nodes, maps:put(K, KBucket, Expected)).

get_k(Self, Key) ->
    trunc(math:log(?DISTANCE(Self, Key)) / math:log(2)).

check_k_buckets(Expected, Actual) ->
    ok = check_ks(maps:keys(Expected), maps:keys(Actual)),
    T = [check_k_bucket(maps:get(K, Expected), maps:get(K, Actual)) || K <- maps:keys(Expected)],
    not lists:member(false, T).

check_ks(Expected, Expected) ->
    ok;
check_ks(Expected, Actual) ->
    io:format("Error, K:s incorrect!~n   Expected: ~p~n   Acutal: ~p~n", [Expected, Actual]), 
    fail.

check_k_bucket(Expected, {_, Actual}) ->
    Res = [lists:member(A, Expected) orelse not is_process_alive(P) || #id{pid=P}=A <- Actual],
    not lists:member(false, Res).

