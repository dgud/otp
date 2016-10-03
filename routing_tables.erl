%%%-------------------------------------------------------------------
%%% @author Zandra Hird
%%% @copyright (C) 2016, Zandra Hird
%%% @doc
%%%
%%% @end
%%% Created : 2016-09-26 by Zandra Hird
%%%-------------------------------------------------------------------
-module(routing_tables).

-include("kademlia.hrl").

-export([new/2, me/1, find_node/2, find_nodes/2, add_nodes/2,
         remove_node/2, sort/2, get_empty_k_buckets/2,
         get_monitored/1, get_k/2]).

%%%===================================================================
%%% API
%%%===================================================================

new(Id, []) ->
    #routing_table{self = Id, neighbours = [], k_buckets = maps:new()};
new(Id, Gates) ->
    {_, RoutingTable} = add_nodes(Gates, new(Id, [])),
    RoutingTable.

me(#routing_table{self = Self}) ->
    Self.

find_node(Key, #routing_table{self = #id{key = Key} = Self}) ->
    Self;
find_node(Key, #routing_table{self = Self} = RT) ->
    {Monitor, _KBucketNodes} = get_k_bucket(Key, RT),
    case Monitor of
        undefined ->
            hd(sort(Key, [Self|get_monitored(RT)]));
        {_Mref, Id} ->
            Id
    end.

find_nodes(Key, #routing_table{self = #id{key = Key} = Self,
                               k_buckets = KBuckets}) ->
    Sorted = sort([], Key, Self, KBuckets),
    lists:sublist(Sorted, ?K);
find_nodes(Key, #routing_table{self = Self,
                              k_buckets = KBuckets} = RT) ->
    {_Monitor, KBucketNodes} = get_k_bucket(Key, RT),
    Sorted = sort(KBucketNodes, Key, Self, KBuckets),
    lists:sublist(Sorted, ?K).

add_nodes(Nodes, #routing_table{self = Self, neighbours = Neighbours,
                                neighbour_size = NeighbourSZ} = RoutingTable) ->
    NewNeighbours = add_neighbours(NeighbourSZ, Nodes, Neighbours, Self),
    add_nodes(Nodes, RoutingTable, RoutingTable#routing_table{neighbours = NewNeighbours}, false).

add_nodes([], _, RoutingTable, Added) ->
    {Added, RoutingTable};
add_nodes([Node|Nodes], OrigRT, RoutingTable0, Added0) ->
    {Added, RoutingTable} = add_node(Node, RoutingTable0),
    add_nodes(Nodes, OrigRT, RoutingTable, Added orelse Added0).

remove_node(#id{key = Key} = Id,
            #routing_table{self = Self,
                           neighbours = NeighbourMons,
                           k_buckets = KBuckets0} = RoutingTable) ->
    {Monitor0, KBucketNodes0} = get_k_bucket(Id, RoutingTable),
    KBucketNodes = lists:delete(Id, KBucketNodes0),
    K = get_k(Self, Key),
    KBuckets = rm_fix_k_buckets(K, Id, Monitor0, KBucketNodes, KBuckets0),
    NewNeighbours = rm_neighbour(Id, NeighbourMons),
    RoutingTable#routing_table{neighbours = NewNeighbours, k_buckets = KBuckets}.

sort(Key, KBucketNodes) ->
    DistanceList = [{?DISTANCE(Key, K), Contact}
                    || #id{key = K} = Contact <- KBucketNodes],
    Sorted = lists:keysort(1, DistanceList),
    [KeyContact || {_D, KeyContact} <- Sorted].

get_empty_k_buckets(KeyBSZ, #routing_table{k_buckets = KBuckets}) ->
    AllKs = lists:seq(0, KeyBSZ-1),
    Ks = maps:keys(KBuckets),
    lists:filter(fun(K) -> not lists:member(K, Ks) end, AllKs).

get_monitored(#routing_table{neighbours = Neighbours, k_buckets = KBuckets}) ->
    [N || {{_, N}, _} <- maps:values(KBuckets)] ++ [N || {_, N} <- Neighbours].

%%%===================================================================
%%% Internal functions
%%%===================================================================

% k bucket handling

get_k_bucket(#id{key=Key}, RoutingTable) ->
    get_k_bucket(Key, RoutingTable);
get_k_bucket(Key, #routing_table{self = Self,
                                          k_buckets = KBuckets}) ->
    maps:get(get_k(Self, Key), KBuckets, {undefined, []}).

sort(KBucketNodes, Key, Self, KBuckets) when length(KBucketNodes) < ?K ->
      sort(Key, get_all_ids(Self, KBuckets));
sort(KBucketNodes, Key, _Self, _KBuckets) ->
      sort(Key, KBucketNodes).

add_node(true, _Node, KBucketNodes) ->
    {false, KBucketNodes};
add_node(_Member, Node, KBucketNodes) when length(KBucketNodes) < ?K->
    {true, KBucketNodes ++ [Node]};
add_node(_Member, Node,  [#id{pid = P} = KBucketNode|KBucketNodes]) ->
    case is_process_alive(P) of
        true ->
            {false, KBucketNodes ++ [KBucketNode]};
        false ->
            {true, KBucketNodes ++ [Node]}
    end.

get_k(#routing_table{self = Self}, Key) ->
    get_k(Self, Key);
get_k(#id{key=Self}, Key) when is_integer(Key) ->
    trunc(math:log(?DISTANCE(Self, Key)) / math:log(2)).

rm_fix_k_buckets(K, _, undefined, [], KBuckets0) ->
    maps:remove(K, KBuckets0);
rm_fix_k_buckets(K, _, {MRef, _Id}, [], KBuckets0) ->
    demonitor(MRef),
    maps:remove(K, KBuckets0);
rm_fix_k_buckets(K, Id, {MRef, Id}, KBucketNodes, KBuckets0) ->
    demonitor(MRef),
    AliveKBucketNodes = lists:filter(fun(#id{pid = Pid}) -> is_process_alive(Pid) end, KBucketNodes),
    case AliveKBucketNodes of
        [] ->
            maps:remove(K, KBuckets0);
        _ ->
            Monitor = setup_monitor(AliveKBucketNodes),
            maps:put(K, {Monitor, AliveKBucketNodes}, KBuckets0)
    end;
rm_fix_k_buckets(K, _, Monitor0, KBucketNodes, KBuckets0) ->
    maps:put(K, {Monitor0, KBucketNodes}, KBuckets0).

% monitoring
setup_mon(Pid) ->
    {monitors, Ms} = process_info(self(), monitors),
    case lists:member(Pid, Ms) of
       true  -> already_montiored;
       false -> monitor(process, Pid)
    end.
setup_monitor(undefined, KBucketNodes) ->
    setup_monitor(KBucketNodes);
setup_monitor(Monitor, _KBucketNodes) ->
    Monitor.

setup_monitor([#id{pid=Pid}=Id|_]) ->
    Mref = setup_mon(Pid),
    {Mref, Id}.

% All

get_all_ids(Self, KBuckets) ->
    KBucketsNodes = [KBucketNodes ||
                     {_Monitor, KBucketNodes} <- maps:values(KBuckets)],
    [Self | lists:append(KBucketsNodes)].

add_node(Self, #routing_table{self = Self} = RoutingTable) ->
    {false, RoutingTable};
add_node(#id{key = Key} = Node,
         #routing_table{self = Self,
                        k_buckets = KBuckets0} = RoutingTable) ->
    {Monitor0, KBucket0} = get_k_bucket(Node, RoutingTable),
    {Added, KBucketNodes} = add_node(lists:member(Node, KBucket0), Node, KBucket0),
    Monitor = setup_monitor(Monitor0, KBucketNodes),
    K = get_k(Self, Key),
    KBuckets = maps:put(K, {Monitor, KBucketNodes}, KBuckets0),
    {Added, RoutingTable#routing_table{k_buckets = KBuckets}}.

rm_neighbour(Id, NeighbourMons) ->
    case lists:keydelete(Id, 2, NeighbourMons) of
        NeighbourMons ->
            NeighbourMons;
        NewNeighbourMons ->
            update_neighbour_monitors(NeighbourMons, NewNeighbourMons)
    end.

add_neighbours(NeighbourSz, Nodes, NeighbourMons, #id{key = MyKey} = Self) ->
    Neighbours = [N || {_, N} <- NeighbourMons],
    All = lists:usort(lists:delete(Self, Nodes) ++ Neighbours),
    case lists:sublist(sort(MyKey, All), NeighbourSz) of
        Neighbours ->
            NeighbourMons;
        NewNeighbours ->
            update_neighbour_monitors(NeighbourMons, NewNeighbours)
    end.

update_neighbour_monitors(Old, New) ->
    [demonitor(MRef) || {MRef, _Id} <- Old],
    [{setup_mon(Pid), Id} || #id{pid = Pid} = Id <- New].

