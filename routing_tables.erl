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

-export([new/2, me/1, find_node/2, add_nodes/2, remove_node/2, sort/2,
         get_empty_k_buckets/2]).

%%%===================================================================
%%% API
%%%===================================================================

new(Id, []) ->
    #routing_table{self = Id, closest_neighbours = [], k_buckets = maps:new()};
new(Id, Gates) ->
    add_nodes(Gates, new(Id, [])).

me(#routing_table{self = Self}) ->
    Self.

find_node(Key, #routing_table{self = #id{key = Key} = Self,
                               k_buckets = KBuckets}) ->
    Sorted = sort([], Key, Self, KBuckets),
    lists:sublist(Sorted, ?K);
find_node(Key, #routing_table{self = Self,
                              k_buckets = KBuckets} = RT) ->
    {_Monitor, KBucketNodes} = get_k_bucket(Key, RT),
    Sorted = sort(KBucketNodes, Key, Self, KBuckets),
    lists:sublist(Sorted, ?K).

add_nodes([], RoutingTable) ->
    update_neighbours(RoutingTable);
add_nodes([Node|Nodes], RoutingTable0) ->
    RoutingTable = add_node(Node, RoutingTable0),
    add_nodes(Nodes, RoutingTable).

remove_node(#id{key = Key} = Id,
            #routing_table{self = Self,
                           k_buckets = KBuckets0} = RoutingTable) ->
    {Monitor0, KBucketNodes0} = get_k_bucket(Id, RoutingTable),
    KBucketNodes = lists:delete(Id, KBucketNodes0),
    K = get_k(Self, Key),
    KBuckets = rm_fix_k_buckets(K, Id, Monitor0, KBucketNodes, KBuckets0),
    update_neighbours(RoutingTable#routing_table{k_buckets = KBuckets}).

sort(Key, KBucketNodes) ->
    DistanceList = [{?DISTANCE(Key, K), Contact}
                    || #id{key = K} = Contact <- KBucketNodes],
    Sorted = lists:keysort(1, DistanceList),
    [KeyContact || {_D, KeyContact} <- Sorted].

get_empty_k_buckets(KeyBSZ, #routing_table{k_buckets = KBuckets}) ->
    AllKs = lists:seq(0, KeyBSZ-1),
    Ks = maps:keys(KBuckets),
    lists:filter(fun(K) -> not lists:member(K, Ks) end, AllKs).

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
    KBucketNodes;
add_node(_Member, Node, KBucketNodes) when length(KBucketNodes) < ?K->
    KBucketNodes ++ [Node];
add_node(_Member, _Node, KBucketNodes) ->
    KBucketNodes.

get_k(#id{key=Self}, Key) when is_integer(Key) ->
    trunc(math:log(?DISTANCE(Self, Key)) / math:log(2)).

rm_fix_k_buckets(K, Id, {MRef, Id}, [], KBuckets0) ->
    demonitor(MRef),
    maps:remove(K, KBuckets0);
rm_fix_k_buckets(K, Id, {MRef, Id}, KBucketNodes, KBuckets0) ->
    demonitor(MRef),
    Monitor = setup_monitor(KBucketNodes),
    maps:put(K, {Monitor, KBucketNodes}, KBuckets0);
rm_fix_k_buckets(K, _, Monitor0, KBucketNodes, KBuckets0) ->
    maps:put(K, {Monitor0, KBucketNodes}, KBuckets0).

% monitoring

setup_monitor(undefined, KBucketNodes) ->
    setup_monitor(KBucketNodes);
setup_monitor(Monitor, _KBucketNodes) ->
    Monitor.

setup_monitor([#id{pid=Pid}=Id|_]) ->
    {monitor(process, Pid), Id}.

% All

get_all_ids(Self, KBuckets) ->
    KBucketsNodes = [KBucketNodes ||
                     {_Monitor, KBucketNodes} <- maps:values(KBuckets)],
    [Self | lists:append(KBucketsNodes)].

add_node(Self, #routing_table{self = Self} = RoutingTable) ->
    RoutingTable;
add_node(#id{key = Key} = Node,
         #routing_table{self = Self,
                        k_buckets = KBuckets0} = RoutingTable) ->
    {Monitor0, KBucket0} = get_k_bucket(Node, RoutingTable),
    KBucketNodes = add_node(lists:member(Node, KBucket0), Node, KBucket0),
    Monitor = setup_monitor(Monitor0, KBucketNodes),
    K = get_k(Self, Key),
    KBuckets = maps:put(K, {Monitor, KBucketNodes}, KBuckets0),
    RoutingTable#routing_table{k_buckets = KBuckets}.

update_neighbours(#routing_table{self = #id{key = Key} = Self,
                                 closest_neighbours = Closest0,
                                 k_buckets = KBuckets,
                                 neighbour_size = NSize} = RoutingTable) ->
    ClosestNeighbours = tl(lists:sublist(sort(Key, get_all_ids(Self, KBuckets)), NSize + 1)),%TODO clean
    Closest = update_neighbour_monitors(Closest0, ClosestNeighbours),
    RoutingTable#routing_table{closest_neighbours = Closest}.

update_neighbour_monitors(Closest0, Closest1) ->
    [demonitor(MRef) || {MRef, _Id} <- Closest0],
    [{monitor(process, Pid), Id} || #id{pid = Pid} = Id <- Closest1].


