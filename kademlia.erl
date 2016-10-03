%%%-------------------------------------------------------------------
%%% @author Zandra Hird <zandra@erlang.org>
%%% @copyright (C) 2016, Zandra Hird
%%% @doc
%%%
%%% @end
%%% Created : 2016-09-26 by Zandra Hird
%%%-------------------------------------------------------------------
-module(kademlia).

-behaviour(gen_server).

-include("kademlia.hrl").
-define(REFRESH_TIMEOUT, 60 * 1000).

%% API
-export([start_link/1, start/1, find_node/2]).

% debug and testing
-export([key/2, print_state/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link([pid()]) -> {ok, pid()} | ignore | {error, term()}.
start_link(Props) ->
    gen_server:start_link(?MODULE, Props, []).

-spec start([{atom(), term()}]) -> {ok, pid()} | ignore | {error, term()}.
start(Props) when is_list(Props) ->
    gen_server:start(?MODULE, Props, []).

-spec find_node(id(), key()) -> id().
find_node(Gate, Key) ->
    node_lookup(Gate, Key).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Props) ->
    Gates = proplists:get_value(gates, Props, []),
    KeyBSZ = proplists:get_value(key_bit_sz, Props, 32),
    Id = id(self(), KeyBSZ),
    GateIds = [id(Gate, KeyBSZ) || Gate <- Gates],
    [cast(Gate, {joining, Id}) || Gate <- GateIds],
    spawn_link(fun() -> join(Id, KeyBSZ) end),
    spawn_link(fun() -> refresh_loop(Id) end),
    RoutingTable = routing_tables:new(Id, GateIds),
    {ok, #state{routing_table = RoutingTable, key_bit_sz = KeyBSZ,
                unstable = lists:seq(1, KeyBSZ)}}.

handle_call({find_node, Key, FromNode}, From,
            #state{unstable = Unstable, routing_table = RT} = State) ->
    spawn_link(fun() -> find_node(Key, RT, From, Unstable) end),
    {_, RoutingTable} = routing_tables:add_nodes([FromNode], RT),
    {noreply, State#state{routing_table = RoutingTable}};
handle_call({find_nodes, Key, FromNode}, From,
            #state{routing_table = RT} = State) ->
    spawn_link(fun() -> find_nodes(Key, RT, From) end),
    {_, RoutingTable} = routing_tables:add_nodes([FromNode], RT),
    {noreply, State#state{routing_table = RoutingTable}};
handle_call(_Request, _From, State) ->
    io:format("Unexpected call message: ~p~n", [_Request]),
    {reply, ok, State}.


handle_cast({joining, Node}, #state{routing_table = RT} = State) ->
    RoutingTable = joining_node(Node, RT),
    {noreply, State#state{routing_table = RoutingTable}};
handle_cast({add_nodes, Nodes}, #state{routing_table = RT} = State) ->
    {_, RoutingTable} = routing_tables:add_nodes(Nodes, RT),
    {noreply, State#state{routing_table = RoutingTable}};
handle_cast(refresh, State) ->
    spawn_link(fun() -> refresh(State) end),
    {noreply, State};
handle_cast({stable, Ks}, #state{unstable = Unstable0} = State) ->
    Unstable = lists:filter(fun(K) -> not lists:member(K, Ks) end, Unstable0),
    {noreply, State#state{unstable = Unstable}};
handle_cast(_Msg, State) ->
    io:format("Unexpected cast message: ~p~n", [_Msg]),
    {noreply, State}.

handle_info({'DOWN', _, process, Pid, _},
            #state{routing_table = RoutingTable0,
                   key_bit_sz = KeyBSZ, unstable = Unstable0} = State) ->
    Self = routing_tables:me(RoutingTable0),
    Id = id(Pid, KeyBSZ),
    K = routing_tables:get_k(Self, Id#id.key),
    Unstable = case not lists:member(K, Unstable0) of
        true ->
            spawn_link(fun() -> refresh_k_buckets(Self, [K]) end),
            [K|Unstable0];
        _ ->
            Unstable0
    end,
    RoutingTable = routing_tables:remove_node(Id, RoutingTable0),
    {noreply, State#state{unstable = Unstable,
                          routing_table = RoutingTable}};
handle_info(_Info, State) ->
    io:format("Unexpected info message: ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

refresh(#state{routing_table = RoutingTable, key_bit_sz = KeyBSZ}) ->
    EmptyKs = routing_tables:get_empty_k_buckets(KeyBSZ, RoutingTable),
    Me = routing_tables:me(RoutingTable),
    refresh_k_buckets(Me, EmptyKs).

refresh_k_buckets(Me, Ks) ->
    [refresh_k_bucket(Me, K) || K <- Ks],
    refresh_k_bucket0(Me, Me#id.key),
    cast(Me, {stable, Ks}).

refresh_k_bucket(#id{key = MyKey} = Me, 0) ->
    Id = 1 bxor MyKey,
    refresh_k_bucket0(Me, Id);
refresh_k_bucket(#id{key = MyKey} = Me, K) ->
    Dist = rand:uniform(?KEY_SIZE(K)) + ?KEY_SIZE(K),
    Id = Dist bxor MyKey,
    refresh_k_bucket0(Me, Id).

refresh_k_bucket0(Me, Id) ->
    Nodes = nodes_lookup(Me, Id),
    cast(Me, {add_nodes, Nodes}), 
    Nodes.

% TODO can we handle this some other way?
refresh_loop(Me) ->
    timer:sleep(?REFRESH_TIMEOUT),
    cast(Me, refresh),
    refresh_loop(Me).

join(#id{key = Key} = Me, KeyBSZ) ->
    Nodes = nodes_lookup(Me, Key),
    cast(Me, {add_nodes, Nodes}),
    [cast(N, {joining, Me}) || N <- Nodes],
    Ks = lists:seq(1, KeyBSZ),
    join(Me, Ks, Ks).

join(Me, [], Ks) ->
    cast(Me, {stable, Ks});
join(Me, [K|Ks], AllKs) ->
    Nodes = refresh_k_bucket(Me, K),
    [cast(N, {joining, Me}) || N <- Nodes],
    join(Me, Ks, AllKs).

joining_node(Node, RoutingTable0) ->
    case routing_tables:add_nodes([Node], RoutingTable0) of
        {false, RoutingTable} ->
            RoutingTable;
        {true, RoutingTable} ->
            ToNotify = routing_tables:get_monitored(RoutingTable),
            [cast(N, {joining, Node}) || N <- ToNotify],
            RoutingTable
    end.

find_node(Key, RoutingTable, {From, Tag}, []) ->
    Reply = routing_tables:find_node(Key, RoutingTable),
    erlang:send(From, {Tag, Reply});
find_node(_Key, _RoutingTable, {From, Tag}, _UnstableKs) ->
    erlang:send(From, {Tag, unstable}).

find_nodes(Key, RoutingTable, {From, Tag}) ->
    Reply = routing_tables:find_nodes(Key, RoutingTable),
    erlang:send(From, {Tag, Reply}).

node_lookup(Me, Key) ->
    node_lookup(Me, Me, Key).

node_lookup(Me, Closest, Key) ->
    try call(Closest, {find_node, Key, Me}) of
        Closest ->
            Closest;
        unstable ->
            node_lookup(Me, Me, Key); %this will get better in the recursive lookup
        Next ->
            node_lookup(Me, Next, Key)
    catch _:_ ->
        node_lookup(Me, Me, Key)
    end.

nodes_lookup(Me, Key) ->
    nodes_lookup(Me, Me, [Me], [], Key).

nodes_lookup(done, _Me, Nodes, _Asked, Key) ->
    lists:sublist(routing_tables:sort(Key, Nodes), ?K);
nodes_lookup(ToAsk, Me, Nodes0, Asked, Key) ->
    try call(ToAsk, {find_nodes, Key, Me}) of
        RetNodes when is_list(RetNodes) ->
            Nodes = looked_up(RetNodes, Nodes0, Key),
            nodes_lookup(get_not_asked(Nodes, [ToAsk|Asked]), Me, Nodes,
                        [ToAsk|Asked], Key)
    catch _:_ ->
        Nodes = lists:delete(ToAsk, Nodes0),
        nodes_lookup(get_not_asked(Nodes, Asked), Me, Nodes, Asked, Key)
    end.

looked_up(OldNodes, NewNodes, Key) ->
    All = lists:usort(OldNodes ++ NewNodes),
    Sorted = routing_tables:sort(Key, All),
    lists:sublist(Sorted, ?K).

get_not_asked([], _) ->
    done;
get_not_asked([Node|Nodes], Asked) ->
    case lists:member(Node, Asked) of
        true ->
            get_not_asked(Nodes, Asked);
        false ->
            Node
    end.

id(Pid, KeyBSZ) ->
    #id{key=key(Pid, KeyBSZ), pid=Pid}.

%TODO we cannot use this when using splitting into nodes!
key(Pid, Size) when is_pid(Pid) ->
    erlang:phash2(pid_to_list(Pid), ?KEY_SIZE(Size)).

cast(#id{pid=Pid}, Msg) ->
    gen_server:cast(Pid, Msg);
cast(Pid, Msg) when is_pid(Pid) ->
    gen_server:cast(Pid, Msg).

call(#id{pid=Pid}, Msg) ->
    call(Pid, Msg);
call(Pid, Msg) when is_pid(Pid) ->
    gen_server:call(Pid, Msg, infinity).

% Debug
print_state(P) ->
    io:format("~n~p~n", [sys:get_state(P)]).
