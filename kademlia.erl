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
    find_node1(Gate, Key, 3).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Props) ->
    Gates = proplists:get_value(gates, Props, []),
    KeyBSZ = proplists:get_value(key_bit_sz, Props, 32),
    Id = id(self(), KeyBSZ),
    GateIds = [id(Gate, KeyBSZ) || Gate <- Gates],
    spawn_link(fun() -> join(Id, KeyBSZ) end),
    spawn_link(fun() -> refresh_loop(Id) end),
    RoutingTable = routing_tables:new(Id, GateIds),
    {ok, #state{routing_table = RoutingTable, key_bit_sz = KeyBSZ}}.

handle_call(print_state, _From, State) ->
    io:format("~n~p~n", [State]),
    {reply, ok, State};
handle_call({find_node, Key, FromNode}, From,
            #state{routing_table = RT} = State) ->
    spawn_link(fun() -> find_node(Key, RT, From) end),
    RoutingTable = routing_tables:add_nodes([FromNode], RT),
    {noreply, State#state{routing_table = RoutingTable}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add_nodes, Nodes}, #state{routing_table = RT} = State) ->
    RoutingTable = routing_tables:add_nodes(Nodes, RT),
    {noreply, State#state{routing_table = RoutingTable}};
handle_cast(refresh, State) ->
    spawn_link(fun() -> refresh(State) end),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _, process, Pid, _},
            #state{routing_table = RoutingTable0,
                   key_bit_sz = KeyBSZ} = State0) ->
    Self = routing_tables:me(RoutingTable0),
    spawn_link(fun() -> refresh(KeyBSZ, Self) end),
    Id = id(Pid, KeyBSZ),
    RoutingTable = routing_tables:remove_node(Id, RoutingTable0),
    {noreply, State0#state{routing_table = RoutingTable}};
handle_info(_Info, State) ->
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
    [refresh_k_bucket(Me, K) || K <- EmptyKs].

refresh(KeyBSZ, Me) ->
    Dist = rand:uniform(?KEY_SIZE(KeyBSZ) + ?KEY_SIZE(KeyBSZ)),
    refresh(Dist, Me, KeyBSZ).

refresh(0, _Me, _K) ->
    ok;
refresh(RandDistance, #id{key = MyKey} = Me, K) ->
    Id = RandDistance bxor MyKey,
    spawn_link(fun() -> refresh_k_bucket(Me, Id, K) end),
    refresh(RandDistance bsr 1, Me, K-1).

refresh_k_bucket(#id{key = MyKey} = Me, K) ->
    Dist = rand:uniform(?KEY_SIZE(K) + ?KEY_SIZE(K)),
    Id = Dist bxor MyKey,
    spawn_link(fun() -> refresh_k_bucket(Me, Id, K) end).

refresh_k_bucket(Me, Id, K) ->
    Nodes = node_lookup(Me, Id),
    cast(Me, {add_nodes, Nodes, K}).

% TODO can we handle this some other way?
refresh_loop(Me) ->
    timer:sleep(?REFRESH_TIMEOUT),
    cast(Me, refresh),
    refresh_loop(Me).

join(#id{key = Key} = Me, KeyBSZ) ->
    Nodes = node_lookup(Me, Key),
    cast(Me, {add_nodes, Nodes}),
    refresh(KeyBSZ, Me).


% TODO make lookup recursive & don't call all the K closest ones
find_node1(Gate, _Key, 0) ->
    {error, {broken_gate, Gate}};
find_node1(Gate, Key, N) ->
    case node_lookup(Gate, Key) of
        [] -> find_node1(Gate, Key, N-1);
        Nodes -> hd(Nodes)
    end.

find_node(Key, RoutingTable, {From, Tag}) ->
    Reply = routing_tables:find_node(Key, RoutingTable),
    erlang:send(From, {Tag, Reply}).

node_lookup(#id{}=Me, Key) ->
    node_lookup(Me, Me, [Me], [], Key).

node_lookup(done, _Me, Nodes, _Asked, Key) ->
    lists:sublist(routing_tables:sort(Key, Nodes), ?K);
node_lookup(ToAsk, Me, Nodes0, Asked, Key) ->
    try call(ToAsk, {find_node, Key, Me}) of
        RetNodes when is_list(RetNodes) ->
            Nodes = looked_up(RetNodes, Nodes0, Key),
            node_lookup(get_not_asked(Nodes, [ToAsk|Asked]), Me, Nodes,
                        [ToAsk|Asked], Key)
    catch _:_ ->
        Nodes = lists:delete(ToAsk, Nodes0),
        node_lookup(get_not_asked(Nodes, Asked), Me, Nodes, Asked, Key)
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
    call(P, print_state).


