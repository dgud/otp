%%%-------------------------------------------------------------------
%%% @author Dan Gudmundsson <dgud@erlang.org>
%%% @copyright (C) 2016, Dan Gudmundsson
%%% @doc
%%%
%%% @end
%%% Created :  7 Jul 2016 by Dan Gudmundsson <dgud@erlang.org>
%%%-------------------------------------------------------------------
-module(choord).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
%% debug
-export([print_state/2, test/0]).

-compile(export_all).

-define(KEY_BIT_SIZE, 3).
-define(KEY_SIZE, (1 bsl ?KEY_BIT_SIZE)).

-record(id, {key::range(), pid::pid()}).
-record(finger, {start::range(), last::range(), node::id()}).
-record(state, {id :: id(),
		pred :: id(),
		succ=[] :: [id()],
		fingers=[] :: [finger()]}).

-type range() :: 0..?KEY_SIZE.
-type id() :: #id{}.
-type finger() :: #finger{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link([pid()]) -> {ok, pid()} | ignore | {error, term()}.
start_link(Gates) ->
    gen_server:start_link(?MODULE, [Gates], []).

-spec key(term()) -> integer().
%% key(Atom) when is_atom(Atom) -> erlang:phash2(atom_to_list(Atom), ?KEY_SIZE);
%% key(Term) -> erlang:phash2(Term, ?KEY_SIZE).
key(_Term) ->
    key_server ! {get_key, self()},
    receive {key, Key} -> Key end.

find_successor(Gate, Id) ->
    Res = {Pred, Succs} = find_predecessor(Gate, Id),
    Succs = call(Pred, get_successor), %% Assert
    Res.

find_predecessor(Gate, Id) ->
    case call(Gate, {find_predecessor, Id}) of
	{ok, Pred, Succs} -> {Pred, Succs};
	{cont, Next} -> find_predecessor(Next, Id)
    end.

print_state(Gate, Level) ->
    cast(Gate, {debug_state, start, Level}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Gates]) ->
    Id = #id{key=key(self()), pid=self()},
    Fingers0 = make_fingers(Id),
    {ok, init_neighbors(Gates, #state{id=Id, pred=Id, succ=[Id], fingers=Fingers0})}.

%% handle_call(fetch_neighbors, _From, #state{pred=Pred, succ=Succs}=State) ->
%%     {reply, {Pred, Succs}, State};
handle_call(get_successor, _From, #state{succ=[Succs|_]}=State) ->
    {reply, Succs, State};
handle_call({find_predecessor, Id}, _From, State) ->
    Reply = find_predecessor_impl(Id, State),
    {reply, Reply, State}.

handle_cast({update_finger_table, Id, _I}, #state{id=Id}=State) ->
    %% My own state is already correct
    {noreply, State};
handle_cast({update_finger_table, S, I}, State) ->
    Fingers = update_finger_table(S, I, State),
    [#finger{node=Succs}|_] = Fingers,
    {noreply, State#state{succ=[Succs], fingers=Fingers}};
handle_cast({set_predecessor, Id}, State) ->
    {noreply, State#state{pred=Id}};
handle_cast({debug_state, Start, Level}, #state{id=Id, succ=[Succs|_]}=State) ->
    case Start of
	Id -> ignore;
	start ->
	    print_state(State),
	    cast(Succs, {debug_state, Id, Level});
	_ ->
	    print_state(State),
	    cast(Succs, {debug_state, Start, Level})
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    io:format("~p: Unhandled ~p~n", [?LINE, _Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    io:format("~p: Unhandled ~p~n", [?LINE, _Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

make_fingers(#id{key=N} = Id) ->
    Start = [(N + (1 bsl (K-1))) rem ?KEY_SIZE ||
		K <- lists:seq(1, ?KEY_BIT_SIZE)],
    make_fingers(Start, N, Id).

make_fingers([Start|[Next|_]=Rest], Last, Id) ->
    [#finger{start=Start, last=Next, node=Id}|make_fingers(Rest,Last,Id)];
make_fingers([Start], Last, Id) ->
    [#finger{start=Start, last=Last, node=Id}].

init_neighbors([Gate|Gates], #state{id=Id, fingers=[#finger{start=Start}=F0|Fingers]}=State0) ->
    case find_successor(Gate, Start) of
	{error, _} ->
	    init_neighbors(Gates, State0);
	{Pred, Succs} ->
	    State1 = State0#state{pred=Pred, succ=[Succs]},
	    F = F0#finger{node=Succs},
	    Fs = [F|init_fingers(Fingers, Id#id.key, Succs, Gate)],
	    cast(Succs, {set_predecessor, Id}),
	    io:format("INIT: ~s~n~s~n", [print_key(Id), [print_finger(Fi) || Fi <- Fs]]),
	    spawn_link(fun() -> update_others(Id, 1) end),
	    State1#state{fingers=Fs}
    end;
init_neighbors(_Id, State) ->
    #state{id=Id, fingers=[F0|Fingers]}=State,
    io:format("INIT: ~s~n F0:~s F1:~s~n", [print_key(Id), print_finger(F0), print_finger(hd(tl(Fingers)))]),
    State.

init_fingers([#finger{start=Start}=F|Fs], N, #id{key=NodeId}=Prev, Gate) ->
    case memberIN(Start, N, NodeId) of
	true  ->
	    [F#finger{node=Prev}|init_fingers(Fs, N, Prev, Gate)];
	false ->
	    {_, Succs} = find_successor(Gate, Start),
	    [F#finger{node=Succs}|init_fingers(Fs, N, Succs, Gate)]
    end;
init_fingers([], _, _, _) -> [].

find_predecessor_impl(Id, #state{id=This, succ=[Succs|_], fingers=Fingers}) ->
    case memberNI(Id, This#id.key, Succs#id.key) of
	true ->
	    {ok, This, Succs};
	false ->
	    %% [io:put_chars(print_finger(F)) || F <- Fingers],
	    case closest_preceding_fingers(This, Id, Fingers) of
		This -> {ok, This, Succs};
		Next -> {cont, Next}
	    end
    end.

closest_preceding_fingers(#id{key=This}=N, Id, Fingers) ->
    Check = fun(#finger{node=#id{key=Key}}) ->
		    %% io:format("mem ~p < ~p < ~p~n",[This, Key, Id]),
		    not memberNN(Key, This, Id)
	    end,
    case lists:dropwhile(Check, lists:reverse(Fingers)) of
	[] -> N;
	[#finger{node=Node}|_] -> Node
    end.

update_others(#id{key=Key}=Id, I)
  when I =< ?KEY_BIT_SIZE ->
    Prev = (?KEY_SIZE + Key - (1 bsl (I-1)) + 1) rem ?KEY_SIZE,
    {Pred,_} = find_predecessor(Id, Prev),
    io:format("UPD ~p (~p) => Pred ~p ~s~n", [I, Key, Prev, print_key(Pred)]),
    cast(Pred, {update_finger_table, Id, I}),
    update_others(Id, I+1);
update_others(_, _) -> ok.

update_finger_table(#id{key=SKey}=S, I,
		    #state{id=#id{key=N}, fingers=Fingers0, pred=Pred}) ->
    {Part1, [F0|Part2]} = lists:split(I-1, Fingers0),
    #finger{node=#id{key=Node}} = F0,
    case memberIN(SKey, N, Node) of
	false ->
	    Fingers0;
	true ->
	    F = F0#finger{node=S},
	    cast(Pred, {update_finger_table, S, I}),
	    Part1 ++ [F|Part2]
    end.

%% Closed range
memberNN(Id, Near, Far)
  when Near < Id andalso Id < Far ->
    true;
memberNN(Id, Near, Far)
  when Near > Far andalso ((Near < Id) orelse (Id < Far)) ->
    true;
memberNN(Id, Near, Near) when Id =/= Near ->
    true;
memberNN(_, _, _) ->
    false.

%% open range post
memberNI(Id, Near, Far)
  when Near < Id andalso Id =< Far ->
    true;
memberNI(Id, Near, Far)
  when Near >= Far andalso ((Near < Id) orelse (Id =< Far)) ->
    true;
%%memberNI(Near, Near, Near) -> true;  % Near = Far max set
memberNI(_, _, _) ->
    false.

%% open range pre
memberIN(Id, Near, Far)
  when Near =< Id andalso Id < Far ->
%    io:format("~p ", [?LINE]),
    true;
memberIN(Id, Near, Far)
  when Near >= Far andalso ((Near =< Id) orelse (Id < Far)) ->
%    io:format("~p ", [?LINE]),
    true;
%%memberIN(Near, Near, Near) -> io:format("~p ", [?LINE]), true;    % Near = Far  min set
memberIN(_, _, _) ->
    false.

cast(#id{pid=Pid}, Msg) ->
    gen_server:cast(Pid, Msg);
cast(Pid, Msg) when is_pid(Pid) ->
    gen_server:cast(Pid, Msg).

call(#id{pid=Pid}, Msg) ->
    gen_server:call(Pid, Msg, infinity);
call(Pid, Msg) when is_pid(Pid) ->
    gen_server:call(Pid, Msg, infinity).

%%% Debug

print_state(#state{id=This, pred=Pred, succ=Succs, fingers=Fingers}) ->
    io:format("~nTHIS: ~p ~s~n", [self(), print_key(This)]),
    io:format("PRED: ~s~n", [print_key(Pred)]),
    io:format("SUCC: ~p~n", [[lists:flatten(print_key(S)) || S <- Succs]]),
    io:format("Fingers: ~n", []),
    [io:put_chars(print_finger(F)) || F <- Fingers].

print_finger(#finger{start=Start, last=Last, node=Node}) ->
    io_lib:format(" {~.3w ~.5w ~s}~n",[Start, Last, print_key(Node)]).

print_key(#id{key=Key, pid=Pid}) ->
    io_lib:format("<~.3p ~p>", [Key,Pid]).


%% Temporary testing

test() ->
    spawn_link(fun() -> key_server(init) end),
    timer:sleep(10),
    io:format("TESTING~n",[]),
    {ok,P1} = choord:start_link([]),
    {ok,_P2} = choord:start_link([P1]),
    timer:sleep(20),
    ok = choord:print_state(P1, debug),
    timer:sleep(20),
    io:format("~n**********~n~n"),
    {ok,_P3} = choord:start_link([P1]),
    timer:sleep(20),
    ok = choord:print_state(P1, debug),
    ok.

%%%%%%%%%%%%
%% Debug

key_server(State) ->
    try
	unlink(whereis(key_server)),
        exit(whereis(key_server), foo)
    catch _:_ -> ok end,
    timer:sleep(10),
    register(key_server, self()),
    key_server_loop(State).

key_server_loop(State) ->
    receive
	{get_key, Pid} ->
	    Next = case State of
		       init -> 0;
		       0 -> 3;
		       3 -> 1;
		       1 -> 6
		   end,
	    Pid ! {key, Next},
	    key_server_loop(Next)
    end.



debug() ->
    i:ii(?MODULE),
    i:ib(?MODULE, handle_call, 3),
    i:ib(?MODULE, handle_cast, 2),
    i:ib(?MODULE, update_others, 2).
