-module(choord_test).

-export([test/0, paper_example/0, test_fast_joins/0, test_multiple_exits/0]).

-define(KEY_SIZE(BIT_SIZE), (1 bsl (BIT_SIZE))).
-compile(export_all).
%% Temporary testing

test() ->
    io:format("Starting all tests~n",[]),
    ok=paper_example(),
    ok=test_fast_joins(),
    ok=test_multiple_exits(),
    ok=test_256_nodes(),
    ok=test_up_down().

paper_example() ->
    KBSZ = 3,
    Props = [{key_bit_sz, KBSZ}],
    io:format("~n~nPAPER EXAMPLE TEST~n",[]),
    io:format("~n********************~n~n"),
    {ok, P1} = choord:start_link([{key,0}|Props]),
    {ok, P2} = choord:start_link([{gates,[P1]}, {key,3}|Props]),
    timer:sleep(1000), ok = choord:print_state(P1),
    try
        ok = check_net([{0,P1},{3,P2}], KBSZ),
        {ok, P3} = choord:start_link([{gates,[P1]}, {key,1}|Props]),
        timer:sleep(1000), ok = choord:print_state(P1),
        ok = check_net([{0,P1},{3,P2},{1,P3}], KBSZ),
        {ok, P4} = choord:start_link([{gates,[P1]}, {key,6}|Props]),
        ok = check_net([{0,P1},{3,P2},{1,P3},{6,P4}], KBSZ),
        ok = choord:print_state(P1),
        ok = choord:print_ring(P1),
        die(P3),
        ok = check_net([{0,P1},{3,P2},{6,P4}], KBSZ),
        die(P2),
        ok = check_net([{0,P1},{6,P4}], KBSZ),
        ok = choord:print_state(P1),
        ok = choord:print_ring(P1),
        die(P1),
        die(P4),
        ok
    catch _:{error, B, C, D} ->
            io:format("~n~nTEST FAILED: ~p: ~p ~s~n", [B, C, lists:flatten(D)]),
            choord:print_state(P1),
            failed;
        error:Reason ->
            io:format("TEST FAILED ~p~n ~p",[Reason, erlang:get_stacktrace()]),
            failed
   end.


test_fast_joins() ->
    io:format("~n~nTESTING MULTIPLE AND FAST JOINS~n",[]),
    io:format("~n*********************************~n~n"),
    KBSZ = 3,
    Props = [{key_bit_sz, KBSZ}],
    {ok, P1} = choord:start([{key,0}|Props]),
    Gates = join(Props, [5, 2, 6, 3, 7, 1, 4], [{0,P1}]),
    try
        ok = check_net(Gates, KBSZ),
        ok = choord:print_state(P1),
        ok = choord:print_ring(P1),
        [die(P) || P <- Gates],
        ok
    catch
        _:{error, B, C, D} ->
            choord:print_state(P1),
            io:format("~n~nTEST FAILED: ~p: ~p ~s~n", [B, C, lists:flatten(D)]),
            failed;
        error:Reason ->
            io:format("TEST FAILED ~p~n ~p",[Reason, erlang:get_stacktrace()]),
            failed
   end.

test_multiple_exits() ->
    io:format("~n~nTESTING MULTIPLE AND FAST EXITS~n",[]),
    io:format("~n*********************************~n~n"),
    KBSZ = 3,
    Props = [{key_bit_sz, KBSZ}],
    {ok, P1} = choord:start([{key,0}|Props]),
    Gates = join(Props, [5, 2, 6, 3, 7, 1, 4], [{0,P1}]),
    ok = check_net(Gates, KBSZ),
    try
        Exits = [hd(Gates), hd(tl(Gates)), hd(tl(tl(Gates)))],
        io:format("Killing: ~p~n", [Exits]),
        [exit(p(P), die) || P <- Exits],
        Left = lists:subtract(Gates, Exits),
        io:format("Left: ~p~n",  [Left]),
        ok = check_net(Left, KBSZ),
        ok = choord:print_ring(p(hd(Left))),
        [die(P) || P <- Left],
        ok
    catch
        _:{error, B, C, D} ->
            choord:print_state(P1),
            io:format("~n~nTEST FAILED: ~p: ~p ~s~n", [B, C, lists:flatten(D)]),
            exit(test_failed),
            failed;
        error:Reason ->
            io:format("TEST FAILED ~p~n ~p",[Reason, erlang:get_stacktrace()]),
            failed
   end.


join(_, [], Gates) ->
    Gates;
join(Props, [Key|Keys], Gates) ->
    Gs = [p(G) || G <- Gates],
    {ok, P} = choord:start([{gates,Gs}, {key,Key}|Props]),
    join(Props, Keys, [{Key, P}|Gates]).


test_256_nodes() ->
    KBSZ = 8,
    Props = [{key_bit_sz, KBSZ}],
    io:format("~n~nTEST 256 NODES~n",[]),
    io:format("~n~n**************~n",[]),
    rand:seed(exs64, {12383, 55421,135412}), %% Set seed so we can debug
    [First|Keys] = make_reordered_keys(?KEY_SIZE(KBSZ)),
    {ok, Pid0} = choord:start([{key, First}|Props]),
    Make = make_fun(Pid0, Props, start_link),
    All = [{First, Pid0} | [{Key, Make(Key)} || Key <- Keys]],
    try
	%% print_ring(Pid0),
	ok = check_net(All, KBSZ),
	A0 = remove(All, 20), % From 256 To 236
	ok = check_net(A0, KBSZ),
	A1 = remove(A0, 40), % From 236 To 196
	ok = check_net(A1, KBSZ),
	A2 = remove(A1, 50), % From 196 To 146
	ok = check_net(A2, KBSZ),
	A3 = remove(A2, 50), % From 146 To 96
	ok = check_net(A3, KBSZ),
	A4 = remove(A3, 50), % From 96 To 46
	ok = check_net(A4, KBSZ),
	A5 = remove(A4, 20), % From 46 To 26
	ok = check_net(A5, KBSZ),
	A6 = remove(A5, 14), % From 26 To 12
	ok = check_net(A6, KBSZ),
	A7 = remove(A6, 8), % From 12 To 4
	ok = check_net(A7, KBSZ),
	choord:print_ring(p(hd(A7))),
	[] = remove(A7, 4),
	ok
    catch error:{error, Line, EGate, Str} ->
	    Res = (catch choord:print_state(p(EGate))),
	    io:format("~p: TEST FAILED: ~s~n",[Line, Str]),
	    {failed, Res};
	  error:Reason ->
	    io:format("TEST FAILED ~p~n ~p",[Reason, erlang:get_stacktrace()]),
	    error
    end.

test_up_down() ->
    KBSZ = 10,
    Props = [{key_bit_sz, KBSZ}],
    io:format("~n~nTEST NODE JOINS AND LEAVES~n",[]),
    io:format("~n~n**************************~n",[]),
    rand:seed(exs64, {12383, 55421,135412}), %% Set seed so we can debug
    [First|Keys] = make_reordered_keys(?KEY_SIZE(KBSZ), 300),
    {ok, Pid0} = choord:start([{key, First}|Props]),
    Make0 = make_fun(Pid0, Props, start),
    All = [{First, Pid0} | [{Key, Make0(Key)} || Key <- Keys]],
    try
        ok = check_net(All, KBSZ),
        A0 = remove(All, 100),
        ok = check_net(A0, KBSZ),
        ReAdd0 = lists:subtract(All, A0),
        Make1 = make_fun(Pid0, Props, start_link),
        Self = self(),
        RMPid = spawn(fun() -> rm(Self, 100, A0) end),
        Added = readd(Make1, ReAdd0, []),
        A1 = receive
            {RMPid, A, done} -> A ++ Added
            after 5000 -> exit(failed)
        end,
        ok = check_net(A1, KBSZ),
        [die(P) || P <- A1],
        ok
    catch _:Reason ->
        choord:print_state(Pid0),
        io:format("TEST FAILED ~p~n ~p", [Reason, erlang:get_stacktrace()]),
        [exit(P, failed) || {_, P} <- All],
        failed
    end.

readd(_, [], Added) -> 
    Added;
readd(Make, [{Key, _}|ReAdd], Added) ->
    timer:sleep(1),
    Pid = Make(Key),
    readd(Make, ReAdd, [{Key, Pid}|Added]).

rm(Parent, 0, Left) ->
    Parent ! {self(), Left, done};
rm(Parent, N, Left) ->
    timer:sleep(1),
    rm(Parent, N-1, remove(Left, 1)).

%% Don't use with large key sizes!! :p
make_reordered_keys(KSZ, Num) ->
    All = lists:seq(1, KSZ),
    ToSort = [{rand:uniform(), X} || X <- All],
    Sorted = lists:sort(ToSort),
    [X || {_, X} <- lists:sublist(Sorted, Num)].

make_reordered_keys(256) ->
    KL0 = [[K+16*I || K <- lists:seq(0, 15)] || I <- lists:seq(0, 15)],
    {KL1,[KL2|KL3]} = lists:split(8, KL0),
    KL4 = zip([KL2|[lists:reverse(Ks) || Ks <- lists:reverse(KL3)]]),
    {KL5, KL6} = lists:split(4, KL1),
    {KL7, KL8} = lists:split(4, KL4),
    lists:flatten(zip([lists:flatten(zip([KL5,KL7])),lists:flatten(zip([KL6,KL8]))])).

zip([[X | Xs] | Xss]) ->
    [[X | [H || [H | _] <- Xss]] | zip([Xs | [T || [_ | T] <- Xss]])];
zip([[] | Xss]) -> zip(Xss);
zip([]) -> [].

remove([Pid], 1) ->
    die(Pid),
    [];
remove(Net, N) when N > 0 ->
    Pick = rand:uniform(length(Net)-1),
    {N1,[Pid|N2]} = lists:split(Pick, Net),
    die(Pid),
    %% io:format("Kill: ~p ~p~n",[_Key, Pid]),
    remove(N1++N2, N-1);
remove(Net, _) -> Net.

die(Process) ->
    Pid = p(Process),
    unlink(Pid),
    exit(Pid, die).

p({_Key,Pid}) -> Pid;
p(Pid) -> Pid.

check_net([Gate], _KBSZ) ->
    {Pred, Pred} = choord:find_successor(p(Gate), 0),
    ok;
check_net([{_Key,_Pid}|_]=Pids, KBSZ) ->
    io:format("Check ~p nodes~n",[length(Pids)]),
    Ordered = lists:sort(Pids),
    GetSucc = successor_test(Ordered),
    check_net(0, Pids, GetSucc, KBSZ),
    Ring = [lists:last(Ordered)|Ordered] ++ Ordered,
    true = check_ring(length(Pids), Ring, Ordered),
    ok.

check_net(Id, Pids, GetSucc, KBSZ) when Id =< KBSZ ->
    KEY_SIZE = ?KEY_SIZE(KBSZ),
    Index = Id * (KEY_SIZE div KBSZ),
    ok = check_net_1((KEY_SIZE + Index - 1) rem KEY_SIZE, GetSucc, Pids),
    ok = check_net_1(Index, GetSucc, Pids),
    check_net(Id+1, Pids, GetSucc, KBSZ);
check_net(_, _, _, _) ->
    ok.

check_net_1(Id, GetSucc, [Gate|Connected]) ->
    Res = {PS = {_, Succs}, _} = find_successor(Gate, Id, 3),
    case check_id(Exp=GetSucc(Id), Succs) of
	true -> ok;
	false ->
	    Str = io_lib:format("Succs ~p: ~p: Exp ~p ~n  got ~p~n", [Gate, Id, Exp, Res]),
	    error({error, ?LINE, Gate, Str})
    end,
    case check_net_2(Connected, Id, PS) of
	ok -> ok;
	{retry, Fail, Failed} ->
	    Str1 = io_lib:format("~p: Lookup ~p: Exp ~p ~n  got ~p~n", [Fail, Id, PS, Failed]),
	    error({error, ?LINE, Gate, Str1})
    end.

check_net_2([], _, {Pred, Succs})
  when Pred =/= Succs -> ok;
check_net_2([Gate|Gates], Id, {_, Succs}=PS) ->
    case find_successor(Gate, Id, 3) of
	{{_, Succs},_} -> check_net_2(Gates, Id, PS);
	Failed -> {retry, Gate, Failed}
    end.

successor_test(Ordered) ->
    fun(Id) -> successor_test(Id, Ordered, hd(Ordered)) end.

successor_test(Id, [{Key,_}=Succs|_], _) when Id =< Key -> Succs;
successor_test(_Id, [], Succs) -> Succs;
successor_test(Id, [_|Ns], First) ->
    successor_test(Id, Ns, First).


find_successor(Gate, _Id, 0) ->
    {failed, Gate};
find_successor(Gate, Id, Retries) ->
    try debug_find_successor(Gate, Id) of
	{{_P, {id, _Key, Pid}}, _Path} = Reply ->
	    case is_process_alive(Pid) of
		true -> Reply;
		false ->
                    %% io:format("Restart find_succ ~p dead~n",[Pid]),
                    timer:sleep(10), %TODO
                    find_successor(Gate, Id, Retries-1)
	    end
    catch exit:_Reason ->
	    %% io:format("Restart ~p ~p~n",[_Reason, erlang:get_stacktrace()]),
	    timer:sleep(10), %TODO
	    find_successor(Gate, Id, Retries-1)
    end.

debug_find_successor(Gate, Id) ->
    {{Pred, Succs},Path} = find_predecessor(Gate, Id, []),
    case choord:call(p(Pred), get_successors) of
	[Succs|_] -> {{Pred, Succs}, Path};
	_ -> % Unstable net, try again
	    timer:sleep(10),
	    debug_find_successor(Gate, Id)
    end.

find_predecessor(Gate, Id, Acc) ->
    case choord:call(p(Gate), {find_predecessor, Id}) of
	{ok, Pred, Succs} -> {{Pred, Succs}, [Gate|Acc]};
	{cont, Next} -> find_predecessor(Next, Id, [Gate|Acc])
    end.


%% White box testing
check_ring(N, [_|Ordered]=All, Ord) when N > 0 ->
    check_ring_1(All, Ord) andalso
	check_ring(N-1, Ordered, Ord);
check_ring(0, _, _) -> true.

check_ring_1([Pred, This|Succs]=All, Ord) ->
    {state, ThisId, PredId, SuccsIds, Fingers, KBsz, _SLsz} = St = sys:get_state(p(This)),
    T1 = check_id(This, ThisId),
    T2 = check_id(Pred, PredId),
    T3 = check_id(hd(Succs), hd(SuccsIds)),
    T4 = check_succs(Succs, SuccsIds),
    Fs = check_fingers(ThisId, Ord, Fingers, KBsz),

    if T1, T2, T3, T4, Fs ->
	    true;
       T2, PredId =:= undefined ->
	    io:format("Pred undefined for ~p~n",[This]),
	    timer:sleep(10),
	    true = check_ring_1(All, Ord);
       not Fs ->
	    choord:print_state(St, debug),
	    error({error, ?LINE, This, "Finger failed"});
       T1, T2, T3, Fs ->
            {Ordered, _} = lists:split(length(SuccsIds),Succs),
            Str1 = io_lib:format("Succ list failed~nExp: ~p~nGot: ~p~n", [Ordered, SuccsIds]),
            error({error, ?LINE, This, Str1});
       true ->
            {Ordered, _} = lists:split(length(SuccsIds),Succs),
	    Str = io_lib:format("RING failed ~p ~p ~p =>~n  ~p ~p ~p~n",
				[Pred, This, Ordered, PredId, ThisId, SuccsIds]),
	    error({error, ?LINE, This, Str})
    end.

check_succs([Succ|All], [Id|Ids]) ->
    check_id(Succ, Id) andalso check_succs(All, Ids);
check_succs(_, []) -> true.

check_id({Key,Pid}, {id,Key,Pid}) -> true;
check_id(_, _) -> false.

check_fingers(ThisId, Ordered, Fingers, KeyBitSize) ->
    check_ranges(ThisId, Fingers, KeyBitSize) andalso
        check_fingers(Ordered, Fingers).

check_fingers(_, []) ->
    true;
check_fingers([O|Os]=Ordered, [{finger, Start, _, Node}|Fingers]) ->
    check_finger(O, Os, Start, Node) andalso
        check_fingers(Ordered, Fingers).

check_finger({_Expected, Node}, [], _, {_, _, Node}) ->
    true;
check_finger(_E, [], _S, _N) ->
    io:format("Finger failed: Start=~p, Expected: ~p, Got: ~p~n", [_S, _E, _N]),
    false;
check_finger({EKey, _}, [{Key, _}=Next|Nodes], Start, Id) when
  Key >= Start, EKey >= Start, EKey > Key ->
    check_finger(Next, Nodes, Start, Id);
check_finger({EKey, _}, [{Key, _}=Next|Nodes], Start, Id) when
  Key >= Start, EKey < Start ->
    check_finger(Next, Nodes, Start, Id);
check_finger(Expected, [_|Nodes], Start, Id) ->
    check_finger(Expected, Nodes, Start, Id).

check_ranges({id, Key, _}, Fingers, KeyBitSize) ->
    KeySize = 1 bsl KeyBitSize,
    Fun = fun(K) -> (Key + (1 bsl (K))) rem KeySize end,
    Ranges = [{Fun(K-1), Fun(K)} || K <- lists:seq(1, KeyBitSize)],
    check_ranges(Fingers, Ranges).

check_ranges([], []) ->
    true;
check_ranges([{finger, Start, Last, _}|Fingers], [{Start, Last}|StartLasts]) ->
    check_ranges(Fingers, StartLasts);
check_ranges(_Fs, _SLs) ->
    false.

make_fun(Pid0, Props, Start) ->
    fun(Key) ->
        {ok, Pid} = choord:Start([{gates,[Pid0]},{key, Key}|Props]),
        Pid
    end.

