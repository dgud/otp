-module(choord_test).

-export([test/0, paper_example/0, test_fast_joins/0, test_multiple_exits/0, debug/0]).

-define(KEY_SIZE(BIT_SIZE), (1 bsl (BIT_SIZE))).

%% Temporary testing

test() ->
    io:format("Starting all tests~n",[]),
    paper_example(),
    test_fast_joins(),
    test_multiple_exits(),
    test_256_nodes().

paper_example() ->
    KBSZ = 3,
    Props = [{key_bit_sz, KBSZ}],
    io:format("~n~nPAPER EXAMPLE TEST~n",[]),
    io:format("~n********************~n~n"),
    {ok, P1} = choord:start([{key,0}|Props]),
    {ok, P2} = choord:start([{gates,[P1]}, {key,3}|Props]),
    ok = check_net([P1,P2], KBSZ),
    {ok, P3} = choord:start([{gates,[P1]}, {key,1}|Props]),
    ok = check_net([P1,P2,P3], KBSZ),
    {ok, P4} = choord:start([{gates,[P1]}, {key,6}|Props]),
    ok = check_net([P1,P2,P3,P4], KBSZ),
    ok = choord:print_state(P1),
    ok = choord:print_ring(P1),
    exit(P3, die),
    ok = check_net([P1,P2,P4], KBSZ),
    exit(P2, die),
    ok = check_net([P1,P4], KBSZ),
    ok = choord:print_state(P1),
    ok = choord:print_ring(P1),
    exit(P1, die),
    exit(P4, die),
    ok.

test_256_nodes() ->
    KBSZ = 8,
    Props = [{key_bit_sz, KBSZ}],
    io:format("~n~nTEST 256 NODES~n",[]), 
    io:format("~n~n**************~n",[]), 
    rand:seed(exs64, {12383, 55421,135412}), %% Set seed so we can debug
    [First|Keys] = make_reordered_keys(?KEY_SIZE(KBSZ)),
    {ok, Pid0} = choord:start([{key, First}|Props]),
    Make = fun(Key) ->
		{ok, Pid} = choord:start_link([{gates,[Pid0]},{key, Key}|Props]),
		Pid
	   end,
    All = [{First, Pid0} | [{Key, Make(Key)} || Key <- Keys]],
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
    choord:print_ring(p(hd(A3))),
    ok.

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

remove(Net, N) when N > 0 ->
    Pick = rand:uniform(length(Net)-1),
    {N1,[{Key, Pid}|N2]} = lists:split(Pick, Net),
    unlink(Pid), exit(Pid, die),
    io:format("Kill: ~p ~p~n",[Key, Pid]),
    remove(N1++N2, N-1);
remove(Net, _) -> Net.


p({_Key,Pid}) -> Pid;
p(Pid) -> Pid.

check_net([Gate], _KBSZ) ->
    {Pred, Pred} = choord:find_successor(p(Gate), 0),
    ok;
check_net(Pids, KBSZ) when is_list(Pids) ->
    check_net(0, Pids, KBSZ).

check_net(Id, Pids, KBSZ) when Id =< KBSZ ->
    KEY_SIZE = ?KEY_SIZE(KBSZ),
    Index = Id * (KEY_SIZE div KBSZ),
    ok = check_net_1((KEY_SIZE + Index - 1) rem KEY_SIZE, Pids),
    ok = check_net_1(Index, Pids),
    check_net(Id+1, Pids, KBSZ);
check_net(_, _, _) ->
    ok.

check_net_1(Id, [Gate|Connected]) ->
    case check_net_1(Connected, Id, find_successor(Gate, Id, 3)) of
	ok -> ok;
	{retry, _, _} ->
	    PS = choord:find_successor(p(Gate), Id),
	    case check_net_1(Connected, Id, PS) of
		ok -> ok;
		{retry, Fail, Failed} ->
		    io:format("~p:~p: Exp ~p got ~p~n", [Fail, Id, PS, Failed]),
		    error
	    end
    end.

check_net_1([], _, {Pred, Succs})
  when Pred =/= Succs -> ok;
check_net_1([Gate|Gates], Id, PS) ->
    case find_successor(Gate, Id, 3) of
	PS -> check_net_1(Gates, Id, PS);
	Failed -> {retry, Gate, Failed}
    end.

find_successor(_Gate, _Id, 0) ->
    failed;
find_successor(Gate, Id, Retries) ->
    case catch choord:find_successor(p(Gate), Id) of
        {'EXIT', _} ->
            timer:sleep(10), %TODO
            find_successor(Gate, Id, Retries-1);
        Reply ->
            Reply
    end.

%%%%%%%%%%%%
%% Debug

debug() ->
    i:ii(choord),
    i:ib(choord, handle_call, 3),
    i:ib(choord, handle_cast, 2),
    i:ib(choord, update_others, 3).

test_fast_joins() ->
    io:format("~n~nTESTING MULTIPLE AND FAST JOINS~n",[]),
    io:format("~n*********************************~n~n"),
    KBSZ = 3,
    Props = [{key_bit_sz, KBSZ}],
    {ok, P1} = choord:start([{key,0}|Props]),
    Gates = join(Props, [5, 2, 6, 3, 7, 1, 4], [P1]),
    ok = check_net(Gates, KBSZ),
    ok = choord:print_state(P1),
    ok = choord:print_ring(P1),
    [exit(P, die) || P <- Gates],
    ok.

test_multiple_exits() ->
    io:format("~n~nTESTING MULTIPLE AND FAST EXITS~n",[]),
    io:format("~n*********************************~n~n"),
    KBSZ = 3,
    Props = [{key_bit_sz, KBSZ}],
    {ok, P1} = choord:start([{key,0}|Props]),
    Gates = join(Props, [5, 2, 6, 3, 7, 1, 4], [P1]),
    ok = check_net(Gates, KBSZ),
    Exits = [P1, hd(Gates), hd(tl(Gates))],
    io:format("Killing: ~p~n", [Exits]),
    [exit(P, die) || P <- Exits],
    timer:sleep(100),
    Left = lists:subtract(Gates, Exits),
    ok = check_net(Left, KBSZ),
    ok = choord:print_ring(hd(Left)),
    [exit(P, die) || P <- Left],
    ok.

join(_, [], Gates) ->
    Gates;
join(Props, [Key|Keys], Gates) ->
    {ok, P} = choord:start([{gates,Gates}, {key,Key}|Props]),
    join(Props, Keys, [P|Gates]).

