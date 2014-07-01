-module(okey_reveal).
-author('Vitaly Voronov').
-compile(export_all).

main() ->
    L0 = [{1,1}, {6,6}, {4,4}, {5,5},{3,3}, okey, {2,2}, 
        {4,4},   {6,6},
        {7,7}, okey, {2,2}, {5,5},  {3,3}],

    L1 = [{4,4},{4,3},{4,6},{4,7},{4,8},
    {3,10},{3,11},{3,12},{3,1},{3,13},
        {2,6},{3,6},{1,6},okey],

    L2 = [{1,7},{1,6},{1,5},{1,4},{1,3},
    {2,9},{2,10},{2,11},{2,12},{2,13},
    {1,1},{2,1},{3,1},{4,1}],

    L3 = [{1,1},{2,1},{3,1},
          {1,3},{2,3},{3,3},
          {1,5},{2,5},{3,5},
          okey,okey,{2,8},{2,3},
          {2,9}],

    L4 = [{1,1},{2,1},{3,1},
        {1,3},{2,3},{3,3},
        {1,5},{2,5},{3,5},
        okey,okey,{2,10},{2,6},
        {2,9}],
        
    L5 = [{3,13},{3,13},{4,11},{4,11},{1,7},{1,2},{2,11},{4,13},{4,10},{4,10},{1,7},{1,2},{2,11},{4,13}],
    
    Tests = [L0,L1,L2,L4,L3,L5],
    [ check_reveal(L) || L <- Tests].
%    check_reveal(L5).

check_reveal(L1) -> 
    L11 = element(1, lists:mapfoldl(fun(X, Acc) when is_atom(X) -> {{okey, okey, Acc}, Acc+1};
                (X, Acc) -> {erlang:append_element(X, Acc), Acc+1} end, 1, L1)),
    L = lists:foldl(fun(X, Acc) when element(2, X) == 1 -> lists:append(lists:append(Acc, [X]), [{element(1, X), 14, element(3, X)}]);
                (X, Acc) -> lists:append(Acc, [X])
        end, [], L11),

    L2 = lists:usort(get2(L)),
    L3 = lists:usort(get3(L)),
    L4 = lists:usort(get4(L)),
    L5 = lists:usort(get5(L)),

    {G1,R1} = check2x7(L2),
    {G2,R2} = check3x3_5x1(L3, L5),
    {G3,R3} = check3x2_4x2(L3, L4),
    {G4,R4} = check4x1_5x2(L4, L5),
    (G1 == true) or (G2 == true) or (G3 == true) or (G4 == true),
    [{Name,Res}||{Name,Res}<-[{x2x7,R1},{x3x5,R2},{x3x4,R3},{x4x5,R4}],Res/=[]].

%% Match pair
get2(L) -> get2(combs_out(L, 2), []).
get2([], Res) -> Res;
get2([H | T], Res) -> 
    [H1|T1]=lists:sort(fun(X1, X2) -> element(2, X1) =< element(2, X2) end, H),
    get2(T, [checksamepair(H1, [H1|T1], [])|Res]).

%% Match 3
get3(L) -> get3(combs_out(L, 3), []).
get3([], Res) -> Res;
get3([H|T], Res) -> 
    [H1|T1] = lists:sort(fun(X1, X2) -> element(2, X1) =< element(2, X2) end, H),
    Lres = [checksame(H1, [H1|T1], []), checkseq(H1, T1, [H1|T1])],
    get3(T, lists:append(lists:filter(fun(X) -> length(X) > 2 end, Lres), Res)).

%% Match 4
get4(L) -> get4(combs_out(L, 4), []).
get4([], Res) -> Res;
get4([H|T], Res) -> 
    [H1|T1]=lists:sort(fun(X1, X2) -> element(2, X1) =< element(2, X2) end, H),
    Lres = [checksame(H1, [H1|T1], []), checkseq(H1, T1, [H1|T1])],
    get4(T, lists:append(lists:filter(fun(X) -> length(X) > 2 end,Lres),Res)).

%% Match 5 
get5(L) -> get5(combs_out(L, 5), []).
get5([], Res) -> Res;
get5([H|T], Res) -> 
    [H1|T1] = lists:sort(fun(X1, X2) -> element(2, X1) =< element(2, X2) end, H),
    Lres = [checkseq(H1, T1, [H1|T1])],
    get5(T, lists:append(lists:filter(fun(X) -> length(X) > 2 end,Lres),Res)).

%%Check same pair
checksamepair(X, L, []) when is_atom(element(2, X)) -> L;
checksamepair(X, [H1|T], Res) when is_atom(element(2, H1)) -> checksamepair(X, T, [H1|Res]);
checksamepair(X, [H1|T], Res) when (element(2, X) == element(2, H1)) and (element(1, X) == element(1, H1)) -> checksamepair(X, T, [H1|Res]);
checksamepair(X, [H1|_T], _Res) when (element(2, X) /= element(2, H1)) or (element(1, X) /= element(1, H1)) -> [];
checksamepair(_X, [], Res) -> Res.

%%Check same (three and four)
checksame(X, L, []) when is_atom(element(2, X)) -> L;
checksame(X, [H1|T], Res) when is_atom(element(2, H1)) -> checksame(X, T, [H1|Res]);
checksame(X, [H1|T], Res) when element(2, X) == element(2, H1) -> 
    case lists:keysearch(element(1, H1), 1, Res) of
        {value, _} -> []; false -> checksame(X, T, [H1|Res]) end;
checksame(X, [H1|_T], _Res) when element(2, X) /= element(2, H1) -> [];
checksame(_X, [], Res) -> Res.

%% Check sequence
checkseq(X, _T, F) when is_atom(element(2, X)) -> F;
checkseq(_X, [H1|_T], F) when is_atom(element(2, H1)) -> F;
checkseq(X, [H1|_T], _F) when (element(1, X) /= element(1, H1)) -> [];
checkseq(X, [H1|[]], _F) when (element(1, X) == element(1, H1)) and ((element(2, X) + 1) /= element(2, H1)) -> []; 
checkseq(X, [H1|T], F) when (element(1, X) == element(1, H1)) and ((element(2, X) + 1) /= element(2, H1)) -> 
    [H2|T2] = lists:reverse(T),
    if is_atom(element(1, H2)) -> checkseq({element(1,X), element(2,X)+1,element(3,H2)}, [H1] ++ lists:reverse(T2), F);
       true -> [] end;
checkseq(X, [H1|T], F) when (element(1, X) == element(1, H1)) and ((element(2, X) + 1) == element(2, H1)) -> checkseq(H1, T, F);
checkseq(_X, [], F) -> F.

%%%%%%%%%%%%%

check2x7(L2) -> 
    L2comb=lists:filter(fun(X) -> length(X) > 2 end,combs_clb(L2, [], 7)),
    case length(L2comb) > 0 of
        true -> {true, [ lists:map(fun({okey,okey,_}) -> okey;
                                    ({C,14,X}) -> {C,1};
                                   ({C,V,_}) -> {C,V} end,L)
                           || L <- lists:nth(length(L2comb),L2comb)]};
        false-> {false,[]} end.

check3x3_5x1(L3, L5) -> check_comb(combs_clb(L3, [], 3), L5).
check3x2_4x2(L3, L4) -> check_comb(combs_clb(L3, [], 2), combs_clb(L4, [], 2)).
check4x1_5x2(L4, L5) -> check_comb(combs_clb(L5, [], 2), L4).

%% Check combination from two lists.
check_comb([], _L2) -> {false,[]};
check_comb([H1|T1], L2) ->
    case check_list(H1, L2) of
        {true,L} -> {true,L};
        {false,_} -> check_comb(T1, L2) end.

check_list(_X, []) -> {false,[]};
check_list(X, [H|T]) ->
    case checkuniq(myflatten(X), myflatten(H)) of
        true -> 
            {true, [ lists:map(fun({okey,_,_}) -> okey;
                                    ({C,14,X}) -> {C,1};
                ({C,V,_}) -> {C,V} end,L) || L <- lists:merge(norm(X),norm(H)) ]};
        false -> check_list(X, T) end.

series([]) -> false;
series(X) -> lists:all(fun(A)->is_tuple(A) end,X).
norm(X) -> case series(X) of true -> [X]; false -> X end.

%% Make list flatten as lists:flatten.
myflatten([]) -> [];
myflatten(X) when is_tuple(X)-> [X];
myflatten([H|T]) -> myflatten(H) ++ myflatten(T).

%% Combine
combs_out(List, Number) -> combs_out(List, [], Number).
combs_out(_Remain, Result, Number) when length(Result) == Number -> [Result];
combs_out([], _Result, _Number) -> [];
combs_out([RemainH|RemainT], Result, Number) ->
    combs_out(RemainT, Result, Number) ++ combs_out(RemainT, [RemainH|Result], Number).

%% Combine with unique card check
combs_clb(Remain, Result, Number) -> combs_clb(Remain, Result, Number, []).
combs_clb(_Remain, Result, Number, _FL) when length(Result) == Number -> [Result];
combs_clb([], _Result, _Number, _FL) -> [];
combs_clb([RemainH|RemainT], Result, Number, FL) ->
    Lres = combs_clb(RemainT, Result, Number, FL),
    RFL = RemainH ++ FL,
    Res = checkuniq(RemainH, FL),
    if Res == true -> combs_clb(RemainT, [RemainH|Result], Number, RFL) ++ Lres;
       true -> Lres end.

%% Check uniqueness of one list to another.
checkuniq([], L) -> true;
checkuniq([H|T], L) -> case lists:keysearch(element(3, H), 3, L) of {value, _} -> false;
                                       false -> checkuniq(T, L) end.

