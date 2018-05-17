%tower 3

tower(0, [], counts([],[],[],[])).
tower(1,[[1]], counts([1],[1],[1],[1])).
tower(N,T,counts(Top, Bottom, Left, Right)) :-
    isTValid(T, N),
    transpose(T,TransposeT),
    isTValid(TransposeT,N),
    isCountsValid(counts(Top,Bottom,Left,Right),N),
    maplist(fd_labeling, T),
    generateRes(N,T,counts(Top,Bottom,Left,Right)).

generateRes(N,T, counts(Top, Bottom, Left, Right)) :-
    length(Right, N),
    length(Left, N),
    reverse(Right,RevRight),
    reverse(Top,RevTop),
    countSide(T,Left),
    rotate90(T, TRotate),
    countSide(TRotate, Bottom),
    rotate90(TRotate, TRotate2),
    countSide(TRotate2, RevRight),
    rotate90(TRotate2, TRotate3),
    countSide(TRotate3, RevTop).

%Rotate Clauz
rotate90(M, MRotate) :-
    transpose(M, MTranspose),
    maplist(reverse,MTranspose,MRotate).

%Transpose Clauz
% Used the following link: https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
            lists_firsts_rests(Rest, Fs, Oss).

%Matrix Contraintz :o
rowLength(N, List) :-
    length(List, N).

checkRange(N, List) :-
    fd_domain(List, 1, N).

goodDomainLists(M, N) :-
    maplist(checkRange(N),M),
    maplist(fd_all_different,M).

isValidSide(Side, N) :-
    length(Side, N),
    fd_domain(Side, 1, N).

isTValid(M,N) :-
    length(M,N),
    maplist(rowLength(N), M),
    goodDomainLists(M,N).

isCountsValid(counts(Top, Bottom, Left, Right), N) :-
    isValidSide(Top, N),
    isValidSide(Bottom, N),
    isValidSide(Left, N),
    isValidSide(Right, N).

%Iterate thru matrix
listEmpty([], true).
listEmpty([_|_], false).

checkRow(CurrCnt,_,TowerCnt,[]) :-
    TowerCnt #=# CurrCnt.
checkRow(CurrCnt,MaxHeight,TowerCnt,[RowHead|RowTail]) :-
    listEmpty([RowHead|RowTail],false),
    RowHead #> MaxHeight,
    NewCurrCnt is CurrCnt+1, !,
    checkRow(NewCurrCnt,RowHead,TowerCnt,RowTail).
checkRow(CurrCnt,MaxHeight,TowerCnt,[RowHead|RowTail]) :-
    listEmpty([RowHead|RowTail],false),
    RowHead #=< MaxHeight, !,
    checkRow(CurrCnt,MaxHeight,TowerCnt,RowTail).

countSide([],[]).
countSide([MHead|MTail],[SideHead|SideTail]) :-
    checkRow(0,0,SideHead,MHead),
    countSide(MTail,SideTail).

%plain_tower 3 

plain_tower(0, [], counts([],[],[],[])).
plain_tower(1,[[1]], counts([1],[1],[1],[1])).
plain_tower(N,T,counts(Top, Bottom, Left, Right)) :-
    isPlainTValid(T, N),
    transpose(T,TransposeT),
    isPlainTValid(TransposeT,N),
    isPlainCountsValid(counts(Top,Bottom,Left,Right),N),
    generateRes(N,T, counts(Top,Bottom,Left,Right)).

%Plain Matrix Contraintz 
isGoodNum(X,N) :-
    X #> 0,
    X #=< N.

checkPlainRange(N, []).
checkPlainRange(N, [Head|Tail]) :-
    isGoodNum(Head, N),
    checkPlainRange(N, Tail).

checkPlainUniq(N,R):-
    findall(Num, between(1, N, Num), L).
    permutation(L,R).

goodPlainDomainLists(M, N) :-
    maplist(checkPlainRange(N),M),
    maplist(checkPlainUniq(N),M),
    rotate90(M,MRotate),
    maplist(thirdDiff(N),MRotate).

isPlainValidSide(Side, N) :-
    length(Side, N),
    checkPlainRange(N, Side).

isPlainTValid(M,N) :-
    length(M,N),
    maplist(rowLength(N), M),
    goodPlainDomainLists(M,N).

isPlainCountsValid(counts(Top, Bottom, Left, Right), N) :-
    isPlainValidSide(Top, N),
    isPlainValidSide(Bottom, N),
    isPlainValidSide(Left, N),
    isPlainValidSide(Right, N).

%Ambiguous

compareMatrices([],[]).
compareMatrices([[1]],[[1]]).
compareMatrices([T1Head|T1Tail],[T2Head|T2Tail]) :-
    compare(=,T1Head,T2Head),
    compareMatrices(T1Tail,T2Tail).

ambiguous(N,C,T1,T2) :-
    tower(N,T1,C),
    tower(N,T2,C),
\+ (T1 = T2).


%SpeedUp

speedup(Ratio) :-
    statistics(cpu_time, [T1Tower|_]),
    tower(4,T, counts([3,3,2,1],[2,1,3,3],[4,2,1,2],[1,2,4,2])),
    statistics(cpu_time,[T2Tower|_]),
    TowerTime is T2Tower - T1Tower + 1,
    statistics(cpu_time, [T1PlainTower|_]),
    plain_tower(4,T, counts([3,3,2,1],[2,1,3,3],[4,2,1,2],[1,2,4,2])),
    statistics(cpu_time,[T2PlainTower|_]),
    PlainTowerTime = T2PlainTower - T1PlainTower + 1,
    Ratio is PlainTowerTime/TowerTime.
