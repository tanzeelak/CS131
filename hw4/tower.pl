tower(0, [], counts([],[],[],[])).
tower(1,[[1]], counts([1],[1],[1],[1])).
tower(N,T,counts(Top, Bottom, Left, Right)) :-
    isMatrixValid(T, N).

%Transpose Clauz
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

%Reverse Clauz
myReverse([],[]) :- !.
myReverse([H|T],X) :-
    !,
    myReverse(H,NewH),
    myReverse(T, NewT),
    append(NewT, [NewH], X).
myReverse(X,X).

%Matrix Contraintz
rowLength(N, List) :-
    length(List, N).

checkRange(N, List) :-
    fd_domain(List, 1, N).

goodDomainLists(M, N) :-
    maplist(checkRange(N),M),
    maplist(fd_all_different,M),
    maplist(fd_labeling,M).

isMatrixValid(M,N) :-
    length(M, N),
    maplist(rowLength(N), M),
    goodDomainLists(M,N),
    transpose(M,TransposedM),
    goodDomainLists(TransposedM,N).

%Iterate thru matrix ;;;;;)
list_empty([], true).
list_empty([_|_], false).

checkRow(_,0,[]).
checkRow(MaxHeight,TowerCnt,[RowHead|RowTail]) :-
 %   TowerCnt >= 0,
 %   list_empty([RowHead|RowTail],false),
    RowHead #> MaxHeight,
    NewTowerCnt is TowerCnt-1,
    checkRow(RowHead,NewTowerCnt,RowTail).
checkRow(MaxHeight,TowerCnt,[RowHead|RowTail]) :-
%    TowerCnt >= 0,
%    list_empty([RowHead|RowTail],false),
%    RowHead #=<# MaxHeight,
    checkRow(MaxHeight,TowerCnt,RowTail).

countLeft([],[]).
countLeft([MHead|MTail],[LeftHead|LeftTail]) :-
    checkRow(0,LeftHead,MHead),
    countLeft(MTail,LeftTail).
