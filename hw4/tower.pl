tower(0, [], counts([],[],[],[])).
tower(1,[[1]], counts([1],[1],[1],[1])).
tower(N,T,counts(Top, Bottom, Left, Right)) :-
    isMatrixValid(T, N),
    countSide(T,Left),
    rotate90(T, TRotate),
    countSide(TRotate, Bottom),
    rotate90(TRotate, TRotate2),
    countSide(TRotate2, Right),
    rotate90(TRotate2, TRotate3),
    countSide(TRotate3, Top)
.

%Rotate Clauz
rotate90(M, MRotate) :-
    transpose(M, MTranspose),
    maplist(reverse,MTranspose,MRotate).

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

%Reverz Clauz ;PPPPPPP

reverseMatrix([],RevMatrix).
reverseMatrix([MHead|MTail],RevMatrix) :-
    reverse(MHead, RevHead),
    append([RevHead],RevMatrix,Res),
    reverseMatrix(MTail,Res).

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

checkRow(CurrCnt,_,TowerCnt,[]) :-
    TowerCnt is CurrCnt.
checkRow(CurrCnt,MaxHeight,TowerCnt,[RowHead|RowTail]) :-
    list_empty([RowHead|RowTail],false),
    RowHead #> MaxHeight,
    NewCurrCnt is CurrCnt+1,
    checkRow(NewCurrCnt,RowHead,TowerCnt,RowTail).
checkRow(CurrCnt,MaxHeight,TowerCnt,[RowHead|RowTail]) :-
    list_empty([RowHead|RowTail],false),
    RowHead #=< MaxHeight,
    checkRow(CurrCnt,MaxHeight,TowerCnt,RowTail).

countSide(_,[],[]).
countSide([MHead|MTail],[SideHead|SideTail]) :-
    checkRow(0,0,SideHead,MHead),
    countSide(MTail,SideTail).
