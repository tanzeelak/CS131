tower(N,T,counts(Top, Bottom, Left, Right)) :-
    isMatrixValid(T, RowCnt, ColCnt, N)
  /*  topCount(T,Top) */
		   .

rowLength(N, List) :-
    length(List, N).

checkRange(N, List) :-
    fd_domain(List, 0, N).

isMatrixValid(M, RowCnt, ColCnt, N) :-
    length(M, RowCnt), /* length(?List,?Int): counts num of rows */
    maplist(rowLength(ColCnt), M), /* currying of matrix of lists: counts num of cols */
    RowCnt#=N,
    ColCnt#=N,
    /* needs to check rows not counts */
    maplist(checkRange(N), M)
	   .



accessMatrixIndex(M, I, J, Val) :-
    nth0(I, M, Row),
        nth0(J, Row, Val).

matrixHasNLists(T, N) :-
    findall(Val, accessMatrixIndex(T, _, _, Val), Row),
    write(Row).

list_empty([], true).
list_empty([_|_], false).

checkRow(_,0,[]).
checkRow(MaxHeight,TowerCnt,[RowHead|RowTail]) :-
    TowerCnt >= 0,
    list_empty([RowHead|RowTail],false),
    RowHead #># MaxHeight,
    NewMaxHeight is RowHead,
    NewTowerCnt is TowerCnt-1,!,
    checkRow(NewMaxHeight,NewTowerCnt,RowTail).
checkRow(MaxHeight,TowerCnt,[RowHead|RowTail]) :-
    TowerCnt >= 0,
    list_empty([RowHead|RowTail],false),
    RowHead #=<# MaxHeight,!,
    checkRow(MaxHeight,TowerCnt,RowTail).

countLeft([MHead:MTail],[LeftHead:LeftTail]) :-
    checkRow(0,LeftHead,MHead),
    countLeft(MTail,LeftTail).
