tower(N,T,counts(Top, Bottom, Left, Right)) :-
    isMatrixValid(T, RowCnt, ColCnt, N)
  /*  topCount(T,Top) */
		   .

rowLength(N, List) :-
    length(List, N).

isMatrix(M, Row, Col) :-
    length(M, Row),
    maplist(rowLength(Col), M).

isMatrixValid(M, RowCnt, ColCnt, N) :-
    length(M, RowCnt), /* length(?List,?Int): counts num of rows */
    maplist(rowLength(ColCnt), M), /* currying of matrix of lists: counts num of cols */
    RowCnt=N,
    ColCnt=N,
    fd_domain(RowCnt, 0, N).


accessMatrixIndex(M, I, J, Val) :-
    nth0(I, M, Row),
        nth0(J, Row, Val).

matrixHasNLists(T, N) :-
    findall(Val, accessMatrixIndex(T, _, _, Val), Row),
    write(Row).

/*topCount(M, Top) :-
*/
    
