tower(0,[],C) :-
    counts(C).
tower(N,T,counts(Top,Bottom, Left, Right)) :-
/*    tower(N,[Head:Tail],C), 
    isMatrix(T, Rows, Cols), */
    isMatrixValid(T, Rows, Cols, N)
		   .

rowLength(N, List) :-
    length(List, N).

isMatrix(M, Row, Col) :-
    length(M, Row),
    maplist(rowLength(Col), M).

isMatrixValid(M, Row, Col, N) :-
    length(M, Row),
    maplist(rowLength(Col), M),
    Row=N,
    Col=N,
    fd_domain(Row, 0, N).


accessMatrixIndex(M, I, J, Val) :-
    nth0(I, M, Row),
        nth0(J, Row, Val).


matrixHasNLists(T, N) :-
    findall(Val, accessMatrixIndex(T, _, _, Val), Row),
    write(Row)
.

counts([],[],[],[]).
counts(C) :-
    counts([H1:T1],[H2:T2],[H3:T3],[H4:T4]).
    
    

