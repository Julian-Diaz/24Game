% ----- Aux -----

solve(const(X), X).
solve(add(X, Y), Z) :- solve(X, X1), solve(Y, Y1), Z is X1+Y1.
solve(diff(X, Y), Z) :- solve(X, X1), solve(Y, Y1), Z is X1-Y1,
    Z >= 0.
solve(mult(X, Y), Z) :- solve(X, X1), solve(Y, Y1), Z is X1*Y1.
solve(divi(X, Y), Z) :- solve(X, X1), solve(Y, Y1),
    Y1 \= 0, Z is X1/Y1.


auxCreateExpr(Xs, Y, E) :- length(Xs, N), N > 1, member(Y, Xs),
    deleteFirstOcc(Xs, Y, Xs1), createExpr(Xs1, E).

createExpr([X], const(X)).
createExpr(Xs, add(const(Y), E)) :- auxCreateExpr(Xs, Y, E).
createExpr(Xs, diff(const(Y), E)) :- auxCreateExpr(Xs, Y, E).
createExpr(Xs, diff(E, const(Y))) :- auxCreateExpr(Xs, Y, E).
createExpr(Xs, mult(const(Y), E)) :- auxCreateExpr(Xs, Y, E).
createExpr(Xs, divi(const(Y), E)) :- auxCreateExpr(Xs, Y, E).
createExpr(Xs, divi(E, const(Y))) :- auxCreateExpr(Xs, Y, E).

deleteFirstOcc([], _, []).
deleteFirstOcc([X|Xs], X, Xs).
deleteFirstOcc([X|Xs], Y, [X|Ys]) :- X \= Y, deleteFirstOcc(Xs, Y, Ys).

init(Xs) :- write('Enter numbers \n'),
    write('First number \n'),
    read(X1),
    write('Second number \n'),
    read(X2),
    write('Third number \n'),
    read(X3),
    write('Fourth number \n'),
    read(X4),
    Xs = [X1, X2, X3, X4].
% ---------------

const(_).
add(_, _).
diff(_, _).
mult(_, _).
divi(_, _).


solution(Xs, E) :- createExpr(Xs, E), solve(E, 24).
