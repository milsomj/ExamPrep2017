%2. (a)
% (i)
fac(0,1).
fac(N,F) :- Next is N-1, fac(Next,FNew), F is N * FNew.
% (ii)
fac(0,1).
fac(N,F) :- fact(N,1,F).
fact(1,F,F).
fact(N,Acc,F) :- Next is N-1, NewAcc is N * Acc, fact(Next,NewAcc,F).

%2. (b)
% (i)
fib(0,0).
fib(1,1).
fib(N,F) :- N1 is N-2,N2 is N-1,fib(N1,F1),fib(N2,F2),F is F1 + F2.
% (ii)
