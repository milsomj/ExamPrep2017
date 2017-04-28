incr(f0(null),f1(null)).
incr(null,f1(null)).
incr(f0(X), f1(X)).
incr(f1(X), f0(Y)) :- incr(X,Y).

legal(f0(null)).
legal(X) :- legal(Y), incr(Y,X).

incrR(X,Y) :- legal(X),incr(X,Y).

add(X,f0(null),X).
add(f0(null),X,X).
add(X,Y,Z) :- incr(X,P), incr(Q,Y), add(P, Q, Z).

mult(X,f0(null),f0(null)).
mult(f0(null),X,X).
mult(X,Y,Z) :- add(f0(null),X,P),incr(R,Y), mult(P, R, Z).


append(f1(X),Y,f1(Y)).
append(f0(X),Y,f0(Y)).
truncate(f1(X),X).
truncate(f0(X),X).

revers(null,Y,Y).
revers(X,Y) :- append(X,null,P), truncate(X,Q), revers(Q,Y,P).
revers(X,Y,Z) :- append(X,Z,P), truncate(X,Q), revers(Q,Y,P).

normalize(X,Y) :- revers(X,P), norm(P,Q), revers(Q,Y).
norm(X,f1(Y)).
norm(X,f0(Y)) :- norm(X,Y).


% test add inputting numbers N1 and N2
testAdd(N1,N2,T1,T2,Sum,SumT) :- numb2pterm(N1,T1), numb2pterm(N2,T2),
add(T1,T2,SumT), pterm2numb(SumT,Sum).
% test mult inputting numbers N1 and N2
testMult(N1,N2,T1,T2,N1N2,T1T2) :- numb2pterm(N1,T1), numb2pterm(N2,T2),
mult(T1,T2,T1T2), pterm2numb(T1T2,N1N2).
% test revers inputting list L
testRev(L,Lr,T,Tr) :- ptermlist(T,L), revers(T,Tr), ptermlist(Tr,Lr).
% test normalize inputting list L
testNorm(L,T,Tn,Ln) :- ptermlist(T,L), normalize(T,Tn), ptermlist(Tn,Ln).
% make a pterm T from a number N    numb2term(+N,?T)
numb2pterm(0,f0(null)).
numb2pterm(N,T) :- N>0, M is N-1, numb2pterm(M,Temp), incr(Temp,T).
% make a number N from a pterm T  pterm2numb(+T,?N)
pterm2numb(null,0).
pterm2numb(f0(X),N) :- pterm2numb(X,M), N is 2*M.
pterm2numb(f1(X),N) :- pterm2numb(X,M), N is 2*M +1.
% reversible  ptermlist(T,L)
ptermlist(null,[]).
ptermlist(f0(X),[0|L]) :- ptermlist(X,L).
ptermlist(f1(X),[1|L]) :- ptermlist(X,L).
