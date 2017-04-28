% 1. (a) Prolog Responses
X = 1.                        % X = 1.
X == Y.                       % false.
0 + 1 = 1 + 0.                % false.
0 + 1 =:= 1 + 0               % false.
X \= f(X).                    % true.
f(X) \= g(Y)                  % true.
[1|[2,3]] = .(1,.(2,[3])).    % true.
X == f(X).                    % false.
X > 0.                        % false.
findall(X, X\=1, L).          % []

% 1. (b) Write isSet
isSet([_|[]]).
isSet([X|Y]) :- not(member(X,Y)), isSet(Y).

%1. (c) Write MoreThanOne (checks if more than one distinct member)
moreThanOne([X|Y]) :- diff(X,Y).
diff(X,[H|_]) :- H \= X.
diff(X,[_|T]) :- diff(X,T).

%1. (d) Write MoreThan (more than N distinct members)
moreThan(LS,Num) :- makeSet(LS,[],Set),length(Set,L), L > Num.
makeSet([H|T],Set,R) :- member(H,Set), makeSet(T,Set,R).
makeSet([H|T],Set,R) :- makeSet(T,[H|Set],R), not(member(H,Set)).
makeSet([],Set,Set).
