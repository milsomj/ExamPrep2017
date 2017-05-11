s --> a(N),b(M),c(K), {K is N + M}, {N + M + K =:= 2*N + 2*M}.
a(0) --> [].
a(NN) --> [0], a(N), {NN is N + 1}.
b(1) --> [].
b(MM) --> [1], b(M), {MM is M + 1}.
c(2) --> [].
c(KK) --> [2], c(K), {KK is K + 1}.
