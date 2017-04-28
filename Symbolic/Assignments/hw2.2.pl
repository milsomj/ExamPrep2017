s2(ACount,A2, BCount,B2,CCount,C2) --> [0],{AC2 is ACount + 2},s2(AC2,A2,BCount,B2,CCount,C2),[0].
s2(ACount,A2, BCount,B2,CCount,C2) --> [1],{BC2 is BCount + 2},s2(ACount,A2,BC2,B2,CCount,C2),[1].
s2(ACount,A2, BCount,B2,CCount,C2) --> [2],{CC2 is CCount + 2},s2(ACount,A2,BCount,B2,CC2,C2),[2].
s2(ACount,ACount,BCount,BCount,CCount,CCount) --> [].
s2(ACount,A2,BCount,BCount,CCount,CCount) --> [0], {A2 is ACount + 1}.
s2(ACount,ACount,BCount,B2,CCount,CCount) --> [1], {B2 is BCount + 1}.
s2(ACount,ACount,BCount,BCount,CCount,C2) --> [2], {C2 is CCount + 1}.

s --> s2(0,A2,0,B2,0,C2),[3],k(A2),n(B2),kn(C2),{C2 is A2 * B2}.

k(0)  -->  [].
k(NewCount)  -->  [0],k(Count), {NewCount  is  Count  +  1}.

n(0)  -->  [].
n(NewCount)  -->  [1],n(Count), {NewCount  is  Count  +  1}.

kn(0)  -->  [].
kn(NewCount)  -->  [2],kn(Count), {NewCount  is  Count  +  1}.
