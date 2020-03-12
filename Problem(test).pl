processes([p1,p2,p3,p4]).

available_resources([[r1, 0], [r2, 0]]).

allocated(p1, [r2]).
allocated(p2, [r1]).
allocated(p3, [r1]).
allocated(p4, [r2,r1,r2,r2]).

requested(p1, [r1]).
requested(p3, [r2]).

safePath([],[]):-!.

safePath([H|T],S):-
requested(H,_),
safePath(T,S),!.

safePath([H|T],[H|T1]):-
safePath(T,T1).


getRequesters([],_,[]).
getRequesters([H1|T1],T2,R):-
member(H1,T2),
getRequesters(T1,T2,R).
getRequesters([H1|T1],T2,[H1|T3]):-
getRequesters(T1,T2,T3),!.

incAvail(P,R,N):-
allocated(P,[R]),
(available_resources([[R,S1],[_,_]]) *-> N is S1 + 1) ;
(available_resources([[_,_],[R,S2]]) *-> N is S2 + 1) .

getArr(S,R):-
processes(P),
safePath(P,S),
getRequesters(P,S,R).

findlen([],X):-
X=0.
findlen([X|Tail],Count):-
findlen(Tail,Prev),
Count is Prev + 1.


remaining([],[]):-!.
deAllocate([],[]):-!.
deAllocate([H|T], [H2|T1]):-
allocated(H,[H2|T2]),
remaining(T2,S),
deAllocate(T,B1),!,
append(S,B1,T1).

remaining([H|T],[H|T1]):-
remaining(T,T1),!.

getAvailResNum(S1,S2):-
available_resources([[H1,S1],[H2,S2]]).

addAvailableR1(T1,0,T1):-!.
addAvailableR1(T1,S1,[r1|T]):-
Snew is S1 - 1,
addAvailableR1(T1,Snew,T),!.

addAvailableR2(T1,0,T1):-!.
addAvailableR2(T1,S1,[r2|T]):-
Snew is S1 - 1,
addAvailableR2(T1,Snew,T),!.
