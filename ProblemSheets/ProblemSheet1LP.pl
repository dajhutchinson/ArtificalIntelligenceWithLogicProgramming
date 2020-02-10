% ['D:/Documents/CS/ArtificialIntelligenceWithLogicProgramming/ProblemSheets/ProblemSheet
% 1LP.pl'].
% Press 'space'
% 'make.' to reload file
%
connected(bond_street,oxford_circus,central).
connected(oxford_circus,tottenham_court_road,central).
connected(bond_street,green_park,jubilee).
connected(green_park,charing_cross,jubilee).
connected(green_park,piccadilly_circus,piccadilly).
connected(piccadilly_circus,leicester_square,piccadilly).
connected(green_park,oxford_circus,victoria).
connected(oxford_circus,piccadilly_circus,bakerloo).
connected(piccadilly_circus,charing_cross,bakerloo).
connected(tottenham_court_road,leicester_square,northern).
connected(leicester_square,charing_cross,northern).

% 1.1
not_too_far(X,Y) :- connected(X,Y,_).
not_too_far(X,Z) :- connected(X,Y,_),connected(Y,Z,_).

reachable(X,Y,[]):-connected(X,Y,L).
reachable(X,Y,[Z|R]):-connected(X,Z,L),reachable(Z,Y,R).

% 1.4
list(L) :-L=[].
list([L|R]) :- list(R).

% 1.5
at_least_two(X,Y,R):-reachable(X,Y,R),length(R,L),L>1.
