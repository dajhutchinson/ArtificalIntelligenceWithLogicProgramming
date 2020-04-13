% ./ailp.pl assignment1
% start. reset. 'press run'

% complete = true iff path length == number squares
% m = can move north,south,east,west
% next = true if start_position followed by path is true
%          || if path followed by start_position is true
%          || if direction is valid
%             && new position has not been traversed to before,
%             &&
candidate_number(55947).

% Q1
q1(X):-ailp_start_position(X).

% Q2a
q2a(X):-q6_new_pos(p(1,1),e,X).

% Q2b
% q6_new_pos(p(1,1),n,X).
q2b(136).

% Q3
% next
q3([s,s,w,w,w,
    n,e,e,n,w,
    w,n,e,e,e]).

% Q4a
q4a([p(4,2),p(4,3),p(4,4)]).

% Q4b
q4b([p(4,2),p(4,3),p(4,4),
     p(3,4),p(2,4),p(1,4)]).

% Q4c
q4c([p(4,2),p(4,3),p(4,4),p(3,4),
     p(2,4),p(1,4),p(1,3),p(2,3),
     p(3,3),p(3,2),p(2,2),p(1,2),
     p(1,1),p(2,1),p(3,1),p(4,1)]).

% Q4d
q4d([p(4,2),p(4,3),p(4,4),p(3,4),
     p(2,4),p(1,4),p(1,3),p(1,2),
     p(1,1),p(2,1),p(2,2),p(2,3),
     p(3,3),p(3,2),p(3,1),p(4,1)]).

% Q5a
% TODO should this be randomised and/or infinite.
% TODO do I need the first step (without it we only visually move to 3 corners)
q5_corner_move:-ailp_show_move(p(1,1),p(1,4)),
                ailp_show_move(p(1,4),p(4,4)),
                ailp_show_move(p(4,4),p(4,1)),
                ailp_show_move(p(4,1),p(1,1)),
                q5_corner_move.

% Q5b
q5_corner_move2:-ailp_grid_size(S),
                 ailp_show_move(p(1,1),p(1,S)),
                 ailp_show_move(p(1,S),p(S,S)),
                 ailp_show_move(p(S,S),p(S,1)),
                 ailp_show_move(p(S,1),p(1,1)),
                 q5_corner_move2.

% Q6
q6_new_pos(p(X,Y), M, p(X1,Y1)) :-
  ( M = s -> X1 =  X,    Y1 is Y+1
  ; M = n -> X1 =  X,    Y1 is Y-1
  ; M = e -> X1 is X+1,  Y1 =  Y
  ; M = w -> X1 is X-1,  Y1 =  Y
  ),
  X1 >= 1, Y1 >=1,
  ailp_grid_size(N),
  X1 =< N, Y1 =< N.

q6_complete(L) :-
  ailp_grid_size(N),
  N2 is N * N,
  length(L,N2),
  ailp_show_complete.

q6_m(s).
q6_m(e).
q6_m(w).
q6_m(n).

q6_next(L) :-
  ailp_start_position(p(X,Y)),
  ailp_show_move(p(X,Y),p(1,1)),
  q6_next(p(1,1),L).
% P: current position
% L: path taken by agent
q6_next(P,L) :-
  q6_next(P,[P],Ps),
  reverse(Ps,L).
q6_next(_,Ps,Ps) :- q6_complete(Ps).
q6_next(P,Ps,R) :-
  q6_m(M),
  q6_new_pos(P,M,P1),
  \+ memberchk(P1,Ps),
  ailp_show_move(P,P1),
  q6_next(P1,[P1|Ps],R).

/*
q6_valid_position(p(X,Y),M,p(X1,Y1),Dia):-
  ailp_grid_size(S), (
    M = s -> X1 =  X,    Y1 is Y+1;% Move in direction
    M = n -> X1 =  X,    Y1 is Y-1;
    M = e -> X1 is X+1,  Y1 =  Y;
    M = w -> X1 is X-1,  Y1 =  Y
  ),
  Min is (S/2)-(Dia/2)+1,
  Max is (S/2)+(Dia/2),(
    X1=:=Min; % New position is on spiral edge
    X1=:=Max;
    Y1=:=Min;
    Y1=:=Max
  ),
  X1>=1, X1@=<S, Y1>=1, Y1@=<S. % New position is on board


q6_spiral(X):-ailp_grid_size(R),  % radius of outer layer
              spiral(p(1,1),X,R). % build spiral beginning in top left

% Clockwise spiral from p(Lower_Lim,Lower_Lim) to p(Lower_lim,Lower_Lim+1)
spiral_new_position/3.
spiral_new_position(p(X,Y), SpiralR, p(X1,Y1)):-
      ailp_grid_size(R), % radius of grid
      Lower_Lim is (R/2)-floor((SpiralR/4)), % determine turning points
      Upper_Lim is (R/2)+floor((SpiralR/4)),(
        (Y=:=Lower_Lim,X@=<Upper_Lim,X1 is X+1,Y1 is Y),!;   % if on top row (not top-right) move right
	      (X=:=Upper_Lim+1,Y@=<Upper_Lim,X1 is X,Y1 is Y+1),!; % if on right col (not bot-right) move down
	      (Y=:=Upper_Lim+1,X>Lower_Lim,X1 is X-1,Y1 is Y),!;           % if on bot row (not bot-left) move left
        (X=:=Lower_Lim,X1 is X,Y1 is Y-1)                          % if on left row (not top-left) move up
      ).

spiral_layer(P1,Ps,SpiralR):- % great a layer of radius SpiralR starting at P1
      spiral_new_position(P1,SpiralR,P2), % Find next position
      \+ member(P1,Ps),
      %ailp_show_move(P1,P2), % show move
      spiral_layer(P2,[P1|Ps],SpiralR). % find next move

% OTHER ATTEMPT
% Gives valid moves on spiral layer
ailp_grid_size(4).
q6_new_pos(p(X,Y), M, p(X1,Y1), SpiralR) :-
  ailp_grid_size(R),
  Lower_Lim is (R/2)-(SpiralR/4), % determine turning points
  Upper_Lim is (R/2)+(SpiralR/4)+1,
  ( M = s -> X1 =  X,    Y1 is Y+1
  ; M = n -> X1 =  X,    Y1 is Y-1
  ; M = e -> X1 is X+1,  Y1 =  Y
  ; M = w -> X1 is X-1,  Y1 =  Y
  ),
  (X1=:=Lower_Lim;X1=:=Upper_Lim;
   Y1=:=Lower_Lim;Y1=:=Upper_Lim),
  X1 @>=1, Y1@>=1,
  X1 @=<R, Y1 @=<R,!.

m(s).
m(e).
m(w).
m(n).

next(P1,M,P,SpiralR):-
    m(M),
    q6_new_pos(P1,M,P,SpiralR).

spiral(P1,Ps,SpiralR):-
	next(P1,_,P2,SpiralR),
    spiral(P2,[P1|Ps],SpiralR).
    */
