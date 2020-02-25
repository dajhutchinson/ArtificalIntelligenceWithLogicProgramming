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
q2a(X):-new_pos(p(1,1),e,X).

% Q2b
% new_pos(p(1,1),n,X).
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
% TODO should this be randomised or infinite.
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
% TODO
