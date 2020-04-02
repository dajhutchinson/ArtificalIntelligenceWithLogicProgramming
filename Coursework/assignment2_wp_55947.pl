% candidate_number(55947).

% set_prolog_flag(answer_write_options,[max_depth(0)]).
% ./ailp.pl assignment2 part3


% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A):-
  (part_module(2)   -> find_identity_2(A)
  ; otherwise -> find_identity_o(A)
  ).

find_identity_2(A):-
  set_of_actors(As),
  full_deduction(As,[A]).

find_identity_o(A):-
  my_agent(Agent),
  set_of_actors(As),
  find_locations(Locations),
  oracle_locations(Locations,OLs),
  charging_locations(Locations,CLs),
  find_by_traversing(CLs,OLs,As,[A]),
  say(A,Agent).

/*
 * Part 2
 * find_identity_2
 */

% Set of actors
% -As
set_of_actors(As):-
 setof(A,actor(A),As).

% get link from oracle
% - Link
get_link(Link):-
 agent_ask_oracle(oscar,o(1),link,Link).

% BASE CASE - Only one actor has all the links test so far so must be them.
full_deduction(Actors,Actors):-
  length(Actors,L),
  L=1,!.

% keep testing links until obly one actor is left
% NOTE - Not keeping list of tested Links so some will be repeated
% +Actors - Have_All_Links
full_deduction(Actors,Have_All_Links):-
  get_link(Link),                           % Link to test
  deduce_id(Actors,Link,Have_Link),         % Deduce which Actors have this link
  full_deduction(Have_Link,Have_All_Links). % Test another link (might hit base case)

% Base Case
% +Current_Actors +Link -Have_Link
deduce_id([],_,[]):-!.

% deduce which actors from a set have a specified link
% Case - Actor does have link
% + Actors +Link -Have_Link
deduce_id([Actor|Other_Actors],Link,Have_Link):-
  actor_has_link(Actor,Link),                     % If actor has link
  Have_Link=[Actor|Other_Have_Link],              % Update list of those with link
  deduce_id(Other_Actors,Link,Other_Have_Link),!. % Test rest of Actors

% Case - Actor does NOT have link
deduce_id([_Actor|Other_Actors],Link,Have_Link):-
  deduce_id(Other_Actors,Link,Have_Link),!.       % Test with rest of Actors, exclude _Actor

% true if A has Link
% +A +Link
actor_has_link(A,Link):-
  actor_links(A,Links),  % List of link actor has
  memberchk(Link,Links). % Link is in list (ie actor has it)

% List of all links of actors page
% +A -Links
actor_links(A,Links):-
  wp(A,Wiki_Text),                           % Get WikiText for actor
  setof(Link,wt_link(Wiki_Text,Link),Links). % Extract set of Links from WikiText

/*
 *  Part 3
 *  TODO
 *    Navigate to Oracles, picking up clues
 *    Keep navigating until actor has been deduced
 *    Need to think about refuelling (NOTE work on this first)
 *
 *  Plan
 *    1. Write predicate to keep moving between oracles, refuelling when needed
 *    2. Pick up clues from oracles
 *    3. Deduce actor. Stop when done
 *
 *  Strategy
 *    Find fuel stations at start.
 *    Record their positions
 *    Then find oracles & fuel up after each one at closest fuel station
 *
 *  IDEA
 *    - Instead of finding locations by doing a BFS from agent, move across board (Requires knowing board dimensions)
 */

% oracles & fuel stations
o(1). o(2). o(3). o(4). o(5). o(6). o(7). o(8). o(9). o(10).
c(1). c(2).

set_of_oracles(Os):-
  setof(o(O),o(O),Os).

set_of_charging_stations(Cs):-
  setof(c(C),c(C),Cs).

/*-------------------*
*  BOARD DIMENSIONS *
*-------------------*/

% valid widths (+1 since one outside the boundary can move onto the boundary)
valid_widths(Width,[]):-
 \+ map_adjacent(p(Width,1),_,_),!. % not in boundaries

valid_widths(Width,Valid_Widths):-
 map_adjacent(p(Width,1),_,_),
 Valid_Widths=[Width|Other_Valid_Widths],
 Next_Width is Width+1,
 valid_widths(Next_Width,Other_Valid_Widths),!.

% valid heights (+1 since one outside the boundary can move onto the boundary)
valid_heights(Height,[]):-
 \+ map_adjacent(p(1,Height),_,_),!. % not in boundaries

valid_heights(Height,Valid_Heights):-
 map_adjacent(p(1,Height),_,_),
 Valid_Heights=[Height|Other_Valid_Heights],
 Next_Height is Height+1,
 valid_heights(Next_Height,Other_Valid_Heights),!.

% get board dimensions
board_dimensions(Width,Height):-
 valid_heights(1,Heights),
 reverse(Heights,RHeights),
 RHeights=[_|[Height|_RestH]],
 valid_widths(1,Widths),
 reverse(Widths,RWidths),
 RWidths=[_|[Width|_RestW]].

/*-----------------*
*  FIND LOCATIONS *
*-----------------*/

% Find the location of every special object
% -Locations
find_locations(Locations):-
 board_dimensions(W,H),
 check_board(1,W,H,Many_Locations),
 sort(Many_Locations,Locations).

% Extract charge locations from result of find_locations(Locations)
% +Locations -Oracle_Locations
charging_locations(Locations,Charge_Locations):-
  Charge_Locations=[Charge_1,Charge_2],
  Locations=[Charge_1|[Charge_2|_Rest]]. % Locations is sorted and number of charge is fixed

% Extract locations from result of find_locations(Locations)
% +Locations -Oracle_Locations
oracle_locations(Locations,Oracle_Locations):-
  Locations=[_Charge_1|[_Charge_2|Oracle_Locations]]. % Locations is sorted and number of charge is fixed

% end of row
% +Pos +Width -Locations
check_row(Pos,Width,[]):-
 Pos=p(X,_Y),
 X>Width,!.

% Adjacent includes an oracle
check_row(Pos,Width,Locations):-
 Pos=p(X,Y),
 map_adjacent(Pos,Obj_Pos,o(O)),
 Locations=[(o(O),Obj_Pos)|Other_Locations],
 check_row(p(X+1,Y),Width,Other_Locations),!.

% Adjacent includes a charging
check_row(Pos,Width,Locations):-
 Pos=p(X,Y),
 map_adjacent(Pos,Obj_Pos,c(C)),
 Locations=[(c(C),Obj_Pos)|Other_Locations],
 Next_X is X+1,
 check_row(p(Next_X,Y),Width,Other_Locations),!.

% Adjacent is not special
check_row(Pos,Width,Locations):-
 Pos=p(X,Y),
 Next_X is X+1,
 check_row(p(Next_X,Y),Width,Locations),!.

% Row not on board
check_board(Row,_Width,Height,[]):-
 Row>Height,!.

% Check every row on board
% +Row +Width +Height -Locations
check_board(Row,Width,Height,Locations):-
 Pos=p(1,Row),
 check_row(Pos,Width,Row_Locations),
 append(Row_Locations,Other_Locations,Locations),
 Next_Row is Row+1,
 check_board(Next_Row,Width,Height,Other_Locations).

/*-----------*
 *  MOVEMENT *
 *-----------*/

% Main Predicate
% Traverse to oracles, picking up clues and deduce the actor
% Stop when deduced (~ 4 oracles)
% -A
find_by_traversing(_Charge_Locations,_Oracle_Locations,Actors,Actors):-
  length(Actors,L),
  L=1,!.

find_by_traversing(Charge_Locations,Oracle_Locations,Actors,Have_All_Links):-
  my_agent(Agent),
  visit_closest(Oracle_Locations,2,_Visited_Oracle_Pos,Visited_Oracle_Obj,Remaining_Oracle_Locations),
  query_world(agent_ask_oracle,[Agent,Visited_Oracle_Obj,link,Link]),
  say(Link,Agent),
  deduce_id(Actors,Link,Have_Link),         % Deduce which Actors have this link
  say(Have_Link,Agent),
  visit_closest(Charge_Locations,1,_Visited_Charge_Pos,Visited_Charge_Obj,_), % if closest charging station is blocked or more than 50 moves away then we fail here
  say("Charged",Agent),
  query_world(agent_topup_energy,[Agent,Visited_Charge_Obj]), % refuel
  find_by_traversing(Charge_Locations,Remaining_Oracle_Locations,Have_Link,Have_All_Links). % Test another link (might hit base case)

visit_all_oracles():-
  find_locations(Locations),
  oracle_locations(Locations,OLs),
  charging_locations(Locations,CLs),
  visit_next_oracle(CLs,OLs).

% Visits (Probs) next closest unvisited oracle then refuels
visit_next_oracle(Charge_Locations,[]):-!. % No more oracles to visit
visit_next_oracle(Charge_Locations,Oracle_Locations):- % oracles to visit
  my_agent(Agent),
  visit_closest(Oracle_Locations,2,_Visited_Oracle_Pos,Visited_Oracle_Obj,Remaining_Oracle_Locations),
  query_world(agent_ask_oracle,[Agent,Visited_Oracle_Obj,link,Link]),
  say(Link,Agent),
  visit_closest(Charge_Locations,1,_Visited_Charge_Pos,Visited_Charge_Obj,_), % if closest charging station is blocked or more than 50 moves away then we fail here
  say("Charged",Agent),
  query_world(agent_topup_energy,[Agent,Visited_Charge_Obj]), % refuel
  visit_next_oracle(Charge_Locations,Remaining_Oracle_Locations),!. % Visit next closests unvisited oracle
  % Get positions of oracles
  % Get positions of charging_stations
  % REPEAT {
    % go to nearest oracle which has not been visited
    % go to nearest charging station
  % }

% Move to closest of Locations in Locations (New_Locations removes this location)
% +Locations +Prune_Level -Visited_Pos -Visited_Obj -New_Locations
visit_closest(Locations,Prune_Level,Visited_Pos,Visited_Obj,New_Locations):-
  distances_to_locations(Locations,Distances), %distances to oracles
  sort(Distances,Sorted_Distances),
  first_n(Sorted_Distances,Prune_Level,Closest_Distances), % PRUNNING - only going to find paths to PRUNE_LEVEL closest oracles
  extract_location_details_from_distance(Closest_Distances,Closest_Locations),
  find_paths(Closest_Locations,Paths), % find paths to all adjacent positions
  sort(Paths,Sorted_Paths),
  Sorted_Paths=[Best|_Rest], % Best = Probs shortest path to an oracle
  Best=(_Best_Cost,Best_Obj,Best_Obj_Pos,Best_Path),
  Visited_Obj=Best_Obj,
  Visited_Pos=Best_Obj_Pos,
  remove((Best_Obj,Best_Obj_Pos),Locations,New_Locations), % remove traversed path
  my_agent(Agent),
  query_world(agent_do_moves,[Agent,Best_Path]),!. % display best path

/*-----------*
 *  UTILITY *
 *-----------*/

empty_adjacent_positions(Pos,Adj_Positions):-
  setof(Adj_Pos,map_adjacent(Pos,Adj_Pos,empty),Adj_Positions).

% find the shortest path to a number of Positions (Uses Part 1)
% pass list of locations of adjacent positions
% +Positions -Paths
find_paths([],[]):-!.
find_paths(Locations,Paths):-
  Locations=[(Obj,Obj_Pos)|Rest_Locations],
  empty_adjacent_positions(Obj_Pos,Adj_Obj_Positions),
  find_paths_to(Adj_Obj_Positions,Obj,Obj_Pos,Possible_Paths),
  append(Possible_Paths,Rest_Paths,Paths),
  find_paths(Rest_Locations,Rest_Paths).

% find paths to specified positions
% used for adjacent positions around objects in find_paths
find_paths_to([],_,_,[]):-!.
find_paths_to(Empty_Positions,Obj,Obj_Pos,Paths):-
  Empty_Positions=[Pos|Rest_Empty_Positions],
  Task=go(Pos),
  find_path(Task,Cost,Path),
  Paths=[(Cost,Obj,Obj_Pos,Path)|Rest_Paths],
  find_paths_to(Rest_Empty_Positions,Obj,Obj_Pos,Rest_Paths).

% distance from agent to defined positions
distances_to_locations([],[]):-!.
distances_to_locations(Locations,Distances):-
  Locations=[(Obj,Obj_Pos)|Rest_Locations],
  my_agent(Agent),
  query_world(agent_current_position,[Agent,Agent_Pos]),
  map_distance(Agent_Pos,Obj_Pos,Distance),
  Distances=[(Distance,Obj,Obj_Pos)|Rest_Distances],
  distances_to_locations(Rest_Locations,Rest_Distances).

% first n elements of list (or full list if |list|<n)
first_n([],_,[]):-!. % List exhausted
first_n(_,0,[]):-!.  % Found n
first_n(List,N,First_n):-
  List=[Elem|Rest_List],
  First_n=[Elem|Rest_First_n],
  Next_N is N-1,
  first_n(Rest_List,Next_N,Rest_First_n).

% extract location details from the format that distance_to_locations produces
extract_location_details_from_distance([],[]):-!.
extract_location_details_from_distance(Distances,Obj_Details):-
  Distances=[(_Dist,Obj,Obj_Pos)|Rest_Distances],
  Obj_Details=[(Obj,Obj_Pos)|Rest_Obj_Details],
  extract_location_details_from_distance(Rest_Distances,Rest_Obj_Details).

% Remove object from list
remove(Obj,[],[]):-!.
remove(Obj,List,New_List):- % Target object at head of list
  List=[Obj|Rest_List],
  remove(Obj,Rest_List,New_List),!.
remove(Obj,List,New_List):- % Another object at head
  List=[X|Rest_List],
  New_List=[X|Rest_New_List],
  remove(Obj,Rest_List,Rest_New_List),!.

/*
 *  TESTS
 */
