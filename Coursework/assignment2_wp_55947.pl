% candidate_number(55947).

% set_prolog_flag(answer_write_options,[max_depth(0)]).

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
  find_by_traversing(A).

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

% Main Predicate
% -A
find_by_traversing(A).
  % same as visit_all but stop when actor deduced

visit_all_oracles():-
  % Get positions of oracles
  % Get positions of charging_stations
  % REPEAT {
    % go to nearest oracle which has not been visited
    % go to nearest charging station
  % }

% oracles & fuel stations
o(1). o(2). o(3). o(4). o(5). o(6). o(7). o(8). o(9). o(10).
c(1). c(2).

set_of_oracles(Os):-
  setof(o(O),o(O),Os).

set_of_charging_stations(Cs):-
  setof(c(C),c(C),Cs).

% Find locations of specified objects (oracles or charging stations)
% BASE CASE
find_locations([],[]):-!.

% +Objects -Locations
find_locations([Object|Rest_Objects],Locations):-
  Task=find(Object),
  find_path(Task,_Cost,Path), % find path to object (This could fail)
  reverse(Path,[Adj_Pos|_RRPath]), % last position in path
  adjacent_object_position(Adj_Pos,Object,Object_Location), % find which adjacent position Object is in
  Locations=[Object_Location|Rest_Locations], % add Object Position to list
  find_locations(Rest_Objects,Rest_Locations),!. % Find location of other objects

% No path to Object
find_locations([_Object|Rest_Objects],Locations):-
  find_locations(Rest_Objects,Locations).

% Returns Object_Position of Object when given Position which is adjacent
% +Position +Object -Object_Position
adjacent_object_position(Position,Object,Object_Position):-
  map_adjacent(Position,Object_Position,Object).

% Find closest charging station
% +Charging_Locations +C +Path
path_to_nearest_charging(Charging_Locations,Path):-
  test_5(Charging_Locations,Adj_Locations),
  find_paths(Adj_Locations,Unsorted_Paths),
  sort(Unsorted_Paths,[(_Cost,Path)|Rest]),!.

% Empty adjacent positions USE to find location adjacent to charging station
% +Charging_Locations -Adj_Locations
% BASE CASE
empty_adjacent_spaces([],[]):-!.
empty_adjacent_spaces([L|Rest_Locations],Adj_Locations):-
  setof(Pos,adjacent_object_position(L,empty,Pos),Adj_Positions),
  append(Adj_Positions,Rest_Adj_Locations,Adj_Locations),
  test_5(Rest_Locations,Rest_Adj_Locations).

% Find path and cost to many locations
% +Locations -Paths
find_paths([],[]):-!.

% there is a path to L
find_paths(Locations,Paths):-
  Locations=[L|Rest_Locations],
  Task=go(L),
  find_path(Task,Cost,Path),
  Paths=[(Cost,Path)|Other_Paths],
  find_paths(Rest_Locations,Other_Paths).

% there is NO path to L
find_paths([_L|Rest_Locations],Paths):-
  find_paths(Rest_Locations,Paths).

/*
 *  TESTS
 */

% Set of all possible actors
% -As
test_1(As):-
  setof(A,actor(A),As).

% list of links in actors page
% +A -Ls
test_2(A,Links):-
  wp(A,Wiki_Text),
  setof(Link,wt_link(Wiki_Text,Link),Links).

% Continuously go between charging locations
% + Locations
test_3([]):-!.

% Go to Locations and charge at each one
test_3(Locations):-
  my_agent(Agent),
  Locations=[L|Rest],
  adjacent_object_position(L,empty,Adj_Location),
  adjacent_object_position(Adj_Location,c(C),L),
  solve_task(go(Adj_Location),_Cost),
  query_world(agent_topup_energy,[Agent,c(C)]),
  test_3(Rest),!.
