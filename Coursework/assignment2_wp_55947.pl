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
  A='Not yet implemented'.

/*
 * Mine
 *    NOTE - Can only get one link at a time (ie cant do setof)
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
