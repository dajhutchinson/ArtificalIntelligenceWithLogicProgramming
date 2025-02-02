candidate_number(42).

/*
 *  start.
 *  press "run"
 *  reset_game. join_game(A). reset_game. start_game.
 *  set_prolog_flag(answer_write_options,[max_depth(0)]).
 */

% solve task using A*
% +Task (go/find) +Cost -Path
solve_task(Task,Cost):-
  my_agent(Agent),
  find_path(Task,Cost,Path),
  query_world(agent_do_moves,[Agent,Path]). % display pay

% Returns path (does not traverse it though)
find_path(Task,Cost,Path):-
  my_agent(Agent),
  query_world(agent_current_position,[Agent,Cur_Pos]), % get current position
  Initial_Agenda=[((0,0,[]),Cur_Pos)], % initial agenda start at current position
  solve_task_as(Task,Initial_Agenda,Cost,RPath), % solve the task
  reverse(RPath,[_Init|Path]),!. % Reverse path & remove starting node

/*
 *  ME
 *  Agenda=[((Total_Cost,Cost_to_Pos,RPath),Pos)|Rest]
 *    Total_Cost=Cost_to_Pos + predicted cost to target
 *    Cost_to_Pos=Cost of path so far (==|RPath|)
 *    Pos = Position of path atm (ie where to check from next)
 *    RPath = reverse of path to Pos
 */

% Get a valid move when doing GO TO task (adjacent positions, which aren't walls & haven't been traversed so far)
% +Pos +Cost_to_Pos +RPath +Task(go/find) +Current_Agenda -NewPos -Total_Cost -Cost_to_Pos -New_RPath
search_as((Pos,Cost_to_Pos,RPath),go(Target_Pos),Current_Agenda,NewPos,(Total_Cost,Cost_to_NewPos,New_RPath)):-
  map_adjacent(Pos,NewPos,empty),       % find valid move (can only move into empty)
  map_distance(NewPos,Target_Pos,Est_Cost), % estimate distance to end (heuristic value)
  \+ memberchk(NewPos,RPath),           % check not already been to position on this path
  Total_Cost is Cost_to_Pos+Est_Cost,   % Update costs
  Cost_to_NewPos is Cost_to_Pos+1,
  cheapest_option(Current_Agenda,NewPos,Cost_to_NewPos), % prune if there is already route to this position which is cheaper
  append([Pos],RPath,New_RPath).        % add new position to path

% Get a valid move when doing FIND task
% difference here is that we cannot estimate distance to goal
% +Pos +Cost_to_Pos +RPath +Task(go/find) +Current_Agenda -NewPos -Total_Cost -Cost_to_Pos -New_RPath
search_as((Pos,Cost_to_Pos,RPath),find(_Target_Obj),Current_Agenda,NewPos,(Total_Cost,Cost_to_NewPos,New_RPath)):-
  map_adjacent(Pos,NewPos,empty),   % find valid move (can only move into empty)
  \+ memberchk(NewPos,RPath),       % check not already been to position on this path
  Cost_to_NewPos is Cost_to_Pos+1,  % == path length
  Total_Cost is Cost_to_NewPos+0,   % Cannot estimate distance to goal so == Cost_to_NewPos
  cheapest_option(Current_Agenda,NewPos,Cost_to_NewPos), % prune if there is already route to this position which is cheaper
  append([Pos],RPath,New_RPath).    % Add new position to path

% Set all possible moves (adjacent positions, which aren't walls & haven't been traversed so far)
% -Details -NewPos +Pos +Cost_To_Pos +RPath +Task +Current_Agenda -NewPos -Details -Ns
new_setof((Details,NewPos),search_as((Pos,Cost_to_Pos,RPath),Task,Current_Agenda,NewPos,Details),Ns):-
  setof((Details,NewPos),search_as((Pos,Cost_to_Pos,RPath),Task,Current_Agenda,NewPos,Details),Ns),!.
  % new_setof((Details,N),search_as((p(1,2),1,[p(1,1)]),go(p(20,20)),N,Details),Ns).
new_setof(_,_,[]). % no posible moves

% Update agenda by looking at first item and adding possible moves
% +Current_Agenda +Task -New_Agenda
update_agenda(Current_Agenda,_Task,[]):- % best path is too long, stop looking
  Current_Agenda=[((_Total_Cost,_Cost_to_Pos,RPath),_Pos)|_Rest],
  my_agent(Agent),
  length(RPath,L),
  L>50,
  string_concat("*",L,C),
  say(C,Agent). %TODO

update_agenda(Current_Agenda,Task,New_Agenda):-
  Current_Agenda=[((_Total_Cost,Cost_To_Pos,RPath),Pos)|Rest],
  my_agent(Agent), %TODO
  length(RPath,L), %TODO
  L<51,
  say(L,Agent), %TODO
  new_setof((Details,NewPos),search_as((Pos,Cost_To_Pos,RPath),Task,Current_Agenda,NewPos,Details),New_Moves),
  append(Rest,New_Moves,Unsorted_New_Agenda),
  sort(Unsorted_New_Agenda,New_Agenda). % More agressive prunning

% GO TO Task COMPLETE if first item in agenda moves onto target position
% +Target +Agenda -Cost -RPath
solve_task_as(_,[],_,[]):-
  my_agent(Agent),
  say("No short path",Agent),!. % Cannot find route shorter than 41

solve_task_as(go(Target_Pos),Agenda,Cost,RPath):-
  Agenda=[(Details,Target_Pos)|_Rest],
  Details=(_,Cost,RRPath),
  append([Target_Pos],RRPath,RPath),!.

% FIND OBJECT task COMPLETE if first item in agenda is adjacent to object
solve_task_as(find(Target_Obj),Agenda,Cost,RPath):-
  Agenda=[(Details,Pos)|_Rest],
  Details=(_,Cost,RRPath),
  map_adjacent(Pos,_,Target_Obj),
  append([Pos],RRPath,RPath),!.

% Task not complete, update agenda with new moves
% +Target +Agenda -Cost -RPath
% Cost & RPath are extracted from agenda
solve_task_as(Task,Agenda,Cost,RPath):-
  update_agenda(Agenda,Task,New_Agenda),
  solve_task_as(Task,New_Agenda,Cost,RPath).
  % solve_task_as(p(20,20),[((0,0,[]),p(1,1))],Cost,RPath).

% finall occurences of New_Pos in Agenda which are cheaper (or equally expensive)
% test_7([((1,2,[3,4]),p(1,1)),((11,12,[13,14]),p(1,2)),((21,22,[23,24]),p(1,1))],p(1,1),10,X).
% Agenda_empty
cheaper_options([],_New_Pos,_New_Cost,[]):-!.

% First item is New_Pos
% +Agenda +New_Pos +New_Cost (Cost to Pos) -New_Pos_Details
cheaper_options(Agenda,New_Pos,New_Cost,Prev_Details):-
  Agenda=[(Details,New_Pos)|Rest],
  Prev_Details=[Details|More_Details],
  Details=(_,Prev_Cost,_),
  Prev_Cost@=<New_Cost,
  cheaper_options(Rest,New_Pos,New_Cost,More_Details),!.

% New_Pos not in first item
cheaper_options(Agenda,New_Pos,New_Cost,Prev_Details):-
  Agenda=[_Item|Rest],
  cheaper_options(Rest,New_Pos,New_Cost,Prev_Details),!.

% Is New_Cost the best Total_Cost encountered to New_Pos yet?
% +Agenda +New_Pos +New_Cost
cheapest_option(Agenda,New_Pos,New_Cost):-
  cheaper_options(Agenda,New_Pos,New_Cost,Better),
  length(Better,L),
  L=0.
/*
 *  TESTS
 */

% Extracting values from agenda (ie head on agenda)
% +Agenda, -Pos, -Rest
test_1(Agenda,Pos,Rest):-
  Agenda=[((_TC,_Cost_to_Pos),Pos)|Rest].
  %test_1([(1,1),p(1,1)],Pos).

test_2(Agenda,Pos,RPath):-
  Agenda=[((_TC,_Cost_to_Pos,RPath),Pos)|_Rest].

test_4(Target,Agenda,C,RP):-
  Agenda=[((_TC,C,RP),Target)|_Rest].

test_5(Target,Agenda,Cost,RPath):-
  Agenda=[(Details,Target)|_Rest],
  Details=(_,Cost,RPath),
  length(RPath,L),
  L@>2.
  % test_5(p(1,1),[((5,6,[7,8,9]),p(1,1)),p(2,2)],Cost,RPath).

% detect other routes already in agenda
% +Agenad +New_Item -Occurences
test_6(Agenda,New_Item,Occurences):-
  New_Item=((_Total_Cost,_Cost_To_NewPos,_RPath),New_Pos),
  findall(((_,_,_),New_Pos),Agenda,Occurences).

% finall occurences of New_Pos in Agenda which are cheaper
% test_7([((1,2,[3,4]),p(1,1)),((11,12,[13,14]),p(1,2)),((21,22,[23,24]),p(1,1))],p(1,1),10,X).
% Agenda_empty
test_7([],_New_Pos,_New_Cost,[]):-!.

% First item is New_Pos
% +Agenda +New_Pos +New_Cost (Cost to Pos) -New_Pos_Details
test_7(Agenda,New_Pos,New_Cost,Prev_Details):-
  Agenda=[(Details,New_Pos)|Rest],
  Prev_Details=[Details|More_Details],
  Details=(_,Prev_Cost,_),
  Prev_Cost@=<New_Cost,
  test_7(Rest,New_Pos,New_Cost,More_Details),!.

% New_Pos not in first item
test_7(Agenda,New_Pos,New_Cost,Prev_Details):-
  Agenda=[_Item|Rest],
  test_7(Rest,New_Pos,New_Cost,Prev_Details),!.

% Is New_Cost the best Total_Cost encountered to New_Pos yet?
% +Agenda +New_Pos +New_Cost
test_8(Agenda,New_Pos,New_Cost):-
  test_7(Agenda,New_Pos,New_Cost,Better),
  length(Better,L),
  L=0.
