% isSolution(+S)
% Checks if S is a valid solution by checking if all cores and tasks are 
% represented exactly once
isSolution(solution(ScheduleList)) :-
	findall(X, core(X), Cores),
	findall(Y, task(Y), Tasks),
	% dependencies
	isSolution(ScheduleList, Cores, Tasks).

%% No more tasks nor cores remaining => this is a valid solution
isSolution([],[],[]).	
%% No more tasks remaining, only cores. Checking if each core is reprented in list of schedules
isSolution([schedule(Core, [])|Schedules], Cores,[]) :-	
	delete_first(Core, Cores, NewCores),
	isSolution(Schedules, NewCores,[]).
%isSolution(_, [], Tasks) :- fail.
isSolution([schedule(Core, Schedule)|Schedules], Cores, Tasks) :-
	delete_first(Core, Cores, NewCores),
	set_diff_strict(Tasks, Schedule, NewTasks),
	isSolution(Schedules, NewCores, NewTasks).



%delete_first(E,L1,L2): L2 is L1 with the first occurance of E removed, fails if E does not occur in L1.
delete_first(E,[E|T],T) :- !.
delete_first(E,[H|T1],[H|T2]) :- 
	delete_first(E,T1,T2).

%% set_diff_strict(+Set1, +Set2, -Diff)
%% Takes the difference Set1 minus Set2.
%% Strict: every element in Set2 must be in Set1 in order to succeed
set_diff_strict([],[],[]).
set_diff_strict([X|Y],Set2,Res):-
	not(member(X,Set2)), !,
	Res = [X|Diff],
	set_diff_strict(Y,Set2,Diff).
set_diff_strict([X|Y],Set2,Diff):-
	delete_first(X, Set2, Set2New),
	set_diff_strict(Y,Set2New,Diff).


%% Test queries
% isSolution(solution([schedule(c1,[t1]), schedule(c2,[t2,t7]), schedule(c3, [t3,t6]), schedule(c4, [t4,t5])])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% execution_time(+S,-ET)
% Expects a valid scheduling solution and returns its Execution Time
execution_time(solution(ScheduleList), ET) :-
	execution_time(ScheduleList, 0,0, ET).
% Schedules: List of schedules in format [schedule(CoreS, [TaskX,...,TaskY]),..., schedule(CoreZ, [TaskZ,...])]
% TimeSoFar: (Per Core) Time of tasks already computed. Reset to 0 when going to the next core
% PreviousET: (Accumulator): Maximum ET computed until now. Becomes the final ET when end of schedulelist is reached
% ET: Final execution time
execution_time([], 0, ET, ET).
execution_time([schedule(_, [])|Schedules], TimeSoFar, PreviousET, ET) :-	
	max(TimeSoFar, PreviousET, CurrentET),
	execution_time(Schedules, 0, CurrentET, ET).	
execution_time([schedule(_, [Htask|Ttasks])|Schedules], TimeSoFar, PreviousET, ET) :-	
	process_cost(Htask,_, TaskTime),
	TimeSoFar2 is TimeSoFar + TaskTime,
	execution_time([schedule(_,Ttasks)|Schedules], TimeSoFar2, PreviousET, ET),!.	

% max(?X, ?Y, ?Max)
% Returns the maximum of X and Y
% Optimized using a green cut
max(X,Y,Y) :- X =< Y, !.
max(X,Y,X) :- X > Y. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% speedup(+S,-Speedup)
% Computes the Speedup of a given solution S
% Speedup = optimal sequential execution time / execution time of S
speedup(S,SpeedUp) :-
	optimal_sequential(ET1),
	execution_time(S, ET),
	Div is ET1 / ET,
	SpeedUp is round(Div),!.

% optimal_sequential(-ET1)
% Determines the optimal sequential execution time
optimal_sequential(ET1) :-
	%% determine fastest core
	FastestCore = c1,
	findall(ET, process_cost(_, FastestCore, ET), ETs),
	sum_list(ETs, ET1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

:- dynamic best/2.

% find_optimal(-S)
% Computes an optimal schedule S
find_optimal(_) :-
	optimal_sequential(ET1),
	ET2 is ET1 + 1,
	assert(best(nil, ET2)),
	find_solution(S),
	execution_time(S,Time),
	update_best(S, Time),
	fail.
find_optimal(S) :-
	best(S,_),
	retract(best(_,_)).

update_best(S, TimeS) :-
	best(_, TimeBest),
	TimeS < TimeBest,
	!,
	retract(best(_,_)),
	assert(best(S, TimeS)).

% find_solution(-S)
% Generates any possible scheduling solution 'S'
find_solution(S) :-
	findall(Core, core(Core), Cores),
	findall(Task, task(Task), Tasks),
	find_solution(ScheduleList, Cores, Tasks),
	S = solution(ScheduleList).

find_solution([],[],[]).
find_solution([schedule(Core,[])|OtherCores], [Core|Cores], []) :-	% Cores remaining, no more tasks
	find_solution(OtherCores, Cores, []),!.
find_solution([schedule(CurrCore, [T|OtherTasks])|OtherCores], Cores, Tasks) :- % Add a task
	task(T),
	member(T, Tasks),
	delete_first(T, Tasks, Tasks2),
	find_solution([schedule(CurrCore, OtherTasks)|OtherCores], Cores, Tasks2).
find_solution([schedule(CurrCore,[])|OtherCores], [CurrCore|Cores], Tasks) :-	% Switch to other Core
	find_solution(OtherCores, Cores, Tasks).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% find_heuristically(-S)
% Returns a schedule solution by heuristic:
% Each time a task is considered, it will be added to the
% (at that moment) core with lowest occupancy (based on execution time)
find_heuristically(S) :-
	findall(Core, core(Core), Cores),
	findall(Task, task(Task), Tasks),
	create_empty_schedule(Cores, InitScheduleList),
	find_heuristically(Tasks, Cores, InitScheduleList, ScheduleList),
	S = solution(ScheduleList). % TODO

find_heuristically([],_, ScheduleList, ScheduleList).
find_heuristically([HTask|Tasks], Cores, CurrSchedule, ScheduleList) :-
	most_inactive_core(CurrSchedule, Core),
	add_to_core(HTask, Core, CurrSchedule, ResultSchedule),
	find_heuristically(Tasks, Cores, ResultSchedule, ScheduleList).

% create_empty_schedule(+Cores, -Schedule)
create_empty_schedule([], []).
create_empty_schedule([HCore|Cores], [schedule(HCore, [])|Schedule]) :-
	create_empty_schedule(Cores, Schedule).

% add_to_core(+Task, +Core, +ScheduleList, -ResultScheduleList)
% Append a task to a core in a ScheduleList
add_to_core(Task, Core, [schedule(Core, Tasks)|Cores], [schedule(Core, [Task|Tasks])|Cores]) :- !.
add_to_core(Task, Core, [schedule(CurrCore, Tasks)|Cores], [schedule(CurrCore, Tasks)|RCores]) :-
	add_to_core(Task, Core, Cores, RCores).

% most_inactive_core(+ScheduleList, -ResultCore)
% Returns a core 'ResultCore' with lowest time occupancy according to 'ScheduleList'
most_inactive_core(ScheduleList, ResultCore) :-
	most_inactive_core(ScheduleList,1000000,_, ResultCore).

most_inactive_core([],_, ResultCore, ResultCore).
most_inactive_core([schedule(Core, Tasks)|Schedules], CurrTime,_, ResultCore) :-
	core_time(Core, Tasks, Time),
	Time =< CurrTime, !,
	most_inactive_core(Schedules, Time, Core, ResultCore).
most_inactive_core([schedule(_,_)|Schedules], CurrTime, CurrCore, ResultCore) :-
	most_inactive_core(Schedules, CurrTime, CurrCore, ResultCore).


% core_time(+Core, +Tasks, -TotalTime)
% Returns the occupancy time of a core 'Core' when processing 'Tasks'
core_time(_, [], 0).
core_time(Core, [HTask|Tasks], TotalTime) :-
	core_time(Core, Tasks, Time), !,
	process_cost(HTask, Core, TaskTime),
	TotalTime is Time + TaskTime.


%% DEPRECATED
find_optimal_task(Tasks, ResultTask, ResultCore) :-
	find_optimal_task(Tasks, 1000000, nil, nil, ResultTask, ResultCore).

find_optimal_task([],_, Task, Core, Task, Core).
find_optimal_task([HTask|Tasks], Min,_,_, ResultTask, ResultCore) :-
	process_cost(HTask, Core, Time),
	Time =< Min, !,
	find_optimal_task(Tasks, Time, HTask, Core, ResultTask, ResultCore).
find_optimal_task([_|Tasks], Min, CurTask, CurCore, ResultTask, ResultCore) :-
	find_optimal_task(Tasks, Min, CurTask, CurCore, ResultTask, ResultCore).
	


