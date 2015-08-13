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
%% No more tasks remaining, only cores. Checking if each core is represented in list of schedules
isSolution([schedule(Core, [])|Schedules], Cores,[]) :-	
	delete_first(Core, Cores, NewCores),
	isSolution(Schedules, NewCores,[]).
%isSolution(_, [], Tasks) :- fail.
isSolution([schedule(Core, Schedule)|Schedules], Cores, Tasks) :-
	delete_first(Core, Cores, NewCores),
	check_dependencies(Schedule),
	set_diff_strict(Tasks, Schedule, NewTasks),
	isSolution(Schedules, NewCores, NewTasks).

% check_dependencies(+TaskSchedule)
% Checks if no dependencies are violated given 
% the ORDER of tasks in 'TaskSchedule' (list of tasks)
check_dependencies([]).
check_dependencies([HTask|Tasks]) :-
	findall(DepTask, depends_on(HTask, DepTask,_), Deps), %% Get all tasks on which I 'HTask' depend
	intersection(Deps, Tasks, []),!,	% No task 'Deps' on which I depend should be scheduled after me 'Tasks'
	check_trans_dependencies(Deps, Tasks),	% Check if there are any transitive dependencies
	check_dependencies(Tasks).

% check_trans_dependencies(+Dependencies, +Tasks)
% Checks if any 'Dependencies' dependency has a 
% transitive dependency on a task in 'Tasks'
% p.e. t5 dep on t3, t3 dep on t1
% => c1(t5,t1),c2(t3) is not a valid schedule
check_trans_dependencies([],_).
check_trans_dependencies([HDep|Deps], TaskList) :-
	not(member(HDep, TaskList)),
	findall(DepTask, depends_on(HDep, DepTask,_), TransDeps),
	check_trans_dependencies(TransDeps, TaskList),
	check_trans_dependencies(Deps, TaskList).


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

%% set_diff(+Set1, +Set2, -Diff)
%% Non-strict version of set_diff
set_diff([],_,[]).
set_diff([X|Y],Set2,Res):-
	not(member(X,Set2)), !,
	Res = [X|Diff],
	set_diff(Y,Set2,Diff).
set_diff([_|Y],Set2,Diff):-
	set_diff(Y,Set2,Diff).

%% Test queries
% isSolution(solution([schedule(c1,[t1]), schedule(c2,[t2,t7]), schedule(c3, [t3,t6]), schedule(c4, [t4,t5])])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% execution_time(+S,-ET)
% Expects a valid scheduling solution and returns its Execution Time
execution_time(solution(ScheduleList), ET) :-
	get_schedule_tasks(ScheduleList, Tasks),
	execution_time(ScheduleList, Tasks, [], 0, ET). % ScheduleList, DepSortTasks, Processed, PreviousET, ET
% ScheduleList: List of schedules in format [schedule(CoreS, [TaskX,...,TaskY]),..., schedule(CoreZ, [TaskZ,...])]
% Tasks: All tasks in the schedule to be considered on order to compute ET
% PreviousET: (Accumulator): Maximum ET computed until now. Becomes the final ET when end of 'Tasks' is reached
% ET: Final execution time
execution_time(_, [],_, ET, ET) :- !.
execution_time(ScheduleList, Tasks, ProcessedTasks, PreviousET, ET) :-
	get_no_dep_tasks(Tasks, NoDepTasks),
	have_only_processed_tasks_before(NoDepTasks, ScheduleList, ProcessedTasks, OPTBeforeTasks),
	etime_nondeps(OPTBeforeTasks, ScheduleList, NonDepET), !,
	NextET is PreviousET + NonDepET,
	set_diff_strict(Tasks, OPTBeforeTasks, NextTasks), %% remove only tasks having only processed tasks scheduled before them
	append(OPTBeforeTasks, ProcessedTasks, NewProcessedTasks),
	execution_time(ScheduleList, NextTasks, NewProcessedTasks, NextET, ET).

%% have_only_processed_tasks_before(+Tasks, +ScheduleList, +ProcessedTasks, -OPTBeforeTasks)
%% Returns 'OPTBeforeTasks' containing only those tasks in 'Tasks' having only processed tasks
%% 'ProcessedTasks' scheduled before them (based on 'ScheduleList')
have_only_processed_tasks_before([],_,_,[]).
have_only_processed_tasks_before([HTask|TTasks], ScheduleList, ProcessedTasks, [HTask|OPTBeforeTasks]) :-
	scheduled_on_core(HTask, ScheduleList, Core),
	core_scheduled_tasks(Core, ScheduleList, CoreTasks),
	%% CASE1: HTask has only processed tasks scheduled before
	has_only_processed_tasks_before(HTask, CoreTasks, ProcessedTasks), !,
	have_only_processed_tasks_before(TTasks, ScheduleList, ProcessedTasks, OPTBeforeTasks).
have_only_processed_tasks_before([_|TTasks], ScheduleList, ProcessedTasks, OPTBeforeTasks) :-
	%% CASE2: (else) There exists a non-processed task scheduled before HTask
	have_only_processed_tasks_before(TTasks, ScheduleList, ProcessedTasks, OPTBeforeTasks).


%% has_only_processed_tasks_before(+Task, +Tasks, +ProcessedTasks)
%% Checks if any task in 'Tasks' before 'Task' is already processed
%% e.g. member of 'ProcessedTasks'
%% 'Task' must be member of 'Tasks' !!
has_only_processed_tasks_before(Task, [Task|_],_) :- !.
has_only_processed_tasks_before(Task, [HTask|TTasks], ProcessedTasks) :-
	member(HTask, ProcessedTasks), !,
	has_only_processed_tasks_before(Task, TTasks, ProcessedTasks).


%% etime_nondeps(+NonDeps, +ScheduleList, -ET)
%% Computes the execution time of tasks 'NonDeps' according
%% to the schedule given by 'ScheduleList'
%% Only the tasks in 'NonDeps' are considered
etime_nondeps(NonDeps, ScheduleList, ET) :-
	etime_nondeps(NonDeps, ScheduleList, [], [], 0, ET).
%% NonDeps: Tasks of which ET has to be computed
%% ScheduleList: List of schedules in which 'NonDeps' are scheduled
%% ProcessedNonDeps: 'NonDeps' tasks already considered in recursion
%% CoresScheduled: Cores already considered in recursion
%% PreviousET: ET at certain point in recursion (can only become larger)
%% ET: Final execution time
etime_nondeps([],_,_,_, ET, ET).
etime_nondeps([HNonDeps|TNonDeps], ScheduleList, ProcessedNonDeps, CoresScheduled, PreviousET, ET) :-
	scheduled_on_core(HNonDeps, ScheduleList, Core),
	member(Core, CoresScheduled), !,					% CASE1: the core has already tasks scheduled
	core_scheduled_tasks(Core, ScheduleList, CoreTasks),
	intersection(CoreTasks, ProcessedNonDeps, CurrCoreTasks), % Only processed NonDep tasks should be considered
	core_time(Core, [HNonDeps|CurrCoreTasks], CoreET),
	max(PreviousET, CoreET, NewET),
	etime_nondeps(TNonDeps, ScheduleList, [HNonDeps|ProcessedNonDeps], CoresScheduled, NewET, ET).
etime_nondeps([HNonDeps|TNonDeps], ScheduleList, ProcessedNonDeps, CoresScheduled, PreviousET, ET) :- 
	% CASE2: Core has not yet any tasks scheduled
	scheduled_on_core(HNonDeps, ScheduleList, Core),
	core_time(Core, [HNonDeps], CoreET),
	max(PreviousET, CoreET, NewET),
	etime_nondeps(TNonDeps, ScheduleList, [HNonDeps|ProcessedNonDeps], [Core|CoresScheduled], NewET, ET).


% max(?X, ?Y, ?Max)
% Returns the maximum of X and Y
% Optimized using a green cut
max(X,Y,Y) :- X =< Y, !.
max(X,Y,X) :- X > Y. 

%% get_schedule_tasks(+ScheduleList, -Tasks)
%% Expects a list of schedules 'ScheduleList' and extracts its tasks.
get_schedule_tasks([], []).
get_schedule_tasks([schedule(Core, [HTask|TTask])|Schedules], [HTask|ResTasks]) :-
	get_schedule_tasks([schedule(Core, TTask)|Schedules], ResTasks), !.
get_schedule_tasks([schedule(_,[])|Schedules], ResTasks) :-
	get_schedule_tasks(Schedules, ResTasks), !.

%% scheduled_on_core(+Task, +ScheduleList, -Core)
%% Returns core 'Core' on which 'Task' is scheduled in 'ScheduleList'
%% 'ScheduleList' must be a valid schedule
scheduled_on_core(Task, [schedule(Core, Tasks)|_], Core) :-
	member(Task, Tasks), !.
scheduled_on_core(Task, [_|Schedules], Core) :-
	scheduled_on_core(Task, Schedules, Core).	


%% core_scheduled_tasks(+Core, +ScheduleList, -Tasks)
%% Returns 'Tasks' scheduled on 'Core' in 'ScheduleList'
core_scheduled_tasks(Core, [schedule(Core, Tasks)|_], Tasks) :- !.
core_scheduled_tasks(Core, [_|OtherCores], Tasks) :-
	core_scheduled_tasks(Core, OtherCores, Tasks).	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% speedup(+S,-Speedup)
% Computes the Speedup of a given solution S
% Speedup = optimal sequential execution time / execution time of S
speedup(S,SpeedUp) :-
	optimal_sequential(ET1),
	execution_time(S, ET),
	SpeedUp is ET1 / ET,!.

% optimal_sequential(-ET1)
% Determines the optimal sequential execution time
optimal_sequential(ET1) :-
	findall(Core, core(Core), Cores),
	findall(Task, task(Task), Tasks),
	sort_by_dependencies(Tasks, SortedTasks),
	fastest_core(Cores, SortedTasks, FastestCore),
	core_time(FastestCore, SortedTasks, ET1),!.

% fastest_core(+Cores, +Tasks, -Core)
% Given a list of cores 'Cores', returns the fastest 'Core'
% by summing the cost of computing all tasks 'Tasks' per core
% and then taking the core with the lowest cost
fastest_core(Cores, Tasks, Core) :-
	fastest_core(Cores, Tasks, 1000000, nil, Core).
fastest_core([],_,_, Core, Core).
fastest_core([HCore|Cores], Tasks, CurrTime, _, Core) :-
	core_time(HCore, Tasks, Time),
	Time =< CurrTime, !,
	fastest_core(Cores, Tasks, Time, HCore, Core).
fastest_core([_|Cores], Tasks, CurrTime, CurrCore, Core) :-
	fastest_core(Cores, Tasks, CurrTime, CurrCore, Core).

% core_time(+Core, +Tasks, -TotalTime)
% Returns the occupancy time of a core 'Core' when processing 'Tasks'
core_time(_, [], 0).
core_time(Core, [HTask|Tasks], TotalTime) :-
	core_time(Core, Tasks, Time), !,
	process_cost(HTask, Core, TaskTime),
	TotalTime is Time + TaskTime.


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
	reverse(Cores, RCores),	% Cosmetics: reverse 'Cores' to get S in normal order: c1,c2,... instead of ...,c2,c1
	findall(Task, task(Task), Tasks),
	find_solution(RCores, Tasks, [], ScheduleList),
	S = solution(ScheduleList).

find_solution([],[], Result, Result).
find_solution([Core|Cores], [], AccSchedule, ScheduleList) :-	% Cores remaining, no more tasks
	find_solution(Cores, [], [schedule(Core,[])|AccSchedule], ScheduleList),!.
find_solution(Cores, Tasks, [schedule(CurrCore, ScheduleTasks)|OtherCores], ScheduleList) :- % Add a task
	task(T),
	member(T, Tasks),
	check_dependencies([T|ScheduleTasks]),
	delete_first(T, Tasks, Tasks2),
	find_solution(Cores, Tasks2, [schedule(CurrCore, [T|ScheduleTasks])|OtherCores], ScheduleList).
find_solution([CurrCore|Cores], Tasks, AccSchedule, ScheduleList) :-	% Switch to other Core
	find_solution(Cores, Tasks, [schedule(CurrCore,[])|AccSchedule], ScheduleList).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% find_heuristically(-S)
% Returns a schedule solution by heuristic:
% Each time a task is considered, it will be added to the
% (at that moment) core with lowest occupancy (based on execution time)
find_heuristically(S) :-
	findall(Core, core(Core), Cores),
	findall(Task, task(Task), Tasks),
	%sort_by_dependencies(Tasks, SortedTasks),
	create_empty_schedule(Cores, InitScheduleList),
	find_heuristically(Tasks, Cores, InitScheduleList, ScheduleList), !,
	S = solution(ScheduleList).

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
add_to_core(Task, Core, [schedule(Core, Tasks)|Cores], [schedule(Core, NewCoreSchedule)|Cores]) :-
	add2end(Task, Tasks, NewCoreSchedule).
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


%% sort_by_dependencies(+Tasks, -Sorted)
%% Sorts 'Tasks' in such way that (from left to right)
%% no dependencies are violated
sort_by_dependencies(Tasks, Sorted) :-
	sort_by_dependencies_do(Tasks, SortedNested),
	flatten(SortedNested, Sorted).
sort_by_dependencies_do([], []) :- !.
sort_by_dependencies_do(Tasks, [NonDeps|Sorted]) :-	
	% Find tasks not depending on any other tasks in the tasklist
	get_no_dep_tasks(Tasks, NonDeps),
	set_diff(Tasks, NonDeps, DepTasks),			% Remove dependencies from tasklist
	sort_by_dependencies_do(DepTasks, Sorted).

%% get_no_dep_tasks(+Tasks, -NonDeps)
%% Expects a list of tasks 'Tasks'
%% Returns tasks 'NonDeps' not depending on any
%% other task in 'Tasks'
get_no_dep_tasks(Tasks, NonDeps) :-
	findall(Task, (member(Task, Tasks), not((depends_on(Task, Dependency,_), member(Dependency, Tasks)))), NonDeps).

%% add2end(+E, +List, -NewList)
%% adds an element to the end of a list
%% != append (if List is empty, append only returns E, not [E])
add2end(E,[H|T],[H|NewT]) :- add2end(E,T,NewT).
add2end(E,[],[E]).

%% DEPRECATED
%% find_optimal_task(Tasks, ResultTask, ResultCore) :-
%% 	find_optimal_task(Tasks, 1000000, nil, nil, ResultTask, ResultCore).

%% find_optimal_task([],_, Task, Core, Task, Core).
%% find_optimal_task([HTask|Tasks], Min,_,_, ResultTask, ResultCore) :-
%% 	process_cost(HTask, Core, Time),
%% 	Time =< Min, !,
%% 	find_optimal_task(Tasks, Time, HTask, Core, ResultTask, ResultCore).
%% find_optimal_task([_|Tasks], Min, CurTask, CurCore, ResultTask, ResultCore) :-
%% 	find_optimal_task(Tasks, Min, CurTask, CurCore, ResultTask, ResultCore).

test_large(ET) :-
	find_heuristically(S),
	execution_time(S,ET).