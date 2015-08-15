% isSolution(+S)
%% Checks if S is a valid solution by checking if all cores and tasks are 
%% represented exactly once
isSolution(solution(ScheduleList)) :-
	findall(X, core(X), Cores),
	findall(Y, task(Y), Tasks),
	isSolution(ScheduleList, Cores, Tasks).

%% No more tasks nor cores remaining => this is a valid solution
isSolution([],[],[]).	
%% No more tasks remaining, only cores. Checking if each core is represented in list of schedules
isSolution([schedule(Core, [])|Schedules], Cores,[]) :-	
	delete_first(Core, Cores, NewCores),
	isSolution(Schedules, NewCores,[]).
isSolution([schedule(Core, Schedule)|Schedules], Cores, Tasks) :-
	delete_first(Core, Cores, NewCores),
	check_dependencies(Schedule),
	set_diff_strict(Tasks, Schedule, NewTasks),
	isSolution(Schedules, NewCores, NewTasks).

% check_dependencies(+TaskSchedule)
%% Checks if no dependencies are violated given 
%% the ORDER of tasks in 'TaskSchedule' (list of tasks)
check_dependencies([]).
check_dependencies([HTask|Tasks]) :-
	findall(DepTask, depends_on(HTask, DepTask,_), Deps), %% Get all tasks on which I 'HTask' depend
	intersection(Deps, Tasks, []),!,	% No task 'Deps' on which I depend should be scheduled after me 'Tasks'
	check_trans_dependencies(Deps, Tasks),	% Check if there are any transitive dependencies
	check_dependencies(Tasks).

%% check_trans_dependencies(+Dependencies, +Tasks)
%% Checks if any 'Dependencies' dependency has a 
%% transitive dependency on a task in 'Tasks'
%% p.e. t5 dep on t3, t3 dep on t1
%% => c1(t5,t1),c2(t3) is not a valid schedule
check_trans_dependencies([],_).
check_trans_dependencies([HDep|Deps], TaskList) :-
	not(member(HDep, TaskList)),
	findall(DepTask, depends_on(HDep, DepTask,_), TransDeps),
	check_trans_dependencies(TransDeps, TaskList),
	check_trans_dependencies(Deps, TaskList).

% delete_first(E,L1,L2)
%% L2 is L1 with the first occurance of E removed, fails if E does not occur in L1.
delete_first(E,[E|T],T) :- !.
delete_first(E,[H|T1],[H|T2]) :- 
	delete_first(E,T1,T2).

% set_diff_strict(+Set1, +Set2, -Diff)
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

% set_diff(+Set1, +Set2, -Diff)
%% Non-strict version of set_diff
set_diff([],_,[]).
set_diff([X|Y],Set2,Res):-
	not(member(X,Set2)), !,
	Res = [X|Diff],
	set_diff(Y,Set2,Diff).
set_diff([_|Y],Set2,Diff):-
	set_diff(Y,Set2,Diff).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% execution_time(+S,-ET)
%% Expects a valid scheduling solution and returns its Execution Time
execution_time(S, ET) :-
	execution_time(S,_,ET).

% execution_time(+S, -FinalTmstpSchedule, -ET)
%% Extended version of execution_time also returning 
%% a timestamped version of the schedule (for each task when it ends)
execution_time(solution(ScheduleList), FinalTmstpSchedule, ET) :-
	timestamped_schedule(ScheduleList, TmstpSchedule),
	get_schedule_tasks(ScheduleList, Tasks),	
	execution_time(ScheduleList, TmstpSchedule, Tasks, [], FinalTmstpSchedule), !,
	max_of_timestamps(FinalTmstpSchedule, ET).

% execution_time(+ScheduleList, +TmstpSchedule, +Tasks, +ProcessedTasks, FinalTmstpSchedule)
%% Gets tasks from 'Tasks' having no dependencies in current iteration  and are not 'blocked'
%% by other tasks that have to be executed before them.
%% Timestamps are then computed for this tasks and filled in 'TmstpSchedule' and they are moved 
%% from 'Tasks' to 'ProcessedTasks', initiating a new iteration.
execution_time(_, FinalTmstpSchedule, [],_, FinalTmstpSchedule) :- !.
execution_time(ScheduleList, TmstpSchedule, Tasks, ProcessedTasks, FinalTmstpSchedule) :-	
	get_no_dep_tasks(Tasks, NoDepTasks),
	have_only_processed_tasks_before(NoDepTasks, ScheduleList, ProcessedTasks, OPTBeforeTasks),
	etime_tasks(ScheduleList, OPTBeforeTasks, TmstpSchedule, ResTmstpSchedule),
	set_diff_strict(Tasks, OPTBeforeTasks, NewTasks),
	append(OPTBeforeTasks, ProcessedTasks, NewProcessedTasks),
	execution_time(ScheduleList, ResTmstpSchedule, NewTasks,  NewProcessedTasks, FinalTmstpSchedule).


% etime_tasks(+ScheduleList, +Tasks, +TmstpSchedule, -ResTmstpSchedule)
%% Fills in timestamps of 'Tasks' in 'TmstpSchedule' based on 'ScheduleList'
%% resulting in 'ResTmstpSchedule'
%% Given 'Tasks', their dependencies are retrieved. For each of these dependencies,
%% etime_deps then calculates at what timestamp 'DataTmstp' the last chunk of data arrives.
%% Based on 'DataTmstp', delay caused by the channel 'ChannelDelay' is computed on which 
%% 'channel_task_timestamp' can compute the final timestamp of the task 'TaskTimeStamp' which
%% is inserted into 'TmstpSchedule' by 'insert_timestamp' before proceeding to the next task
etime_tasks(_, [], FinalTmstpSchedule, FinalTmstpSchedule).
etime_tasks(ScheduleList, [HTask|Tasks], TmstpSchedule, FinalTmstpSchedule) :-
	findall(Dep, depends_on(HTask, Dep,_), Deps),
	scheduled_on_core(HTask, ScheduleList, Core),
	etime_deps(ScheduleList, HTask, Core, Deps, TmstpSchedule, DataTmstp),
	prev_task_timestamp(HTask, TmstpSchedule, PrevTaskTmstp),
	ChannelDelay is DataTmstp - PrevTaskTmstp,
	channel_task_timestamp(HTask, Core, PrevTaskTmstp, ChannelDelay, TaskTimeStamp),
	insert_timestamp(HTask, Core, TmstpSchedule, TaskTimeStamp, ResTmstpSchedule),
	etime_tasks(ScheduleList, Tasks, ResTmstpSchedule, FinalTmstpSchedule).


% etime_deps(+ScheduleList, +Task, +Core, +Deps, +TmstpSchedule, -DataTmstp)
%% Computes the timestamp at which the last chunk of data 
%% arrives from the dependent tasks 'Deps' of 'Task'
%% according to 'ScheduleList'
etime_deps(ScheduleList, Task, Core, Deps, TmstpSchedule, DataTmstp) :-
	etime_deps(ScheduleList, Task, Core, Deps, TmstpSchedule, 0, DataTmstp).

etime_deps(_,_,_,[],_, FinalDataTmstp, FinalDataTmstp) :- !.
etime_deps(ScheduleList, Task, TaskCore,[HDep|Deps], TmstpSchedule, CurrentMax, FinalDataTmstp) :-
	scheduled_on_core(HDep, ScheduleList, DepCore),
	DepCore = TaskCore, !,		%% CASE1: Dependency and task are scheduled on same core: no comm.
	task_timestamp(HDep, TmstpSchedule, DepTmstp),
	max(CurrentMax, DepTmstp, NewMax),
	etime_deps(ScheduleList, Task, TaskCore, Deps, TmstpSchedule, NewMax, FinalDataTmstp).
etime_deps(ScheduleList, Task, TaskCore, [HDep|Deps], TmstpSchedule, CurrentMax, FinalDataTmstp) :-
	task_timestamp(HDep, TmstpSchedule, DepTmstp),
	scheduled_on_core(HDep, ScheduleList, DepCore),
	channel(DepCore, TaskCore, Latency, Bandwidth), %%
	depends_on(Task, HDep, Data),
	DataDelay is Data/Bandwidth,
	DataTmstp is DepTmstp + Latency + DataDelay,	%% CASE2: Dependency and task scheduled on diff cores
	max(CurrentMax, DataTmstp, NewMax),
	etime_deps(ScheduleList, Task, TaskCore, Deps, TmstpSchedule, NewMax, FinalDataTmstp).


% channel_task_timestamp(+Task, +PrevTaskTmstp, + ChannelDelay, -TaskTimeStamp)
%% Computes the timestamp 'TaskTimeStamp' at which 'Task' completes, taking
%% 'PrevTaskTmstp' and 'ChannelDelay' into account. If 'ChannelDelay' is negative,
%% the channel does not cause any delay on the core. The task can be processed
%% immediately after its predecessor. If 'ChannelDelay' is positive, the executing
%% core has to wait for 'ChannelDelay' time after the predecessor has been processed.
channel_task_timestamp(Task, Core, PrevTaskTmstp, ChannelDelay, TaskTimeStamp) :-
	ChannelDelay =< 0, !,
	process_cost(Task, Core, TaskTime),
	TaskTimeStamp is PrevTaskTmstp + TaskTime.
channel_task_timestamp(Task, Core, PrevTaskTmstp, ChannelDelay, TaskTimeStamp) :-
	ChannelDelay >= 0,
	process_cost(Task, Core, TaskTime),
	TaskTimeStamp is PrevTaskTmstp + ChannelDelay + TaskTime.


% max_of_timestamps(+TmstpSchedule, -Max)
%% Returns the highest timsestamp 'Max' in TmstpSchedule
max_of_timestamps(TmstpSchedule, Max) :-
	max_of_timestamps(TmstpSchedule, 0, Max).

max_of_timestamps([], Max, Max).
max_of_timestamps([sch(Core, [[_, Tmstp]|Tasks])|Cores], CurrMax, Max) :- !,
	max(CurrMax, Tmstp, NewMax),
	max_of_timestamps([sch(Core, Tasks)|Cores], NewMax, Max).
max_of_timestamps([sch(_, [])|Cores], CurrMax, Max) :-
	max_of_timestamps(Cores, CurrMax, Max).


% task_timestamp(+Task, +TmstpSchedule, -TimeStamp)
%% Retrieves the 'Timestamp' of 'Task' from 'TmstpSchedule'
task_timestamp(Task, [sch(_, [[Task, TimeStamp]|_])|_], TimeStamp).
task_timestamp(MTask, [sch(_,[])|Cores], TimeStamp) :-
	task_timestamp(MTask, Cores, TimeStamp), !.
task_timestamp(MTask, [sch(Core, [[_,_]|Tasks])|Cores], TimeStamp) :-
	task_timestamp(MTask, [sch(Core, Tasks)|Cores], TimeStamp).


% insert_timestamp(+Task, +Core, +TmstpSchedule, +TimeStamp, -ResSchedule)
%% Changes the timestamp of 'Task' scheduled on 'Core' in 'TmstpSchedule' to 'TimeStamp',
%% returning 'ResSchedule'. The use of 'Core' is not mandatory for the algorithm, but is used here
%% to improve efficiency
insert_timestamp(Task, Core, [sch(Core, [[Task,_]|Tasks])|Cores], TimeStamp, [sch(Core, [[Task, TimeStamp]|Tasks])|Cores]).
insert_timestamp(Task, Core, [sch(Core, [[HTask, HTaskTmstp]|Tasks])|Cores], TimeStamp, [sch(Core, [[HTask, HTaskTmstp]|ResTasks])|ResCores]) :-
	insert_timestamp(Task, Core, [sch(Core, Tasks)|Cores], TimeStamp, [sch(Core, ResTasks)|ResCores]).
insert_timestamp(Task, Core, [sch(HCore, Tasks)|Cores], TimeStamp, [sch(HCore, Tasks)|ResCores]) :-
	insert_timestamp(Task, Core, Cores, TimeStamp, ResCores), !.


% prev_task_timestamp(+Task, +TmstpSchedule, -TimeStamp)
%% Retrieves the timestamp of the predecessing task of 'Task'
%% according to TmstpSchedule; If no predecessor on the same core
%% exists, 'TimeStamp' is set to 0
prev_task_timestamp(Task, TmstpSchedule, TimeStamp) :-
	prev_task_timestamp(Task, TmstpSchedule, [_, 0], TimeStamp).

prev_task_timestamp(Task, [sch(_, [[Task,_]|_])|_], [_, TimeStamp], TimeStamp).
prev_task_timestamp(MTask, [sch(Core, [TaskTmstp|TasksTmstps])|Cores],_,TimeStamp) :-
	prev_task_timestamp(MTask, [sch(Core, TasksTmstps)|Cores], TaskTmstp, TimeStamp), !.
prev_task_timestamp(MTask, [sch(_, [])|Cores], _, TimeStamp) :-
	prev_task_timestamp(MTask, Cores, [_,0], TimeStamp).
	

% timestamped_schedule(+ScheduleList, -TstpSchedule)
%% Creates an empty timestamped schedule 'TstpSchedule'
%% in accordance with 'ScheduleList'
timestamped_schedule([],[]).
timestamped_schedule([schedule(Core, [])|Cores], [sch(Core, [])|TstpCores]) :-
	timestamped_schedule(Cores, TstpCores), !.
timestamped_schedule([schedule(Core, [HTask|Tasks])|Cores], [sch(Core, [[HTask,0]|TstpTasks])|TstpCores]) :-
	timestamped_schedule([schedule(Core, Tasks)|Cores], [sch(Core, TstpTasks)|TstpCores]).


% have_only_processed_tasks_before(+Tasks, +ScheduleList, +ProcessedTasks, -OPTBeforeTasks)
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


% has_only_processed_tasks_before(+Task, +Tasks, +ProcessedTasks)
%% Checks if any task in 'Tasks' before 'Task' is already processed
%% e.g. member of 'ProcessedTasks'
%% 'Task' must be member of 'Tasks' !!
has_only_processed_tasks_before(Task, [Task|_],_) :- !.
has_only_processed_tasks_before(Task, [HTask|TTasks], ProcessedTasks) :-
	member(HTask, ProcessedTasks), !,
	has_only_processed_tasks_before(Task, TTasks, ProcessedTasks).

% max(?X, ?Y, ?Max)
%% Returns the maximum of X and Y
%% Optimized using a green cut
max(X,Y,Y) :- X =< Y, !.
max(X,Y,X) :- X > Y. 


% get_schedule_tasks(+ScheduleList, -Tasks)
%% Expects a list of schedules 'ScheduleList' and extracts its tasks.
get_schedule_tasks([], []).
get_schedule_tasks([schedule(Core, [HTask|TTask])|Schedules], [HTask|ResTasks]) :-
	get_schedule_tasks([schedule(Core, TTask)|Schedules], ResTasks), !.
get_schedule_tasks([schedule(_,[])|Schedules], ResTasks) :-
	get_schedule_tasks(Schedules, ResTasks), !.

% scheduled_on_core(+Task, +ScheduleList, -Core)
%% Returns core 'Core' on which 'Task' is scheduled in 'ScheduleList'
%% 'ScheduleList' must be a valid schedule
scheduled_on_core(Task, [schedule(Core, Tasks)|_], Core) :-
	member(Task, Tasks), !.
scheduled_on_core(Task, [_|Schedules], Core) :-
	scheduled_on_core(Task, Schedules, Core).	


% core_scheduled_tasks(+Core, +ScheduleList, -Tasks)
%% Returns 'Tasks' (in the same order) scheduled on 'Core' in 'ScheduleList'
core_scheduled_tasks(Core, [schedule(Core, Tasks)|_], Tasks) :- !.
core_scheduled_tasks(Core, [_|OtherCores], Tasks) :-
	core_scheduled_tasks(Core, OtherCores, Tasks).	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% speedup(+S,-Speedup)
%% Computes the Speedup of a given solution S
%% Speedup = optimal sequential execution time / execution time of S
speedup(S,SpeedUp) :-
	optimal_sequential(ET1),
	execution_time(S, ET),
	SpeedUp is ET1 / ET,!.

% optimal_sequential(-ET1)
%% Determines the optimal sequential execution time
optimal_sequential(ET1) :-
	findall(Core, core(Core), Cores),
	findall(Task, task(Task), Tasks),
	sort_by_dependencies(Tasks, SortedTasks),
	fastest_core(Cores, SortedTasks, FastestCore),
	core_time(FastestCore, SortedTasks, ET1),!.

% fastest_core(+Cores, +Tasks, -Core)
%% Given a list of cores 'Cores', returns the fastest 'Core'
%% by summing the cost of computing all tasks 'Tasks' per core
%% and then taking the core with the lowest cost
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
%% Returns the occupancy time of a core 'Core' when processing 'Tasks'
core_time(_, [], 0).
core_time(Core, [HTask|Tasks], TotalTime) :-
	core_time(Core, Tasks, Time), !,
	process_cost(HTask, Core, TaskTime),
	TotalTime is Time + TaskTime.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

:- dynamic best/2.

% find_optimal(-S)
%% Computes an optimal schedule S
find_optimal(_) :-
	optimal_sequential(ET1),
	%% TWEAK: TO Remove
	%%ET1 = 1000,
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
%% Generates any possible scheduling solution 'S'
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
%% Returns a schedule solution by heuristic:
%% Each time a task is considered, it will be added to the
%% (at that moment) core with lowest occupancy (based on execution time)
find_heuristically(S) :-
	findall(Core, core(Core), Cores),
	findall(Task, task(Task), Tasks),
	sort_by_dependencies(Tasks, [HDepTask|DepTasks]),
	create_empty_schedule(Cores, InitScheduleList),
	find_heuristically([HDepTask|DepTasks], Cores, InitScheduleList, FinalScheduleList), !,
	S = solution(FinalScheduleList).

% find_heuristically(+Tasks, +Cores, +PrevScheduleList, -FinalScheduleList)
%% Helper for find_heuristically, iterating over tasks
find_heuristically([],_,FinalScheduleList,FinalScheduleList) :- !.
find_heuristically([HTask|Tasks], Cores, PrevScheduleList, FinalScheduleList) :-	
	find_heuristically_core(HTask, Cores, PrevScheduleList, BestCore),
	add_to_core(HTask, BestCore, PrevScheduleList, NextScheduleList),
	find_heuristically(Tasks, Cores, NextScheduleList, FinalScheduleList).


% find_heuristically_core(+Task, +Cores, +ScheduleList, -FinalBestCore)
%% Helper for find_heuristically, iterating over cores (per task)
find_heuristically_core(Task, [HCore|Cores], ScheduleList, FinalBestCore) :-
	add_to_core(Task, HCore, ScheduleList, SimulatedScheduleList),
	execution_time(solution(SimulatedScheduleList), SimulatedET),
	find_heuristically_core(Task, Cores, ScheduleList, HCore, SimulatedET, FinalBestCore).

find_heuristically_core(_, [],_, FinalBestCore,_, FinalBestCore) :- !.
find_heuristically_core(Task, [HCore|Cores], ScheduleList,_, PrevBestET, FinalBestCore) :-
	add_to_core(Task, HCore, ScheduleList, SimulatedScheduleList),
	execution_time(solution(SimulatedScheduleList), CurrET),
	CurrET =< PrevBestET, !,
	find_heuristically_core(Task, Cores, ScheduleList, HCore, CurrET, FinalBestCore).
find_heuristically_core(Task, [_|Cores], ScheduleList, PrevBestCore, PrevBestET, FinalBestCore) :-
	find_heuristically_core(Task, Cores, ScheduleList, PrevBestCore, PrevBestET, FinalBestCore).

% create_empty_schedule(+Cores, -Schedule)
%% Creates a blank schedulelist 'Schedule' 
%% (schedule(c1,[t1,t2,...]), schedule(c2,...),...)
%% from the given list of cores 'Cores'
create_empty_schedule([], []).
create_empty_schedule([HCore|Cores], [schedule(HCore, [])|Schedule]) :-
	create_empty_schedule(Cores, Schedule).

% add_to_core(+Task, +Core, +ScheduleList, -ResultScheduleList)
%% Add a task 'Task' as last task to a core 'Core' in 'ScheduleList'
add_to_core(Task, Core, [schedule(Core, Tasks)|Cores], [schedule(Core, NewCoreSchedule)|Cores]) :-
	add2end(Task, Tasks, NewCoreSchedule), !.
add_to_core(Task, Core, [schedule(CurrCore, Tasks)|Cores], [schedule(CurrCore, Tasks)|RCores]) :-
	add_to_core(Task, Core, Cores, RCores).


% sort_by_dependencies(+Tasks, -Sorted)
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

% get_no_dep_tasks(+Tasks, -NonDeps)
%% Expects a list of tasks 'Tasks'
%% Returns tasks 'NonDeps' not depending on any
%% other task in 'Tasks'
get_no_dep_tasks(Tasks, NonDeps) :-
	findall(Task, (member(Task, Tasks), not((depends_on(Task, Dependency,_), member(Dependency, Tasks)))), NonDeps).

% add2end(+E, +List, -NewList)
%% adds an element to the end of a list
%% != append (if List is empty, append only returns E, not [E])
add2end(E,[H|T],[H|NewT]) :- add2end(E,T,NewT).
add2end(E,[],[E]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pretty_print(+S)
%% Prints a solution 'S' in a human readable format
pretty_print(solution(S)) :-
	execution_time(solution(S), TmstpSchedule, ET),
	write('\n'),
	write('============================='), write('\n'),
	write('Schedule:'), write('\n'),
	write('------------------------'), write('\n'),
	[schedule(Core, _)|_] = S,
	write('>>> Core: '), write(Core), write('\n'),
	pretty_print_loop(S), !,
	write('Execution Time: '), write(ET), write('\n'),
	write('============================='), write('\n'),
	speedup(solution(S), SpeedUp),
	write('SpeedUp: '), write(SpeedUp), write('\n'),
	write('============================='), write('\n'),
	write('Timestamped per task (endtimes): '), write('\n'),
	write(TmstpSchedule), write('\n'),
	write('============================='), write('\n'),
	write('Timeline View (starttimes)'), write('\n'),
	write('-----------------------------'), write('\n'),
	print_timeline(TmstpSchedule),
	write('============================='), write('\n').


pretty_print_loop([]) :-
	write('============================='), write('\n').
pretty_print_loop([schedule(Core, [HTask|Tasks])|Cores]) :-
	write('> Task: '), write(HTask), write('\n'),
	pretty_print_loop([schedule(Core, Tasks)|Cores]).
pretty_print_loop([schedule(_, []), schedule(Core, Tasks)|Cores]) :-
	write('>>> Core: '), write(Core), write('\n'),
	pretty_print_loop([schedule(Core, Tasks)|Cores]).
pretty_print_loop([schedule(_,[])|Cores]) :-
	pretty_print_loop(Cores).

% print_timeline(+TmstpSchedule)
%% Extention to 'pretty_print'
%% Prints a timeline on which the user can see
%% which tasks are executed after another, parallell, etc.
%% in a more visual way
print_timeline(TmstpSchedule) :-
	[sch(Core,_)|_] = TmstpSchedule,
	write(Core),
	print_timeline_loop(TmstpSchedule, 0).

print_timeline_loop([],_) :-
	write('\n'), !.
print_timeline_loop([sch(CurrCore, [[CurrTask,Tmstp]|Tasks])|Cores], PrevTmstp) :-
	RelTmstp is Tmstp / 10,
	Spaces is RelTmstp - PrevTmstp,
	write_spaces(Spaces),
	write(CurrTask), !,
	print_timeline_loop([sch(CurrCore, Tasks)|Cores], RelTmstp).
print_timeline_loop([sch(_,[]), sch(Core, Tasks)|Cores],_) :-
	write('\n'),
	write(Core), !,
	print_timeline_loop([sch(Core, Tasks)|Cores], 0).
print_timeline_loop([sch(_,[])|Cores],_) :-
	print_timeline_loop(Cores, 0).

write_spaces(Spaces) :- 
	Spaces =< 0, !.
write_spaces(Spaces) :-
	write(' '),
	NewSpaces is Spaces - 1,
	write_spaces(NewSpaces).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test_optimal(-S, -ET)
%% Predicate for testing optimal
%% Returns the solution 'S' found and its execution_time
test_optimal(S, ET) :-
	find_optimal(S),
	execution_time(S,ET).

% test_heuristically(-S, -ET)
%% Analog to test-optimal
test_heuristically(S, ET) :-
	find_heuristically(S),
	execution_time(S, ET).