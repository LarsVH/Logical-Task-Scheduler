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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
	execution_time([schedule(_,Ttasks)|Schedules], TimeSoFar2, PreviousET, ET).	

%% max(?X, ?Y, ?Max)
%% Returns the maximum of X and Y
%% Optimized using a green cut
max(X,Y,Y) :- X =< Y, !.
max(X,Y,X) :- X > Y. 