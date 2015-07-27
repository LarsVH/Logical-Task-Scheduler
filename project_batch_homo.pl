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