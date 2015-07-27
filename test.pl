set_diff_strict([],[],[]).
set_diff_strict([X|Y],Set2,Res):-
	not(member(X,Set2)), !,
	Res = [X|Diff],
	set_diff_strict(Y,Set2,Diff).
set_diff_strict([X|Y],Set2,Diff):-
	delete_first(X, Set2, Set2New),
	set_diff_strict(Y,Set2New,Diff).

brol(sol(A,B,C)).

delete_first(E,[E|T],T) :- !.
delete_first(E,[H|T1],[H|T2]) :- 
	delete_first(E,T1,T2).

