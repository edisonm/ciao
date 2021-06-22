:- module(sets,
	[ insert/3, ord_delete/3,
	  ord_member/2, ord_test_member/3, ord_subtract/3,
	  ord_intersection/3, ord_intersection_diff/4, ord_intersect/2,
	  ord_subset/2, ord_subset_diff/3,
	  ord_union/3, ord_union_diff/4, ord_union_symdiff/4,
	  ord_union_change/3, merge/3,
	  ord_disjoint/2, setproduct/3
	]).

insert([], Element, [Element]).
insert([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	insert_comp(Order, Head, Tail, Element, Set).

insert_comp(<, Head, Tail, Element, [Head|Set]) :-
	insert(Tail, Element, Set).
insert_comp(=, Head, Tail, _, [Head|Tail]).
insert_comp(>, Head, Tail, Element, [Element,Head|Tail]).

ord_delete([Y|Ys],X,Zs) :-
	X @> Y, !,
	Zs = [Y|NewYs],
	ord_delete(Ys,X,NewYs).
ord_delete([Y|Ys],X,Zs) :-
	X == Y, !,
	Zs = Ys.
ord_delete([Y|Ys],_X,Zs) :-
	/* X @< Y */
	Zs = [Y|Ys].
ord_delete([],_,[]).

ord_member(X,[Y|Ys]) :-
	compare(D,X,Y),
	ord_member1(D,X,Ys).

ord_member1(=,_,_).
ord_member1(>,X,[Y|Ys]):-
	compare(D,X,Y),
	ord_member1(D,X,Ys).

ord_test_member([],_Y,no).
ord_test_member([X|Xs],Y,Flag) :-
	compare(D,X,Y),
	ord_test_member_(D,Xs,Y,Flag).

ord_test_member_(=,_,_,yes).
ord_test_member_(<,Xs,Y,Flag):-
	ord_test_member(Xs,Y,Flag).
ord_test_member_(>,_Xs,_Y,no).

ord_subtract([], _, []) :- !.
ord_subtract(Set1, [], Set1) :- !.
ord_subtract([Head1|Tail1], [Head2|Tail2], Difference) :-
	compare(Order, Head1, Head2),
	ord_subtract_(Order, Head1, Tail1, Head2, Tail2, Difference).

ord_subtract_(<, Head1, [], _, _, [Head1]) :- !.
ord_subtract_(<, Head0, [Head1|Tail1], Head2, Tail2, [Head0|Difference]) :-
	compare(Order, Head1, Head2),
	ord_subtract_(Order, Head1, Tail1, Head2, Tail2, Difference).
ord_subtract_(=, _, Tail1, _, Tail2, Difference) :-
	ord_subtract(Tail1, Tail2, Difference).
ord_subtract_(>, Head1, Tail1, _, [], [Head1|Tail1]) :- !.
ord_subtract_(>, Head1, Tail1, _, [Head2|Tail2], Difference) :-
	compare(Order, Head1, Head2),
	ord_subtract_(Order, Head1, Tail1, Head2, Tail2, Difference).

ord_intersect([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_intersect_(Order, Head1, Tail1, Head2, Tail2).

ord_intersect_(<, _, [Head1|Tail1], Head2, Tail2) :-
	compare(Order, Head1, Head2),
	ord_intersect_(Order, Head1, Tail1, Head2, Tail2).
ord_intersect_(=, _, _, _, _).
ord_intersect_(>, Head1, Tail1, _, [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_intersect_(Order, Head1, Tail1, Head2, Tail2).

ord_intersection([], _, []) :- !.
ord_intersection(_, [], []) :- !.
ord_intersection([Head1|Tail1], [Head2|Tail2], Intersection) :-
	compare(Order, Head1, Head2),
	ord_intersection_(Order, Head1, Tail1, Head2, Tail2, Intersection).

ord_intersection_(<, _, [], _, _, []) :- !.
ord_intersection_(<, _, [Head1|Tail1], Head2, Tail2, Intersection) :-
	compare(Order, Head1, Head2),
	ord_intersection_(Order, Head1, Tail1, Head2, Tail2, Intersection).
ord_intersection_(=, Head, Tail1, _, Tail2, [Head|Intersection]) :-
	ord_intersection(Tail1, Tail2, Intersection).
ord_intersection_(>, _, _, _, [], []) :- !.
ord_intersection_(>, Head1, Tail1, _, [Head2|Tail2], Intersection) :-
	compare(Order, Head1, Head2),
	ord_intersection_(Order, Head1, Tail1, Head2, Tail2, Intersection).

ord_intersection_diff([], _, [], []) :- !.
ord_intersection_diff(Xs, [], [], Xs) :- !.
ord_intersection_diff([O|Os], [N|Ns], Int, NotInt) :-
	compare(C, O, N), 
	ord_intersection_diff_(C, O, Os, N, Ns, Int, NotInt).
	
ord_intersection_diff_(<, O, [], _, _, [], [O]) :- !.
ord_intersection_diff_(<, O1, [O|Os], N, Ns, Int, [O1|NotInt]) :-
	compare(C, O, N), 
	ord_intersection_diff_(C, O, Os, N, Ns, Int, NotInt).
ord_intersection_diff_(=, _, Os, N, Ns, [N|Int], NotInt) :-
	ord_intersection_diff(Os, Ns, Int, NotInt).
ord_intersection_diff_(>, O, Os, _, [], [], [O|Os]) :- !.
ord_intersection_diff_(>, O, Os, _, [N|Ns], Int, NotInt) :-
	compare(C, O, N), 
	ord_intersection_diff_(C, O, Os, N, Ns, Int, NotInt).

ord_subset([], _).
ord_subset([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_subset_(Order, Head1, Tail1, Tail2).

ord_subset_(=, _, Tail1, Tail2) :-
	ord_subset(Tail1, Tail2).
ord_subset_(>, Head1, Tail1, [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_subset_(Order, Head1, Tail1, Tail2).

ord_subset_diff([], Ys, Ys).
ord_subset_diff([Head1|Tail1], [Head2|Tail2], Diff) :-
	compare(Order, Head1, Head2),
	ord_subset_diff_(Order, Head1, Tail1, Head2,Tail2, Diff).

ord_subset_diff_(=, _, Tail1, _, Tail2, Diff) :-
	ord_subset_diff(Tail1, Tail2, Diff).
ord_subset_diff_(>, Head1, Tail1, Head2, [Head3|Tail2], [Head2|Diff]) :-
	compare(Order, Head1, Head3),
	ord_subset_diff_(Order, Head1, Tail1, Head3, Tail2, Diff).

ord_union([], Set2, Set2) :- !.
ord_union(Set1, [], Set1) :- !.
ord_union([Head1|Tail1], [Head2|Tail2], Union) :-
	compare(Order, Head1, Head2),
	ord_union_(Order, Head1, Tail1, Head2, Tail2, Union).

ord_union_(<, Head0, [], Head2, Tail2, [Head0,Head2|Tail2]) :- !.
ord_union_(<, Head0, [Head1|Tail1], Head2, Tail2, [Head0|Union]) :-
	compare(Order, Head1, Head2),
	ord_union_(Order, Head1, Tail1, Head2, Tail2, Union).
ord_union_(=, Head,  Tail1, _,	  Tail2, [Head|Union]) :-
	ord_union(Tail1, Tail2, Union).
ord_union_(>, Head1, Tail1, Head0, [], [Head0,Head1|Tail1]) :- !.
ord_union_(>, Head1, Tail1, Head0, [Head2|Tail2], [Head0|Union]) :-
	compare(Order, Head1, Head2),
	ord_union_(Order, Head1, Tail1, Head2, Tail2, Union).

merge(Set1,Set2,Union):- ord_union(Set1,Set2,Union).

ord_union_change(Set1, [], Set1) :- !,
	nonempty(Set1).
ord_union_change([Head1|Tail1], [Head2|Tail2], Union) :-
	compare(Order, Head1, Head2),
	ord_union_change_(Order, Head1, Tail1, Head2, Tail2, Union).

ord_union_change_(<, Head0, [], Head2, Tail2, [Head0,Head2|Tail2]) :- !.
ord_union_change_(<, Head0, [Head1|Tail1], Head2, Tail2, [Head0|Union]) :-
	compare(Order, Head1, Head2),
	ord_union_(Order, Head1, Tail1, Head2, Tail2, Union).
ord_union_change_(=, Head,  Tail1, _, Tail2, [Head|Union]) :-
	ord_union_change(Tail1, Tail2, Union).
ord_union_change_(>, Head1, Tail1, Head0, [], [Head0,Head1|Tail1]) :- !.
ord_union_change_(>, Head1, Tail1, Head0, [Head2|Tail2], [Head0|Union]) :-
	compare(Order, Head1, Head2),
	ord_union_change_(Order, Head1, Tail1, Head2, Tail2, Union).

nonempty([_|_]).

ord_union_diff([], Set, Set, Set) :- !.
ord_union_diff(Set, [], Set, []) :- !.
ord_union_diff([O|Os], [N|Ns], Set, New) :-
	compare(C, O, N), 
	ord_union_diff_(C, O, Os, N, Ns, Set, New).
	
ord_union_diff_(<, O, [], N, Ns, [O,N|Ns], [N|Ns]) :- !.
ord_union_diff_(<, O1, [O|Os], N, Ns, [O1|Set], New) :-
	compare(C, O, N), 
	ord_union_diff_(C, O, Os, N, Ns, Set, New).
ord_union_diff_(=, _, Os, N, Ns, [N|Set], New) :-
	ord_union_diff(Os, Ns, Set, New).
ord_union_diff_(>, O, Os, N, [], [N,O|Os], [N]) :- !.
ord_union_diff_(>, O, Os, N1, [N|Ns], [N1|Set], [N1|New]) :-
	compare(C, O, N), 
	ord_union_diff_(C, O, Os, N, Ns, Set, New).

ord_union_symdiff(Set1,Set2,Union,Diff):-
	ord_symdiff_union(Set1,Set2,Diff,Union).

ord_symdiff_union([],Set2,Set2,Set2) :- !.
ord_symdiff_union(Set1,[],Set1,Set1) :- !.
ord_symdiff_union([H1|Tail1],[H2|Tail2],Diff,Union) :-
	compare(Order,H1,H2),
	ord_symdiff_union_(Order,H1,Tail1,H2,Tail2,Diff,Union).

ord_symdiff_union_(<,H0,[],H2,Tail2,Diff,Union) :- !,
	Diff = [H0,H2|Tail2],
	Union = Diff.
ord_symdiff_union_(<,H0,[H1|Tail1],H2,Tail2,[H0|Diff],[H0|Union]) :-
	compare(Order,H1,H2),
	ord_symdiff_union_(Order,H1,Tail1,H2,Tail2,Diff,Union).
ord_symdiff_union_(=,H1,Tail1,_,Tail2,Diff,[H1|Union]) :-
	ord_symdiff_union(Tail1,Tail2,Diff,Union).
ord_symdiff_union_(>,H1,Tail1,H0,[],Diff,Union) :- !,
	Diff = [H0,H1|Tail1],
	Union = Diff.
ord_symdiff_union_(>,H1,Tail1,H0,[H2|Tail2],[H0|Diff],[H0|Union]) :-
	compare(Order,H1,H2),
	ord_symdiff_union_(Order,H1,Tail1,H2,Tail2,Diff,Union).

ord_disjoint([], _) :- !.
ord_disjoint(_, []) :- !.
ord_disjoint([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_disjoint_(Order, Head1, Tail1, Head2, Tail2).

ord_disjoint_(<, _, [], _, _) :- !.
ord_disjoint_(<, _, [Head1|Tail1], Head2, Tail2) :-
	compare(Order, Head1, Head2),
	ord_disjoint_(Order, Head1, Tail1, Head2, Tail2).
ord_disjoint_(>, _, _, _, []) :- !.
ord_disjoint_(>, Head1, Tail1, _, [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_disjoint_(Order, Head1, Tail1, Head2, Tail2).

setproduct([], _, []).
setproduct([Head|Tail], Set, SetProduct)  :-
	setproduct_(Set, Head, SetProduct, Rest),
	setproduct(Tail, Set, Rest).

setproduct_([], _, Set, Set).
setproduct_([Head|Tail], X, [Set|TailX], Tl) :-
	sort([Head,X],Set),
	setproduct_(Tail, X, TailX, Tl).
