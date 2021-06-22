:- module(idlists,
	[ member_0/2, memberchk/2,
	  list_insert/2, add_after/4, add_before/4, delete/3,
	  subtract/3, union_idlists/3
	]).

memberchk(X, Ys) :- member_0(X, Ys).

member_0(X, [Y|_]) :- X == Y, !.
member_0(X, [_|L]) :- member_0(X, L).

list_insert(List, Term) :-
	var(List), !,
	List=[Term|_].
list_insert([Term0|_], Term) :-
	Term0==Term, !.
list_insert([_|List], Term) :-
	list_insert(List, Term).

add_after([], _, E, [E]).
add_after([E|Es], E0, E1, NEs) :-
        E == E0, !,
        NEs = [E0,E1|Es].
add_after([E|Es], E0, E1, [E|NEs]) :-
        add_after(Es, E0, E1, NEs).

add_before(L, E0, E, NL) :-
        add_before_existing(L, E0, E, NL), !.
add_before(L, _, E, [E|L]).

add_before_existing([E|Es], E0, E1, NEs) :-
        E == E0, !,
        NEs = [E1,E0|Es].
add_before_existing([E|Es], E0, E1, NEEs) :-
        add_before_existing(Es, E0, E1, NEs), !,
        NEEs = [E|NEs].

delete([], _, []).
delete([Head|Tail], Element, Rest) :-
	Head==Element, !,
	delete(Tail, Element, Rest).
delete([Head|Tail], Element, [Head|Rest]) :-
	delete(Tail, Element, Rest).

subtract([], _, []).
subtract([Element|Residue], Set, Difference) :-
	memberchk(Element, Set), !, 
	subtract(Residue, Set, Difference).
subtract([Element|Residue], Set, [Element|Difference]) :-
	subtract(Residue, Set, Difference).

union_idlists([],Ys,Ys).
union_idlists([X|Xs],Ys,Zs) :- 
	( memberchk(X,Ys) ->
	  Zs = Ws
        ; Zs = [X|Ws]
        ),
	union_idlists(Xs,Ys,Ws).
