:- module(formulae,
	[ list_to_conj/3,
	  list_to_conj/2,
	  conj_to_list/2,
	  list_to_disj/2,
	  disj_to_list/2,
	  conj_to_llist/2,
	  llist_to_conj/2,
	  disj_to_llist/2,
	  llist_to_disj/2,
	  %
	  body2list/2,
	  asbody_to_conj/2
	]).

list_to_conj([X|More],(X,Next),End):-
        list_to_conj(More,Next,End).
list_to_conj([],End,End).

list_to_conj([], true) :- !.
list_to_conj([A|B], (A,Br)) :- B \== [], !,
	list_to_conj_(B, Br).
list_to_conj([A], A) :- !.
list_to_conj(A, _) :-
	throw(error(domain_error(list, A), list_to_conj/2)).

list_to_conj_(B, Br) :- var(B), var(Br), !,
	[Br] = B.
list_to_conj_(B, Br) :-
	list_to_conj(B, Br).

conj_to_list(A, B) :- list_to_conj(B, A). % TODO: recover faster impl

list_to_disj([], false) :- !.
list_to_disj([A|B], (A;Br)) :- B \== [], !,
	list_to_disj_(B, Br).
list_to_disj([A], A) :- !.
list_to_disj(A, _) :-
	throw(error(domain_error(list, A), list_to_disj/2)).

list_to_disj_(B, Br) :- var(B), var(Br), !,
	[Br] = B.
list_to_disj_(B, Br) :-
	list_to_disj(B, Br).

list_to_disj2([],false):- !.
list_to_disj2([X],X):- !.
list_to_disj2([X|Xs],(X;Ys)):-
	list_to_disj2(Xs,Ys).

disj_to_list(D, L) :-
	list_to_disj(L, D). % TODO: recover efficient impl

conj_to_llist(D,L):-
	conj_to_llist_diff(D,L,[]).

conj_to_llist_diff((A,B),LL,LT):- !,
	conj_to_llist_diff(A,LL,LA),
	conj_to_llist_diff(B,LA,LT).
conj_to_llist_diff((A;B),[LL|LT],LT):- !,
	disj_to_llist_diff(A,LL,LA),
	disj_to_llist_diff(B,LA,[]).
conj_to_llist_diff(A,[A|LT],LT).

llist_to_conj([LL],C):- !,
	llist_to_disj(LL,C).
llist_to_conj([LL|LLs],(C,Cs)):- !,
	llist_to_disj(LL,C),
	llist_to_conj(LLs,Cs).
llist_to_conj(C,C).

disj_to_llist(D,L):-
	disj_to_llist_diff(D,L,[]).

disj_to_llist_diff((A;B),LL,LT):- !,
	disj_to_llist_diff(A,LL,LA),
	disj_to_llist_diff(B,LA,LT).
disj_to_llist_diff((A,B),[LL|LT],LT):- !,
	conj_to_llist_diff(A,LL,LA),
	conj_to_llist_diff(B,LA,[]).
disj_to_llist_diff(A,[A|LT],LT).

llist_to_disj([LL],D):- !,
	llist_to_conj(LL,D).
llist_to_disj([LL|LLs],(D;Ds)):- !,
	llist_to_conj(LL,D),
	llist_to_disj(LLs,Ds).
llist_to_disj(D,D).

asbody_to_conj(A, B) :- var(A), !,
	( var(B) ->
	    A = B
	; conj_to_list_of_list(B, A, [])
	).
asbody_to_conj(A, B) :-
	list_of_list_to_conj(A, B).

list_of_list_to_conj([(A;B)|C], Out) :- !,
	list_to_conj(A, AC),
	list_of_list_to_conj([B], BC),
	list_of_list_to_conj(C, CC),
	( CC == true ->
	    Out = (AC;BC)
	; Out = ((AC;BC),CC)
	).
list_of_list_to_conj([A|B], (AC,BC)) :- B \== [], !,
	list_of_list_to_conj(A, AC),
	list_of_list_to_conj(B, BC).
list_of_list_to_conj([AL], A) :-
	list_of_list_to_conj(AL, A),
	!.
list_of_list_to_conj(AL, A) :-
	is_list(AL), % TODO: calling a prop, instantiation?
	!,
	list_to_conj(AL, A).
list_of_list_to_conj(A, A).

conj_to_list_of_list((A,B), Ac, TAc) :- !,
	conj_to_list_of_list(A, Ac, T),
	conj_to_list_of_list(B, T, TAc).
conj_to_list_of_list((A;B), [(AC;BC)|T], T) :- !,
	conj_to_list__(A, AC),
	conj_to_list__(B, BC).
conj_to_list_of_list(true, T, T) :- !.
conj_to_list_of_list(A, [A|T], T).

conj_to_list__((A,B), [A|Bs]) :- !,
	conj_to_list(B, Bs).
conj_to_list__((A;B), (As;Bs)) :- !,
	conj_to_list(A, As),
	conj_to_list__(B, Bs).
conj_to_list__(A, [A]).

body2list((First,Rest), [NewFirst|More]) :- !,
	p_exp2list(First, NewFirst),
        body2list(Rest, More).
body2list(Last, [NewLast]) :-
	p_exp2list(Last, NewLast).

p_exp2list('&'(G,Goals), [G|More]) :- !,
	p_exp2list__(Goals, More).
p_exp2list(Goal, Goal).

p_exp2list__('&'(G,Goals), [G|More]) :-
	p_exp2list__(Goals, More).
p_exp2list__(Goal, [Goal]).

