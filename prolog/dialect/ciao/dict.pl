/* Copyright (C) 1996-2002 UPM-CLIP */

:- module(dict,[dictionary/1, dictionary/5, dic_node/2,
		 dic_lookup/3, dic_lookup/4,
		 dic_get/3, dic_replace/4,
		 old_or_new/1,
		 non_empty_dictionary/1]).

dictionary(X) :- term(X).  % A free variable is a valid dictionary.

non_empty_dictionary(dic(K,V,L,R)):-
	term(K),
	term(V),
	dictionary(L),
	dictionary(R).

term(_).

old_or_new(old).
old_or_new(new).

dictionary(dic(K,V,L,R),K,V,L,R).

dic_node([], _) :- !, fail. % variable
dic_node(Node, Node).
dic_node(dic(_,_,L,_), Node) :- dic_node(L, Node).
dic_node(dic(_,_,_,R), Node) :- dic_node(R, Node).

dic_lookup(Dic, Key, Val) :-
        dic_lookup(Dic, Key, Val, _Occ).

dic_lookup(Dic, Key, Val, Occ) :-
	var(Dic), !,
	Dic=dic(Key,Val,_,_),
        Occ = new.
dic_lookup(dic(K,V,L,R), Key, Val, Occ) :-
	compare(Rel, Key, K),
	dic_lookup_(Rel, Key, Val, V, L, R, Occ).

dic_lookup_(=, _, Val, Val, _, _, old).
dic_lookup_(<, Key, Val, _, L, _, Occ) :- dic_lookup(L, Key, Val, Occ).
dic_lookup_(>, Key, Val, _, _, R, Occ) :- dic_lookup(R, Key, Val, Occ).

dic_get(Dic, Key, Val) :-
	nonvar(Dic),
	Dic=dic(K,V,L,R),
	compare(X, Key, K),
	dic_get_(X, Key, Val, V, L, R).

dic_get_(=, _, Val, Val, _, _).
dic_get_(<, Key, Val, _, L, _) :- dic_get(L, Key, Val).
dic_get_(>, Key, Val, _, _, R) :- dic_get(R, Key, Val).

dic_replace(Dic, Key, Val, Dic1) :-
	var(Dic), !,
	Dic1=dic(Key,Val,_,_).
dic_replace(dic(Key1,Val1,L1,R1), Key, Val, dic(Key1,Val2,L2,R2)) :-
	compare(X, Key, Key1),
	dic_replace_(X, Key, Val, Key1, Val1, L1, R1, Val2, L2, R2).

dic_replace_(=, _, Val, _, _, L, R, Val, L, R).
dic_replace_(<, Key, Val, _, Val1, L1, R, Val1, L2, R) :-
	dic_replace(L1, Key, Val, L2).
dic_replace_(>, Key, Val, _, Val1, L, R1, Val1, L, R2) :-
	dic_replace(R1, Key, Val, R2).
