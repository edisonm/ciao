:- module(attr,
          [ get_attr_local/2,
            put_attr_local/2,
            del_attr_local/1
          ]).
:- meta_predicate
    get_attr_local(:, -),
    put_attr_local(:, +),
    del_attr_local(:).

:- multifile
    user:goal_expansion/2.

user:goal_expansion(get_attr_local(Var, Value),
                    get_attr(Var, Module, Value)) :-
    prolog_load_context(module, Module).
user:goal_expansion(put_attr_local(Var, Value),
                    put_attr(Var, Module, Value)) :-
    prolog_load_context(module, Module).
user:goal_expansion(del_attr_local(Var),
                    del_attr(Var, Module)) :-
    prolog_load_context(module, Module).

get_attr_local(M:Var, Value) :-
    get_attr(Var, M, Value).
put_attr_local(M:Var, Value) :-
    put_attr(Var, M, Value).
del_attr_local(M:Var) :-
    del_attr(Var, M).

