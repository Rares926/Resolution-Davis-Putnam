% return the negated form of the literal
negate(n(A),A).
negate(A,n(A)).

% checks if two listst are the same --> same params same order 
same(X, X).
same([A| B], [C| D]):-
    A=C,
    same(B,D).


%orders two lists 
check_same(A,B):-
    sort(A,A_sorted),
    sort(B,B_sorted),
    same(A_sorted,B_sorted).

% merges two list without the duplicates
merge_lists_no_duplicates(L1, L2, R):-
    append(L1,L2,R1),
    list_to_set(R1, R).

% gets 
subtract_helper(_,[],R1,R1).
subtract_helper([C,D],[[H1,H2]|T],R1,R2):-
    subtract(C,[H1],R3),
    subtract(D,[H2],R4),
    merge_lists_no_duplicates(R3,R4,R5),
    append(R1,[R5],R1_NEW),
    subtract_helper([C,D],T,R1_NEW,R2).


%removes duplicates from a list 
remove_duplicates([],[]).
remove_duplicates(X,Y) :-
    setof(Z,member(Z,X),Y).

% returns all combinations of 2 elements
comb_of_2(_,[]):- true.
comb_of_2([X|T],[X|Comb]):-
    comb_of_2(T,Comb).
comb_of_2([_|T],[X|Comb]):-
    comb_of_2(T,[X|Comb]).

% finds each combination of value negated value from 2 lists in the good order 
fnv([],_,R1,R1).
fnv([H|T],L2,R1,R2):-
    negate(H,N),
    member(N,L2),
    append(R1,[[H,N]],R1_NEW),
    fnv(T,L2,R1_NEW,R2).
fnv([H|T],L2,R1,R2):-
    fnv(T,L2,R1,R2).

% to_append is always a list of lists if it is an empty list it does not append nothing
% if it contains lists in it it appends those list to the orig KB 
append_helper(KB,[],KB).
append_helper(KB,TO_APPEND,R):-
    append(KB,TO_APPEND,R).

%applies resolution on a pair of two lists 
resolution([C,D], R3):-
    fnv(C,D,[],R2),
    subtract_helper([C,D],R2,[],R3).

% applies resolution on every pair of two lists 
res_on_all([],KB,KB).
res_on_all([H|T],KB,R):-
    resolution(H,R2),
    append_helper(KB,R2,KB_NEW),
    res_on_all(T,KB_NEW,R).


res(KB):- 
    member([], KB),
    write("unsatisfiable"),!.
res(KB):-
    findall([A,B],comb_of_2(KB,[A,B]),ALL_COMB),
    res_on_all(ALL_COMB,KB,R),
    remove_duplicates(R,KB_NEW_NO_DUPLICATES),
    (
    check_same(KB,KB_NEW_NO_DUPLICATES)
    ->
    write("satisfiable");
    res(KB_NEW_NO_DUPLICATES)
    ),!.


apply_res_on_all([]).
apply_res_on_all([H|T]):-
    write(H),
    nl,
    res(H),
    nl,
    apply_res_on_all(T).

read_file(S,[]) :-
    at_end_of_stream(S).
read_file(S,[L|R]) :- 
    not(at_end_of_stream(S)),
    read(S,L),
    read_file(S,R).

apply_res_on_file:- 
    open('ex1_data.txt', read, S),
    read_file(S, L),
    apply_res_on_all(L),
    close(S).