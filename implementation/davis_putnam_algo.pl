
negate(n(A),A).
negate(A,n(A)).

minus(A,B,R):-
    R is A-B.

%merges all the elements from a list of lists into a single list 
merge_lists_of_lists([],R1,R1).
merge_lists_of_lists([H|T],R1,R):-
    append(R1,H,R1_NEW),
    merge_lists_of_lists(T,R1_NEW,R).

%merges all the elements from a list of lists into a single list with no duplicated elements
merge_lists_of_lists_no_dup([],R1,R1).
merge_lists_of_lists_no_dup([H|T],R1,R):-
    append(R1,H,R1_NEW),
    list_to_set(R1_NEW, R1_NEW2),
    merge_lists_of_lists_no_dup(T,R1_NEW2,R).

% return the number of occurences of an element from a list 
nooc([],_,0).
nooc([H|T],E,N):-
    H=E,
    nooc(T,E,N1),
    N is N1+1.
nooc([H|T],E,N):-
    nooc(T,E,N).

% creates a frequency vector 
make_frequency_vec([],_,TMP,TMP).
make_frequency_vec([H|T],LD,TMP,R):-
    nooc(LD,H,OC),
    append(TMP,[[H,OC]],TMP_NEW),
    make_frequency_vec(T,LD,TMP_NEW,R).

% creates a frequency vector of balances for each element balance =abs(nr(negated)a-nr(a))
make_balance_frequency_vec([],_,TMP,TMP):-!.
make_balance_frequency_vec([H|T],LD,TMP,R):-
    nooc(LD,H,OC),
    negate(H,H_N),
    nooc(LD,H_N,OC_N),
    minus(OC,OC_N,EX),
    abs(EX,EX_ABS),
    append(TMP,[[H,EX_ABS]],TMP_NEW),
    make_balance_frequency_vec(T,LD,TMP_NEW,R),!.


% returns the element with the max appearences from a frequency vector 
max_app_element([[A,B]],A).
max_app_element([[A,B],[A1,B1]|T],M) :-
    B>=B1,
    max_app_element([[A,B]|T],M),!.
max_app_element([[A,B],[A1,B1]|T],M) :-
    B<B1,
    max_app_element([[A1,B1]|T],M),!.

%return all possible a,b,c,d evben if let s say a does not appear and n(a) appears
all_possible_non_negated_literals([],[]):-!.
all_possible_non_negated_literals([H|T],[H_NEG|R]):-
    negate(H,H_NEG),
    H_NEG\=n(H),
    all_possible_non_negated_literals(T,R).
all_possible_non_negated_literals([H|T],[H|R]):-
    negate(H,H_NEG),
    H_NEG==n(H),
    all_possible_non_negated_literals(T,R).

% return the element with the most apperences from a list of lists
elem_with_most_app(L,R):-
    merge_lists_of_lists_no_dup(L,[],LND),
    merge_lists_of_lists(L,[],LD),
    make_frequency_vec(LND,LD,[],R2),
    max_app_element(R2,R),!.

% return the least balanced element 
elem_least_balanced(L,R):-
    merge_lists_of_lists_no_dup(L,[],LND),
    merge_lists_of_lists(L,[],LD),
    all_possible_non_negated_literals(LND,LND_NN),
    list_to_set(LND_NN,LND_NN_SET),
    make_balance_frequency_vec(LND_NN_SET,LD,[],R2),
    max_app_element(R2,R),!.


% does the * between c and a literal based on book rules page 77
reduction(L, C, R):-
    negate(C,C_N),
    member(C_N,L),
    subtract(L,[C_N],R),
    !.
reduction(L, C, "IN"):-
    member(C,L),
    !.
reduction(L,C,L):-
    !.


% apllies the reduction between all c from the list and the given literal

reduction_with_all([],_,TMP,R):-
    subtract(TMP,["IN"],R),!.
reduction_with_all([H|T],LIT,TMP,R):-
    reduction(H,LIT,R1),
    append(TMP,[R1],TMP_NEW),
    reduction_with_all(T,LIT,TMP_NEW,R),!.

% adaugat aseara pentru a doua metoda de alegere a predicatului e pr grup poza
eliminate_negated_elems([],[]):-!.
eliminate_negated_elems([H|T],R):-
    negate(H,H_NEG),
    H_NEG\=n(H),!,
    eliminate_negated_elems(T,R).
eliminate_negated_elems([H|T],[H|R]):-
    negate(H,H_NEG),
    H_NEG==n(H),!,
    eliminate_negated_elems(T,R).


% another possible way to chose beside max would be chose the element wth the max negated
% value appearence 
dp([],[]):-
    !.
dp(L,_):-
    member([],L),
    !,
    fail.
dp(L,[(LIT/true)|S]):-
    elem_with_most_app(L,LIT),
    reduction_with_all(L,LIT,[],L1),
    dp(L1,S),
    !.
dp(L,[(LIT/false)|S]):-
    elem_with_most_app(L,LIT),
    negate(LIT,LIT_N),
    reduction_with_all(L,LIT_N,[],L1),
    dp(L1,S),
    !.

dp_2([],[]):-
    !.
dp_2(L,_):-
    member([],L),
    !,
    fail.
dp_2(L,[(LIT/true)|S]):-
    elem_least_balanced(L,LIT),
    reduction_with_all(L,LIT,[],L1),
    dp(L1,S),
    !.
dp_2(L,[(LIT/false)|S]):-
    elem_least_balanced(L,LIT),
    negate(LIT,LIT_N),
    reduction_with_all(L,LIT_N,[],L1),
    dp(L1,S),
    !.


apply_dp_on_all([]).
apply_dp_on_all([H|T]):-
    write('input-->'),
    write(H),
    nl,
    (
    dp(H,R)
    ->
    write('max appearance:'), write(R);
    write('max appearance: false')
    ),
    nl,
    (
    dp_2(H,R2)
    ->
    write('least balanced:'), write(R2);
    write('least balanced: false')
    ),
    nl,
    write('______________________________'),
    nl,
    apply_dp_on_all(T).


read_file(S,[]) :-
    at_end_of_stream(S).
read_file(S,[L|R]) :- 
    not(at_end_of_stream(S)),
    read(S,L),
    read_file(S,R).

apply_dp_on_file:- 
    write('______________________________'),
    nl,
    open('ex2_data.txt', read, S),
    read_file(S, L),
    apply_dp_on_all(L),
    close(S).

