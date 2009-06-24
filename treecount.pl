:- use_module(library(chr)).

:- chr_constraint
	sentence/1, sentence/2, word/2, sentence_length/1,
        rule/2, root/1, nonroot/1,
	treecount/5, level/1, treecount_done/0, next_nonterm_number/1.

max(A,B,A) :- A > B.
max(A,B,B) :- A =< B.

:- set_prolog_flag(chr_toplevel_show_store,false).

sentence_properties @
sentence(S) <=> length(S,L) | sentence_length(L), sentence(1,S).

remove_empty_sentence @ sentence(_,[]) <=> true.

split_sentence @
sentence(Index,[Word|R]) <=> 
    NextIndex is Index + 1 | word(Index,Word), sentence(NextIndex,R).

% Rewrite grammar to chomsky normal form:
rule(N,[N1,N2,N3|Rest]), next_nonterm_number(NN) <=>
    NextNumber is NN + 1,
    atom_concat('ins_nn', NN, NewNTName)
    |
    rule(N,[N1,NewNTName]),
    rule(NewNTName, [N2,N3|Rest]),
    next_nonterm_number(NextNumber).


% Find root and nonroot nonterminals

remove_dup_root @
root(N) \ root(N) <=> true.

remove_dup_nonroot @
nonroot(N) \ nonroot(N) <=> true.

remove_false_roots @
nonroot(N) \ root(N) <=> true.

add_root @
rule(N,_) ==> root(N).

add_nonroot @
rule(_,[A,B]) ==> nonroot(A), nonroot(B).

treecount(N,L1,P,Q,Count1), treecount(N,L2,P,Q,Count2) <=>
    L1 =< L2,
    Count is Count1 + Count2
    |
    treecount(N,L2,P,Q,Count).

base_case @
level(1), % Not strictly necessary
word(Pos,Word), rule(NonTerm,[Word]) ==>
    treecount(NonTerm, 1, Pos, Pos,1).

recursion @
level(L), rule(NonTermJ, [NonTermL,NonTermR]),
treecount(NonTermL,L1,P,D,CountL),
treecount(NonTermR,L2,D1,Q,CountR) ==>
    max(L1,L2,L),
    NextLevel is L + 1,
    D1 is D + 1,
    Count is CountL * CountR
    |
    treecount(NonTermJ, NextLevel, P, Q, Count).

increase_tree_level @
sentence_length(SL) \ level(L) <=> L < SL, M is L + 1 | level(M).

infer_treecount_done @
sentence_length(L), level(L) ==> treecount_done.

treecount_done,treecount(S,_,_,_,Count), root(S) <=>
    nl,write('Tada...'),nl,write('The number of parses is: '), write(Count).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main goal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [grammar].% A small sample grammar

test_count :-
	S = [astronomers, saw, stars, with, ears, with, ears, with, ears, with, ears, with, ears, with, ears, with, ears, with, ears, with, ears, with, ears, with, ears, with, ears, with, ears, with, ears, with, ears, with, ears],
	count(S).

count(S) :-
	next_nonterm_number(0),
	init_grammar,
	sentence(S),
	level(1).

