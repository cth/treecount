:- use_module(library(chr)).

:- set_prolog_flag(chr_toplevel_show_store,false).

:- chr_constraint
	sentence/1, sentence/2, word/2, sentence_length/1,
    rule/2, root/1, nonroot/1, nonterminal/1, delete_rule/1,
	treecount/5, level/1, treecount_done/0, next_nonterm_number/1, 
	normalize/0, normalize_done/0.

max(A,B,A) :- A > B.
max(A,B,B) :- A =< B.

%:- set_prolog_flag(chr_toplevel_show_store,false).

sentence_properties @
sentence(S) <=> length(S,L) | sentence_length(L), sentence(1,S).

remove_empty_sentence @ sentence(_,[]) <=> true.

split_sentence @
sentence(Index,[Word|R]) <=> 
    NextIndex is Index + 1 | word(Index,Word), sentence(NextIndex,R).

rule(A,B) \ rule(A,B) <=> true.

% Grammatical inference
root(N) \ root(N) <=> true.
nonroot(N) \ nonroot(N) <=> true.
nonterminal(N) \ nonterminal(N) <=> true.
nonroot(N) \ root(N) <=> true.
rule(N,_) ==> root(N).
rule(N,_) ==> nonterminal(N).
rule(_,[A,B]) ==> nonroot(A), nonroot(B).

rule(LHS,RHS), delete_rule(rule(LHS,RHS)) <=> true.

normalize ==> next_nonterm_number(0).
	
% Rewrite grammar to chomsky normal form:
normalize \ rule(N,[N1,N2,N3|Rest]), next_nonterm_number(NN) <=>
    NextNumber is NN + 1,
    atom_concat('ins_nn', NN, NewNTName)
    |
    rule(N,[N1,NewNTName]),
    rule(NewNTName, [N2,N3|Rest]),
    next_nonterm_number(NextNumber).

% This will leave the original rule in the grammar, 
% but no treecount 
normalize, nonterminal(N1), rule(N, [N1]), rule(N1,RHS) ==>
%	write('rewriting single rule: '), write(rule(N, [N1])), write(' -> '), write(rule(N,RHS)), nl,
	rule(N,RHS).
	
normalize, nonterminal(N1), rule(N, [N1]) ==>
%	write('rewrite done: '), write(rule(N,[N1])), nl, 
	delete_rule(rule(N,[N1])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tree counting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Assumes that grammar has been loaded and normalized and that
% the sentence has been loaded.

% FIXME: simplify this!
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

:- [hairpin_grammar].% A small sample grammar

test_count :-
	S = [a,t,g,a,a,t,g,c,t,g,c,t,a,t,t,t,t,c,c,g,c,t,t,c,t,t], %,t,t,t,t,t,a,c,t,t,t,a,g,c,a,c,c,t,g,a],
	count(S).


count(S) :-
	next_nonterm_number(0),
	init_grammar,
	normalize,
	sentence(S),
	level(1).

