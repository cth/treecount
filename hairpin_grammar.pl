%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A sample grammmar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_grammar :-
	rule(start, [sequence]),

	% sequences
	rule(sequence, [n,sequence]),
	rule(sequence, [hairpin,sequence]),
	rule(sequence, [hairpin]),
	rule(sequence, [n]),
	
	% hairpins
	rule(hairpin, [na,hairpin,nt]),
	rule(hairpin, [nt,hairpin,na]),
	rule(hairpin, [nc,hairpin,ng]),
	rule(hairpin, [ng,hairpin,nc]),
	rule(hairpin, [loop]),

	% loops
	rule(loop, [n,loop]),
	rule(loop, [n]),
	
	% terminalsequence
	rule(n,[a]),
    rule(n,[t]),
    rule(n,[c]),
    rule(n,[g]),

	rule(na,[a]),
    rule(nt,[t]),
    rule(nc,[c]),
    rule(ng,[g]).