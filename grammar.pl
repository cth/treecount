%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A sample grammmar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_grammar :-
    rule(s,[np,vp]),
    rule(pp,[p,np]),
    rule(vp,[v,np]),
    rule(vp,[vp,pp]),
    rule(p,[with]),
    rule(v,[saw]),
    rule(np,[np,pp]),
    rule(np,[astronomers]),
    rule(np,[ears]),
    rule(np,[saw]),
    rule(np,[stars]),
    rule(np,[telescopes]).