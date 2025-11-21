% Car-expert-system.pl
% Simple car fault diagnosis expert system (Prolog)
% Authors: Raja Abdullah Shafique & Lhakpa Tamang
:- dynamic(known/2).    % known(QuestionAtom, yes/no)
:- dynamic(asserted_sym/1). % asserted_sym(SymptomAtom)
:- discontiguous(sympton_question/2).
:- discontiguous(diagnosis/3).
% Utility helpers

reset_kb:-
    retractall(known(_, _)),
    retractall(asserted_sym(_)).