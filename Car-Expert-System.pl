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
    
ask(QuestionAtom, Prompt) :-
    ( known(QuestionAtom, Answer) ->
        Answer = yes  % succeed only if recorded as yes
    ;
      format('~w (yes/no): ', [Prompt]),
        read(UserResponse),
        (UserResponse = yes ; UserResponse = y) ->
            asserta(known(QuestionAtom, yes)),
            asserta(asserted_sym(QuestionAtom));
        asserta(known(QuestionAtom, no)), fail
    ).