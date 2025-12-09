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
% ask_if/1: ask the question and return true if user answered yes
ask_if(QuestionAtom) :-
    symptom_question(QuestionAtom, Prompt),
    ask(QuestionAtom, Prompt).

% assert_sym_for_test/1: programmatically assert symptoms for tests
assert_sym_for_test(Symptom) :-
    ( asserted_sym(Symptom) -> true ; asserta(asserted_sym(Symptom)) ).

% check_sym/1: succeed if symptom is asserted (user said yes or test asserted)
check_sym(Symptom) :-
    asserted_sym(Symptom).

% match_count(List, Count, Matched) -> Count is number matched, Matched is list of matched symptoms
match_count([], 0, []).
match_count([H|T], N, [H|M]) :-
    check_sym(H), !,
    match_count(T, N1, M),
    N is N1 + 1.
match_count([_H|T], N, M) :-
    match_count(T, N, M). 

% symptom_weighted_match(Rules, SumWeightsMatched, SumWeightsTotal, MatchedList)
% Rules is list of symptom-weight pairs [symptom:W, ...]
symptom_weighted_match([], 0, 0, []).
symptom_weighted_match([S:W | T], MatchedW, TotalW, MatchedList) :-
    symptom_weighted_match(T, MatchedW1, TotalW1, MatchedList1),
    TotalW is TotalW1 + W,
    ( check_sym(S) ->
        MatchedW is MatchedW1 + W,
        MatchedList = [S | MatchedList1]
    ;
        MatchedW = MatchedW1,
        MatchedList = MatchedList1
    ).

% confidence_percent(MatchedW, TotalW, Percent)
confidence_percent(_, 0, 0) :- !.
confidence_percent(M, T, P) :-
    P is round((M * 100) / T).
% Symptom questions (atoms and user readable prompts)
% We'll define the symptom atom name and the prompt to ask the user.

symptom_question(engine_does_not_crank, 'Does the engine NOT crank when you try to start?').
symptom_question(engine_cranks_slowly, 'Does the engine crank slowly (slow cranking) when starting?').
symptom_question(lights_dim_when_starting, 'Are the headlights/lights dim when you try to start the car?').
symptom_question(lights_bright_when_starting, 'Are the headlights bright when you try to start the car (i.e., power seems available)?').
symptom_question(clicking_when_trying_start, 'Do you hear a clicking noise when trying to start the engine?').
symptom_question(engine_cranks_but_wont_start, 'Does the engine crank but NOT start?').
symptom_question(strong_fuel_smell, 'Do you notice a strong smell of petrol/gasoline?').
symptom_question(check_engine_light_on, 'Is the "check engine" light illuminated on the dashboard?').
symptom_question(car_dies_while_driving, 'Does the car suddenly die or stall while driving?').
symptom_question(battery_warning_light_on, 'Is the battery/alternator warning light illuminated?').
symptom_question(overheating, 'Is the engine temperature rising and the car overheating?').