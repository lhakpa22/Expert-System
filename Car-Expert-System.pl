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
symptom_question(coolant_low, 'Is the coolant level low or do you see coolant leaking?').
symptom_question(radiator_smoke_or_steam, 'Do you see steam or smoke from the radiator/engine bay?').
symptom_question(steering_hard, 'Is steering suddenly hard or heavy to turn?').
symptom_question(vibration_or_bumps, 'Do you feel vibration or bumps while driving (wheel vibration)?').
symptom_question(soft_brake_pedal, 'Is the brake pedal soft/spongy when you press it?').
symptom_question(no_lights_at_all, 'Do you have absolutely no lights (no dash lights, no headlights)?').
symptom_question(burnt_smell_electrical, 'Do you smell burning or see smoke inside the car (electrical smell)?').
symptom_question(starts_then_stalls, 'Does the engine start and then immediately stall?').
symptom_question(weak_battery_voltage, 'Have you measured battery voltage and found it below ~12V?').
symptom_question(car_sits_long_and_wont_start, 'Has the car been sitting unused a long time and now won''t start?').
symptom_question(parasitic_drain, 'Does the battery repeatedly go flat after being charged or replaced?').
symptom_question(intermittent_electrical_failures, 'Are electrical accessories (radio, windows, lights) failing intermittently?').
symptom_question(sudden_loss_power_acceleration, 'Do you experience sudden loss of power when accelerating?').
symptom_question(brake_noise, 'Do the brakes make grinding or squealing noises?').
% thermostat_stuck was referenced in a diagnosis; define the question
symptom_question(thermostat_stuck, 'Does the temperature rise quickly after starting, indicating thermostat may be stuck?').

% Diagnosis rules
% Each diagnosis is: diagnosis(Key, [Symptom:Weight, ...], Description)
% Weight indicates importance of the symptom for that diagnosis.

diagnosis(battery_flat, [
    engine_does_not_crank:4,
    lights_dim_when_starting:3,
    engine_cranks_slowly:3,
    no_lights_at_all:4,
    weak_battery_voltage:5,
    parasitic_drain:2
], 'Battery flat or highly discharged; check battery and terminals; try jump-start or replace battery.').

diagnosis(starter_motor_fault, [
    engine_does_not_crank:4,
    lights_bright_when_starting:2,
    clicking_when_trying_start:4,
    engine_cranks_slowly:1
], 'Starter motor or starter solenoid fault; clicking noise with bright lights suggests starter engagement issue.').