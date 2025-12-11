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

diagnosis(alternator_fault, [
    car_dies_while_driving:4,
    battery_warning_light_on:4,
    lights_dim_when_starting:2,
    intermittent_electrical_failures:3,
    weak_battery_voltage:3
], 'Alternator failing; battery not charging while engine runs; requires alternator test/inspection.').

diagnosis(ignition_spark_failure, [
    engine_cranks_but_wont_start:4,
    check_engine_light_on:3,
    starts_then_stalls:3,
    car_sits_long_and_wont_start:1
], 'Ignition problem (spark plugs, coils); check spark and ignition components.').

diagnosis(fuel_delivery_problem, [
    engine_cranks_but_wont_start:3,
    strong_fuel_smell:3,
    starts_then_stalls:3,
    sudden_loss_power_acceleration:3
], 'Fuel delivery issue (fuel pump/filter/clogged lines); smells of fuel or failing under load indicate fuel system checks.').

diagnosis(coolant_leak_thermostat, [
    overheating:4,
    coolant_low:4,
    radiator_smoke_or_steam:4,
    thermostat_stuck:2
], 'Coolant leak or thermostat stuck closed; inspect coolant level, hoses, thermostat and radiator.').

diagnosis(flat_tyre, [
    steering_hard:3,
    vibration_or_bumps:2,
    sudden_loss_power_acceleration:1
], 'Low tyre pressure or puncture; check tyres and pressure.').

diagnosis(brake_issue, [
    soft_brake_pedal:4,
    brake_noise:3
], 'Brake fluid low or worn pads/rotors; inspect brake system immediately.').

diagnosis(electrical_short, [
    burnt_smell_electrical:5,
    radiator_smoke_or_steam:2,
    no_lights_at_all:2
], 'Possible electrical short or serious wiring fault; stop and investigate urgently.').

diagnosis(parasitic_battery_drain, [
    parasitic_drain:5,
    intermittent_electrical_failures:3,
    no_lights_at_all:2
], 'Parasitic drain on battery when car off; investigate trunk/aftermarket devices and wiring.').

diagnosis(fuel_stale_or_immobilizer, [
    car_sits_long_and_wont_start:4,
    engine_cranks_but_wont_start:2
], 'Stale fuel or immobilizer/security lock issue for long-standing parked vehicles.').S

% Core diagnosis engine
all_diagnoses(Ds) :-
    findall(Name, diagnosis(Name, _, _), Ds).

    evaluate_all(Ranked) :-
    % Ensure we have the list of possible diagnoses
    findall(Result, (
        diagnosis(Name, SymList, Description),
        symptom_weighted_match(SymList, MatchedW, TotalW, MatchedList),
        confidence_percent(MatchedW, TotalW, Percent),
        Result = diagnosis_result(Name, MatchedW, TotalW, Percent, MatchedList, Description)
    ), Results),
    % Filter out diagnoses with 0 total weight (shouldn't happen) and sort by confidence desc
    exclude(zero_total, Results, Filtered),
    sort_by_confidence_desc(Filtered, Ranked).

    zero_total(diagnosis_result(_, _, 0, _, _, _)) :- !.

    % sorting helper: use keysort with negative confidence
sort_by_confidence_desc(List, Sorted) :-
    map_list_to_pairs(key_by_negative_confidence, List, Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, Sorted).
key_by_negative_confidence(diagnosis_result(Name, MW, TW, C, Matched, Desc), Key-diagnosis_result(Name, MW, TW, C, Matched, Desc)) :-
    Key is -C.

% Present top N diagnoses
present_ranked([], _) :-
    writeln('No diagnoses to present. Possibly no rules defined.'), !.
present_ranked(Ranked, N) :-
    ( Ranked = [] -> writeln('No matching diagnoses found.')
    ; take_first_n(Ranked, N, Top),
      writeln('--- Ranked diagnoses (top choices) ---'),
      present_list(Top)
    ).

present_list([]).
present_list([diagnosis_result(Name, _, _, Conf, Matched, Desc) | T]) :-
    format('Diagnosis: ~w (~w%% confidence)~n', [Name, Conf]),
    ( Matched = [] -> true ; format('  Matched symptoms: ~w~n', [Matched]) ),
    format('  Suggestion: ~w~n~n', [Desc]),
    present_list(T).

take_first_n(_, 0, []) :- !.
take_first_n([], _, []).
take_first_n([H|T], N, [H|R]) :-
    N1 is N - 1, take_first_n(T, N1, R).
% Interactive run
% define the default questionnaire order (to ask in interactive mode)
questionnaire_order([
    engine_does_not_crank,
    engine_cranks_slowly,
    lights_dim_when_starting,
    lights_bright_when_starting,
    clicking_when_trying_start,
    engine_cranks_but_wont_start,
    strong_fuel_smell,
    check_engine_light_on,
    car_dies_while_driving,
    battery_warning_light_on,
    overheating,
    coolant_low,
    radiator_smoke_or_steam,
    steering_hard,
    vibration_or_bumps,
    soft_brake_pedal,
    no_lights_at_all,
    burnt_smell_electrical,
    starts_then_stalls,
    weak_battery_voltage,
    car_sits_long_and_wont_start,
    parasitic_drain,
    intermittent_electrical_failures,
    sudden_loss_power_acceleration,
    brake_noise,
    thermostat_stuck
]).

interactive_ask_all([]).
interactive_ask_all([Q|Rest]) :-
    ( ask_if(Q) -> true ; true ),    % ask_if succeeds only if yes; if no, continue
    interactive_ask_all(Rest).

