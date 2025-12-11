Car Fault Diagnosis Expert System (Prolog):

This project is a rule-based Expert System designed to diagnose common car faults using user-provided symptoms. It was developed as part of the COM6008: Knowledge-Based Systems in AI module and demonstrates core AI concepts including knowledge acquisition, rule-based reasoning, weighted inference, and symbolic AI design.
The system interacts with the user through a series of yes/no questions about car symptoms and produces a ranked list of possible car faults, each with a confidence score and explanation. Weighted rules allow the system to reflect the real-world importance of different symptoms, resulting in more realistic diagnostic suggestions.

Features:

Interactive yes/no symptom questionnaire
Weighted rule-based diagnosis
Confidence score calculation for each fault
Ranked list of most likely car problems
Clear suggestions for each diagnosis
Automated test cases for validation
Transparent and interpretable reasoning

How It Works:

The user runs the system in SWI-Prolog.
The system asks a series of questions about symptoms such as dim lights, slow cranking, overheating, warning lights, or electrical smells.
Each “yes” answer is recorded as a fact.
All diagnoses are evaluated by comparing user symptoms to weighted rules.
Confidence percentages are calculated using matched weight ÷ total weight.
Diagnoses are sorted and displayed from most likely to least likely.
The user receives explanations and recommended next steps.
Weighted rules allow the system to model real diagnostic behaviour more realistically than simple yes/no rules.

Installation:

Install SWI-Prolog from:
https://www.swi-prolog.org/

Clone this repository:

git clone 

Open SWI-Prolog and consult the file:
?- consult('Car-Expert-System.pl').
Running the Expert System
Use the following command:
?- run.

This starts the interactive questionnaire and produces the top 5 likely diagnoses.

Running Automated Tests:

The system includes several predefined test scenarios. Run them using:
?- run_tests.


This evaluates multiple simulated fault cases and prints their results, helping validate the system.

Project Structure:
Car-Expert-System.pl   → Main expert system implementation
README.md              → Project documentation


Key components inside the Prolog file:

Symptom questions
Diagnosis rules (with weighting)
Inference engine
Confidence calculation
Test suite

Example Output:

--- Ranked diagnoses (top choices) ---
Diagnosis: battery_flat (85% confidence)
  Matched symptoms: [engine_does_not_crank, lights_dim_when_starting]
  Suggestion: Battery flat or highly discharged; check battery and terminals.
Diagnosis: starter_motor_fault (60% confidence)
  Matched symptoms: [clicking_when_trying_start]
  Suggestion: Starter motor or solenoid fault.

Knowledge Sources and Design Inspiration:

This system is inspired by classical AI expert systems and diagnostic reasoning frameworks described in:

Jackson (1986) – Rule-based expert system foundations
Bratko (2001) – Prolog for AI and knowledge representation
Durkin (1994) – Expert system design methodologies
Giarratano & Riley (2005) – Modern expert system principles
Liao (2005) – Expert system applications and techniques

Weighted reasoning is influenced by certainty-factor approaches used in early diagnostic systems such as MYCIN.

Learning Outcomes Demonstrated:

Rule-based AI reasoning
Knowledge acquisition and structuring
Weighted inference mechanism design
Use of Prolog for symbolic AI
Understanding limitations of expert systems
System testing and refinement

Future Improvements:

More detailed symptoms and follow-up questions
Additional diagnoses
Integration with fuzzy logic
Web or GUI interface
Probabilistic reasoning instead of weighted matching

Authors:

Developed by Raja Abdullah Shafique & Lhakpa Tamang as part of the COM6008 Knowledge-Based Systems in AI coursework.
