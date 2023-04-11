:- initialization(main, main).

:- use_module(library(csv)).

:- dynamic years/1.
:- dynamic earnings/1.
:- dynamic retired/0.
:- dynamic age/1.
:- dynamic income/1.
:- dynamic investment_experience/1.
:- dynamic investment_goal/1.


% ============
% RISK PROFILE
% ============

short_term :-
	years(Years),
	Years =< 2.

medium_term :-
	years(Years),
	Years > 2,
	Years =< 10.

long_term :-
	years(Years),
	Years > 10.


low_earnings_potential :-
	earnings(Savings),
	Savings =< 60000.

moderate_earnings_potential :-
	earnings(Savings),
	Savings > 60000,
	Savings =< 180000.

high_earnings_potential :-
	earnings(Savings),
	Savings > 180000.


low_experience_risk_profile :-
	investment_experience(1).

moderate_experience_risk_profile :-
	investment_experience(2).

high_experience_risk_profile :-
	investment_experience(3).


capital_preservation_goal :-
	investment_goal(1).

balanced_growth_goal :-
	investment_goal(2).

aggressive_growth_goal :-
	investment_goal(3).


% =======
% SCORING
%
% Each factor is scored from 0 to 10
%
% =======

term_score(0) :-
	short_term.
term_score(5) :-
	medium_term.
term_score(10) :-
	long_term.


earnings_score(0) :-
	low_earnings_potential.
earnings_score(5) :-
	moderate_earnings_potential.
earnings_score(10) :-
	high_earnings_potential.


retirement_score(0) :-
	retired, !.
retirement_score(10).


age_score(10) :-
	age(N),
	N >= 18, N < 30.
age_score(8) :-
	age(N),
	N >= 30, N < 40.
age_score(6) :-
	age(N),
	N >= 40, N < 50.
age_score(4) :-
	age(N),
	N >= 50, N < 60.
age_score(2) :-
	age(N),
	N >= 60.


experience_score(0) :-
	low_experience_risk_profile.
experience_score(5) :-
	moderate_experience_risk_profile.
experience_score(10) :-
	high_experience_risk_profile.


goal_score(0) :-
	capital_preservation_goal.
goal_score(5) :-
	balanced_growth_goal.
goal_score(10) :-
	aggressive_growth_goal.


% =========
% WEIGHTING
% =========

final_score(Score) :-
	term_score(TermScore),
	retirement_score(RetirementScore),
	earnings_score(EarningsScore),
	age_score(AgeScore),
	experience_score(ExperienceScore),
	goal_score(GoalScore),
	Score is
		( TermScore * 3
		+ RetirementScore * 2
		+ EarningsScore * 3
		+ AgeScore * 2
		+ ExperienceScore * 1
		+ GoalScore * 1
		) / 12.


% =========
% PORTFOLIO
% =========

low_risk_portfolio :-
	final_score(N),
	N =< 3.

moderate_risk_portfolio :-
	final_score(N),
	N > 3,
	N =< 6.

high_risk_portfolio :-
	final_score(N),
	N > 6.

portfolio(low_risk) :- low_risk_portfolio.
portfolio(moderate_risk) :- moderate_risk_portfolio.
portfolio(high_risk) :- high_risk_portfolio.

main :-
	(   current_prolog_flag(argv, [CSV_file|_])
    ->  csv_read_file(CSV_file, CSV, [])
    ;   csv_read_stream(current_input, CSV, [])
    ),
	CSV = [Names|Rows],
	maplist(parse_row, Rows, RowsWithPortfolio),
	Names =.. [row|NameFields],
	append(NameFields, [portfolio], NameFieldsWithPortfolio),
	NamesWithPortfolio =.. [row|NameFieldsWithPortfolio],
	NewCSV = [NamesWithPortfolio|RowsWithPortfolio],
	(   current_prolog_flag(argv, [CSV_file|_])
    ->  csv_write_file(CSV_file, NewCSV, [])
    ;   csv_write_file("output.csv", NewCSV, [])
    ).

parse_row(Row, RowWithPortfolio) :-
	Row =.. [row|Fields],
	Fields = [Years, Earnings, Retired, Age, Income, InvestmentExperience, InvestmentGoal],
	asserta(years(Years)),
	asserta(earnings(Earnings)),
	(  Retired == yes
	-> asserta(retired)
	;  !),
	asserta(age(Age)),
	asserta(income(Income)),
	asserta(investment_experience(InvestmentExperience)),
	asserta(investment_goal(InvestmentGoal)),
	portfolio(Portfolio),
	append(Fields, [Portfolio], FieldsWithPortfolio),
	RowWithPortfolio =.. [row|FieldsWithPortfolio],
	reset.


reset :-
	retractall(years(_)),
	retractall(earnings(_)),
	retractall(retired),
	retractall(age(_)),
	retractall(income(_)),
	retractall(investment_experience(_)),
	retractall(investment_goal(_)).
