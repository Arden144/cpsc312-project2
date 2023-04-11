:- dynamic years/1.
:- dynamic earnings/1.
:- dynamic retired/0.
:- dynamic age/1.
:- dynamic income/1.
:- dynamic investment_experience/1.
:- dynamic investment_goal/1.

% ================
% USER INFORMATION
% ================

years(N) :-
	writeln("How many years in the future are you planning to invest for?"),
	read(N),
	integer(N),
	asserta(years(N) :- !), !.

years(N) :-
	writeln("Invalid input. Please enter a number."),
	years(N).


earnings(N) :-
	writeln("Approximately how much money do you have available to invest?"),
	read(N),
	integer(N),
	asserta(earnings(N) :- !), !.

earnings(N) :-
	writeln("Invalid input. Please enter a number."),
	earnings(N).


retired :-
	writeln("Are you saving for retirement?"),
	read(yes),
	asserta(retired :- !), !.

retired :-
	asserta((retired :- !, fail)),
	fail.


age(N) :-
	writeln("How old are you?"),
	read(N),
	integer(N),
	N >= 18,
	asserta(age(N) :- !), !.

age(N) :-
	writeln("Invalid input. Please enter an age 18 or older."),
	age(N).


income(N) :-
	writeln("What is your annual income?"),
	read(N),
	integer(N),
	asserta(income(N) :- !), !.

income(N) :-
	writeln("Invalid input. Please enter a number."),
	income(N).


investment_experience(N) :-
	writeln("How much experience do you have with investing? (Enter 1 for none, 2 for some, or 3 for a lot)"),
	read(X),
	integer(X),
	X >= 1,
	X =< 3,
	!,
	asserta((investment_experience(_) :- !, fail)),
	asserta(investment_experience(X) :- !),
	N is X.

investment_experience(N) :-
	writeln("Invalid input. Please enter 1, 2, or 3."),
	investment_experience(N).


investment_goal(N) :-
	writeln("What is your primary investment goal? (Enter 1 for capital preservation, 2 for balanced growth, or 3 for aggressive growth)"),
	read(X),
	integer(X),
	X >= 1,
	X =< 3,
	!,
	asserta((investment_goal(_) :- !, fail)),
	asserta(investment_goal(X) :- !),
	N is X.

investment_goal(N) :-
	writeln("Invalid input. Please enter 1, 2, or 3."),
	investment_goal(N).


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


% ====
% MAIN
% ====

start :-
	low_risk_portfolio,
	writeln("Based on your profile, a low risk portfolio is the suggested choice. We recommend the following portfolio:"),
    writeln("50% money market funds or short-term bond funds for stability and liquidity."),
    writeln("30% high-quality bonds or bond funds for income with lower risk."),
    writeln("20% bond funds or income-producing stocks for growth potential with moderate risk.").


start :-
	moderate_risk_portfolio,
	writeln("Based on your profile, a moderate risk portfolio is the suggested choice. We recommend the following portfolio:"),
	writeln("30% money market funds or short-term bond funds for stability and liquidity."),
    writeln("40% balanced funds or index funds for a diversified portfolio with growth potential."),
    writeln("30% corporate bonds or bond funds for income with moderate risk.").

start :-
	high_risk_portfolio,
	writeln("Based on your profile, a high risk portfolio is the suggested choice. We recommend the following portfolio:"),
	writeln("20% money market funds or short-term bond funds for stability and liquidity."),
    writeln("50% stocks or equity funds for long-term growth potential."),
	writeln("30% high-yield bonds or dividend-paying stocks for income with higher risk potential.").
