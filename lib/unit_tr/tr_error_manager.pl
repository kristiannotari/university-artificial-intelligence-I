:- module(tr_error_manager, [assert_error/2,
			     assert_warning/2]).

:- dynamic(error/2).
%  error(TermError, Module:Context)

unit(tr_error_manager,[]).

pred(assert_error(error_term, err_context)).
pred(assert_warning(warning_term, err_context)).

:- dynamic(warning/2).
%  warning(TermError, Module:Context)

assert_error(Term, Cnt) :-
	assert(error(Term, Cnt)).

assert_warning(Term, Cnt) :-
	assert(warning(Term, Cnt)).
