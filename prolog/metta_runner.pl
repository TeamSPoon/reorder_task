
:- module(metta_runner, [run_metta_program/1]).

%! run_metta_program(+Entry)
%
% Symbolically executes a goal like main_loop or another root predicate.
run_metta_program(Goal) :-
    format("🎯 Running MeTTa program via: ~q~n", [Goal]),
    ( current_predicate(Goal/0) ->
        call(Goal)
    ; format("⚠️ Goal ~q not defined or missing arity 0.~n", [Goal])
    ).
