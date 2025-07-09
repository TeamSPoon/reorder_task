
:- begin_tests(llm_annotator).
:- [reorder_task:prolog/llm_annotator].

test(critical_section_by_name) :-
    llm_annotate_goal(update_user(X), Annotated),
    assertion(Annotated == is_critical_section(update_user(X))).

test(shared_detection_by_name) :-
    llm_annotate_goal(read_db(ID), Annotated),
    assertion(Annotated == is_shared(read_db(ID))).

test(parallel_by_name) :-
    llm_annotate_goal(fast_calc(Y), Annotated),
    assertion(Annotated == is_parallel(fast_calc(Y))).

test(sequential_by_name) :-
    llm_annotate_goal(log_error(Msg), Annotated),
    assertion(Annotated == is_sequential(log_error(Msg))).

test(fallback_no_annotation) :-
    llm_annotate_goal(do_nothing(X), Annotated),
    assertion(Annotated == do_nothing(X)).

:- end_tests(llm_annotator).
