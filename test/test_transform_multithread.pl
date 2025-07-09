
:- use_module(reorder_task_static).
:- use_module(library(plunit)).

:- begin_tests(multithread_transform).

test(rewrite_nb_setval) :-
    transform_unsafe_multithread(nb_setval(x,1), Out, [multithread_safe(true), target_thread(main)]),
    assertion(Out == call_in_thread(main, nb_setval(x,1))).

test(rewrite_thread_self) :-
    transform_unsafe_multithread(thread_self(ID), Out, [multithread_safe(true), target_thread(t1)]),
    assertion(Out == call_in_thread(t1, thread_self(ID))).

test(skip_rewrite_when_flag_off) :-
    transform_unsafe_multithread(nb_setval(x,1), Out, [multithread_safe(false)]),
    assertion(Out == nb_setval(x,1)).

:- end_tests(multithread_transform).
