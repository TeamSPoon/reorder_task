
:- initialization(run_all_tests, main).

run_all_tests :-
    format("Running all reorder_task tests...~n"),
    consult(reorder_task:test/test_executor_inline),
    consult(reorder_task:test/test_inline_all_code),
    consult(reorder_task:test/test_transform_multithread),
    consult(reorder_task:test/test_shared_sync_logic),
    consult(reorder_task:test/test_sync_attr),
    consult(reorder_task:test/test_reorder_sync_pool),
    consult(reorder_task:test/task_executor_tests),
    run_tests.
