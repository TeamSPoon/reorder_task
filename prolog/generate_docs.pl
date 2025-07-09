
:- initialization(main, main).

main :-
    DocDir = 'doc',
    make_directory_path(DocDir),
    doc_save(DocDir, [if(true)]),
    format("📚 PlDoc documentation saved to ~w/~n", [DocDir]).
