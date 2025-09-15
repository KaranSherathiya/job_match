:- consult('jobs.pl').
:- consult('candidates.pl').

print_top_n(0, _, _) :- !.
print_top_n(_, [], _) :- !.
print_top_n(N, [Score-Job | T], FullName) :-
    format("~w -> ~w (Score: ~2f)~n", [FullName, Job, Score]),
    N1 is N - 1,
    print_top_n(N1, T, FullName).

print_matches :-
    candidate(Id, FullName, _, _, _),
    ( findall(Score-Job, match_score(Id, Job, Score), Pairs), Pairs \= [] ->
        predsort(compare_scores, Pairs, Sorted),
        print_top_n(3, Sorted, FullName)
    ; format("~w -> No matches~n", [FullName])
    ),
    fail.
print_matches.

main :- print_matches.
