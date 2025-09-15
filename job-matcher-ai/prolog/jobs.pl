% job(JobAtom, RequiredSkillsList, MinExperienceYears, DomainAtom).
job(data_scientist, [python, machine_learning, sql], 3, data).
job(data_engineer, [python, sql], 1, data).
job(backend_dev, [java, sql], 2, software).

% skill match percent
skill_match(CSkills, JSkills, SkillScore) :-
    findall(S, (member(S, JSkills), member(S, CSkills)), Common),
    length(Common, MatchCount),
    length(JSkills, TotalReq),
    ( TotalReq > 0 -> SkillScore is (MatchCount / TotalReq) * 100 ; SkillScore is 0 ).

% experience score
exp_score(CExp, MinExp, ExpScore) :-
    ( MinExp =:= 0 -> ExpScore = 100
    ; Ratio is CExp / MinExp,
      ( Ratio >= 1 -> ExpScore = 100 ; ExpScore is Ratio * 100 )
    ).

% final score: 70% skills, 30% exp; domain must match
match_score(CId, Job, FinalScore) :-
    candidate(CId, _Full, CSkills, CExp, CDomain),
    job(Job, JSkills, MinExp, JDomain),
    JDomain = CDomain,
    skill_match(CSkills, JSkills, SkillScore),
    exp_score(CExp, MinExp, ExpScore),
    FinalScore is SkillScore * 0.7 + ExpScore * 0.3.

% comparator for sorting
compare_scores(Delta, Score1-_, Score2-_) :-
    ( Score1 > Score2 -> Delta = '<'
    ; Score1 < Score2 -> Delta = '>'
    ; Delta = '=' ).

% best matches
best_matches(CId, Sorted) :-
    findall(Score-Job, match_score(CId, Job, Score), Pairs),
    Pairs \= [],
    predsort(compare_scores, Pairs, Sorted).
