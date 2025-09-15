# app.py
import streamlit as st
import pdfplumber
from dateutil import parser
from datetime import datetime
import re
import tempfile
import os
import shutil
import subprocess
import json

st.set_page_config(page_title="Resume → Job Matcher", layout="centered")

# ---------------------------
# Utilities: text extraction, normalization
# ---------------------------
def normalize_dashes(text: str) -> str:
    if not text:
        return text
    # replace various dash-like characters with plain hyphen
    return text.replace("–", "-").replace("—", "-").replace("", "-").replace("\u2010", "-")

def extract_text_from_pdf_bytes(file_bytes) -> str:
    # file_bytes: bytes-like (Streamlit UploadedFile)
    text = ""
    with pdfplumber.open(file_bytes) as pdf:
        for p in pdf.pages:
            page_text = p.extract_text()
            if page_text:
                text += page_text + "\n"
    return normalize_dashes(text)

# ---------------------------
# Atomify for Prolog-safe atoms
# ---------------------------
def atomify(s: str) -> str:
    s = s.lower().strip()
    s = re.sub(r'\+', 'plus', s)
    s = re.sub(r'[^a-z0-9]+', '_', s)
    s = re.sub(r'_+$', '', s)
    s = re.sub(r'^_+', '', s)
    return s if s else 'unknown'

# ---------------------------
# Experience extraction (robust)
# ---------------------------
def extract_experience_from_text(text: str) -> float:
    text = normalize_dashes(text)
    total_months = 0

    # Case 1: Month YYYY - Month YYYY / Present
    pattern1 = re.findall(r"([A-Za-z]+ \d{4})\s*-\s*([A-Za-z]+ \d{4}|Present)", text, re.IGNORECASE)
    for start_str, end_str in pattern1:
        try:
            start_date = parser.parse(start_str)
            end_date = datetime.now() if "present" in end_str.lower() else parser.parse(end_str)
            months = (end_date.year - start_date.year) * 12 + (end_date.month - start_date.month)
            total_months += max(0, months)
        except Exception:
            pass

    # Case 2: YYYY - YYYY / Present  (treat as whole years difference)
    pattern2 = re.findall(r"(\d{4})\s*-\s*(\d{4}|Present)", text, re.IGNORECASE)
    for start_str, end_str in pattern2:
        try:
            start_year = int(start_str)
            end_year = datetime.now().year if "present" in end_str.lower() else int(end_str)
            years = max(0, end_year - start_year)
            total_months += years * 12
        except Exception:
            pass

    # Case 3 (fallback): explicit "X years"
    m = re.search(r"(\d+(?:\.\d+)?)\s*(?:\+)?\s*(?:years|yrs|year)", text, re.IGNORECASE)
    if m and total_months == 0:
        try:
            val = float(m.group(1))
            return round(val, 1)
        except:
            pass

    years = round(total_months / 12, 1)
    return years

# ---------------------------
# Resume parser: skills, domain, name
# ---------------------------
COMMON_SKILLS = [
    "python","java","sql","machine learning","nlp","c++","c","c#","r","javascript",
    "docker","spring boot","pandas","git"
]

def parse_resume_text(text: str) -> dict:
    lines = [ln.strip() for ln in text.splitlines() if ln.strip()]
    name = lines[0] if lines else "Unknown"

    # Skills: check "Skills:" line first
    skills = []
    m = re.search(r"Skills\s*[:\-]\s*(.*)", text, re.IGNORECASE)
    if m:
        skills = [s.strip() for s in m.group(1).split(',') if s.strip()]
    else:
        # fallback: detect common skill keywords in text
        for kw in COMMON_SKILLS:
            if re.search(r'\b' + re.escape(kw) + r'\b', text, re.IGNORECASE):
                skills.append(kw)

    # Experience
    exp = extract_experience_from_text(text)

    # Domain
    domain = "general"
    m = re.search(r"(Preferred Domain|Domain)\s*[:\-]\s*(.*)", text, re.IGNORECASE)
    if m:
        domain = m.group(2).strip()
    else:
        if re.search(r'\bdata\b|\bdata science\b|\bmachine learning\b', text, re.IGNORECASE):
            domain = "data"
        elif re.search(r'\bbackend\b|\bsoftware\b|\bdeveloper\b', text, re.IGNORECASE):
            domain = "software"

    skills_norm = [atomify(s) for s in skills]
    domain_atom = atomify(domain)

    return {
        "full_name": name,
        "id": atomify(name),
        "skills": skills_norm,
        "experience": exp,
        "domain": domain_atom,
        "raw_skills": skills
    }

# ---------------------------
# Write Prolog facts files (for local Prolog demo)
# ---------------------------
def write_candidates_pl(candidates, path="candidates.pl"):
    with open(path, "w") as f:
        for c in candidates:
            skills_list = "[" + ",".join(c['skills']) + "]"
            name_escaped = '"' + c['full_name'].replace('"','') + '"'
            fact = f"candidate({c['id']}, {name_escaped}, {skills_list}, {c['experience']}, {c['domain']}).\n"
            f.write(fact)

def write_jobs_pl(path="jobs.pl"):
    # Jobs facts + rules for Prolog. Keep compare_scores only here.
    jobs_code = r'''
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

% experience score (proportion, capped at 100)
exp_score(CExp, MinExp, ExpScore) :-
    ( MinExp =:= 0 -> ExpScore = 100
    ; Ratio is CExp / MinExp,
      ( Ratio >= 1 -> ExpScore = 100 ; ExpScore is Ratio * 100 )
    ).

% final match score: 70% skills, 30% exp; domain must match
match_score(CId, Job, FinalScore) :-
    candidate(CId, _Full, CSkills, CExp, CDomain),
    job(Job, JSkills, MinExp, JDomain),
    JDomain = CDomain,
    skill_match(CSkills, JSkills, SkillScore),
    exp_score(CExp, MinExp, ExpScore),
    FinalScore is SkillScore * 0.7 + ExpScore * 0.3.

% comparator for predsort (descending by score)
compare_scores(Delta, Score1-_, Score2-_) :-
    ( Score1 > Score2 -> Delta = '<'
    ; Score1 < Score2 -> Delta = '>'
    ; Delta = '=' ).

% best matches
best_matches(CId, Sorted) :-
    findall(Score-Job, match_score(CId, Job, Score), Pairs),
    Pairs \= [],
    predsort(compare_scores, Pairs, Sorted).
'''
    with open(path, "w") as f:
        f.write(jobs_code)

def write_run_pl(path="run.pl"):
    run_code = r'''
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
'''
    with open(path, "w") as f:
        f.write(run_code)

# ---------------------------
# Python matching fallback (same logic as Prolog)
# ---------------------------
JOBS_PY = [
    {"title": "data_scientist", "skills": ["python","machine_learning","sql"], "min_exp": 3, "domain": "data"},
    {"title": "data_engineer", "skills": ["python","sql"], "min_exp": 1, "domain": "data"},
    {"title": "backend_dev", "skills": ["java","sql"], "min_exp": 2, "domain": "software"},
]

def skill_match_py(cskills, jskills):
    common = set(cskills) & set(jskills)
    return (len(common) / len(jskills)) * 100 if len(jskills) > 0 else 0

def exp_score_py(cexp, minexp):
    if minexp == 0:
        return 100.0
    ratio = cexp / minexp
    return 100.0 if ratio >= 1.0 else ratio * 100.0

def match_score_py(candidate, job):
    if candidate['domain'] != job['domain']:
        return 0.0
    skill_score = skill_match_py(candidate['skills'], job['skills'])
    e_score = exp_score_py(candidate['experience'], job['min_exp'])
    final = skill_score * 0.7 + e_score * 0.3
    return round(final, 2)

def best_matches_py(candidate, top_n=3):
    scores = []
    for job in JOBS_PY:
        s = match_score_py(candidate, job)
        scores.append((job['title'], s))
    scores_sorted = sorted(scores, key=lambda x: x[1], reverse=True)
    return scores_sorted[:top_n]

# ---------------------------
# Try running SWI-Prolog (if available)
# ---------------------------
def is_swipl_available():
    return shutil.which("swipl") is not None

def run_prolog_and_capture():
    # Ensure files exist
    write_jobs_pl("jobs.pl")
    write_run_pl("run.pl")
    # run swipl
    try:
        # -q quiet, -s script, -g goal
        proc = subprocess.run(["swipl", "-s", "run.pl", "-g", "main", "-t", "halt"],
                              capture_output=True, text=True, timeout=20)
        return proc.stdout, proc.stderr
    except Exception as e:
        return "", str(e)

# ---------------------------
# Streamlit UI
# ---------------------------
st.title("📄 Resume → Job Matcher (Streamlit)")

st.markdown("Upload resume PDFs and get top job matches. Prolog will be used if available; otherwise Python fallback will rank matches.")

uploaded_files = st.file_uploader("Upload PDF resume(s)", type=["pdf"], accept_multiple_files=True)

use_prolog = st.checkbox("Try use SWI-Prolog (local) if available", value=True)
show_prolog_info = st.expander("Deployment note: Prolog availability")
show_prolog_info.write("""
If you want the SWI-Prolog path to run on a server, the server must have `swipl` installed.
For quick web deployments, the Python fallback is recommended (no SWI-Prolog needed).
""")

if uploaded_files and st.button("Parse and Match"):
    candidates = []
    for uf in uploaded_files:
        text = extract_text_from_pdf_bytes(uf)
        parsed = parse_resume_text(text)
        candidates.append(parsed)

    st.subheader("Parsed candidates")
    for c in candidates:
        st.write(f"**{c['full_name']}**")
        st.write(f"Skills (raw): {c['raw_skills']}")
        st.write(f"Skills (normalized): {c['skills']}")
        st.write(f"Domain: {c['domain']}")
        st.write(f"Experience (years): {c['experience']}")
        st.write("---")

    # Write prolog files for local demo
    write_candidates_pl(candidates, path="candidates.pl")
    write_jobs_pl(path="jobs.pl")
    write_run_pl(path="run.pl")
    st.success("Wrote candidates.pl, jobs.pl, run.pl (in notebook folder)")

    results = []

    if use_prolog and is_swipl_available():
        st.info("SWI-Prolog found — running Prolog engine...")
        out, err = run_prolog_and_capture()
        if err:
            st.error("Prolog returned error:")
            st.text(err)
        if out:
            st.text("Prolog output:")
            st.text(out)
            # also parse the printed lines into structured results
            for line in out.splitlines():
                # Example: John Doe -> data_scientist (Score: 100.00)
                m = re.match(r'(.+?) -> (\w+) \(Score: ([\d\.]+)\)', line.strip())
                if m:
                    full, job, score = m.groups()
                    results.append({"candidate": full, "job": job, "score": float(score)})
    else:
        if use_prolog:
            st.warning("SWI-Prolog not available - falling back to Python matching.")
        # Python fallback: produce top-3 for each
        st.info("Using Python fallback matcher (identical logic).")
        for c in candidates:
            # ensure candidate's skills & domain align with JOBS_PY format
            candidate_py = {"skills": c['skills'], "experience": c['experience'], "domain": c['domain'], "id": c['id'], "full_name": c['full_name']}
            top = best_matches_py(candidate_py, top_n=3)
            for job, score in top:
                results.append({"candidate": c['full_name'], "job": job, "score": score})

    if results:
        st.subheader("Top matches")
        st.table(results)
    else:
        st.info("No matches found (either domain mismatch or insufficient data).")
