import streamlit as st
import pdfplumber
from dateutil import parser
from datetime import datetime
import re
import tempfile
import os
import shutil
import subprocess

# ---------------------------
# Basic Config
# ---------------------------
st.set_page_config(page_title="AI Job Matcher", page_icon="💼", layout="wide")

# ---------------------------
# Helper Functions
# ---------------------------
def normalize_dashes(text: str) -> str:
    return text.replace("–", "-").replace("—", "-").replace("", "-")

def extract_text_from_pdf(file_bytes) -> str:
    text = ""
    with pdfplumber.open(file_bytes) as pdf:
        for p in pdf.pages:
            page_text = p.extract_text()
            if page_text:
                text += page_text + "\n"
    return normalize_dashes(text)

def atomify(s: str) -> str:
    s = s.lower().strip()
    s = re.sub(r'\+', 'plus', s)
    s = re.sub(r'[^a-z0-9]+', '_', s)
    s = re.sub(r'_+$', '', s)
    s = re.sub(r'^_+', '', s)
    return s if s else 'unknown'

def extract_experience_from_text(text: str) -> float:
    total_months = 0
    # Case 1: Month YYYY – Month YYYY
    pattern1 = re.findall(r"([A-Za-z]+ \d{4})\s*-\s*([A-Za-z]+ \d{4}|Present)", text, re.IGNORECASE)
    for start_str, end_str in pattern1:
        try:
            start_date = parser.parse(start_str)
            end_date = datetime.now() if "present" in end_str.lower() else parser.parse(end_str)
            months = (end_date.year - start_date.year) * 12 + (end_date.month - start_date.month)
            total_months += max(0, months)
        except: pass
    # Case 2: YYYY – YYYY
    pattern2 = re.findall(r"(\d{4})\s*-\s*(\d{4}|Present)", text, re.IGNORECASE)
    for start_str, end_str in pattern2:
        try:
            start_year = int(start_str)
            end_year = datetime.now().year if "present" in end_str.lower() else int(end_str)
            years = max(0, end_year - start_year)
            total_months += years * 12
        except: pass
    return round(total_months / 12, 1)

COMMON_SKILLS = ["python","java","sql","machine learning","nlp","c++","c","c#","r","javascript","docker","spring boot"]

def parse_resume(text: str) -> dict:
    lines = [ln.strip() for ln in text.splitlines() if ln.strip()]
    name = lines[0] if lines else "Unknown"
    # Skills
    skills = []
    m = re.search(r"Skills\s*[:\-]\s*(.*)", text, re.IGNORECASE)
    if m:
        skills = [s.strip() for s in m.group(1).split(',') if s.strip()]
    else:
        for kw in COMMON_SKILLS:
            if re.search(r'\b' + re.escape(kw) + r'\b', text, re.IGNORECASE):
                skills.append(kw)
    exp = extract_experience_from_text(text)
    # Domain
    domain = "general"
    m = re.search(r"(Preferred Domain|Domain)\s*[:\-]\s*(.*)", text, re.IGNORECASE)
    if m:
        domain = m.group(2).strip()
    else:
        if re.search(r'data|machine learning', text, re.IGNORECASE): domain = "data"
        elif re.search(r'backend|software|developer', text, re.IGNORECASE): domain = "software"
    return {
        "full_name": name,
        "id": atomify(name),
        "skills": [atomify(s) for s in skills],
        "raw_skills": skills,
        "experience": exp,
        "domain": atomify(domain)
    }

# ---------------------------
# Dummy Jobs (Python fallback if Prolog not available)
# ---------------------------
JOBS = [
    {"title": "Data Scientist", "skills": ["python","machine_learning","sql"], "min_exp": 3, "domain": "data"},
    {"title": "Data Engineer", "skills": ["python","sql"], "min_exp": 1, "domain": "data"},
    {"title": "Backend Developer", "skills": ["java","sql"], "min_exp": 2, "domain": "software"},
    {"title": "data_scientist",       "skills": ["python","machine_learning","sql"],     "min_exp": 3, "domain": "data"},
    {"title": "data_engineer",        "skills": ["python","sql","etl"],                 "min_exp": 1, "domain": "data"},
    {"title": "machine_learning_eng", "skills": ["python","machine_learning","pytorch"], "min_exp": 2, "domain": "data"},
    {"title": "nlp_engineer",         "skills": ["python","nlp","tensorflow"],          "min_exp": 2, "domain": "data"},
    {"title": "business_analyst",     "skills": ["sql","excel","powerbi"],              "min_exp": 1, "domain": "data"},
    {"title": "backend_dev",          "skills": ["java","sql","rest"],                  "min_exp": 2, "domain": "software"},
    {"title": "frontend_dev",         "skills": ["javascript","react","css"],           "min_exp": 1, "domain": "software"},
    {"title": "fullstack_dev",        "skills": ["javascript","python","sql"],          "min_exp": 2, "domain": "software"},
    {"title": "devops_engineer",      "skills": ["docker","kubernetes","ci_cd"],        "min_exp": 2, "domain": "software"},
    {"title": "qa_engineer",          "skills": ["selenium","java","testing"],          "min_exp": 1, "domain": "software"},
    {"title": "mobile_dev",           "skills": ["java","kotlin","android"],            "min_exp": 2, "domain": "software"},
    {"title": "product_manager",      "skills": ["agile","communication","sql"],        "min_exp": 3, "domain": "product"},
    {"title": "cloud_architect",      "skills": ["aws","docker","kubernetes"],          "min_exp": 4, "domain": "cloud"},
    {"title": "security_engineer",    "skills": ["networking","python","security"],     "min_exp": 3, "domain": "security"},
    {"title": "analytics_engineer",   "skills": ["sql","python","dbt"],                 "min_exp": 2, "domain": "data"},
]

def skill_match(cs, js): return (len(set(cs) & set(js)) / len(js)) * 100 if js else 0
def exp_score(ce, me): return 100 if ce >= me else (ce/me)*100 if me>0 else 0
def match_score(c, j): 
    if c['domain'] != j['domain']: return 0
    return round(skill_match(c['skills'], j['skills'])*0.7 + exp_score(c['experience'], j['min_exp'])*0.3, 2)

def best_matches(c, top=3):
    scores = [(j['title'], match_score(c,j)) for j in JOBS]
    return sorted(scores, key=lambda x: x[1], reverse=True)[:top]

# ---------------------------
# Streamlit UI
# ---------------------------
st.markdown("<h1 style='text-align:center'>💼 AI Job Matcher</h1>", unsafe_allow_html=True)
st.markdown("<p style='text-align:center; color:grey'>Upload resumes and see top job recommendations instantly</p>", unsafe_allow_html=True)

uploaded_files = st.file_uploader("📄 Upload Resume PDFs", type="pdf", accept_multiple_files=True)

if uploaded_files:
    for uf in uploaded_files:
        text = extract_text_from_pdf(uf)
        cand = parse_resume(text)

        st.subheader(f"👤 Candidate: {cand['full_name']}")
        col1, col2 = st.columns(2)
        with col1:
            st.markdown(f"**Experience:** {cand['experience']} years")
            st.markdown(f"**Domain:** {cand['domain'].capitalize()}")
        with col2:
            st.markdown("**Skills:**")
            st.write(", ".join(cand['raw_skills']) if cand['raw_skills'] else "Not detected")

        st.markdown("### 🔎 Top Recommendations")
        matches = best_matches(cand, 3)
        st.table([{"Job": m[0], "Match Score": f"{m[1]}%"} for m in matches])
        st.markdown("---")

