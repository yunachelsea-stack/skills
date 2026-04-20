import streamlit as st
import json
import pandas as pd
import io
import re
from collections import defaultdict, deque

st.set_page_config(page_title="Digital Skills Survey Builder", layout="wide")
st.title("Digital Skills Survey Builder — Prototype")

# =========================
# Toolkit High-Level Domains
# =========================
MODULE_ORDER = [
    "Information & Data Literacy",
    "Communication & Collaboration",
    "Digital Content Creation"
]

def module_sort_key(m: str):
    if m in MODULE_ORDER:
        return (MODULE_ORDER.index(m), m)
    return (999, m)

# =========================
# Helpers
# =========================
VAR_REF_PATTERN = re.compile(r"\$\{([A-Za-z0-9_]+)\}")

def referenced_vars(relevance: str) -> set[str]:
    if not relevance:
        return set()
    return set(VAR_REF_PATTERN.findall(relevance))

def format_options(it: dict) -> str:
    if it.get("type") != "select_one":
        return ""
    choices = it.get("choices", [])
    if not choices:
        return "(No choices listed)"
    return "; ".join([f"{c.get('name')}={c.get('label')}" for c in choices])

def build_xlsform_excel(all_items, included_ids):
    survey_rows = []
    choices_rows = []

    for it in all_items:
        qid = it["id"]
        if qid not in included_ids:
            continue

        qtype = it.get("type", "")
        label = it.get("question", "")
        relevance = it.get("relevance", "") or ""

        if qtype == "select_one":
            survey_rows.append({
                "type": f"select_one {qid}",
                "name": qid,
                "label": label,
                "relevance": relevance,
                "required": ""
            })
            for ch in it.get("choices", []):
                choices_rows.append({
                    "list_name": qid,
                    "name": ch.get("name", ""),
                    "label": ch.get("label", "")
                })
        else:
            survey_rows.append({
                "type": qtype,
                "name": qid,
                "label": label,
                "relevance": relevance,
                "required": ""
            })

    survey_df = pd.DataFrame(survey_rows)
    choices_df = pd.DataFrame(choices_rows)

    output = io.BytesIO()
    with pd.ExcelWriter(output, engine="openpyxl") as writer:
        survey_df.to_excel(writer, sheet_name="survey", index=False)
        choices_df.to_excel(writer, sheet_name="choices", index=False)

    return output.getvalue()

# =========================
# Load items.json
# =========================
with open("items.json", "r", encoding="utf-8") as f:
    items = json.load(f)

id_to_item = {it["id"]: it for it in items}

# Group by domain
items_by_module = defaultdict(list)
for it in items:
    items_by_module[it["module"]].append(it)

for m in items_by_module:
    items_by_module[m].sort(key=lambda x: x["id"])

all_modules = sorted(items_by_module.keys(), key=module_sort_key)

# =========================
# Dependency Maps
# =========================
prereq_map = {qid: referenced_vars(it.get("relevance", "") or "") for qid, it in id_to_item.items()}
dependents_map = defaultdict(set)
for qid, prereqs in prereq_map.items():
    for p in prereqs:
        dependents_map[p].add(qid)

# =========================
# Session State Initialization
# =========================
if "selected_modules" not in st.session_state:
    st.session_state.selected_modules = set(MODULE_ORDER)

def compute_available_ids():
    ids = set()
    for m in st.session_state.selected_modules:
        for it in items_by_module.get(m, []):
            ids.add(it["id"])
    return ids

def ensure_question_keys(qid, default=True):
    inc_key = f"inc_{qid}"
    if inc_key not in st.session_state:
        st.session_state[inc_key] = default

    v = st.session_state[inc_key]
    for prefix in ("sb_", "sv_"):
        key = f"{prefix}{qid}"
        if key not in st.session_state:
            st.session_state[key] = v

def sync_widgets_from_inc(qid):
    v = st.session_state.get(f"inc_{qid}", False)
    st.session_state[f"sb_{qid}"] = v
    st.session_state[f"sv_{qid}"] = v

def downstream_dependents(qid, available_ids):
    removed = set()
    q = deque([qid])
    while q:
        cur = q.popleft()
        for d in dependents_map.get(cur, set()):
            if d in available_ids and d not in removed:
                removed.add(d)
                q.append(d)
    return removed

def apply_logic(qid, new_val, available_ids):
    st.session_state[f"inc_{qid}"] = new_val
    sync_widgets_from_inc(qid)

    if not new_val:
        for d in downstream_dependents(qid, available_ids):
            st.session_state[f"inc_{d}"] = False
            sync_widgets_from_inc(d)

def handle_toggle(qid, source_key):
    available_ids = st.session_state.available_ids
    new_val = st.session_state[source_key]
    apply_logic(qid, new_val, available_ids)

# =========================
# Sidebar: Domain Selection
# =========================
st.sidebar.header("Select domains")

for m in all_modules:
    key = f"mod_{m}"
    if key not in st.session_state:
        st.session_state[key] = (m in st.session_state.selected_modules)

    checked = st.sidebar.checkbox(m, key=key)
    if checked:
        st.session_state.selected_modules.add(m)
    else:
        st.session_state.selected_modules.discard(m)

st.session_state.available_ids = compute_available_ids()
available_ids = st.session_state.available_ids

for qid in available_ids:
    ensure_question_keys(qid)

# =========================
# Sidebar: Question Toggles
# =========================
st.sidebar.divider()
st.sidebar.header("Include / exclude questions")

for m in sorted(st.session_state.selected_modules, key=module_sort_key):
    st.sidebar.markdown(f"**{m}**")
    for it in items_by_module[m]:
        qid = it["id"]
        st.sidebar.checkbox(
            qid,
            key=f"sb_{qid}",
            on_change=handle_toggle,
            args=(qid, f"sb_{qid}")
        )

# =========================
# Build Ordered Question List
# =========================
all_items = []
for m in sorted(st.session_state.selected_modules, key=module_sort_key):
    all_items.extend(items_by_module[m])

included_ids = {qid for qid in available_ids if st.session_state.get(f"inc_{qid}", False)}

# =========================
# Summary
# =========================
col1, col2, col3 = st.columns(3)
col1.metric("Domains selected", len(st.session_state.selected_modules))
col2.metric("Questions available", len(all_items))
col3.metric("Questions included", len(included_ids))

# =========================
# Preview Tabs
# =========================
st.subheader("Preview")
tab_survey, tab_prog = st.tabs(["Survey preview", "Programming preview (SurveyCTO/XLSForm style)"])

with tab_survey:
    for idx, it in enumerate(all_items, start=1):
        qid = it["id"]
        domain = it["module"]

        left, right = st.columns([12, 3])

        with left:
            st.markdown(f"### {idx}. {it['question']}")
            st.caption(f"Domain: {domain} • Variable: {qid}")

            # Survey preview should NOT change when included/excluded
            if it["type"] == "select_one":
                labels = [c.get("label", "") for c in it.get("choices", [])]
                st.radio(
                    "Select one:",
                    labels,
                    index=None,
                    key=f"resp_{qid}"
                )
            else:
                st.text_input("Response:", key=f"resp_{qid}")

        with right:
            st.checkbox(
                "Included",
                key=f"sv_{qid}",
                on_change=handle_toggle,
                args=(qid, f"sv_{qid}")
            )

        st.divider()

with tab_prog:
    rows = []
    for it in all_items:
        rows.append({
            "Domain": it["module"],
            "ID": it["id"],
            "Included": "Yes" if st.session_state.get(f"inc_{it['id']}", False) else "No",
            "Type": it["type"],
            "Relevance": it.get("relevance", ""),
            "Question": it["question"],
            "Options": format_options(it)
        })

    df = pd.DataFrame(rows)
    st.dataframe(df, use_container_width=True, hide_index=True)

# =========================
# Export
# =========================
st.subheader("Export")

if st.button("Generate XLSForm (Excel)"):
    data = build_xlsform_excel(all_items, included_ids)
    st.download_button(
        "Download digital_skills_survey.xlsx",
        data=data,
        file_name="digital_skills_survey.xlsx",
        mime="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )