import pandas as pd
import streamlit as st
from .config import JNJ_RED, MUTED


def clean_text_values(series: pd.Series) -> list[str]:
    return sorted(series.dropna().astype(str).str.strip().loc[lambda s: s.ne('')].unique().tolist())


def render_sidebar_filters(studies: pd.DataFrame) -> tuple[pd.DataFrame, set[str]]:
    with st.sidebar:
        st.markdown(
            f'<div style="padding:10px 0 20px 0;"><div style="font-family:Georgia,serif;font-size:24px;font-weight:700;color:{JNJ_RED};white-space:nowrap;">Johnson&amp;Johnson</div>'
            f'<div style="font-size:11px;color:{MUTED};margin-top:6px;">ARGES Commons · Clinical Data Inventory</div></div>',
            unsafe_allow_html=True,
        )
        st.markdown('### Filters')
        eligible = studies.copy()
        for label, column in [('Study', 'Study Name'), ('Disease', 'Disease'), ('Phase', 'Phase'), ('Trial status', 'Trial Status'), ('Compound', 'Compound')]:
            options = clean_text_values(eligible.get(column, pd.Series(dtype=str)))
            selected = st.multiselect(label, options)
            if selected and column in eligible:
                eligible = eligible[eligible[column].astype(str).str.strip().isin(selected)]
        ids = set(eligible['Study ID'].dropna().astype(str))
        st.markdown('---')
        st.caption(f"{len(ids)} of {studies['Study ID'].nunique()} studies selected")
        with st.expander(f'View selected studies ({len(ids)})', expanded=False):
            if not ids:
                st.caption('No studies match the current filters.')
            else:
                rows = eligible.dropna(subset=['Study ID']).drop_duplicates('Study ID')
                if 'Study Name' in rows:
                    rows = rows.sort_values('Study Name')
                for _, row in rows.iterrows():
                    name = row.get('Study Name', row['Study ID'])
                    st.markdown(f'**{name}**')
                    st.caption(str(row['Study ID']))
                    details = [str(row.get(c)) for c in ('Disease', 'Phase') if pd.notna(row.get(c)) and str(row.get(c)).strip()]
                    if details:
                        st.caption(' · '.join(details))
    return eligible, ids
