import pandas as pd
import streamlit as st
from src.config import REQUIRED_SHEETS, PAGES
from src.data_loader import load_workbook, resolve_default_workbook, filter_by_study
from src.filters import render_sidebar_filters
from src.ui import apply_theme, render_header
from views import (overview, studies_assets, data_availability, processing, data_splits,
                   ard, variable_definitions, data_analysis, external_data, arges_models, deployments)

st.set_page_config(page_title='ARGES Commons', page_icon='🧬', layout='wide', initial_sidebar_state='expanded')
apply_theme()
source = resolve_default_workbook()
if source is None:
    st.error('ARGES_COMMONS.xlsx was not found. Place it beside app.py.')
    st.stop()
try:
    data = load_workbook(source)
except Exception as exc:
    st.error(f'The workbook could not be loaded: {exc}')
    st.stop()
missing = [s for s in REQUIRED_SHEETS if s not in data]
if missing:
    st.warning('Some expected sheets are not present: ' + ', '.join(missing))
studies = data.get('01_STUDIES', pd.DataFrame()).copy()
if studies.empty or 'Study ID' not in studies.columns:
    st.error('Sheet 01_STUDIES with a Study ID column is required.')
    st.stop()
eligible, selected_ids = render_sidebar_filters(studies)
render_header()
page = st.segmented_control('Navigation', PAGES, default='Overview', label_visibility='collapsed') or 'Overview'
filtered = {name: filter_by_study(df, selected_ids) for name, df in data.items()}
filtered['01_STUDIES'] = eligible.copy()
# Preserve global reference tables; ensure empty selections do not restore study rows.
if not selected_ids:
    filtered = {name: (df.iloc[0:0].copy() if 'Study ID' in df.columns else df) for name, df in filtered.items()}
ROUTES = {
    'Overview': overview.render,
    'Studies & Assets': studies_assets.render,
    'Data Availability': data_availability.render,
    'Processing': processing.render,
    'Data Splits': data_splits.render,
    'ARD': ard.render,
    'Variable Definitions': variable_definitions.render,
    'Data Analysis': data_analysis.render,
    'External Data': external_data.render,
    'ARGES Models': arges_models.render,
    'Deployments': deployments.render,
}
ROUTES[page](filtered, data)
st.markdown('---')
st.caption('ARGES Commons · Data shown reflects the selected workbook and active filters. Validate source metadata before operational or clinical use.')
