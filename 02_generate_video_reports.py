"""Generate ARGES-ready study totals and visit-level video coverage from Thea.View inventory.
Usage: python 02_generate_video_reports.py --input video_inventory_output/all_video_inventory.csv
No API access; source CSV is read-only. Requires pandas and artifact_tool.
"""
import argparse
import re
from pathlib import Path
import pandas as pd
from artifact_tool import Workbook, SpreadsheetFile

SCHEDULED = ['Screening','I-12','I-24','M-44','M-92','M-140','M-188','M-236']
REQUIRED = {'study','data_type','site','patient_id','visit','filename','video_class','examination_date','anatomical_region'}

def norm_name(s):
    s = re.sub(r'\.[^.]+$', '', str(s).strip(), flags=re.I)
    s = re.sub(r'(?i)(?:[._ -]+transcoded)+$', '', s)
    return re.sub(r'\s+', ' ', s).strip().casefold()

def excel(path, tabs):
    wb=Workbook.create()
    for name, frame in tabs.items():
        ws=wb.worksheets.add(name)
        data=[list(frame.columns)] + [[None if pd.isna(v) else (int(v) if isinstance(v, (int,)) else v) for v in row] for row in frame.itertuples(index=False, name=None)]
        if data:
            ws.get_range_by_indexes(0,0,len(data),len(data[0])).values=data
            ws.get_range_by_indexes(0,0,1,len(data[0])).format.font.bold=True
            ws.freeze_panes.freeze_rows(1)
            ws.get_range_by_indexes(0,0,len(data),len(data[0])).format.autofit_columns()
    SpreadsheetFile.export_xlsx(wb).save(str(path))

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--input',default='video_inventory_output/all_video_inventory.csv')
    ap.add_argument('--output-dir',default='video_reports_output')
    a=ap.parse_args()
    path=Path(a.input)
    if not path.is_file(): ap.error(f'Input not found: {path}')
    out=Path(a.output_dir);out.mkdir(parents=True,exist_ok=True)
    df=pd.read_csv(path,dtype=str,keep_default_na=False,encoding='utf-8-sig')
    missing=REQUIRED-set(df.columns)
    if missing: ap.error(f'Missing columns: {sorted(missing)}')
    if df.empty: ap.error('Empty inventory')
    df=df.copy()
    for col in REQUIRED: df[col]=df[col].str.strip()
    df['patient_key']=df['site']+'::'+df['patient_id']
    df['valid_patient']=(df.site!='')&(df.patient_id!='')
    df['recording_name']=df.filename.map(norm_name)
    identity=['study','data_type','site','patient_id','visit','examination_date','anatomical_region','recording_name']
    priority={'RAW_ORIGINAL':0,'RAW_UNCLASSIFIED':1,'RAW_TRANSCODED':2,'PROCESSED':0}
    df['priority']=df.video_class.map(priority).fillna(3)
    rep=df.sort_values('priority',kind='stable').drop_duplicates(identity)
    studies=[]; visit_rows=[]; qa=[]
    for study,sg in df.groupby('study',sort=True):
        rec={'study':study}
        for typ,tag in [('rawdata','raw'),('processed','processed')]:
            part=sg[sg.data_type==typ]
            rec[f'total_{tag}_file_items']=len(part)
            rec[f'total_{tag}_unique_patients']=part.loc[part.valid_patient,'patient_key'].nunique()
            qa.append({'study':study,'data_type':typ,'file_items':len(part),'estimated_technical_recordings':len(rep[(rep.study==study)&(rep.data_type==typ)]),'missing_patient_identifiers':int((~part.valid_patient).sum()),'source_flagged_review':int(part.get('needs_review',pd.Series('',index=part.index)).str.lower().isin(['true','1','yes']).sum())})
        studies.append(rec)
        for typ,tag in [('rawdata','rawdata'),('processed','processed')]:
            part=sg[sg.data_type==typ]; rpart=rep[(rep.study==study)&(rep.data_type==typ)]
            visits=list(SCHEDULED)+sorted(set(part.visit)-set(SCHEDULED))
            seen=set(); complete=None
            for visit in visits:
                pf=part[part.visit==visit]; pr=rpart[rpart.visit==visit]
                patients=set(pf.loc[pf.valid_patient,'patient_key'])
                if visit in SCHEDULED:
                    seen|=patients
                    complete=patients if complete is None else complete&patients
                    cumulative=len(seen); longitudinal=len(complete)
                else: cumulative='N/A';longitudinal='N/A'
                visit_rows.append({'study':study,'visit':visit,'visit_type':'SCHEDULED' if visit in SCHEDULED else 'UNSCHEDULED','data_type':typ,f'{tag}_unique_patients_at_visit':len(patients),f'{tag}_file_items':len(pf),f'{tag}_technical_recordings_est':len(pr),f'{tag}_cumulative_unique_patients':cumulative,'complete_longitudinal_patients':longitudinal})
    summary=pd.DataFrame(studies)
    visits=pd.DataFrame(visit_rows)
    raw=visits[visits.data_type=='rawdata'].drop(columns='data_type').reset_index(drop=True)
    processed=visits[visits.data_type=='processed'].drop(columns='data_type').reset_index(drop=True)
    definitions=pd.DataFrame([
        ('unique_patients_at_visit','Distinct site + patient_id combinations with >=1 video file at the specified visit.'),
        ('file_items','Number of video file items found in the inventory, including transcoded versions and potential copies; not folder count.'),
        ('technical_recordings_est','Estimated distinct technical recordings after metadata-based filename and examination deduplication; not clinically verified unique videos.'),
        ('cumulative_unique_patients','Distinct patients with >=1 video at any scheduled visit up to and including the indicated visit (union).'),
        ('complete_longitudinal_patients','Distinct patients with >=1 video at every scheduled visit from Screening through the indicated visit (intersection). Assumes all listed visits apply.'),
        ('unscheduled visits','Included in per-visit and study totals; excluded from scheduled cumulative/longitudinal counts.'),
        ('total_*_unique_patients','Distinct site + patient_id combinations across ALL visits, including unscheduled; never sum per-visit counts.'),
        ('data_source','Thea.View extracted inventory; counts are inventory-derived, not a protocol compliance measure.')
    ],columns=['metric','interpretation'])
    summary.to_csv(out/'video_study_summary.csv',index=False,encoding='utf-8-sig')
    visits.to_csv(out/'video_visit_coverage.csv',index=False,encoding='utf-8-sig')
    pd.DataFrame(qa).to_csv(out/'video_quality_checks.csv',index=False,encoding='utf-8-sig')
    excel(out/'video_study_summary.xlsx',{'study_summary':summary,'definitions':definitions})
    excel(out/'video_visit_coverage.xlsx',{'raw_videos':raw,'processed_videos':processed,'definitions':definitions})
    print('Generated:',out.resolve());print(summary.to_string(index=False))

if __name__=='__main__':main()
