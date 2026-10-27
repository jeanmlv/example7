"""Extract ADaM records relevant to unresolved Slim ARD conflicts.
Run: python extract_anthem_adam_reference.py --adam-dir "C:\\path\\to\\csv" --conflicts "C:\\path\\to\\slim_ard_stage1_conflicts.csv"
"""
import argparse
import json
from pathlib import Path
import pandas as pd

KEYS = ['USUBJID', 'AVISIT', 'AVISITN', 'PARAMCD']
KEEP = ['USUBJID','AVISIT','AVISITN','PARAMCD','PARAM','AVAL','AVALC','DTYPE','ASEQ','ANL01FL','ANL02FL','ADT','ADY','ADT1','ADY1','ATPT','ATPTN','BASETYPE','ABLFL','TRTP','TRTA','SRCDOM','SRCVAR','SRCSEQ']

def normalize(x):
    return x.astype('string').fillna('').str.strip()

def main():
    p=argparse.ArgumentParser()
    p.add_argument('--adam-dir', type=Path, default=Path('.'))
    p.add_argument('--conflicts', type=Path, default=Path('slim_ard_stage1_conflicts.csv'))
    p.add_argument('--output-dir', type=Path, default=Path('adam_reference_output'))
    p.add_argument('--chunksize', type=int, default=50000)
    args=p.parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    conflicts=pd.read_csv(args.conflicts, dtype=str, keep_default_na=False, encoding='utf-8-sig')
    needed={'ADMAYO','ADLBI','ADIBDQ'}
    conflicts=conflicts[conflicts['ADaM_required'].str.upper().isin(needed)].copy()
    # Keys are taken from actual conflicts, not inferred from all subjects in the ARD.
    targets={}
    for ds, group in conflicts.groupby('ADaM_required'):
        t=group[['USUBJID','AVISIT','AVISITN','PARAMCD']].drop_duplicates().copy()
        for k in KEYS: t[k]=normalize(t[k])
        targets[ds.upper()]=set(map(tuple,t[KEYS].itertuples(index=False,name=None)))
    paths={}
    for f in args.adam_dir.glob('*.csv'):
        stem=f.stem.upper()
        for ds in needed:
            if stem==ds or stem.startswith(ds+'_') or stem.startswith(ds+'-'):
                paths.setdefault(ds,[]).append(f)
    output=args.output_dir/'anthem_adam_reference.csv.gz'
    audit=[]; first=True
    for ds in sorted(needed):
        if not paths.get(ds):
            audit.append({'dataset':ds,'file':'NOT FOUND','status':'MISSING','records_read':0,'records_exported':0})
            continue
        for f in sorted(paths[ds]):
            read_count=export_count=0
            try:
                reader=pd.read_csv(f,dtype=str,keep_default_na=False,encoding='utf-8-sig',chunksize=args.chunksize,low_memory=False,on_bad_lines='error')
                for chunk in reader:
                    read_count+=len(chunk)
                    missing=[k for k in KEYS if k not in chunk.columns]
                    if missing:
                        raise ValueError(f'{f.name}: required columns missing: {missing}')
                    for k in KEYS: chunk[k]=normalize(chunk[k])
                    keys=list(zip(*(chunk[k] for k in KEYS)))
                    selected=chunk.loc[[k in targets[ds] for k in keys]].copy()
                    if selected.empty: continue
                    for k in KEEP:
                        if k not in selected: selected[k]=''
                    selected=selected[KEEP]
                    selected.insert(0,'SOURCE_DATASET',ds)
                    selected.insert(1,'SOURCE_LOAD',f.stem)
                    selected.insert(2,'SOURCE_FILE',f.name)
                    selected.insert(3,'SOURCE_ROW_NUMBER',pd.Series(selected.index,index=selected.index).astype(str))
                    # Row number above is chunk-local; keep provenance via file and chunk position too.
                    selected['SOURCE_ROW_NUMBER']=(selected.index.to_series()+read_count-len(chunk)+2).astype(str)
                    selected.to_csv(output,mode='wt' if first else 'at',header=first,index=False,compression='gzip',encoding='utf-8')
                    first=False
                    export_count+=len(selected)
                audit.append({'dataset':ds,'file':f.name,'status':'OK','records_read':read_count,'records_exported':export_count})
            except Exception as e:
                audit.append({'dataset':ds,'file':f.name,'status':'ERROR: '+str(e),'records_read':read_count,'records_exported':export_count})
                print('ERROR:',f.name,e)
    pd.DataFrame(audit).to_csv(args.output_dir/'extraction_audit.csv',index=False)
    if first:
        print('No matching records exported. Review extraction_audit.csv and key formats.')
    else:
        print('Saved:',output,'size MB:',round(output.stat().st_size/1024**2,2))
    print(pd.DataFrame(audit).to_string(index=False))
    print('Keep the original ADaM files; do not delete them.')

if __name__=='__main__': main()
