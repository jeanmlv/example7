"""Generic ADaM extractor for Slim ARD conflict validation.

Example (PowerShell):
 python extract_adam_reference_v2.py --adam-dir "$env:USERPROFILE\Downloads" --conflicts "$env:USERPROFILE\Downloads\slim_ard_stage1_conflicts.csv" --study QUASAR

Requires pandas. Reads large ADaM CSVs in chunks. No candidate is selected here.
"""
import argparse
import re
from pathlib import Path
import pandas as pd

KEYS = ['USUBJID', 'AVISIT', 'AVISITN', 'PARAMCD']
KEEP = ['USUBJID','AVISIT','AVISITN','PARAMCD','PARAM','AVAL','AVALC','DTYPE','ASEQ','ANL01FL','ANL02FL','ADT','ADY','ADT1','ADY1','ATPT','ATPTN','BASETYPE','ABLFL','TRTP','TRTA','SRCDOM','SRCVAR','SRCSEQ']
SOURCE_ALIASES = ['SOURCE','ADAM_REQUIRED','ADAM_DATASET','SOURCE_DATASET','DATASET','ADAM']

def norm(series, key):
    x = series.astype('string').fillna('').str.strip()
    if key == 'AVISITN':
        x = x.str.replace(r'^(\-?\d+)\.0+$', r'\1', regex=True)
    return x

def load_targets(path):
    c = pd.read_csv(path, dtype=str, keep_default_na=False, encoding='utf-8-sig')
    c.columns = [str(x).strip().upper() for x in c.columns]
    src = next((x for x in SOURCE_ALIASES if x in c.columns), None)
    if src is None:
        raise ValueError('Dataset source column not found. Available: ' + ', '.join(c.columns))
    missing = [k for k in KEYS if k not in c.columns]
    if missing:
        raise ValueError('Conflict columns missing: ' + ', '.join(missing))
    for k in KEYS:
        c[k] = norm(c[k], k)
    c[src] = norm(c[src], src).str.upper()
    c = c[c[src].ne('') & c['USUBJID'].ne('') & c['PARAMCD'].ne('')].copy()
    targets = {}
    for ds, g in c.groupby(src):
        targets[ds] = set(g[KEYS].itertuples(index=False, name=None))
    return targets, len(c)

def dataset_for_file(path, needed):
    stem = path.stem.upper()
    matches = [ds for ds in needed if stem == ds or stem.startswith(ds+'_') or stem.startswith(ds+'-')]
    return max(matches, key=len) if matches else None

def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--adam-dir', type=Path, required=True)
    p.add_argument('--conflicts', type=Path, required=True)
    p.add_argument('--output-dir', type=Path, default=Path('adam_reference_output'))
    p.add_argument('--study', default='study', help='Output filename prefix, e.g. QUASAR')
    p.add_argument('--chunksize', type=int, default=50000)
    args = p.parse_args()
    if not args.adam_dir.is_dir(): p.error('ADaM folder not found: '+str(args.adam_dir))
    if not args.conflicts.is_file(): p.error('Conflict CSV not found: '+str(args.conflicts))
    if args.chunksize < 1: p.error('--chunksize must be positive')
    targets, nconf = load_targets(args.conflicts)
    if not targets: p.error('No usable conflicts found')
    needed = sorted(targets)
    print('Conflict rows:', nconf, '| ADaM required:', ', '.join(needed))
    paths = {ds: [] for ds in needed}
    for f in args.adam_dir.rglob('*.csv'):
        ds = dataset_for_file(f, needed)
        if ds: paths[ds].append(f)
    args.output_dir.mkdir(parents=True, exist_ok=True)
    prefix = re.sub(r'[^a-zA-Z0-9_-]+', '_', args.study).strip('_').lower() or 'study'
    out = args.output_dir / f'{prefix}_adam_reference.csv.gz'
    audit_path = args.output_dir / 'extraction_audit.csv'
    # Do not append to stale outputs from a previous run.
    if out.exists(): out.unlink()
    audit, first, failed = [], True, False
    for ds in needed:
        if not paths[ds]:
            failed = True
            audit.append(dict(dataset=ds, file='NOT FOUND', status='MISSING', records_read=0, records_exported=0, target_keys=len(targets[ds])))
            continue
        for f in sorted(paths[ds]):
            read_count = export_count = 0
            try:
                reader = pd.read_csv(f, dtype=str, keep_default_na=False, encoding='utf-8-sig', chunksize=args.chunksize, on_bad_lines='error')
                for chunk in reader:
                    chunk.columns = [str(x).strip().upper() for x in chunk.columns]
                    missing = [k for k in KEYS if k not in chunk.columns]
                    if missing: raise ValueError('Required ADaM columns missing: '+', '.join(missing))
                    for k in KEYS: chunk[k] = norm(chunk[k], k)
                    matched = pd.Series([key in targets[ds] for key in chunk[KEYS].itertuples(index=False, name=None)], index=chunk.index)
                    selected = chunk.loc[matched].copy()
                    if not selected.empty:
                        for k in KEEP:
                            if k not in selected.columns: selected[k] = ''
                        original_row = (selected.index.to_series() + read_count + 2).astype(str)
                        selected = selected[KEEP]
                        selected.insert(0, 'SOURCE_DATASET', ds)
                        selected.insert(1, 'SOURCE_LOAD', f.stem)
                        selected.insert(2, 'SOURCE_FILE', str(f.relative_to(args.adam_dir)))
                        selected.insert(3, 'SOURCE_ROW_NUMBER', original_row)
                        selected.to_csv(out, mode='wt' if first else 'at', header=first, index=False, compression='gzip', encoding='utf-8')
                        first = False
                        export_count += len(selected)
                    read_count += len(chunk)
                status = 'OK' if export_count else 'ZERO_MATCHES'
                if export_count == 0: failed = True
                audit.append(dict(dataset=ds, file=str(f.relative_to(args.adam_dir)), status=status, records_read=read_count, records_exported=export_count, target_keys=len(targets[ds])))
            except Exception as e:
                failed = True
                audit.append(dict(dataset=ds, file=str(f.relative_to(args.adam_dir)), status='ERROR: '+str(e), records_read=read_count, records_exported=export_count, target_keys=len(targets[ds])))
    pd.DataFrame(audit).to_csv(audit_path, index=False)
    print(pd.DataFrame(audit).to_string(index=False))
    print('Audit:', audit_path)
    if not first: print('Reference:', out, '| MB:', round(out.stat().st_size / 1048576, 2))
    else: print('No records exported; review column names, visit mappings and extraction audit.')
    if failed: print('WARNING: Some files missing, failed, or had zero matches. Review audit before Stage 2.')
    print('Original ADaM files remain unchanged.')

if __name__ == '__main__': main()
