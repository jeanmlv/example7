"""Build ANTHEM Slim ARD using record-level ADaM reference exported from Domino.
Run on Windows after copying anthem_adam_record_reference.csv to input_folder.
No replacement unless the selected value matches a candidate already in the ARD cell.
"""
from pathlib import Path
from collections import defaultdict
from decimal import Decimal, InvalidOperation
import pandas as pd

INPUT_FOLDER = Path(r'C:\Users\JMende95\OneDrive - JNJ\Desktop\ard_data\csv')
INPUT_FILE = INPUT_FOLDER / '77242113UCO2001_anthem_wk78_ard_merged_20260818.csv'
REFERENCE_FILE = INPUT_FOLDER / 'anthem_adam_record_reference.csv'
OUTPUT_FOLDER = INPUT_FOLDER / 'slim'
OUTPUT_FILE = OUTPUT_FOLDER / '77242113UCO2001_anthem_wk78_ard_merged_slim_dtype.csv'
KEYS = ['USUBJID','AVISIT','AVISITN']

PARAMS = {
 'ADMAYO': {
  'AVAL': 'ASTOOL ASTOOLAL DRBLEED DSTOOL ENASCOR ENCSCOR ENFSCOR ENFSCORI ENLSCOR FMAYO FMAYOAL MFMAYO MFMAYOAL MMAYO MMAYOAL MMMAYO MMMAYOAL MPMAYO MPMAYOAL NFMAYO NFMAYOAL NMMAYO NMMAYOAL NPMAYO NPMAYOAL PGSCOR PMAYO PMAYOAL RBSCOR RBSCORAL RBSCORI SFSCOR SFSCORAL SFSCORI',
  'AVALC': 'CREM CREMA1 CREMA2 CREMAL CRESP CRESPAL CS1CREM CSCREM ENIMP ENREM RBSCOR0 SFSCOR0 SFSCOR01 SREM SREMA1 SREMA2 SREMAL'
 },
 'ADIBDQ': {'AVAL':'IBDQB IBDQE IBDQSF IBDQSY IBDQTOT','AVALC':'IBDQREM'},
 'ADLBI': {'AVAL':'CALPRO CALPROP CRP CRPP LCALPRO LCALPROP LCRP LCRPP', 'AVALC':'CRPNRM CRPNRMP'}
}
VARIABLES_OF_INTEREST = [f'{src}_PARAMCD_{param}_{kind}' for src, kinds in PARAMS.items() for kind, names in kinds.items() for param in names.split()]
VARIABLES_OF_INTEREST += ['ADSL_AGE','ADSL_AGEU','ADSL_SEX','ADSL_RACE','ADSL_ETHNIC','ADSL_COUNTRY','ADSL_TRT02P','ADSL_TRTSEQP']
DTYPE_PRIORITY = {'':1,'DERIVED':2,'AVERAGE':3,'LOCF':4,'WOCF':5,'BOCF':6,'TF':7}


def norm_key(x):
    s = str(x).strip()
    if s.lower() in ('nan','none','<na>'): return ''
    return s


def norm_visitn(x):
    s = norm_key(x)
    try:
        return str(Decimal(s).normalize()) if s else ''
    except InvalidOperation:
        return s.upper()


def norm_value(x, kind):
    s = norm_key(x)
    if not s: return ''
    if kind == 'AVAL':
        try:
            v = Decimal(s)
            return str(v.normalize()) if v.is_finite() else ''
        except InvalidOperation:
            return ''
    return s


def candidates(x, kind):
    if not norm_key(x): return []
    parts = [v.strip() for v in str(x).split('|')]
    return list(dict.fromkeys(v for v in parts if norm_value(v,kind)))


def main():
    OUTPUT_FOLDER.mkdir(parents=True,exist_ok=True)
    if not INPUT_FILE.is_file(): raise FileNotFoundError(INPUT_FILE)
    if not REFERENCE_FILE.is_file(): raise FileNotFoundError(REFERENCE_FILE)
    df = pd.read_csv(INPUT_FILE,dtype=str,keep_default_na=False,low_memory=False)
    ref = pd.read_csv(REFERENCE_FILE,dtype=str,keep_default_na=False,low_memory=False)
    required = KEYS + ['SOURCE','PARAMCD','AVAL','AVALC','DTYPE','LOAD','LOAD_RANK']
    missing_ref = [c for c in required if c not in ref]
    if missing_ref: raise ValueError(f'Reference missing columns: {missing_ref}')
    if any(k not in df for k in KEYS): raise ValueError('ARD missing one or more keys')
    columns = KEYS + [c for c in VARIABLES_OF_INTEREST if c in df]
    missing = [c for c in VARIABLES_OF_INTEREST if c not in df]
    slim = df[columns].copy()
    # Build index by exact patient + visit + visit number + source + parameter.
    index = defaultdict(list)
    for row in ref.to_dict('records'):
        key = (norm_key(row['USUBJID']),norm_key(row['AVISIT']).upper(),norm_visitn(row['AVISITN']),norm_key(row['SOURCE']).upper(),norm_key(row['PARAMCD']).upper())
        index[key].append(row)
    qc = []
    stats = defaultdict(int)
    for col in [c for c in slim if '_PARAMCD_' in c and c.endswith(('_AVAL','_AVALC'))]:
        source, rest = col.split('_PARAMCD_',1)
        param, kind = rest.rsplit('_',1)
        for rowidx in slim.index:
            old = slim.at[rowidx,col]
            old_parts = candidates(old,kind)
            if len(old_parts) < 2:
                continue  # do not alter cells that are not conflicting
            key = (norm_key(slim.at[rowidx,'USUBJID']),norm_key(slim.at[rowidx,'AVISIT']).upper(),norm_visitn(slim.at[rowidx,'AVISITN']),source,param)
            matches = index.get(key,[])
            status = 'NO_REFERENCE'; new = old; chosen_load = ''; rank = ''; details = ''
            if matches:
                # Only values already present in the original ARD cell may be selected.
                available = {norm_value(v,kind) for v in old_parts}
                matches = [r for r in matches if norm_value(r.get(kind,''),kind) in available]
                if not matches:
                    status = 'REFERENCE_VALUES_NOT_IN_ARD'
                else:
                    # Prefer the highest-priority load that contains matching candidates.
                    best_load = min(int(r['LOAD_RANK']) for r in matches)
                    matches = [r for r in matches if int(r['LOAD_RANK']) == best_load]
                    chosen_load = matches[0]['LOAD']
                    ranked = [(DTYPE_PRIORITY.get(norm_key(r['DTYPE']).upper(),99),r) for r in matches]
                    best_rank = min(k for k,_ in ranked)
                    rank = best_rank
                    winners = [r for k,r in ranked if k == best_rank]
                    selected = {norm_value(r[kind],kind) for r in winners}
                    # Retain original order and spelling; remove lower-priority candidates.
                    selected_parts = [v for v in old_parts if norm_value(v,kind) in selected]
                    selected_parts = list(dict.fromkeys(selected_parts))
                    if best_rank == 99:
                        status = 'UNKNOWN_DTYPE'; new = old
                    elif len(selected_parts) == 1:
                        status = 'RESOLVED'; new = selected_parts[0]
                    else:
                        status = 'TIED_VALUES'; new = ' | '.join(selected_parts)
                    details = '; '.join(f"{r.get(kind,'')} [DTYPE={r['DTYPE'] or '(blank)'}, ASEQ={r.get('ASEQ','')}]" for r in winners[:20])
            slim.at[rowidx,col] = new
            stats[status] += 1
            qc.append({'ROW_NUMBER':int(rowidx)+2,'USUBJID':key[0],'AVISIT':key[1],'AVISITN':key[2],
                       'ARD_COLUMN':col,'ORIGINAL_VALUE':old,'OUTPUT_VALUE':new,'STATUS':status,
                       'LOAD':chosen_load,'DTYPE_RANK':rank,'SELECTED_RECORDS':details})
    slim.to_csv(OUTPUT_FILE,index=False,encoding='utf-8-sig')
    pd.DataFrame(qc,columns=['ROW_NUMBER','USUBJID','AVISIT','AVISITN','ARD_COLUMN','ORIGINAL_VALUE','OUTPUT_VALUE','STATUS','LOAD','DTYPE_RANK','SELECTED_RECORDS']).to_csv(OUTPUT_FOLDER/'anthem_dtype_selection_qc.csv',index=False,encoding='utf-8-sig')
    pd.DataFrame({'MISSING_COLUMN':missing}).to_csv(OUTPUT_FOLDER/'anthem_missing_columns.csv',index=False)
    print(f'Original: {df.shape} | Slim: {slim.shape} | Missing requested columns: {len(missing)}')
    print('Conflict outcomes:',dict(stats))
    print('Output:',OUTPUT_FILE)
    print('QC:',OUTPUT_FOLDER/'anthem_dtype_selection_qc.csv')

if __name__ == '__main__': main()
