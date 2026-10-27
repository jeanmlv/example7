# example7

from pathlib import Path
import pandas as pd


# ============================================================
# CONFIGURATION
# ============================================================

input_folder = Path(
    r"C:\Users\JMende95\OneDrive - JNJ\Desktop\ard_data\csv"
)

input_file = (
    input_folder
    / "77242113UCO2001_anthem_wk78_ard_merged_20260818.csv"
)

output_folder = input_folder / "slim"
output_folder.mkdir(exist_ok=True)

output_file = (
    output_folder
    / "77242113UCO2001_anthem_wk78_ard_merged_slim_20261027.csv"
)

# ============================================================
# COLUMNS TO KEEP
# ============================================================

keys = [
    "USUBJID",
    "AVISIT",
    "AVISITN"
]

variables_of_interest = [
"ADMAYO_PARAMCD_ASTOOL_AVAL",
"ADMAYO_PARAMCD_ASTOOLAL_AVAL",
"ADMAYO_PARAMCD_DRBLEED_AVAL",
"ADMAYO_PARAMCD_DSTOOL_AVAL",
"ADMAYO_PARAMCD_ENASCOR_AVAL",
"ADMAYO_PARAMCD_ENCSCOR_AVAL",
"ADMAYO_PARAMCD_ENFSCOR_AVAL",
"ADMAYO_PARAMCD_ENFSCORI_AVAL",
"ADMAYO_PARAMCD_ENLSCOR_AVAL",
"ADMAYO_PARAMCD_FMAYO_AVAL",
"ADMAYO_PARAMCD_FMAYOAL_AVAL",
"ADMAYO_PARAMCD_MFMAYO_AVAL",
"ADMAYO_PARAMCD_MFMAYOAL_AVAL",
"ADMAYO_PARAMCD_MMAYO_AVAL",
"ADMAYO_PARAMCD_MMAYOAL_AVAL",
"ADMAYO_PARAMCD_MMMAYO_AVAL",
"ADMAYO_PARAMCD_MMMAYOAL_AVAL",
"ADMAYO_PARAMCD_MPMAYO_AVAL",
"ADMAYO_PARAMCD_MPMAYOAL_AVAL",
"ADMAYO_PARAMCD_NFMAYO_AVAL",
"ADMAYO_PARAMCD_NFMAYOAL_AVAL",
"ADMAYO_PARAMCD_NMMAYO_AVAL",
"ADMAYO_PARAMCD_NMMAYOAL_AVAL",
"ADMAYO_PARAMCD_NPMAYO_AVAL",
"ADMAYO_PARAMCD_NPMAYOAL_AVAL",
"ADMAYO_PARAMCD_PGSCOR_AVAL",
"ADMAYO_PARAMCD_PMAYO_AVAL",
"ADMAYO_PARAMCD_PMAYOAL_AVAL",
"ADMAYO_PARAMCD_RBSCOR_AVAL",
"ADMAYO_PARAMCD_RBSCORAL_AVAL",
"ADMAYO_PARAMCD_RBSCORI_AVAL",
"ADMAYO_PARAMCD_SFSCOR_AVAL",
"ADMAYO_PARAMCD_SFSCORAL_AVAL",
"ADMAYO_PARAMCD_SFSCORI_AVAL",
"ADMAYO_PARAMCD_CREM_AVALC",
"ADMAYO_PARAMCD_CREMA1_AVALC",
"ADMAYO_PARAMCD_CREMA2_AVALC",
"ADMAYO_PARAMCD_CREMAL_AVALC",
"ADMAYO_PARAMCD_CRESP_AVALC",
"ADMAYO_PARAMCD_CRESPAL_AVALC",
"ADMAYO_PARAMCD_CS1CREM_AVALC",
"ADMAYO_PARAMCD_CSCREM_AVALC",
"ADMAYO_PARAMCD_ENIMP_AVALC",
"ADMAYO_PARAMCD_ENREM_AVALC",
"ADMAYO_PARAMCD_RBSCOR0_AVALC",
"ADMAYO_PARAMCD_SFSCOR0_AVALC",
"ADMAYO_PARAMCD_SFSCOR01_AVALC",
"ADMAYO_PARAMCD_SREM_AVALC",
"ADMAYO_PARAMCD_SREMA1_AVALC",
"ADMAYO_PARAMCD_SREMA2_AVALC",
"ADMAYO_PARAMCD_SREMAL_AVALC",
"ADIBDQ_PARAMCD_IBDQB_AVAL",
"ADIBDQ_PARAMCD_IBDQE_AVAL",
"ADIBDQ_PARAMCD_IBDQSF_AVAL",
"ADIBDQ_PARAMCD_IBDQSY_AVAL",
"ADIBDQ_PARAMCD_IBDQTOT_AVAL",
"ADIBDQ_PARAMCD_IBDQREM_AVALC",
"ADLBI_PARAMCD_CALPRO_AVAL",
"ADLBI_PARAMCD_CALPROP_AVAL",
"ADLBI_PARAMCD_CRP_AVAL",
"ADLBI_PARAMCD_CRPP_AVAL",
"ADLBI_PARAMCD_LCALPRO_AVAL",
"ADLBI_PARAMCD_LCALPROP_AVAL",
"ADLBI_PARAMCD_LCRP_AVAL",
"ADLBI_PARAMCD_LCRPP_AVAL",
"ADLBI_PARAMCD_CRPNRM_AVALC",
"ADLBI_PARAMCD_CRPNRMP_AVALC",
"ADSL_AGE",
"ADSL_AGEU",
"ADSL_SEX",
"ADSL_RACE",
"ADSL_ETHNIC",
"ADSL_COUNTRY",
"ADSL_TRT02P",
"ADSL_TRTSEQP"
]

columns_to_keep = keys + variables_of_interest

# ============================================================
# VALIDATE INPUT FILE
# ============================================================

if not input_file.exists():
    raise FileNotFoundError(
        f"CSV file not found:\n{input_file}"
    )

print(f"Processing: {input_file.name}")


# ============================================================
# READ CSV
# ============================================================

df = pd.read_csv(
    input_file,
    low_memory=False
)

print(f"Original rows: {len(df):,}")
print(f"Original columns: {len(df.columns):,}")


# ============================================================
# CHECK COLUMNS
# ============================================================

available_columns = [
    col for col in columns_to_keep
    if col in df.columns
]

missing_columns = [
    col for col in columns_to_keep
    if col not in df.columns
]


print("\nAvailable requested columns:")
for col in available_columns:
    print(f"  ✔ {col}")


if missing_columns:
    print("\nRequested columns NOT FOUND:")
    
    for col in missing_columns:
        print(f"  ✘ {col}")


# ============================================================
# CREATE SLIM ARD
# ============================================================

df_slim = df[available_columns].copy()


# ============================================================
# EXPORT
# ============================================================

df_slim.to_csv(
    output_file,
    index=False,
    encoding="utf-8-sig"
)


# ============================================================
# SUMMARY
# ============================================================

print("\n========================================")
print("SLIM ARD CREATED SUCCESSFULLY")
print("========================================")

print(f"Rows: {len(df_slim):,}")
print(f"Columns: {len(df_slim.columns)}")

print(f"\nOutput file:\n{output_file}")

if missing_columns:
    print(
        f"\nWarning: {len(missing_columns)} "
        "requested variable(s) were not found."
    )
