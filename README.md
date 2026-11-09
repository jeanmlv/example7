# example7


from pathlib import Path
import pandas as pd
import re

source = Path(
    "video_inventory_output_stardust/all_video_inventory.csv"
)
target = Path(
    "video_inventory_output_stardust/all_video_inventory_corrected.csv"
)

df = pd.read_csv(
    source,
    dtype=str,
    keep_default_na=False,
    encoding="utf-8-sig"
)

def parse_stardust_path(path):
    parts = str(path).replace("\\", "/").split("/")

    patient = ""
    visit = ""

    for i, part in enumerate(parts):
        match = re.match(r"(?i)^Patient\s+(.+)$", part.strip())

        if match:
            patient = match.group(1).strip()
            if i + 1 < len(parts):
                visit = parts[i + 1].strip()
            break

    return pd.Series([patient, visit])

mask = df["data_type"].eq("processed")

parsed = df.loc[mask, "relative_path"].apply(parse_stardust_path)
parsed.columns = ["patient_id", "visit"]

# Preserve existing values when parsing does not succeed.
for column in ["patient_id", "visit"]:
    values = parsed[column]
    valid = values.ne("")
    df.loc[values.index[valid], column] = values[valid]

# Keep existing site values. Where absent, use a constant
# namespace so that patient IDs remain countable.
missing_site = mask & df["site"].eq("") & df["patient_id"].ne("")
df.loc[missing_site, "site"] = "STARDUST_SITE_UNKNOWN"

df.to_csv(target, index=False, encoding="utf-8-sig")

processed = df.loc[mask].copy()
valid = processed["patient_id"].ne("")

print("Corrected inventory:", target)
print("Processed video items:", len(processed))
print("Unique patient IDs:", processed.loc[valid, "patient_id"].nunique())
print("Missing patient IDs:", (~valid).sum())
print("Visits:", sorted(processed["visit"].unique()))

