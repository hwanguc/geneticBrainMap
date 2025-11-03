#!/usr/bin/env python3
import os
import re
import glob
import pandas as pd

# --- Paths (edit if needed) ---
BASE = "/home/hanwang/Documents/gos_ich/cre_project/Data/data_proc/grin2aproj"
VOLUMES_GLOB = os.path.join(BASE, "synthseg3/output", "sub-IXI*", "anat", "*_vol.csv")
DEMO_CSV = os.path.join(BASE, "demographics-IXI/IXI.csv")
OUT_DIR = os.path.join(BASE, "synthseg3/combined")
OUT_CSV = os.path.join(OUT_DIR, "IXI_synthseg_volumes_with_demo.csv")

os.makedirs(OUT_DIR, exist_ok=True)

# --- Load demographics ---
# Expecting columns: IXI_ID, AGE, Sex (AGE and Sex can be missing)
df_demo = pd.read_csv(DEMO_CSV)

# Normalize types
if "IXI_ID" not in df_demo.columns:
    raise RuntimeError("IXI.csv must contain column 'IXI_ID'.")

# Ensure IXI_ID is integer-like (strip spaces, coerce)
df_demo["IXI_ID"] = pd.to_numeric(df_demo["IXI_ID"], errors="coerce").astype("Int64")

# Keep only relevant columns (others will be ignored)
keep_cols = [c for c in ["IXI_ID", "AGE", "Sex"] if c in df_demo.columns]
df_demo = df_demo[keep_cols].copy()

# --- Helper: parse IXI number (e.g., 'sub-IXI132_T1w.nii' -> 132) ---
subj_re = re.compile(r"sub-IXI(\d{3})_T1w\.nii$", re.IGNORECASE)

def parse_ixi_num(subject_str: str):
    m = subj_re.search(subject_str.strip())
    if not m:
        return None
    return int(m.group(1))

# --- Collect & merge volume CSVs ---
rows = []
vol_files = sorted(glob.glob(VOLUMES_GLOB))

if not vol_files:
    raise SystemExit(f"No volume CSVs found with pattern:\n  {VOLUMES_GLOB}")

for f in vol_files:
    df = pd.read_csv(f)
    if "subject" not in df.columns:
        raise RuntimeError(f"File missing 'subject' column: {f}")

    if len(df) != 1:
        # If multiple rows appear, keep the first (typical synthseg vol files have 1 row)
        df = df.iloc[[0]].copy()

    subj = str(df.loc[df.index[0], "subject"])
    ixi_num = parse_ixi_num(subj)

    if ixi_num is None:
        # Skip unexpected filenames but warn via print
        print(f"[WARN] Could not parse IXI ID from subject '{subj}' in file: {f}")
        continue

    # Attach IXI_ID
    df.insert(1, "IXI_ID", ixi_num)

    # Lookup demo for this IXI_ID (exact match)
    demo_row = df_demo.loc[df_demo["IXI_ID"] == ixi_num]

    if demo_row.empty or demo_row["AGE"].isna().any() or demo_row["Sex"].isna().any():
        age_val = "missing"
        sex_val = "missing"
    else:
        age_val = demo_row["AGE"].iloc[0]
        sex_val = demo_row["Sex"].iloc[0]

    # Add Age/Sex columns right after IXI_ID
    df.insert(2, "Age", age_val)
    df.insert(3, "Sex", sex_val)

    rows.append(df)

# Concatenate all
combined = pd.concat(rows, ignore_index=True)

# Reorder columns: subject, IXI_ID, Age, Sex, (then all original metrics)
front = ["subject", "IXI_ID", "Age", "Sex"]
rest = [c for c in combined.columns if c not in front]
combined = combined[front + rest]

# Save
combined.to_csv(OUT_CSV, index=False)
print(f"[OK] Wrote: {OUT_CSV}")
print(f"[INFO] N participants: {len(combined)}")
print(f"[INFO] N with missing demo: {(combined['Age'] == 'missing').sum()}")
