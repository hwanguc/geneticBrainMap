#!/usr/bin/env python3
import os
import re
import glob
import pandas as pd

# --- Paths (edit if needed) ---
BASE = "/home/hanwang/Documents/gos_ich/cre_project/Data/data_proc/grin2aproj"
VOLUMES_GLOB = os.path.join(BASE, "synthseg3/output", "sub-IXI*", "anat", "*_vol.csv")
DEMO_CSV = os.path.join(BASE, "demographics-IXI/IXI.csv")
RAW_IXI_DIR = os.path.join(BASE, "raw_IXI")  # contains filenames like IXI002-Guys-0828-T1.nii.gz
OUT_DIR = os.path.join(BASE, "synthseg3/combined")
OUT_CSV = os.path.join(OUT_DIR, "IXI_synthseg_volumes_with_demo.csv")

os.makedirs(OUT_DIR, exist_ok=True)

# --- Load demographics ---
df_demo = pd.read_csv(DEMO_CSV)

if "IXI_ID" not in df_demo.columns:
    raise RuntimeError("IXI.csv must contain column 'IXI_ID'.")

# Normalize types
df_demo["IXI_ID"] = pd.to_numeric(df_demo["IXI_ID"], errors="coerce").astype("Int64")

keep_cols = [c for c in ["IXI_ID", "AGE", "Sex"] if c in df_demo.columns]
df_demo = df_demo[keep_cols].copy()

# --- Regex helpers ---
# subject in volumes csv: 'sub-IXI132_T1w.nii' -> 132
subj_re = re.compile(r"sub-IXI(\d{3})_T1w\.nii$", re.IGNORECASE)

def parse_ixi_num(subject_str: str):
    m = subj_re.search(subject_str.strip())
    return int(m.group(1)) if m else None

# raw IXI filename: 'IXI002-Guys-0828-T1.nii.gz' -> (2, 'Guys')
raw_re = re.compile(r"^IXI(\d{3})-([A-Za-z]+)-.*-T1\.nii\.gz$", re.IGNORECASE)

# --- Build IXI_ID -> scanner map from raw filenames ---
scanner_map = {}
raw_files = sorted(glob.glob(os.path.join(RAW_IXI_DIR, "IXI*-T1.nii.gz")))
for rf in raw_files:
    base = os.path.basename(rf)
    mm = raw_re.match(base)
    if not mm:
        continue
    num = int(mm.group(1))
    scanner = mm.group(2)
    # If duplicates exist, keep the first seen and warn only if conflicting
    if num in scanner_map and scanner_map[num] != scanner:
        print(f"[WARN] Conflicting scanner entries for IXI{num:03d}: "
              f"{scanner_map[num]} vs {scanner}. Keeping first.")
        continue
    scanner_map[num] = scanner

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
        df = df.iloc[[0]].copy()

    subj = str(df.loc[df.index[0], "subject"])
    ixi_num = parse_ixi_num(subj)
    if ixi_num is None:
        print(f"[WARN] Could not parse IXI ID from subject '{subj}' in file: {f}")
        continue

    # Insert IXI_ID
    df.insert(1, "IXI_ID", ixi_num)

    # Demographics lookup
    demo_row = df_demo.loc[df_demo["IXI_ID"] == ixi_num]
    if demo_row.empty or demo_row["AGE"].isna().any() or demo_row["Sex"].isna().any():
        age_val = "missing"
        sex_val = "missing"
    else:
        age_val = demo_row["AGE"].iloc[0]
        sex_val = demo_row["Sex"].iloc[0]

    # Scanner lookup
    scanner_val = scanner_map.get(ixi_num, "missing")

    # Add Age/Sex/Scanner columns
    df.insert(2, "Age", age_val)
    df.insert(3, "Sex", sex_val)
    df.insert(4, "scanner", scanner_val)

    rows.append(df)

# Concatenate
combined = pd.concat(rows, ignore_index=True)

# Reorder: subject, IXI_ID, Age, Sex, scanner, (metrics…)
front = ["subject", "IXI_ID", "Age", "Sex", "scanner"]
rest = [c for c in combined.columns if c not in front]
combined = combined[front + rest]

# Save
combined.to_csv(OUT_CSV, index=False)
print(f"[OK] Wrote: {OUT_CSV}")
print(f"[INFO] N participants (rows): {len(combined)}")
print(f"[INFO] N with missing demo: {(combined['Age'] == 'missing').sum()}")
print(f"[INFO] N with missing scanner: {(combined['scanner'] == 'missing').sum()}")
