#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./0_copy_t1_multi-datasets.sh                # defaults to GRIN2A-Aus
#   ./0_copy_t1_multi-datasets.sh IXI            # run on IXI batch
#   ./0_copy_t1_multi-datasets.sh KdV            # run on KdV batch
#   ./0_copy_t1_multi-datasets.sh GRIN2A-Aus     # run on GRIN2A-Aus batch explicitly
#
# Notes:
# - IXI requires MRtrix3's `mrconvert` on PATH.

BATCH="${1:-GRIN2A-Aus}"

# ---- Targets (common) ----


case "$BATCH" in
  "GRIN2A-Aus")
    echo "[INFO] Running GRIN2A-Aus mode"

    # ---- Source and Target ----
    Dir_Origin="/home/hanwang/Documents/gos_ich/cre_project/Data/data_proc/grin2aproj/pre_processing"
    Dir_Target="/home/hanwang/Documents/gos_ich/cre_project/Data/data_proc/grin2aproj/synthseg3/raw"

    # ---- Subject list (edit as needed) ----
    #Subjs=("114" "119" "121" "122" "123" "130" "131" "132" "133" "1690" "0020" "0437" "0903" "0012" "1098" "1117" "1266" "1527" "1572" "1726" "g001" "g002" "g003" "g004" "g005" "g006" "g008" "g010")
    Subjs=("0922")

    # Prefix with "sub-"
    mapfile -t Subjs < <(for Subj in "${Subjs[@]}"; do echo "sub-$Subj"; done)

    for Subj in "${Subjs[@]}"; do
      echo "[INFO] Copying *.nii for $Subj -> synthseg folder"
      mkdir -p "$Dir_Target/$Subj/anat"
      cp "$Dir_Origin/$Subj/anat/"*.nii "$Dir_Target/$Subj/anat/"
    done
    ;;

  "IXI")
    echo "[INFO] Running IXI mode"

    # ---- Source and Target----
    Dir_IXI_Raw="$HOME/Documents/gos_ich/cre_project/Data/data_proc/grin2aproj/raw_IXI"
    Dir_Target="/home/hanwang/Documents/gos_ich/cre_project/Data/data_proc/grin2aproj/synthseg3/raw"

    # Ensure mrconvert exists
    if ! command -v mrconvert >/dev/null 2>&1; then
      echo "[ERROR] 'mrconvert' not found on PATH. Please install MRtrix3 or add it to PATH." >&2
      exit 1
    fi

    shopt -s nullglob
    ixi_files=("$Dir_IXI_Raw"/IXI*-T1.nii.gz)

    if (( ${#ixi_files[@]} == 0 )); then
      echo "[WARN] No IXI *-T1.nii.gz files found in: $Dir_IXI_Raw"
      exit 0
    fi

    for f in "${ixi_files[@]}"; do
      base=$(basename "$f")
      # Extract 'IXI###' from filenames like 'IXI002-Guys-0828-T1.nii.gz'
      subj_id=$(sed -E 's/^IXI([0-9]{3}).*/IXI\1/' <<< "$base")
      Subj="sub-$subj_id"

      out_dir="$Dir_Target/$Subj/anat"
      out_nii="$out_dir/${Subj}_T1w.nii"

      echo "[INFO] Converting $base -> $out_nii"
      mkdir -p "$out_dir"
      mrconvert "$f" "$out_nii" -force
    done
    ;;
  
  "KdV")
    echo "[INFO] Running KdV mode"

    # ---- Source and Target ----
    Dir_Origin="/home/hanwang/Documents/Data/ucl/gos_ich/kdvproj/pre_processing"
    Dir_Target="/home/hanwang/Documents/Data/ucl/gos_ich/kdvproj/synthseg/raw"

    # ---- Subject list (edit as needed) ----
    #Subjs=("114" "119" "121" "122" "123" "130" "131" "132" "133" "1690" "0020" "0437" "0903" "0012" "1098" "1117" "1266" "1527" "1572" "1726" "g001" "g002" "g003" "g004" "g005" "g006" "g008" "g010")
    Subjs=("113" "114" "115" "116" "117" "121" "123" "126" "127" "128" "k304" "k308" "k309" "k345" "k347" "k373" "k374")

    # Prefix with "sub-"
    mapfile -t Subjs < <(for Subj in "${Subjs[@]}"; do echo "sub-$Subj"; done)

    for Subj in "${Subjs[@]}"; do
      echo "[INFO] Copying *.nii for $Subj -> synthseg folder"
      mkdir -p "$Dir_Target/$Subj/anat"
      cp "$Dir_Origin/$Subj/anat/"*.nii "$Dir_Target/$Subj/anat/"
    done
    ;;

  *)
    echo "[ERROR] Unknown batch: $BATCH"
    echo "Valid options: GRIN2A-Aus | IXI | KdV"
    exit 1
    ;;
esac

echo "[DONE] $BATCH processing complete."
