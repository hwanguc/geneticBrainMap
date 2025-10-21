#!/usr/bin/env bash

#Subjs=("114" "119" "121" "122" "123" "130" "131" "132" "133" "1690" "0020" "0437" "0903" "0012" "1098" "1117" "1266" "1527" "1572" "1726" "g001" "g002" "g003" "g004" "g005" "g006" "g008" "g010")
Subjs=("0922")
mapfile -t Subjs < <(for Subj in "${Subjs[@]}"; do echo "sub-$Subj"; done) # substute the subject IDs with the format "sub-SUBJ_ID"

Dir_Origin="/home/hanwang/Documents/gos_ich/cre_project/Data/data_proc/grin2aproj/pre_processing"
Dir_Target="/home/hanwang/Documents/gos_ich/cre_project/Data/data_proc/grin2aproj/synthseg2/raw"

for Subj in "${Subjs[@]}"; do
	echo "Copying T1.nii data for $Subj to the synthseg folder"
    mkdir -p "$Dir_Target/$Subj/anat"
	cp "$Dir_Origin/$Subj/anat/"*.nii "$Dir_Target/$Subj/anat/"
done