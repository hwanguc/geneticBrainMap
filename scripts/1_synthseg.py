import os
import glob
import subprocess

def run_synthseg_t1(file, base_output=None):
    if base_output is None:
        base_output = file.split('.nii')[0]
    output_file = base_output + '.nii.gz'
    volume_file = base_output + '_vol.csv'
    qc_file = base_output + '_qc.csv'
    
    if not os.path.isfile(output_file):
        # Set up FreeSurfer environment
        freesurfer_home = "/home/meldstudent/Programmes/FreeSurfer/freesurfer"
        os.environ['FREESURFER_HOME'] = freesurfer_home
        subprocess.check_call(f"source {freesurfer_home}/FreeSurferEnv.sh", shell=True, executable='/bin/bash')
        
        # Run SynthSeg
        command = f"mri_synthseg --i '{file}' --o '{output_file}' --parc --vol '{volume_file}' --qc '{qc_file}' --robust"
        proc = subprocess.Popen(command, shell=True)
        proc.wait()


# Set folder path
folder = '/home/meldstudent/Documents/RDS_GrowthCharts/bids_data/KANSL1'
derivatives_folder = os.path.join(folder, 'derivatives', 'synthseg')

# Ensure the derivatives folder exists
os.makedirs(derivatives_folder, exist_ok=True)

# Get the list of subjects using glob
subject_dirs = glob.glob(os.path.join(folder, 'sub-*'))
#just run on sub-11
for subject in subject_dirs:
    subject_id = os.path.basename(subject)
    anat_folder = os.path.join(subject, 'anat')
    print(anat_folder)
    if os.path.isdir(anat_folder):
        nifti_files = glob.glob(os.path.join(anat_folder, '*.nii.gz'))
        print(nifti_files)
        for nifti_file in nifti_files:
            # Create the output folder for the subject in the derivatives folder
            subject_derivatives_folder = os.path.join(derivatives_folder, subject_id, 'anat')
            os.makedirs(subject_derivatives_folder, exist_ok=True)
            # Define the base output path in the derivatives folder
            base_output = os.path.join(subject_derivatives_folder, os.path.basename(nifti_file).split('.nii')[0])
            run_synthseg_t1(nifti_file, base_output)