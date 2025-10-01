import os
import glob
import pandas as pd

def extract_volume_estimates(subs, age_sex_tsv, output_csv,subjects_folder_path):
    dataframes = []
    # Load age and sex information from participants.tsv
    age_sex = pd.read_csv(age_sex_tsv, sep='\t')
    # Loop through each subject directory
    for sub in subs:
        # Construct the path to the CSV file for this subject
        # look inside derivatives for the csv file
        subject_id = os.path.basename(sub)
        csvs = glob.glob(os.path.join(subjects_folder_path,'derivatives',
                                      'synthseg',subject_id,'anat','*_vol.csv'))  
        if not csvs:
            print(f"No volume CSV file found for subject: {sub}")
            continue
        if len(csvs) > 1:
            print(f"Multiple volume CSV files found for subject: {sub}")
            # find the csv with t1 in the name
            csvs = [c for c in csvs if 't1' in c or 'T1' in c]
        csv_path = csvs[0]
        # Check if the CSV file exists
        if os.path.exists(csv_path):
            # Load the CSV file into a DataFrame
            try:
                df = pd.read_csv(csv_path)
                # Add columns for subject ID, Age, and Sex
                df['subject'] = subject_id
                df['Age'] = age_sex['age'][age_sex['participant_id'] == subject_id].values[0]
                df['Sex'] = age_sex['sex'][age_sex['participant_id'] == subject_id].values[0]
                df['gene'] = age_sex['gene'][age_sex['participant_id'] == subject_id].values[0]
                # Append the DataFrame to the list
                dataframes.append(df)
            except IndexError:
                print(f"Age or Sex information not found for subject: {sub}")
            except Exception as e:
                print(f"Error processing CSV for subject {sub}: {e}")
        else:
            print(f"CSV file not found for subject: {sub}")
    # Combine all the individual DataFrames into one large DataFrame
    combined_df = pd.concat(dataframes, ignore_index=True)
    combined_df.to_csv(output_csv, index=False)
    print(f"Combined data saved to {output_csv}")

if __name__ == "__main__":
    # Define the paths
    folder_root = '/Volumes/ritd-ag-project-rd01ml-kwags13/'
    folder_root = '/home/meldstudent/Documents/RDS_GrowthCharts/'
    bids_dir = os.path.join(folder_root,'bids_data')  # Update with the actual path to subject directories
    cohort = 'KANSL1'
    subjects_folder_path = os.path.join(bids_dir,cohort)
    subs = glob.glob(os.path.join(subjects_folder_path,'sub-*'))  # Update with the actual path to subject directories
    age_sex_tsv = os.path.join(subjects_folder_path,'participants.tsv')  # Update with the actual path to participants.tsv
    output_dir = os.path.join(folder_root,'pilot_growthcharts','data')

    output_csv = os.path.join(output_dir,f'{cohort}_summary_stats.csv')  # Update with the desired output path

    # Run the extraction
    extract_volume_estimates(subs, age_sex_tsv, output_csv,subjects_folder_path)