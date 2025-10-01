# convert csv to tsv

import os
import pandas as pd

def convert_csv_to_tsv(csv_file, tsv_file):
    """
    Convert a CSV file to TSV format.
    """
    df = pd.read_csv(csv_file)
    df.to_csv(tsv_file, sep='\t', index=False)
    return

# Example usage
csv_file = '/home/meldstudent/Documents/RDS_GrowthCharts/bids_data/DHDDS/subject_key.csv'
tsv_file = '/home/meldstudent/Documents/RDS_GrowthCharts/bids_data/DHDDS/participants.tsv'
convert_csv_to_tsv(csv_file, tsv_file)