import pandas as pd
import numpy as np
from pygam import LinearGAM,s, f, ExpectileGAM


class VolumeStatsProcessor:
    def __init__(self, control_stats_files):
        self.summary_stats_files = control_stats_files
        self.dataframes = [pd.read_csv(file) for file in control_stats_files]
        self.controls = pd.concat(self.dataframes, ignore_index=True)
        self.all_rois = self.controls.columns[2:-3]
        self.test_subs = None

    def load_test_subjects(self, test_stats_files):
        """
        Load the test subjects' data from a list of CSV files"""
        self.test_dataframes = [pd.read_csv(file) for file in test_stats_files]
        self.test_subs = pd.concat(self.test_dataframes, ignore_index=True)
        return
    
    def z_scores_for_all_rois(self):
        """
        Calculate z-scores for all regions of interest (ROIs) using controls and test subjects data.
        """
        self.z_scores = {}
        for roi in self.all_rois:
            self.z_scores[roi] = self.z_scores_for_roi(roi)
        return self.z_scores

    def z_scores_for_roi(self, roi='left hippocampus'):
        """
        Calculate z-scores for a given region of interest (ROI) using controls and test subjects data.
        
        Parameters:
        - controls: DataFrame containing control subjects' data with columns for 'Age', 'Sex', and the ROI.
        - test_subs: DataFrame containing test subjects' data with columns for 'Age', 'Sex', and the ROI.
        - roi: String specifying the region of interest (default is 'left hippocampus').
        
        Returns:
        - z_scores: Array of z-scores for the test subjects.
        """
        
        # Step 1: Prepare the input features for the controls
        X_controls = np.zeros((len(self.controls), 2))
        X_controls[:, 0] = np.log(self.controls['Age'])
        
        
        X_controls[:, 1] = (self.controls['Sex'] == 'M').astype(int)
        
        # Step 2: Extract the ROI values for the controls
        y_controls = self.controls[roi].values
        
        # Step 3: Fit the GAM model on the control data (Location model)
        location_model = LinearGAM(s(0) + f(1)).gridsearch(X_controls, y_controls)
        
        # Predict mean for each observation in controls
        y_pred_mean = location_model.predict(X_controls)
        
        # Step 4: Estimate residuals and fit a scale model
        residuals = y_controls - y_pred_mean
        scale_model = LinearGAM(s(0) + f(1)).gridsearch(X_controls, np.abs(residuals))  # Fit to absolute residuals as a proxy for scale
        
        # Predict scale (variance proxy) for each observation
        y_pred_scale = scale_model.predict(X_controls)
        #control_z_scores = (y_controls-y_pred_mean) / y_pred_scale
        
        # Step 5: Prepare the input features for the test subjects
        X_test = np.zeros((len(self.test_subs), 2))
        X_test[:, 0] = np.log(self.test_subs['Age'])
        X_test[:, 1] = (np.logical_or(self.test_subs['Sex'] == 'M', self.test_subs['Sex'] == 'm')).astype(int)

        # Step 6: Predict the mean for the test subjects
        y_pred_test_mean = location_model.predict(X_test)

        # Predict scale for the test subjects
        y_pred_test_scale = scale_model.predict(X_test)

        # Step 7: Compute the residuals for the test subjects
        y_test = self.test_subs[roi].values
        residuals_test = y_test - y_pred_test_mean
        
        # Step 8: Calculate the z-scores (quantitative scores)
        z_scores = residuals_test / y_pred_test_scale  # Using scale predictions for z-scores
        
        return z_scores
    
    


