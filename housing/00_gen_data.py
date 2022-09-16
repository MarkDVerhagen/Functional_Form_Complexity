# Model Complexity housing example
# Use: Generate data for the housing example in the paper
# Author: Mark Verhagen

# Load libraries
import pandas as pd
import os
import itertools
import numpy as np
import shap
from Data import Data
from Predict import Predict
from shap import plots as shp
from sklearn.ensemble import GradientBoostingRegressor
import statsmodels.api as sm
import pickle

raw_data_dir = os.path.join(os.path.abspath(''), 'data', 'raw')
edit_data_dir = os.path.join(os.path.abspath(''), 'data', 'edit')

data = pd.read_csv(os.path.join(raw_data_dir, 'tranall2011_19.csv'))
data.to_pickle(os.path.join(raw_data_dir, 'tranall2011_19.pkl'))

# Load full set from last Predict() object in scratch.py
pred_obj = Predict(regions=['London'], RF=False, GB=False, hot_load_models=True,
                   features=['tfarea', 'numberrooms', 'propertytype', 'oldnew', 'BUILT_FORM',
                             'TRANSACTION_TYPE', 'travel_to_centre', 'crime_disorder', 'imd'], merge_lsoa=True)

pred_obj.X_train.to_csv(os.path.join(edit_data_dir, "X_train_full.csv"))
pred_obj.X_test.to_csv(os.path.join(edit_data_dir, "X_test_full.csv"))
pred_obj.y_train.to_csv(os.path.join(edit_data_dir, "y_train_full.csv"))
pred_obj.y_test.to_csv(os.path.join(edit_data_dir, "y_test_full.csv"))

# Load full set from last Predict() object in scratch.py
pred_obj_temp = Predict(regions=['London'], RF=False, GB=True, hot_load_models=True,
                        model_append=['SL_numtemp_sl_'],
                        gb_model=GradientBoostingRegressor(
                            n_estimators=1500, max_depth=7, learning_rate=0.3),
                        features=['tfarea', 'numberrooms', 'propertytype', 'oldnew', 'BUILT_FORM',
                                  'TRANSACTION_TYPE', 'travel_to_centre', 'crime_disorder', 'imd', 'year', 'month'], merge_lsoa=True)

pred_obj_temp.X_train.to_csv(os.path.join(edit_data_dir, "X_train_temp.csv"))
pred_obj_temp.X_test.to_csv(os.path.join(edit_data_dir, "X_test_temp.csv"))
pred_obj_temp.y_train.to_csv(os.path.join(edit_data_dir, "y_train_temp.csv"))
pred_obj_temp.y_test.to_csv(os.path.join(edit_data_dir, "y_test_temp.csv"))
