# Model Complexity housing example
# Use: Calculate Shapley values for housing example
# Author: Mark Verhagen

# Load libraries
import numpy as np
import pandas as pd
import os
import pickle
import shap
from Data import Data
from Predict import Predict
from shap import plots as shp
from sklearn.ensemble import GradientBoostingRegressor
import statsmodels.api as sm

%load_ext autoreload
%autoreload 2

root = os.path.abspath('..')

pred_travel_crime_imd_temp = Predict(regions=['London'], RF=False, GB=True, hot_load_models=True,
                                     model_append=['SL_numtemp_sl_'],
                                     gb_model=GradientBoostingRegressor(
                                         n_estimators=1500, max_depth=7, learning_rate=0.3),
                                     features=['tfarea', 'numberrooms', 'propertytype', 'oldnew',
                                               'BUILT_FORM', 'TRANSACTION_TYPE', 'travel_to_centre',
                                               'crime_disorder', 'imd', 'year', 'month'],
                                     merge_lsoa=True)

explainer_gb = shap.Explainer(pred_travel_crime_imd_temp.gb)
explainer_gb.expected_value = explainer_gb.expected_value[0]
e = explainer_gb(pred_travel_crime_imd_temp.X_test)

with open(os.path.join(data_dir, 'housing_SL_shap_values_temp_kernel.pkl'), 'wb') as f:
    pickle.dump(e, f)

with open(os.path.join(data_dir, 'housing_SL_shap_values_temp_kernel.pkl'), 'rb') as f:
    e = pickle.load(f)

pd.DataFrame({'tfarea': e[:, "tfarea"].data,
              'imd': e[:, "imd"].data,
              'crime_disorder': e[:, "crime_disorder"].data,
              'travel_to_centre': e[:, "travel_to_centre"].data,
              'numberrooms': e[:, "numberrooms"].data,
              'month': e[:, "month"].data,
              'year': e[:, "year"].data,
              'tfarea_shap': e[:, "tfarea"].values,
              'imd_shap': e[:, "imd"].values,
              'crime_disorder_shap': e[:, "crime_disorder"].values,
              'travel_to_centre_shap': e[:, "travel_to_centre"].values,
              'numberrooms_shap': e[:, "numberrooms"].values,
              'month_shap': e[:, "month"].values,
              'year_shap': e[:, "year"].values, }).\
    to_csv(os.path.join(root, 'data', 'edit', 'housing_shap_temp.csv'))
