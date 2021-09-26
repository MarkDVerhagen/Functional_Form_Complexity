import os
from sklearn.ensemble import GradientBoostingRegressor
import numpy as np
import pandas as pd
import shap
import numpy as np
import xgboost
from shap import plots as shp

## Set directory paths
root = os.path.abspath('')
data_folder = os.path.join(root, 'data')

## Read simulated dataset
df = pd.read_csv(os.path.join(data_folder, 'edit', 'analysis_df.csv')).\
    drop(columns = 'Unnamed: 0')

## Generate ols sets
outcomes = ['ln_y_I', 'ln_y_II', 'ln_y_III', 'ln_y_IV']
df_lm_1 = df[outcomes + ['S', 'X']]
df_lm_2 = df[outcomes +  ['S', 'X']].assign(X2 = df['X']^2)
df_lm_3 = df[outcomes +  ['S', 'S_0_8', 'S_9_10', 'S_11_12', 'S_13_14',
                          'S_15p', 'X']]
df_lm_4 = df[outcomes +  ['S_0_8', 'S_9_10', 'S_11_12', 'S_13_14',
              'S_15p', 'X', 'S', 'gender']]

## Estimate models
xgb = GradientBoostingRegressor()  # defaults are optimal according to SL
X1 = df_lm_1[['S', 'X']]
y1 = df_lm_1['ln_y_I']

X2 = df_lm_2[['S', 'X']]
y2 = df_lm_2['ln_y_II']

X3 = df_lm_3[['S', 'X']]
y3 = df_lm_3['ln_y_III']

X4 = df_lm_4[['S', 'X', 'gender']]
y4 = df_lm_4['ln_y_IV']

model1 = xgboost.XGBRegressor().fit(X1, y1)
model2 = xgboost.XGBRegressor().fit(X2, y2)
model3 = xgboost.XGBRegressor().fit(X3, y3)
model4 = xgboost.XGBRegressor().fit(X4, y4)

## Explain the model's predictions using SHAP
## Kernel Shap

explainer1 = shap.Explainer(model1)
shap_values1 = explainer1(X1)

explainer2 = shap.Explainer(model2)
shap_values2 = explainer2(X2)

explainer3 = shap.Explainer(model3)
shap_values3 = explainer3(X3)

explainer4 = shap.Explainer(model4)
shap_values4 = explainer4(X4)

pd.DataFrame({'X1': shap_values1[:, "X"].data,
              'S1': shap_values1[:, "S"].data,
              'sex': shap_values4[:, "gender"].data,
              'X1_shap': shap_values1[:, "X"].values,
              'S1_shap': shap_values1[:, "S"].values,
              'X2': shap_values2[:, "X"].data,
              'S2': shap_values2[:, "S"].data,
              'X2_shap': shap_values2[:, "X"].values,
              'S2_shap': shap_values2[:, "S"].values,
              'X3': shap_values3[:, "X"].data,
              'S3': shap_values3[:, "S"].data,
              'X3_shap': shap_values3[:, "X"].values,
              'S3_shap': shap_values3[:, "S"].values,
              'X4': shap_values4[:, "X"].data,
              'S4': shap_values4[:, "S"].data,
              'X4_shap': shap_values4[:, "X"].values,
              'S4_shap': shap_values4[:, "S"].values}).\
                  to_csv(os.path.join(root, 'data', 'edit', 'mincerian_shap.csv'))
                  