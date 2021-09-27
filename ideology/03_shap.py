# Model Complexity ideology example
# Use: Calculate Shapley values and interaction values
# Author: Mark Verhagen

# Load libraries
import numpy as np
import pandas as pd
import os
import pickle
import shap
from shap import plots as shp
import statsmodels.api as sm
import pickle
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor

%load_ext autoreload
%autoreload 2

data_dir = os.path.join(os.path.abspath(''), 'data', 'edit')

X_train = pd.read_csv(
    'ideology/data/edit/x_train.csv').drop(columns=['Unnamed: 0'])
X_test = pd.read_csv(
    'ideology/data/edit/x_test.csv').drop(columns=['Unnamed: 0'])
y_train = pd.read_csv(
    'ideology/data/edit/y_train.csv').drop(columns=['Unnamed: 0'])
y_test = pd.read_csv(
    'ideology/data/edit/y_test.csv').drop(columns=['Unnamed: 0'])

gb = GradientBoostingRegressor(
    n_estimators=75, max_depth=3, learning_rate=0.15)
gb.fit(X_train.to_numpy(), y_train[['x']].values.ravel())

explainer_gb = shap.Explainer(gb)
explainer_gb.expected_value = explainer_gb.expected_value[0]

shap_gb_values = explainer_gb(X_test)

with open(os.path.join(data_dir, 'shap_values_gb_year.pkl'), 'wb') as f:
    pickle.dump(shap_gb_values, f)

with open(os.path.join(data_dir, 'shap_values_gb_year.pkl'), 'rb') as f:
    shap_gb_values = pickle.load(f)


pd.DataFrame({'race': shap_gb_values[:, "race"].data,
              'age': shap_gb_values[:, "age"].data,
              'educ': shap_gb_values[:, "educ"].data,
              'year': shap_gb_values[:, "year"].data,
              'income': shap_gb_values[:, "income"].data,
              'sex': shap_gb_values[:, "sex"].data,
              'race_shap': shap_gb_values[:, "race"].values,
              'age_shap': shap_gb_values[:, "age"].values,
              'educ_shap': shap_gb_values[:, "educ"].values,
              'year_shap': shap_gb_values[:, "year"].values,
              'income_shap': shap_gb_values[:, "income"].values,
              'sex_shap': shap_gb_values[:, "sex"].values}).\
    to_csv(os.path.join(data_dir, 'gss_shap_gb.csv'))

shap_interaction_values = shap.TreeExplainer(gb).\
    shap_interaction_values(X_test)

with open(os.path.join(data_dir, 'gss_shap_values_year_tree_int.pkl'), 'wb') as f:
    pickle.dump(shap_interaction_values, f)

with open(os.path.join(data_dir, 'gss_shap_values_year_tree_int.pkl'), 'rb') as f:
    shap_interaction_values = pickle.load(f)

pd.DataFrame({'age_age_shap': shap_interaction_values[:, 0][:, 0],
              'age_educ_shap': shap_interaction_values[:, 0][:, 1],
              'age_income_shap': shap_interaction_values[:, 0][:, 2],
              'age_sex_shap': shap_interaction_values[:, 0][:, 3],
              'age_race_shap': shap_interaction_values[:, 0][:, 4],
              'age_year_shap': shap_interaction_values[:, 0][:, 5],
              'educ_age_shap': shap_interaction_values[:, 1][:, 0],
              'educ_educ_shap': shap_interaction_values[:, 1][:, 1],
              'educ_income_shap': shap_interaction_values[:, 1][:, 2],
              'educ_sex_shap': shap_interaction_values[:, 1][:, 3],
              'educ_race_shap': shap_interaction_values[:, 1][:, 4],
              'educ_year_shap': shap_interaction_values[:, 1][:, 5],
              'income_age_shap': shap_interaction_values[:, 2][:, 0],
              'income_educ_shap': shap_interaction_values[:, 2][:, 1],
              'income_income_shap': shap_interaction_values[:, 2][:, 2],
              'income_sex_shap': shap_interaction_values[:, 2][:, 3],
              'income_race_shap': shap_interaction_values[:, 2][:, 4],
              'income_year_shap': shap_interaction_values[:, 2][:, 5],
              'sex_age_shap': shap_interaction_values[:, 3][:, 0],
              'sex_educ_shap': shap_interaction_values[:, 3][:, 1],
              'sex_income_shap': shap_interaction_values[:, 3][:, 2],
              'sex_sex_shap': shap_interaction_values[:, 3][:, 3],
              'sex_race_shap': shap_interaction_values[:, 3][:, 4],
              'sex_year_shap': shap_interaction_values[:, 3][:, 5],
              'race_age_shap': shap_interaction_values[:, 4][:, 0],
              'race_educ_shap': shap_interaction_values[:, 4][:, 1],
              'race_income_shap': shap_interaction_values[:, 4][:, 2],
              'race_sex_shap': shap_interaction_values[:, 4][:, 3],
              'race_race_shap': shap_interaction_values[:, 4][:, 4],
              'race_year_shap': shap_interaction_values[:, 4][:, 5],
              'year_age_shap': shap_interaction_values[:, 5][:, 0],
              'year_educ_shap': shap_interaction_values[:, 5][:, 1],
              'year_income_shap': shap_interaction_values[:, 5][:, 2],
              'year_sex_shap': shap_interaction_values[:, 5][:, 3],
              'year_race_shap': shap_interaction_values[:, 5][:, 4],
              'year_year_shap': shap_interaction_values[:, 5][:, 5],
              'age': X_test['age'],
              'educ': X_test['educ'],
              'income': X_test['income'],
              'sex': X_test['sex'],
              'race': X_test['race'],
              'year': X_test['year']}).\
    to_csv(os.path.join('data', 'edit', 'int_shap_values.csv'))
