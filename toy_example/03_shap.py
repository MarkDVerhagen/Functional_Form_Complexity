### Model Complexity Toy Example
## Use: Calculate shap values for Toy Example
## Author: Mark Verhagen

## Load libraries
import pandas as pd
import os
import shap
from shap import plots as shp
import pickle
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.model_selection import train_test_split

data_dir = os.path.join(os.path.abspath(''), 'data', 'edit')

df = pd.read_csv(os.path.join(data_dir, 'toy_df.csv'))

X_train, X_test, y_train, y_test = train_test_split(df[['age', 'sex', 'med_use', 'assist']],
                                                    df[['fall']], test_size = 0.2,
                                                    random_state=1704)

gb = GradientBoostingRegressor()    ## Defaults are already amongst top performing algorithms in SL
gb.fit(X_train.to_numpy(), y_train[['fall']].values.ravel())

explainer_gb = shap.Explainer(gb)
explainer_gb.expected_value = explainer_gb.expected_value[0]

shap_gb_values = explainer_gb(X_test)

with open(os.path.join(data_dir, 'shap_values_toy.pkl'), 'wb') as f:
    pickle.dump(shap_gb_values, f)

with open(os.path.join(data_dir, 'shap_values_toy.pkl'), 'rb') as f:
    shap_gb_values = pickle.load(f)


pd.DataFrame({'age': shap_gb_values[:, "age"].data,
              'med_use': shap_gb_values[:, "med_use"].data,
              'sex': shap_gb_values[:, "sex"].data,
              'assist': shap_gb_values[:, "assist"].data,
              'age_shap': shap_gb_values[:, "age"].values,
              'sex_shap': shap_gb_values[:, "sex"].values,
              'assist_shap': shap_gb_values[:, "assist"].values,
              'med_use_shap': shap_gb_values[:, "med_use"].values}).\
                  to_csv(os.path.join(data_dir, 'toy_shap.csv'))