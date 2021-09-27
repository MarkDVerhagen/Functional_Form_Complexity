from Data import Data
import itertools
import joblib
import numpy as np
import pandas as pd
import pickle
import re
import statsmodels.api as sm
import sys
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import train_test_split
from sklearn.neural_network import MLPClassifier
from sklearn import svm
from sklearn.preprocessing import OneHotEncoder
import os


class Predict(object):
    """This class makes predictions of house prices"""
    
    def __init__(self, features = ['tfarea', 'numberrooms', 'propertytype', 'oldnew'], LR=True,
                 RF=False, GB=False, test_prop=0.2, regions=[], seed = 1704, outcome = ['ln_y'],
                 hot_load_models=True, save_models=True, model_append = [''], output_geo=True,
                 merge_lsoa=False, gb_model = GradientBoostingRegressor()):
        self.model_dir = os.path.join(os.path.abspath(''), 'models')
        self.data_dir = os.path.join(os.path.abspath(''), 'data')
        self.features = features
        self.LR = LR
        self.RF = RF
        self.GB = GB
        self.gb_model = gb_model
        self.test_prop = test_prop
        self.regions = regions
        self.seed = seed
        self.outcome = outcome
        self.hot_load_models = hot_load_models
        self.save_models = save_models
        self.merge_lsoa = merge_lsoa
        self.model_append = model_append
        self.feature_acronyms = [i[0:3] for i in self.features]
        
        if self.model_append == ['']:
            self.model_append = '_'.join(self.regions + self.feature_acronyms)
        else:
            self.model_append = '_'.join(self.regions + self.feature_acronyms + self.model_append)
        
        self.output_geo = output_geo
        self.data = Data(regions=self.regions, merge_lsoa = self.merge_lsoa).data
        self.generate_outcome()
        self.generate_features()
        self.train_test_split()
        self.estimate_model(LR = self.LR, RF = self.RF, GB = self.GB)
        self.oos_r2()
        if self.output_geo:
            self.output_geo_df()
    
    def train_test_split(self):
        self.X_train, self.X_test, self.y_train, self.y_test =\
            train_test_split(self.data[self.features], self.data['outcome'],
                             test_size = self.test_prop, random_state = self.seed)
        
        print("Training set dimensions: {}".format(self.X_train.shape))
    
    def generate_outcome(self):
        self.data['y'] = self.data['price']
        self.data['ln_y'] = self.data['price'].apply(np.log)
        self.data['rel_y'] = self.data['priceper']
        self.data['outcome'] = self.data[self.outcome]
        
    def generate_features(self):
        """ Generate features to include into the predictions"""
        
        # identify categorical versus continuous features
        self.cat_features =\
            list(itertools.compress(self.features, [i == 'object' for i in self.data[self.features].dtypes]))
        self.other_features=\
            list(itertools.compress(self.features, [i != 'object' for i in self.data[self.features].dtypes]))

        print("Categorical features identified: {}".format(self.cat_features))
        print("Continous features identified: {}".format(self.other_features))

        # one-hot encode all categorical observations
        enc = OneHotEncoder(handle_unknown='ignore')
        enc.fit(self.data[self.cat_features])
        self.data[enc.get_feature_names(self.cat_features)] = enc.\
            transform(self.data[self.cat_features]).toarray()

        # new features
        self.features = list(itertools.chain(*[self.other_features,
                        list(enc.get_feature_names(self.cat_features))]))
    
    def estimate_model(self, LR, RF, GB):
        if LR:
            self.lr()
        else:
            self.lr_predictions = np.nan
        if RF:
            self.rf()
        else:
            self.rf_predictions = np.nan
        if GB:
            self.gb(gb_model = self.gb_model)
        else:
            self.gb_predictions = np.nan
    
    def output_geo_df(self):
        assert pd.Series(self.X_test.index).isin(pd.Series(self.data.index)).mean() == 1
        assert pd.Series(self.y_test.index).isin(pd.Series(self.data.index)).mean() == 1
        geo_output = pd.DataFrame({'true': self.y_test.values,
                           'lr_pred': self.lr_predictions,
                           'rf_pred': self.rf_predictions,
                           'gb_pred': self.gb_predictions,
                           },
                          index = self.y_test.index)
        geo_df  = self.data[['lsoa11', 'msoa11', 'laua', 'lad11nm', 'gor', 'rgn11nm']]
        full_geo = pd.merge(geo_output, geo_df, left_index=True, right_index=True)
        filename = 'geo_output_' + '_'.join(self.regions) + '.csv'
        
        print("Writing " + filename)
        full_geo.to_csv(os.path.join(self.data_dir, 'edit', filename))
    
    def oos_r2(self):
        TSS = np.square(self.y_test - self.y_test.mean()).sum()
        ESS_lr = np.square(self.y_test - self.lr_predictions).sum()
        ESS_rf = np.square(self.y_test - self.rf_predictions).sum()
        ESS_gb = np.square(self.y_test - self.gb_predictions).sum()
        self.LR_oos_r2 = (TSS - ESS_lr)/TSS
        self.RF_oos_r2 = (TSS - ESS_rf)/TSS
        self.GB_oos_r2 = (TSS - ESS_gb)/TSS
    
    def lr(self, predict_linreg=True, verbose=True):
        """Run a standard OLS"""

        model_path = os.path.join(self.model_dir, 'LR' + self.model_append + '.sav')

        # setup model, either hot load or estimate directly
        if self.hot_load_models:
            print("Hotloading model")
            try:
                self.reg = pickle.load(open(model_path, 'rb'))
            except FileNotFoundError:
                print("Could not find saved model for hot loading")
                sys.exit(1)
        else:
            self.reg = LinearRegression()
            self.reg.fit(self.X_train, self.y_train)  # train
            if self.save_models:
                print("Saving LR model {}".format(model_path))
                pickle.dump(self.reg, open(model_path, 'wb'))

        self.lr_coeff = self.reg.coef_
        self.lr_predictions = self.reg.predict(self.X_test)
        self.lr_rmse = mean_squared_error(self.lr_predictions, self.y_test)
        if verbose:
            print('LR RMSE: {:3f}'.format(self.lr_rmse))

    def rf(self, rf=RandomForestRegressor(n_estimators=1000),
            verbose=True):
        """ Estimate Random Forest model on the pre-specified feature space,
        option to perform cross validation on pre-defined paramter grid
        """
        self.rf = rf
        # setup models
        model_path = os.path.join(self.model_dir, 'RF' + self.model_append + '.sav')

        # setup model, either hot load or estimate directly
        if self.hot_load_models:
            print("Hotloading model")
            try:
                self.rf = pickle.load(open(model_path, 'rb'))
            except FileNotFoundError:
                print("Could not find saved model for hot loading")
                sys.exit(1)
        else:
            self.rf.fit(self.X_train.to_numpy(), self.y_train.ravel())
            if self.save_models:
                print("Saving RF model {}".format(model_path))
                pickle.dump(self.rf, open(model_path, 'wb'))

        # estimate RF model on train set and evaluate performance on test set
        

        self.rf_predictions = self.rf.predict(self.X_test)
        self.rf_rmse = mean_squared_error(self.rf_predictions, self.y_test)

        if verbose:
            print('RF RMSE: {}'.format(self.rf_rmse))

    def gb(self, verbose=True, gb_model = GradientBoostingRegressor()):
        """ Estimate Gradien Boosting model to pre-specified feature space,
        option to perform cross validation on pre-defined paramter grid
        """

        # estimate GB model on the train set and evaluate predictive accuracy
        
        # setup models
        model_path = os.path.join(self.model_dir, 'GB' + self.model_append + '.sav')

        self.gb = gb_model
        
        # setup model, either hot load or estimate directly
        if self.hot_load_models:
            print("Hotloading model")
            try:
                self.gb = pickle.load(open(model_path, 'rb'))
            except FileNotFoundError:
                print("Could not find saved model for hot loading")
                sys.exit(1)
        else:
            self.gb.fit(self.X_train.to_numpy(), self.y_train.ravel())
            if self.save_models:
                print("Saving GB model {}".format(model_path))
                pickle.dump(self.gb, open(model_path, 'wb'))

        self.gb_predictions = self.gb.predict(self.X_test)
        self.gb_rmse = mean_squared_error(self.gb_predictions, self.y_test)

        if verbose:
            print('GB RMSE: {}'.format(self.gb_rmse))
            