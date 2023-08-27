#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May  9 23:43:26 2023

@author: hakan
"""

# explicitly require this experimental feature
from sklearn.experimental import enable_halving_search_cv # noqa
# now you can import normally from model_selection
from sklearn.model_selection import HalvingGridSearchCV
from sklearn.model_selection import KFold
from sklearn.decomposition import PCA
from sklearn.model_selection import GridSearchCV
#from skopt import BayesSearchCV
#from skopt.space import Real, Categorical, Integer
from sklearn.linear_model import LinearRegression
from sklearn.tree import DecisionTreeRegressor
from sklearn.neighbors import KNeighborsRegressor
from sklearn.svm import SVR
from sklearn.ensemble import RandomForestRegressor
from sklearn.neural_network import MLPRegressor
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.model_selection import StratifiedKFold
from sklearn.linear_model import SGDRegressor
from sklearn import metrics
from sklearn.preprocessing import power_transform
from sklearn.model_selection import train_test_split
from sklearn.pipeline import Pipeline
from sklearn.compose import ColumnTransformer
from sklearn.impute import SimpleImputer
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import OneHotEncoder
from sklearn.utils import resample
from sklearn.decomposition import TruncatedSVD
#from imblearn.over_sampling import SMOTE
import joblib
import pandas as pd
import numpy as  np
import random
import time
import warnings
from warnings import simplefilter
from sklearn.exceptions import ConvergenceWarning
simplefilter("ignore", category=ConvergenceWarning)
warnings.simplefilter(action='ignore')
from sklearn.ensemble import GradientBoostingRegressor




random_state=2023

X_train = pd.read_csv(r"data/train.csv")
y_train = X_train['OV']
X_train = X_train.drop(columns=['OV','IK'],axis=1)

X_test = pd.read_csv(r"data/test.csv")
y_test = X_test['OV']
X_test = X_test.drop(columns=['OV','IK'],axis=1)
# bins = np.linspace(min(y)-1, max(y)+1, 20)
# y_binned = np.digitize(y, bins)
# X_train, X_test, y_train, y_test = train_test_split(X,y, 
#                                                     train_size = 0.75 ,
#                                                     random_state = random_state,
#                                                     shuffle = True,
#                                                     stratify=y_binned)


pca = PCA()
scaler = StandardScaler()
mlr = LinearRegression()
dt = DecisionTreeRegressor()
sgd = SGDRegressor()
rf = RandomForestRegressor()
knn = KNeighborsRegressor()
mlp = MLPRegressor()
svr = SVR()
gb = GradientBoostingRegressor()


mlr_params = {
 #   "preprocess.ColumnTransformer.pca__n_components": (2,5,6) , 
}
dt_params = {
     "Regressor__min_samples_split" : (2,25,45,65, 90,100,110,150,250), 
     "Regressor__max_depth": (1,6,7,8,9, 15,20,25),  #(8,), #(1,6,7,8,9, 15,20,25),
     "Regressor__min_samples_leaf": (1,3,6,7,15,16,19,20,21,22,25,24,23,26,27,28,29,30,31,), 
     "Regressor__ccp_alpha":  (1e-8,1e-3, 0.1), 
      "Regressor__criterion" :  ['squared_error',], #['squared_error', 'friedman_mse', 
                                  # 'absolute_error','poisson'],
     "Regressor__max_features" : ['sqrt', 'log2', None, 1.0],
     "Regressor":[dt,]
    }
knn_params = {
    'Regressor__algorithm' : ['auto', 'ball_tree', 'kd_tree', 'brute'] ,
    'Regressor__leaf_size' : (1,7,11,20,30,40,50,90,180,220),
    'Regressor__metric' :  ["minkowski","manhattan","euclidean","cityblock"], 
    'Regressor__n_neighbors' : (1,3,7,10,20,), 
    #'Regressor__p' : (2,), #(1,2), 
    'Regressor__weights' : ['uniform', 'distance'],
    "Regressor":[knn,]
} 
sgd_params =     {
#    "pca__n_components": Integer(2, 25) ,
#     "Regressor__loss" : ['squared_error', 'huber', 
#                           'epsilon_insensitive', 
#                           'squared_epsilon_insensitive'],
    "Regressor__penalty" : ['l1', 'l2', 'elasticnet',None],
    'Regressor__alpha': (1e-8, 1e-6,1e-4,0.01),
    'Regressor__max_iter': (30000,),
#	'Regressor__tol': (1e-6,1e-3,0.1,),
    'Regressor__learning_rate': ['constant', 'optimal', 
                                  'invscaling', 'adaptive'],
    'Regressor__early_stopping': [True,],
   "Regressor__eta0" : (0.000001, 0.1 , 0.0001 , 0.01),
#    "Regressor__power_t" : (1/4,1/3,1/2),
    "Regressor":[sgd,]
}
svr_params =     {
    "Regressor__kernel" : ("poly",),#['poly', 'linear','rbf'],
    "Regressor__degree" : [2,3,4,5,],
     'Regressor__gamma':  ['scale','auto'],
    # 'Regressor__max_iter': (10000,),
     'Regressor__C':  [500,600,700,800],
     "Regressor__tol" : (0.1,0.001,0.01, 1e-4 ),
     "Regressor__epsilon" :  (1e-03,1e-04,0.01,.1), 
     "Regressor__coef0" : (1.0,0.0,0.5,),
    "Regressor":[svr,],
}


rf_params = {
      'Regressor__n_estimators':np.linspace(50,500,4).astype(int),
     "Regressor__max_depth": np.linspace(1,20,4).astype(int),
      "Regressor__min_samples_split" :np.linspace(2,50,4).astype(int),
      "Regressor__min_samples_leaf": np.linspace(1,20,4).astype(int),
     "Regressor__criterion" :  ['squared_error', ],  
       "Regressor__max_features" : ['sqrt', 'log2', None,1.0,],
       "Regressor__ccp_alpha": (1e-08, 1e-06, 1e-04, 0.1),
    "Regressor":[rf,]
    }
mlp_params = {
    'Regressor__hidden_layer_sizes': (10, 50,250,350,500,
                                      (50,100),(150,250),(350,500),(200,300,100),
                                      (10,50,10),(25,250,10),),
    'Regressor__max_iter': (12500,),
    'Regressor__learning_rate': (['adaptive',]),
   'Regressor__early_stopping': (True,),
     "Regressor__solver" : ['adam',],
      # "Regressor__beta_1" : (.85,.90,.95,.99),
      # "Regressor__beta_2" :(.85,.90,.95,.99),
      # "Regressor__epsilon" : (1e-8 ,1e-12,1e-4, 0.1),
    "Regressor":[mlp,]
    }

gb_params = {
      'Regressor__n_estimators': np.linspace(50,600,3).astype(int),
     "Regressor__max_depth": np.linspace(1,20,3).astype(int),
      "Regressor__min_samples_split" :np.linspace(2,50,3).astype(int),
      "Regressor__min_samples_leaf": np.linspace(1,20,3).astype(int),
     "Regressor__criterion" :  ['squared_error', ],
       "Regressor__max_features" : ['sqrt', 'log2', None,1.0,],
       "Regressor__ccp_alpha": (1e-08, 1e-06, 1e-04,),
       # "Regressor__subsample": (1.0,0.6,0.3,),
       # "Regressor__min_impurity_decrease": (0.0,1.0,10.0,),
       # "Regressor__learning_rate": (0.01,0.1,1.0,10.0,),
    "Regressor":[gb,]
    }

pipe_params = [
    mlr_params,
    # dt_params,
    # knn_params,
    # sgd_params,
    # svr_params,
    #     rf_params,
    # mlp_params,  
   # gb_params,
]

regressors = [
    mlr,
    #     dt,
    # knn,
    #  sgd,
    # svr,
    # rf,
    # mlp,
   # gb,
    
]


mdls = [
    "MLR",
#     "DT",
#     "KNN",
# "SGD",
#     "SVR",
#     "RF",
#     "MLP",
    # "GB",
]

numeric_features = ['TSS', 'TS', 'BS', 'IBTA','SOA','OLS','VLO', 'MERAO', 'OTLO', 'PPZR', 'TKO', 'IKT']
numeric_transformer = Pipeline(steps=[
    ('scaler', StandardScaler()),
    ('pca', PCA()),
])

categorical_features = ['SGS', 'IRK', 'MET', 'YSI', 'IBM' ]
categorical_transformer = OneHotEncoder(handle_unknown='ignore')

preprocessor = ColumnTransformer(
    transformers=[
        ('num', numeric_transformer, numeric_features),
        ('cat', categorical_transformer, categorical_features),
    ])


for i in range(len(regressors)):
    mod = mdls[i]
    bps = []
    pipe = Pipeline([
        ('preprocess',preprocessor),
        ('Regressor',regressors[i])
    ])
    Start_Time = time.time()
    Results= HalvingGridSearchCV(  
        estimator = pipe, 
        param_grid= pipe_params[i],
        cv = 10, random_state= random_state,
        scoring = "neg_mean_squared_error", #"neg_mean_absolute_percentage_error"
        n_jobs = -1, verbose=1)
    Results.fit(X_train,y_train)
    print(Results.best_params_ )
    bps.append(Results.best_params_ )
    End_Time = time.time()
    Time_Spent = End_Time - Start_Time
    print("Search has been completed in ",Time_Spent,"seconds.")
    Best_Fitted_Model = Results.best_estimator_
    preds = Results.predict(X_train)
    obs = y_train
    print("Train :", metrics.explained_variance_score(y_pred=preds,y_true=obs),
          metrics.mean_absolute_error(y_pred=preds, y_true=obs),
          metrics.mean_squared_error(y_pred=preds, y_true=obs,  squared=False),
          metrics.mean_absolute_percentage_error(y_true=obs, y_pred=preds))
    preds = Results.predict(X_test)
    obs = y_test
    print("Test :", metrics.explained_variance_score(y_pred=preds,y_true=obs),
          metrics.mean_absolute_error(y_pred=preds, y_true=obs),
          metrics.mean_squared_error(y_pred=preds, y_true=obs, squared=False),
          metrics.mean_absolute_percentage_error(y_true=obs, y_pred=preds))
    joblib.dump(Best_Fitted_Model, 'results/'+mod+'_mod.pkl', compress = 9)
    with open(r'results/bayes_best_est.txt', 'a') as fp:
        for item in bps:
            fp.write("%s\n" % item)
            print('Done')

# from sklearn.inspection import permutation_importance
# r = permutation_importance(Results, X, Y,
#                             n_repeats=30,
#                             random_state=0)

# for i in r.importances_mean.argsort()[::-1]:
#     if r.importances_mean[i] - 2 * r.importances_std[i] > 0:
#         print(f"{X.columns[i]:<8}"
#               f"{r.importances_mean[i]:.3f}"
#               f" +/- {r.importances_std[i]:.3f}")
