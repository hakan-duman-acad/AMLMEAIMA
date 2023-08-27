#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Intel işlemciler için aşağıdaki iki satır hızlandırma sağlar
# from sklearnex import patch_sklearn
# patch_sklearn()
from sklearn.experimental import enable_halving_search_cv  # noqa
from sklearn.model_selection import HalvingGridSearchCV
from sklearn.decomposition import PCA
from sklearn.linear_model import LinearRegression
from sklearn.tree import DecisionTreeRegressor
from sklearn.neighbors import KNeighborsRegressor
from sklearn.svm import SVR
from sklearn.metrics import mean_absolute_error, r2_score, mean_absolute_percentage_error, mean_squared_error
from sklearn.ensemble import RandomForestRegressor
from sklearn.neural_network import MLPRegressor
from sklearn.pipeline import Pipeline
from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import OneHotEncoder
import joblib
import math
import numpy as np
import time
import warnings
from warnings import simplefilter
from sklearn.exceptions import ConvergenceWarning
simplefilter("ignore", category=ConvergenceWarning)
warnings.simplefilter(action='ignore')
random_state = 2023


class ModelEgitim:
    def __init__(self, X, y, modelName=["ALL",], useParams = True):
        self.X = X
        self.y = y
        self.useParams = useParams
        self.allModelNames = ("MLR", "DT", "KNN", "SVR", "RF", "MLP",)
        if type(modelName) is not list:
            modelName = [modelName]
        if modelName[0] == "ALL":
            self.modelName = self.allModelNames
        elif all(model in self.allModelNames for model in modelName):
            self.modelName = modelName
        else:
            self.modelName = "MLR"

    def fit(self, cv=10):
        mlr = LinearRegression()
        dt = DecisionTreeRegressor()
        svr = SVR()
        rf = RandomForestRegressor()
        knn = KNeighborsRegressor()
        mlp = MLPRegressor()
        mlr_params = {
            #    "pca__n_components": Integer(2,25) ,
        }
        dt_params = {
            "Regressor__min_samples_split": (2, 25, 45, 65, 90,
                                             100, 110, 150, 250),
            "Regressor__max_depth": (1, 6, 7, 8, 9, 15, 20, 25),
            "Regressor__min_samples_leaf": (1, 3, 6, 7, 15, 16,
                                            19, 20, 21, 22, 25, 24, 23,
                                            26, 27, 28, 29, 30, 31,),
            "Regressor__ccp_alpha":  (1e-8, 1e-3, 0.1),
            "Regressor__criterion":  ['squared_error', 'friedman_mse',
                                      'absolute_error', 'poisson'],
            "Regressor__max_features": ['sqrt', 'log2', None, 1.0],
            "Regressor": [dt,]
        }

        knn_params = {
            'Regressor__algorithm': ['auto', 'ball_tree',
                                     'kd_tree', 'brute'],
            'Regressor__leaf_size': (1, 7, 11, 20, 30, 40, 50, 90, 180, 220),
            'Regressor__metric':  ["minkowski", "manhattan",
                                   "euclidean", "cityblock"],
            'Regressor__n_neighbors': (1, 3, 7, 10, 20,),
            'Regressor__weights': ['uniform', 'distance'],
            "Regressor": [knn,]
        }

        svr_params = {
            "Regressor__kernel": ['poly', 'linear', 'rbf'],
            "Regressor__degree": [2, 3, 4, 5,],
            'Regressor__gamma':  ['scale', 'auto'],
            'Regressor__C':  [500, 600, 700, 800],
            "Regressor__tol": (0.1, 0.001, 0.01, 1e-4),
            "Regressor__epsilon":  (1e-03, 1e-04, 0.01, .1),
            "Regressor__coef0": (1.0, 0.0, 0.5,),
            "Regressor": [svr,],
        }

        rf_params = {
            'Regressor__n_estimators': np.linspace(50, 500, 4).astype(int),
            "Regressor__max_depth": np.linspace(1, 20, 4).astype(int),
            "Regressor__min_samples_split": np.linspace(2, 50, 4).astype(int),
            "Regressor__min_samples_leaf": np.linspace(1, 20, 4).astype(int),
            "Regressor__criterion":  ['squared_error', ],
            "Regressor__max_features": ['sqrt', 'log2', None, 1.0,],
            "Regressor__ccp_alpha": (1e-08, 1e-06, 1e-04, 0.1),
            "Regressor": [rf,]
        }
        mlp_params = {
            'Regressor__hidden_layer_sizes': (10, 50, 250, 350, 500,
                                              (50, 100), (150, 250), (350, 500),
                                              (200, 300, 100),
                                              (10, 50, 10), (25, 250, 10),),
            'Regressor__max_iter': (12500,),
            'Regressor__learning_rate': (['adaptive',]),
            'Regressor__early_stopping': (True,),
            "Regressor__solver": ['adam',],
            "Regressor__beta_1": (.85, .90, .95, .99),
            "Regressor__beta_2": (.85, .90, .95, .99),
            "Regressor__epsilon": (1e-8, 1e-12, 1e-4, 0.1),
            "Regressor": [mlp,]
        }
        if self.useParams:
            pipeParam = [mlr_params, dt_params, knn_params, svr_params,
                         rf_params, mlp_params,]
        else: 
            pipeParam = [{},{},{},{},{},{}]           
        

        regressors = [mlr, dt, knn, svr, rf, mlp,]

        mdls = self.allModelNames
        mdlIndex = [idx for idx, modname in enumerate(self.allModelNames)
                    if modname in self.modelName]
        pipeParam = [pipeParam[i] for i in mdlIndex]
        regressors = [regressors[i] for i in mdlIndex]
        mdls = [mdls[i] for i in mdlIndex]

        numeric_features = ['TSS', 'TS', 'BS', 'IBTA', 'SOA', 'OLS',
                            'VLO', 'MERAO', 'OTLO', 'PPZR', 'TKO', 'IKT']
        numeric_transformer = Pipeline(steps=[
            ('scaler', StandardScaler()),
            ('pca', PCA()),
        ])

        categorical_features = ['SGS', 'IRK', 'MET', 'YSI', 'IBM']
        categorical_transformer = OneHotEncoder(handle_unknown='ignore')

        preprocessor = ColumnTransformer(
            transformers=[
                ('num', numeric_transformer, numeric_features),
                ('cat', categorical_transformer, categorical_features),
            ])
        for i in range(len(regressors)):
            mod = mdls[i]
            pipe = Pipeline([
                ('preprocess', preprocessor),
                ('Regressor', regressors[i])
            ])
            startTime = time.time()
            results = HalvingGridSearchCV(
                estimator=pipe,
                param_grid=pipeParam[i],
                cv=int(cv), random_state=random_state,
                scoring="neg_mean_squared_error",
                n_jobs=-1, verbose=0)
            results.fit(self.X, self.y)
            endTime = time.time()
            timeSpent = endTime - startTime
            cvSkoru = -results.cv_results_['mean_test_score']
            esRMSE = mean_squared_error(self.X,self.y, squared= True)
            esMAE = mean_absolute_error(y_pred=results.predict(self.X),
                                        y_true=self.y)
            esMAPE = mean_absolute_percentage_error(y_pred=results.predict(self.X),
                                        y_true=self.y)
            esR2 = r2_score(y_pred=results.predict(self.X),
                                        y_true=self.y)
            print("".ljust(60,"*"))
            print(f"Veri satır sayısı {self.X.shape[0]}")
            print(f"Model : {mod}")
            print(f"Eğitim süresi : {timeSpent} saniye.")
            print(f"Eğitim skoru - RMSE : {math.sqrt(esRMSE):#.3f}, MAE : {esMAE:#.3f}, R2 : {esR2:#.3f}, MAPE : {esMAPE:#.3f}")
            print(f"CV skoru - RMSE : {math.sqrt(np.nanmean(cvSkoru)):#.3f}\n\n")
            joblib.dump(results.best_estimator_, 'results/'+mod+'_mod.pkl',
                        compress=9)
