#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jun  2 19:02:56 2023

@author: hakan
"""

from sklearn.ensemble import RandomForestRegressor
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.svm import SVR
from sklearn.linear_model import LinearRegression
from sklearn.tree import DecisionTreeRegressor
from sklearn.neighbors import KNeighborsRegressor
import joblib


class FitReg:
    def __init__(self, model, X, y):
        self.model = joblib.load(filename="results/" + model + "_mod.pkl")
        self.X = X
        self.y = y

    def predict(self):
        self.fitted = self.model.predict(self.X)
        return self.fitted
