#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Aug 27 14:55:57 2023

@author: hakan
"""

import joblib


class ModelTahmin:
    def __init__(self, X):
        self.X = X

    def predict(self, modelName="MLR"):
        model = joblib.load(filename="results/" + modelName + "_mod.pkl")
        return model.predict(self.X)
