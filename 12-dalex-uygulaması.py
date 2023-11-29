import dalex as dx
import time
import pandas as pd
import numpy as np
import plotly.graph_objects as go
import joblib
import warnings

class Dalex:
    def __init__(self, model="MLP", full=False):
        self.data = pd.read_csv("./data/tum_data.csv")
        self.X = self.data.drop(columns=["OV", ], axis=1)
        self.y = self.data["OV"]
        self.randomstate = 2023
        self.models = ["MLR", "MLP", "SVR"]
        if model not in self.models:
            raise Exception("model adı doğru değil")
        self.modelname = model
        self.path_images = "./images/"
        self.path_models = "./results/"
        self.model = None
    def prepare_VI(self):
        pass

    def load_VI(self):
        path = self.path_models + self.modelname + "_mod.pkl"
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", category=UserWarning)
            self.model = joblib.load(path)

    def save_VI_plots(self):
        pass
