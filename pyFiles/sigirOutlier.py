import pandas as pd
import numpy as np
from  sklearn.ensemble import IsolationForest

class Outlier:
    def __init__(self,x):
        self.x=x
    def fit(self):
        self.model=IsolationForest(n_estimators=100, max_samples='auto',
         max_features=1.0, random_state = 2023 )
        self.model.fit(self.x)
    def predict(self):
        self.scores = self.model.decision_function(self.x)
        self.anomaly = self.model.predict(self.x)
        return self.anomaly

