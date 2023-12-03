import dalex as dx
import time
import os
from numpy import compress
import pandas as pd
from pandas.core.interchange.dataframe_protocol import enum

# import numpy as np
import plotly.graph_objects as go
import joblib
import warnings

warnings.simplefilter("always", UserWarning)


class DxVI:
    def __init__(
        self, model="MLP", full=False, cpus=-1, randomstate=2023, max_vars=None
    ):
        self.data = pd.read_csv("./data/tum_data.csv")
        self.X = self.data.drop(
            columns=[
                "OV",
            ],
            axis=1,
        )
        self.max_vars = 15 if max_vars is None else max_vars
        self.y = self.data["OV"]
        self.randomstate = randomstate
        self.models = ["MLR", "MLP", "SVR"]
        if model not in self.models:
            raise Exception("model adı doğru değil")
        self.modelname = model
        self.path_images = "./images/"
        self.path_models = "./results/"
        self.model = None
        self.B = 2000 if full else 50
        self.cpus = os.cpu_count() if cpus == -1 else cpus
        self.vi = None
        self.vi_model_path = self.path_models + "dalex/vi_" + self.modelname + ".pkl"
        self.vi_csv_path = self.path_models + "dalex/vi_" + self.modelname + ".csv"
        self.model_path = self.path_models + self.modelname + "_mod.pkl"
        self.vi_plot_path = self.path_images + self.modelname + "_vi.png"
        self.vi_plot_object_path = (
            self.path_models + "dalex/" + self.modelname + "_fig_vi.pkl"
        )

        self.VI_groups = None
        self.fig_VI = None

    def load_all_data(self):
        self.load_model()
        self.load_explainer()
        self.load_VI()
        self.load_VI_plot_object()

    def prepare_all(self, force=False):
        if self.model is None:
            self.load_all_data()
        self.prepare_VI(save=True, force=force)
        self.prepare_VI_plot_object(save=True, force=force)

    def load_explainer(self):
        self.exp = dx.Explainer(self.model, self.X, self.y)
        print(f"Explainer dosyası oluşturuldu.")

    def load_model(self):
        if os.path.exists(self.model_path):
            self.model = joblib.load(self.model_path)
            print(f"{self.modelname} modeli yüklendi : {self.model_path}")
        else:
            raise Exception("Model oluşturulmamış veya hatalı")
            self.model = None

    def prepare_VI(self, force=False, save=False):
        if (self.vi is not None) and not force:
            warnings.warn(
                """VI modeli daha önceden oluşturulmuştur. 
                Yeniden oluşturmak için force=True parametresiyle kulanınız."""
            )
            return
        start = time.time()
        self.vi = self.exp.model_parts(
            processes=self.cpus,
            type="difference",
            random_state=self.randomstate,
            B=self.B,
            variable_groups=self.VI_groups,
        )
        end = time.time()
        print(f"VI modeli oluşturuldu. Geçen süre : {end - start}")
        if save:
            self.save_VI()
        # joblib.dump(self.vi "results/dalex/viMLR.pkl", compress=9)

    def save_VI(self, perm=True):
        path = self.vi_model_path
        if self.vi is not None:
            joblib.dump(self.vi, path, compress=9)
            print(f"VI modeli kayıt edildi : {self.vi_model_path}")
            if perm:
                self.save_VI_perm_list()
        else:
            warnings.warn("Öncelikle VI modeli hazırlayınız.")

    def save_VI_perm_list(self):
        path = self.vi_csv_path
        if self.vi is not None:
            full = self.vi.permutation.iloc[1]["_full_model_"]
            mu = self.vi.permutation - full
            temp = mu.quantile([0.025, 0.25, 0.5, 0.75, 0.975])
            temp = pd.DataFrame(temp)
            temp.to_csv(path)
            print(f"VI permutasyon değerleri kayıt edilmiştir : {path}")
        else:
            warnings.warn("VI modeli yüklü değildir.")

    def load_VI(self):
        if os.path.exists(self.vi_model_path):
            self.vi = joblib.load(self.vi_model_path)
            print(f"VI modeli yüklendi : {self.vi_model_path}")
        else:
            warnings.warn("VI modeli oluşturulmamıştır.")
            self.vi = None

    def prepare_VI_plot_object(self, save=False, force=False):
        if self.vi is not None:
            fig = self.vi.plot(max_vars=self.max_vars, title="", show=False)
            fig.update_layout(
                shapes=[
                    go.layout.Shape(
                        type="rect",
                        xref="paper",
                        yref="paper",
                        x0=0,
                        y0=-0.0,
                        x1=1.01,
                        y1=1.02,
                        line={"width": 1, "color": "black"},
                    )
                ]
            )
            self.fig_VI = fig
            fig.show()
            if save:
                self.save_VI_plot_object(force=force)
                self.save_VI_plot_png()
        else:
            warnings.warn("VI modeli oluşturulmamıştır.")

    def save_VI_plot_png(self):
        if self.fig_VI is not None:
            self.fig_VI.write_image(
                self.vi_plot_path, format="png", width=900, height=1200
            )
            print(f"VI grafiği oluşturuldu : {self.vi_plot_path}")
        else:
            print("VI grafiği oluşturulmamıştır.")

    def save_VI_plot_object(self, force=False):
        is_file_exists = os.path.exists(self.vi_plot_object_path)
        if self.fig_VI is not None:
            if not is_file_exists or force:
                joblib.dump(self.fig_VI, self.vi_plot_object_path, compress=9)
                print(f"VI image objesi kayıt edildi : {self.vi_plot_object_path}")
            else:
                print(
                    "VI image image soyası zaten var. force = True argümanını kullanabilirsiniz"
                )
        else:
            print("VI image objesi oluşturulmamıştır.")

    def load_VI_plot_object(self):
        is_path_exists = os.path.exists(self.vi_plot_object_path)
        if is_path_exists:
            self.fig_VI = joblib.load(self.vi_plot_object_path)
            print(f"VI image objesi yüklendi : {self.vi_plot_object_path}")
        else:
            print("VI image objesi mevcut değildir.")


class DxVIgr(DxVI):
    def __init__(
        self, model="MLP", full=False, cpus=-1, randomstate=2023, max_vars=None
    ):
        super().__init__(model, full, cpus, randomstate)

        self.max_vars = 3 if max_vars is None else max_vars
        self.vi_plot_path = self.path_images + self.modelname + "_vi_gr.png"
        self.vi_plot_object_path = (
            self.path_models + "dalex/" + self.modelname + "_fig_vi_gr.pkl"
        )
        self.vi_model_path = self.path_models + "dalex/vi_" + self.modelname + "_gr.pkl"
        self.vi_csv_path = self.path_models + "dalex/vi_" + self.modelname + "_gr.csv"
        self.VI_groups = {
            "Çevresel Koşullar": ["MET", "MERAO", "OTLO", "YSI", "TKO", "IKT", "PPZR"],
            "Çiftlik": [
                "TSS",
                "SGS",
            ],
            "Sığır": ["IRK", "TS", "IBTA", "OLS", "IBM", "VLO", "SOA", "BS"],
        }


class DxAle:
    def __init__(
        self,
        vars=None,
        models=None,
        full=False,
        cpus=-1,
        randomstate=2023,
        profile_type="accumulated",
        all=False,
    ):
        self.vars = (
            [
                "TSS",
            ]
            if vars is None
            else vars
        )
        self.numeric_features = [
            "TSS",
            "TS",
            "BS",
            "IBTA",
            "SOA",
            "OLS",
            "VLO",
            "MERAO",
            "OTLO",
            "PPZR",
            "TKO",
            "IKT",
        ]
        self.categorical_features = ["SGS", "IRK", "MET", "YSI", "IBM"]
        self.profile_type = profile_type
        self.modelnames = (
            [
                "MLR",
                "MLP",
            ]
            if models is None
            else models
        )
        self.model = []
        self.data = pd.read_csv("./data/tum_data.csv")
        self.X = self.data.drop(
            columns=[
                "OV",
                "IA",
            ],
            axis=1,
        )
        self.y = self.data["OV"]
        self.randomstate = randomstate
        self.path_images = "./images/"
        self.path_models = "./results/"
        self.cpus = os.cpu_count() if cpus == -1 else cpus
        self.ales = []
        self.model_path = [self.path_models + i + "_mod.pkl" for i in self.modelnames]
        # self.vi_csv_path = self.path_models + "dalex/vi_" + self.modelname + ".csv"
        # self.model_path = self.path_models + self.modelname + "_mod.pkl"
        # self.vi_plot_path = self.path_images + self.modelname + "_vi.png"
        # self.vi_plot_object_path = (
        #     self.path_models + "dalex/" + self.modelname + "_fig_vi.pkl"
        # )
        self.exp = []
        self.fig_ale = None
        self.all = all

    def prepare_all(self):
        features = self.categorical_features + self.numeric_features
        if len(self.model) == 0:
            self.load_model()
            self.load_explainer()
        if not self.all:
            self.prepare_ales()
            return
        for feature in features:
            self.vars = feature
            self.prepare_ales()

    def load_explainer(self):
        if len(self.exp) == 0:
            for idx, model in enumerate(self.model):
                self.exp.append(dx.Explainer(model, self.X, self.y))
                print(f"Explainer dosyası oluşturuldu : {self.modelnames[idx]}")

    def load_model(self):
        for model_path in self.model_path:
            if os.path.exists(model_path):
                self.model.append(joblib.load(model_path))
                print(f"Model yüklendi : {model_path}")
            else:
                raise Exception("Model oluşturulmamış veya hatalı")

    def prepare_ales(self):
        self.ales = []
        for idx, exp in enumerate(self.exp):
            variable_type = (
                "categorical" if self.vars in self.categorical_features else "numerical"
            )
            self.ales.append(
                exp.model_profile(
                    type=self.profile_type,
                    variable_type=variable_type,
                    variables=self.vars,
                    processes=self.cpus,
                    random_state=self.randomstate,
                    label=self.modelnames[idx],
                )
            )
        self.save_plots(
            modelname="_".join(self.modelnames), varname="_".join(self.vars)
        )

    def save_plots(self, modelname, varname, force=False):
        path = f"{self.path_images}{modelname}_{varname}_ale.png"
        fig = self.ales[0].plot(self.ales[1], show=False)
        fig.update_layout(title="")
        fig.update_layout(legend_title_text="")
        fig.update_layout(yaxis_tickformat="digit")
        fig.write_image(path, format="png", width=900, height=400)
        fig.show()
