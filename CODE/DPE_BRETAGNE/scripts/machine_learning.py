# Import des packages
import requests
import io
import os
import pandas as pd

from scipy import stats
from sklearn.preprocessing import StandardScaler, PolynomialFeatures
from sklearn.preprocessing import OneHotEncoder
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import (
    GridSearchCV,
    RandomizedSearchCV,
    train_test_split,
    learning_curve,
    cross_val_score,
)

from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score  # root_mean_squared_error

# Interpretability
from sklearn.inspection import (
    partial_dependence,
    PartialDependenceDisplay,
    permutation_importance,
)

# from artemis.interactions_methods.model_agnostic import FriedmanHStatisticMethod
# from alibi.explainers import ALE, plot_ale
import lime
import lime.lime_tabular
import numpy as np

np.__version__ = "2.1.3"  # Force la version détectée
import shap  # Devrait maintenant fonctionner


import matplotlib.pyplot as plt
import seaborn as sns
import warnings

warnings.filterwarnings("ignore")


pd.set_option("display.max_colwidth", None)


def winsorize_data(xtrain, xtest, feature):
    """_summary_

    Fonction permettant de winsorizez un jeu d'entrainement et de test en calculant
    les quantiles sur le jeu d'entrainement et l'appliquant sur le jeu test.

    Pour se prémunir d'un data leak.

    """

    # Définir les quantiles sur xtrain
    lower_quantile = 0.05  # 5% quantile
    upper_quantile = 0.95  # 95% quantile

    # Calcul des bornes à partir des quantiles sur xtrain
    lower_bound = np.quantile(xtrain[feature], lower_quantile)
    upper_bound = np.quantile(xtrain[feature], upper_quantile)

    # Appliquer la winsorisation sur xtrain
    xtrain_winsorized = np.clip(xtrain[feature], lower_bound, upper_bound)

    # Appliquer les mêmes bornes sur xtest
    xtest_winsorized = np.clip(xtest[feature], lower_bound, upper_bound)

    return (xtrain_winsorized, xtest_winsorized)


# Calculer le PDP sur les données standardisées


def display_original_pdp_values(num_col, model, df, scaler):
    """_summary_

      Display origina pdp values permet d'afficher les données originales des PDP après un scaling features


    Args:
        num_col (_type_): Numéro de l'index de la colonne à afficher
        model (_type_): Modèle utilisé
        df (_type_): DataFrame avec vos données
        scaler (_type_): Scaler utilisé, uniquement sklearn
    """
    features_to_plot = [(num_col,)]  # Indices des colonnes à analyser

    pdp_results = partial_dependence(
        model, df, features=features_to_plot, kind="average"
    )

    # Extraire les valeurs PDP (standardisées)
    grid = pdp_results["grid_values"]  # Grille de valeurs pour chaque feature
    pdp = pdp_results["average"]  # Valeurs moyennes du PDP

    # Appliquer l'inverse transformation sur la grille de valeurs
    original_grid = [scaler.mean_[num_col] + grid[0] * scaler.scale_[num_col]]

    plt.plot(original_grid[0], pdp[0])
    plt.xlabel("Variable : {}".format(df.columns[3]))
    plt.ylabel("Partian depence plot value")
    plt.title("Original Partial depence plot ")

    return ()


# Lecture des données
df = pd.read_csv("data/DF_FINAL.csv")
df.head()
df = df.dropna()
df.shape  # (68070, 24)
df.dtypes

# Transformation des données
df["date"] = pd.to_datetime(df["date"])

# Convertir en type catégoriel avec l'ordre spécifié
df["age_batiment"] = 2025 - df["annee_constuction"]
appart = df[(df["type"] == "appartement") & (df["code_commune"] == 29019)]
maison = df[(df["type"] == "maison") & (df["code_commune"] == 29019)]

# Création de one hot encoder pour les DPE

# Création du jeu de données pour la modélisation
analyse = df.copy()
target = np.log(analyse["prix"])

# On drop les colonnes inutiles
features = analyse.drop(
    columns=[
        "prix",
        "CATNAT",
        "part_proprio",
        "revenu_median",
        "VERT",
        "ROUGE",
        "BLEU",
        "prix_std",
        "n_vide",
        "date",
        "type",
        "adresse",
        "code_commune",
        "commune",
        "annee_constuction",
    ]
)
print("\n target : \n", list(target))
print("\n features :\n ", list(features))


X = features
y = target

X.shape  # (1299, 11)

# Encodage ordinal pour DPE (respecte l'ordre énergétique)
dpe_order = {"A": 7, "B": 6, "C": 5, "D": 4, "E": 3, "F": 2, "G": 1}
X["DPE_score"] = X["dpe"].map(dpe_order)

# On renomme la colonne
X_encoded = X

# Suppression de la colonne original "dpe"
X_encoded = X_encoded.drop("dpe", axis=1)
features = X_encoded.columns


# Séparation en train-test
X_train, X_test, y_train, y_test = train_test_split(
    X_encoded, y, test_size=0.15, random_state=42, shuffle=True
)

# Winsorize outliers
for col in X_encoded:
    if not col.startswith("is_"):
        print(f" winsorization de la variable : {col}")
        X_train[col], X_test[col] = winsorize_data(
            xtrain=X_train, xtest=X_test, feature=col
        )


# Scaling features

scaler = StandardScaler()
scaler.fit(X_train)

X_train_sc = scaler.transform(X_train)
X_test_sc = scaler.transform(X_test)


# Modèle Random Forest
rf_model = RandomForestRegressor(n_estimators=100, random_state=42)
rf_model.fit(X_train_sc, y_train)
y_pred_rf = rf_model.predict(X_test_sc)
rf_mse = mean_squared_error(y_test, y_pred_rf)
print(f"Random Forest MSE: {rf_mse:.4f}")  # Random Forest MSE: 0.2365
r2 = r2_score(y_test, y_pred_rf)  # 0.5129

# Modèle Régression Linéaire
lr_model = LinearRegression()
lr_model.fit(X_train_sc, y_train)
y_pred_lr = lr_model.predict(X_test_sc)
lr_mse = mean_squared_error(y_test, y_pred_lr)
print(f"Régression Linéaire MSE: {lr_mse:.4f}")  # Régression Linéaire MSE: 0.3000


# Interprétation de la régression linéaire
def get_pvalues(X, y, model):
    n = len(y)
    p = X.shape[1]
    residuals = y - model.predict(X)
    mse = np.sum(residuals**2) / (n - p - 1)
    var_coef = mse * np.linalg.inv(X.T @ X).diagonal()
    t_stats = model.coef_ / np.sqrt(var_coef)
    p_values = 2 * (1 - stats.t.cdf(np.abs(t_stats), n - p - 1))
    return p_values


# Obtenir les résultats
p_values = get_pvalues(X_train_sc, y_train, lr_model)

# Créer le DataFrame des résultats
results = pd.DataFrame(
    {
        "Variable": X_train.columns,  # Noms des variables
        "Coefficient": lr_model.coef_,
        "P-value": p_values,
    }
)

print(results)

#             Variable  Coefficient  P-value
# 0           code_dep     0.064566 0.000000
# 1  surface_habitable     0.217527 0.000000
# 2    surface_terrain     0.037484 0.000000
# 3          nb_etages     0.010160 0.000056
# 4          nb_pieces     0.147699 0.000000
# 5       DIST_VERT_KM     0.014674 0.000000
# 6       DIST_BLEU_KM    -0.171870 0.000000
# 7      DIST_ROUGE_KM    -0.026109 0.000000
# 8       age_batiment    -0.100079 0.000000
# 9          DPE_score     0.082398 0.000000


# Tuner le RF
param_dist = {
    "n_estimators": [50, 100],
    "max_depth": [5, 10],
    "min_samples_split": [5, 10],
    "min_samples_leaf": [2, 4],
    "max_features": ["sqrt"],
    "criterion": ["squared_error"],
}

rf = RandomForestRegressor(random_state=42)

random_search = RandomizedSearchCV(
    estimator=rf,
    param_distributions=param_dist,
    n_iter=10,  # Teste 30 configurations
    cv=3,
    scoring="neg_mean_squared_error",
    n_jobs=-1,
    verbose=2,
    random_state=42,
)

random_search.fit(X_train_sc, y_train)
print("Meilleurs paramètres (RandomizedSearch) :", random_search.best_params_)

# Trier les résultats du meilleur au pire
# Récupérer tous les résultats
results_df = pd.DataFrame(random_search.cv_results_)
results_df = results_df.sort_values(by="rank_test_score")

# Afficher les meilleurs essais
results_df[["rank_test_score", "mean_test_score", "std_test_score", "params"]].head(10)


# Modèle Random Forest
best_model = RandomForestRegressor(
    n_estimators=100,
    random_state=42,
    max_depth=10,
    criterion="squared_error",
    max_features="sqrt",
    min_samples_split=5,
    min_samples_leaf=4,
)
best_model.fit(X_train_sc, y_train)

y_pred_rf_test = best_model.predict(X_test_sc)
y_pred_rf_train = best_model.predict(X_train_sc)

rf_mse_test = mean_squared_error(y_test, y_pred_rf_test)
rf_mse_train = mean_squared_error(y_train, y_pred_rf_train)

print(f"Random Forest MSE TRAIN: {rf_mse_train:.4f}")
print(f"Random Forest MSE TEST: {rf_mse_test:.4f}")

# Random Forest MSE TRAIN: 0.2007
# Random Forest MSE TEST: 0.2505


# Qualité de prédiction du modèle
fig = plt.figure(figsize=(12, 8))
# fig.suptitle("Actual vs Predicted MEDV [RandomForest]")
plt.scatter(y_test, y_pred_rf_test, alpha=0.4)
plt.plot([0, 20], [0, 20], "--k")
plt.show()


# Feature importance
feature_importance_df = pd.DataFrame(
    {"Feature": features, "Importance": best_model.feature_importances_}
)

# Plot de l'importance des features
order_importance = feature_importance_df.sort_values(by="Importance", ascending=False)

plt.figure(figsize=(10, 6))
plt.barh(order_importance["Feature"], order_importance["Importance"], color="skyblue")
plt.xlabel("Importance")
plt.ylabel("Feature")
# plt.title("Importance des Variables dans Random Forest")
plt.gca().invert_yaxis()  # Inverser pour que la plus importante soit en haut
plt.show()

# Prendre les données scaled et leur attribuer les noms des colonnes
df_interpret = pd.DataFrame(X_test_sc, columns=features)

# PDP pour plusieurs caractéristiques
# Index des caractéristiques
features = [
    "surface_habitable",
    "nb_pieces",
    "DIST_BLEU_KM",
    "age_batiment",
    "surface_terrain",
    "DPE_score",
    ("surface_habitable", "surface_terrain"),
]
_, ax1 = plt.subplots(figsize=(12, 6))
PartialDependenceDisplay.from_estimator(
    best_model,  # votre modèle
    df_interpret,
    features,
    kind="average",
    ax=ax1,
    grid_resolution=100,  # Nombre de points estimés pour le tracer de la courbe
    n_cols=4,
)
# plt.suptitle("Partial Dependence Plots - LGR")
plt.tight_layout()
plt.show()

display_original_pdp_values(3, best_model, df_interpret, scaler)

result = permutation_importance(
    best_model,
    X_test_sc,
    y_test,
    n_repeats=15,
    random_state=0,
    scoring="neg_mean_squared_error",
)


# Comparaison entre random forest variable importance et variable importance agnostic

perm_sorted_idx = result.importances_mean.argsort()

fig, (ax1) = plt.subplots(1, 1, figsize=(10, 6))
ax1.boxplot(
    result.importances[perm_sorted_idx].T, vert=False, labels=features[perm_sorted_idx]
)
# plt.title("Permutation importance feature")
fig.text(0.5, 0.001, "Feature importance : Loss function MSE", ha="center")

fig.tight_layout()
plt.show()


# PDP pour plusieurs caractéristiques (par exemple, 'sepal length' et 'petal width')
features = ["surface_habitable", "surface_terrain"]  # Index des caractéristiques
_, ax1 = plt.subplots(figsize=(12, 6))
PartialDependenceDisplay.from_estimator(
    best_model,  # votre modèle
    df_interpret,
    features,
    kind="individual",
    ax=ax1,
    n_cols=3,
    centered=True,
)
# plt.suptitle("Partial Dependence Plots - random- forest")
plt.tight_layout()
plt.show()


features = X_encoded.columns
# lime explaination
explainer = lime.lime_tabular.LimeTabularExplainer(
    X_train_sc,
    feature_names=features,
    class_names=["medv"],
    verbose=True,
    mode="regression",
)


i = 50
exp = explainer.explain_instance(X_test_sc[i], best_model.predict)

exp.show_in_notebook(show_table=True, show_all=False)

# Shapley Values
# Init shapley values
explainer = shap.TreeExplainer(best_model)
shap_values = explainer(df_interpret)


plt.clf()  # Efface le graphique actuel
shap.initjs()
# visualize the first prediction's explanation
shap.waterfall_plot(shap_values[50])

plt.clf()  # Efface le graphique actuel

shap.plots.beeswarm(shap_values)
