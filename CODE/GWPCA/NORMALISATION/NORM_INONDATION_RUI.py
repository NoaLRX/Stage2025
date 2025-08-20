#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import geopandas as gpd
import pandas as pd
import numpy as np
import os
import warnings
warnings.filterwarnings('ignore')

print("Début du traitement d'agrégation des données d'inondation par ruissellement (RUI) par code postal...")

# Définition des chemins des fichiers
risque_inondation_path = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Inondation/INO_1_RUI/n_inondable_02_01_for_s_FIXED.gpkg"
codes_postaux_path = "/Users/noa/Desktop/PRISM/Data/MISC/Codes postaux - Communes/codes_postaux_region.shp"
output_path = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Inondation/INO_1_RUI/inondation_rui_codes_postaux.gpkg"

# Vérification de l'existence des fichiers source
for file_path in [risque_inondation_path, codes_postaux_path]:
    if not os.path.exists(file_path):
        raise FileNotFoundError(f"Le fichier {file_path} n'existe pas")

# 1. Lecture des données
print("Lecture des données...")
# Lire les données de risque d'inondation
gdf_inondation = gpd.read_file(risque_inondation_path)
print(f"Données d'inondation RUI chargées: {len(gdf_inondation)} zones")

# Lire le shapefile des codes postaux
gdf_codes_postaux = gpd.read_file(codes_postaux_path)
print(f"Données des codes postaux chargées: {len(gdf_codes_postaux)} codes postaux")

# Création d'une colonne CODE_POST dans le shapefile si elle n'existe pas
if 'CODE_POST' not in gdf_codes_postaux.columns:
    print("Utilisation de la colonne 'ID' comme code postal")
    gdf_codes_postaux['CODE_POST'] = gdf_codes_postaux['ID']

# Préparation des données
gdf_codes_postaux['CODE_POST'] = gdf_codes_postaux['CODE_POST'].astype(str)
print(f"Premier code postal: {gdf_codes_postaux['CODE_POST'].iloc[0]}")

# 2. Préparation des données pour l'analyse spatiale
print("Préparation des données pour l'analyse spatiale...")

# Afficher les systèmes de coordonnées des deux datasets
print(f"CRS des données d'inondation: {gdf_inondation.crs}")
print(f"CRS des codes postaux: {gdf_codes_postaux.crs}")

# S'assurer que les deux GeoDataFrames utilisent le même système de coordonnées
if gdf_inondation.crs != gdf_codes_postaux.crs:
    print("Transformation des systèmes de coordonnées...")
    gdf_inondation = gdf_inondation.to_crs(gdf_codes_postaux.crs)

# 3. Calcul des aires
print("Calcul des aires...")

# Ajouter une colonne d'aire pour les zones à risque d'inondation
gdf_inondation['inondation_area'] = gdf_inondation.geometry.area

# Ajouter une colonne d'aire pour les codes postaux
gdf_codes_postaux['cp_area'] = gdf_codes_postaux.geometry.area

# 4. Overlay spatial
print("Calcul de l'overlay spatial entre zones d'inondation et codes postaux...")
print("Ce processus peut prendre du temps...")

# Utilisation de l'opération overlay pour calculer les intersections
overlay = gpd.overlay(gdf_inondation, gdf_codes_postaux, how='intersection')

# Calcul de l'aire de chaque intersection
overlay['intersection_area'] = overlay.geometry.area

print(f"Nombre d'intersections calculées: {len(overlay)}")
print("Colonnes après l'overlay:")
print(overlay.columns.tolist())

# 5. Agrégation des données par code postal
print("Agrégation des données par code postal...")

# Calcul du pourcentage de la surface du code postal couverte par les zones d'inondation
overlay['pct_cp_area'] = overlay['intersection_area'] / overlay['cp_area'] * 100

# Grouper par code postal pour calculer le score final
result = overlay.groupby('CODE_POST').agg({
    'intersection_area': 'sum',
    'cp_area': 'first',
    'pct_cp_area': 'sum'
}).reset_index()

# Calculer le score normalisé (pourcentage de la surface du code postal couverte par l'inondation)
result['score_inondation'] = result['pct_cp_area']

# Normalisation du score entre 0 et 1
if len(result) > 0:
    max_score = result['score_inondation'].max()
    if max_score > 0:
        result['score_normalise'] = result['score_inondation'] / max_score
    else:
        result['score_normalise'] = 0
    
    # Limiter à 1 au maximum
    result['score_normalise'] = result['score_normalise'].clip(0, 1)
else:
    print("Attention: Aucun résultat d'intersection trouvé!")

print(f"Résultat: {len(result)} codes postaux avec risque d'inondation")

# 6. Fusion avec tous les codes postaux
print("Fusion avec tous les codes postaux...")

# Créer un GeoDataFrame final avec tous les codes postaux
gdf_final = gdf_codes_postaux[['CODE_POST', 'geometry', 'cp_area']].copy()

# Fusionner avec les résultats de l'agrégation
gdf_final = pd.merge(
    gdf_final,
    result[['CODE_POST', 'score_inondation', 'score_normalise']],
    on='CODE_POST',
    how='left'
)

# Remplacer les valeurs NaN par 0 (codes postaux sans risque d'inondation)
gdf_final['score_inondation'] = gdf_final['score_inondation'].fillna(0)
gdf_final['score_normalise'] = gdf_final['score_normalise'].fillna(0)

print(f"GeoDataFrame final: {len(gdf_final)} codes postaux")

# Afficher quelques statistiques
print("\nStatistiques sur le score d'inondation:")
print(gdf_final['score_inondation'].describe())

print("\nNombre de codes postaux par catégorie de risque:")
risk_categories = pd.cut(gdf_final['score_normalise'], 
                         bins=[0, 0.2, 0.4, 0.6, 0.8, 1.0],
                         labels=['Très faible', 'Faible', 'Moyen', 'Élevé', 'Très élevé'])
print(risk_categories.value_counts().sort_index())

# 7. Enregistrement des résultats
print(f"Enregistrement des résultats dans {output_path}...")
gdf_final.to_file(output_path, driver="GPKG")

print("Traitement terminé avec succès!") 