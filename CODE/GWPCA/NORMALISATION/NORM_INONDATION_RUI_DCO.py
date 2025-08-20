#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import geopandas as gpd
import pandas as pd
import numpy as np
import os
import warnings
warnings.filterwarnings('ignore')

print("Début du traitement d'agrégation des données d'inondation par ruissellement (RUI) et débordement de cours d'eau (DCO) par code postal...")

# Définition des chemins des fichiers
risque_inondation_rui_path = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Inondation/INO_1_RUI/n_inondable_02_01_for_s_FIXED.gpkg"
risque_inondation_dco_path = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Inondation/INO_1_DCO/n_inondable_01_01for_s_FIXED.gpkg"
codes_postaux_path = "/Users/noa/Desktop/PRISM/Data/MISC/Codes postaux - Communes/codes_postaux_region.shp"
output_path = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Inondation/inondation_rui_dco_codes_postaux.gpkg"

# Vérification de l'existence des fichiers source
for file_path in [risque_inondation_rui_path, risque_inondation_dco_path, codes_postaux_path]:
    if not os.path.exists(file_path):
        raise FileNotFoundError(f"Le fichier {file_path} n'existe pas")

# 1. Lecture des données
print("Lecture des données...")
# Lire les données de risque d'inondation RUI
gdf_inondation_rui = gpd.read_file(risque_inondation_rui_path)
print(f"Données d'inondation RUI chargées: {len(gdf_inondation_rui)} zones")

# Lire les données de risque d'inondation DCO
gdf_inondation_dco = gpd.read_file(risque_inondation_dco_path)
print(f"Données d'inondation DCO chargées: {len(gdf_inondation_dco)} zones")

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

# Ajout d'un identifiant de type d'inondation pour suivre la source
gdf_inondation_rui['type_inondation'] = 'RUI'
gdf_inondation_dco['type_inondation'] = 'DCO'

# S'assurer que les trois GeoDataFrames utilisent le même système de coordonnées
print(f"CRS des données d'inondation RUI: {gdf_inondation_rui.crs}")
print(f"CRS des données d'inondation DCO: {gdf_inondation_dco.crs}")
print(f"CRS des codes postaux: {gdf_codes_postaux.crs}")

if gdf_inondation_rui.crs != gdf_codes_postaux.crs:
    print("Transformation des systèmes de coordonnées RUI...")
    gdf_inondation_rui = gdf_inondation_rui.to_crs(gdf_codes_postaux.crs)

if gdf_inondation_dco.crs != gdf_codes_postaux.crs:
    print("Transformation des systèmes de coordonnées DCO...")
    gdf_inondation_dco = gdf_inondation_dco.to_crs(gdf_codes_postaux.crs)

# 3. Combinaison des données d'inondation
print("Combinaison des données d'inondation RUI et DCO...")
# Utiliser les mêmes colonnes pour les deux GeoDataFrames pour pouvoir les concaténer
common_columns = ['geometry', 'type_inondation']
gdf_inondation_rui = gdf_inondation_rui[common_columns]
gdf_inondation_dco = gdf_inondation_dco[common_columns]

# Concaténer les deux jeux de données
gdf_inondation_combined = pd.concat([gdf_inondation_rui, gdf_inondation_dco])
print(f"Données d'inondation combinées: {len(gdf_inondation_combined)} zones")

# 4. Calcul des aires
print("Calcul des aires...")

# Ajouter une colonne d'aire pour les zones à risque d'inondation
gdf_inondation_combined['inondation_area'] = gdf_inondation_combined.geometry.area

# Ajouter une colonne d'aire pour les codes postaux
gdf_codes_postaux['cp_area'] = gdf_codes_postaux.geometry.area

# 5. Overlay spatial
print("Calcul de l'overlay spatial entre zones d'inondation et codes postaux...")
print("Ce processus peut prendre du temps...")

# Utilisation de l'opération overlay pour calculer les intersections
overlay = gpd.overlay(gdf_inondation_combined, gdf_codes_postaux, how='intersection')

# Calcul de l'aire de chaque intersection
overlay['intersection_area'] = overlay.geometry.area

print(f"Nombre d'intersections calculées: {len(overlay)}")
print("Colonnes après l'overlay:")
print(overlay.columns.tolist())

# 6. Agrégation des données par code postal
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

# 7. Agrégation par type d'inondation pour analyse supplémentaire
print("Agrégation par type d'inondation...")

# Calcul des scores pour chaque type d'inondation séparément
overlay_by_type = overlay.groupby(['CODE_POST', 'type_inondation']).agg({
    'intersection_area': 'sum',
    'cp_area': 'first',
    'pct_cp_area': 'sum'
}).reset_index()

# Pivoter le tableau pour avoir une colonne par type d'inondation
overlay_pivot = overlay_by_type.pivot_table(
    index='CODE_POST', 
    columns='type_inondation', 
    values='pct_cp_area', 
    fill_value=0
).reset_index()

# Renommer les colonnes
if 'RUI' in overlay_pivot.columns:
    overlay_pivot.rename(columns={'RUI': 'score_rui'}, inplace=True)
else:
    overlay_pivot['score_rui'] = 0

if 'DCO' in overlay_pivot.columns:
    overlay_pivot.rename(columns={'DCO': 'score_dco'}, inplace=True)
else:
    overlay_pivot['score_dco'] = 0

# 8. Fusion avec tous les codes postaux
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

# Fusionner avec les scores par type
gdf_final = pd.merge(
    gdf_final,
    overlay_pivot[['CODE_POST', 'score_rui', 'score_dco']],
    on='CODE_POST',
    how='left'
)

# Remplacer les valeurs NaN par 0 (codes postaux sans risque d'inondation)
gdf_final['score_inondation'] = gdf_final['score_inondation'].fillna(0)
gdf_final['score_normalise'] = gdf_final['score_normalise'].fillna(0)
gdf_final['score_rui'] = gdf_final['score_rui'].fillna(0)
gdf_final['score_dco'] = gdf_final['score_dco'].fillna(0)

print(f"GeoDataFrame final: {len(gdf_final)} codes postaux")

# Afficher quelques statistiques
print("\nStatistiques sur le score d'inondation global:")
print(gdf_final['score_inondation'].describe())

print("\nStatistiques sur le score d'inondation RUI:")
print(gdf_final['score_rui'].describe())

print("\nStatistiques sur le score d'inondation DCO:")
print(gdf_final['score_dco'].describe())

print("\nNombre de codes postaux par catégorie de risque (score global):")
risk_categories = pd.cut(gdf_final['score_normalise'], 
                         bins=[0, 0.2, 0.4, 0.6, 0.8, 1.0],
                         labels=['Très faible', 'Faible', 'Moyen', 'Élevé', 'Très élevé'])
print(risk_categories.value_counts().sort_index())

# 9. Enregistrement des résultats
print(f"Enregistrement des résultats dans {output_path}...")
gdf_final.to_file(output_path, driver="GPKG")

print("Traitement terminé avec succès!")
