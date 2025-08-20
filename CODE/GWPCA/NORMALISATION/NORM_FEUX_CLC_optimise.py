import os
import pandas as pd
import geopandas as gpd
import numpy as np
from tqdm import tqdm

# Définir les chemins des fichiers
clc_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/IncendieFeuxForêts/FF_1_CLC"
code_postal_path = "/Users/noa/Desktop/PRISM/Data/MISC/Codes postaux - Communes/codes_postaux_region.shp"

# Définir les codes CLC à conserver et leurs coefficients
code_weights = {
    "312": 1.0,  # Forêts de conifères
    "313": 1.0,  # Forêts mélangées
    "311": 0.9,  # Forêts de feuillus
    "324": 0.8,  # Forêt et végétation arbustive en mutation
    "322": 0.7,  # Landes et broussailles
    "323": 0.7   # Végétation sclérophylle
}

# Charger les données des codes postaux
print("Chargement des codes postaux...")
code_postal_gdf = gpd.read_file(code_postal_path)
print(f"Nombre de codes postaux: {len(code_postal_gdf)}")

# Calculer et stocker l'aire des codes postaux
code_postal_gdf['cp_area'] = code_postal_gdf.geometry.area

# Préparer le DataFrame pour les résultats
result_df = pd.DataFrame({'ID': code_postal_gdf['ID'].unique()})
result_df['SCORE'] = 0.0

# Créer un GeoDataFrame vide pour stocker tous les polygones filtrés
all_filtered_polygons = gpd.GeoDataFrame(columns=['CODE_12', 'geometry'], geometry='geometry', crs=code_postal_gdf.crs)

# Lister tous les fichiers shapefile des régions
region_files = [f for f in os.listdir(clc_dir) if f.endswith('.shp')]
print(f"Nombre de fichiers de régions trouvés: {len(region_files)}")

# Traiter chaque fichier de région et les fusionner
for i, region_file in enumerate(region_files):
    print(f"Traitement du fichier {i+1}/{len(region_files)}: {region_file}")
    region_path = os.path.join(clc_dir, region_file)
    
    try:
        # Charger le fichier shapefile de la région
        region_gdf = gpd.read_file(region_path)
        
        # Filtrer les polygones selon les codes demandés
        filtered_gdf = region_gdf[region_gdf['CODE_12'].astype(str).isin(code_weights.keys())].copy()
        
        if len(filtered_gdf) > 0:
            print(f"  {len(filtered_gdf)} polygones trouvés avec les codes requis")
            
            # S'assurer que les deux GeoDataFrames utilisent le même CRS
            if filtered_gdf.crs != code_postal_gdf.crs:
                filtered_gdf = filtered_gdf.to_crs(code_postal_gdf.crs)
            
            # Garder uniquement les colonnes nécessaires
            filtered_gdf = filtered_gdf[['CODE_12', 'geometry']]
            
            # Ajouter à notre collection
            all_filtered_polygons = pd.concat([all_filtered_polygons, filtered_gdf])
        else:
            print(f"  Aucun polygone trouvé avec les codes requis dans ce fichier")
    except Exception as e:
        print(f"Erreur lors du traitement du fichier {region_file}: {e}")

# Réinitialiser l'index après la concaténation
all_filtered_polygons.reset_index(drop=True, inplace=True)

print(f"Nombre total de polygones filtrés: {len(all_filtered_polygons)}")

# Si nous avons des polygones filtrés, procéder aux calculs d'intersection
if len(all_filtered_polygons) > 0:
    print("Calcul des intersections et des scores...")
    
    # Calculer les intersections entre les codes postaux et les polygones filtrés
    # Utiliser overlay pour calculer les intersections
    intersections = gpd.overlay(code_postal_gdf[['ID', 'cp_area', 'geometry']], 
                                all_filtered_polygons, 
                                how='intersection')
    
    # Calculer l'aire des intersections
    intersections['intersection_area'] = intersections.geometry.area
    
    # Calculer la proportion de l'intersection par rapport à l'aire du code postal
    intersections['proportion'] = intersections['intersection_area'] / intersections['cp_area']
    
    # Appliquer les coefficients selon le code
    intersections['coefficient'] = intersections['CODE_12'].apply(lambda x: code_weights[str(x)])
    
    # Calculer la contribution pondérée
    intersections['contribution'] = intersections['proportion'] * intersections['coefficient']
    
    # Agréger les contributions par code postal
    score_by_cp = intersections.groupby('ID')['contribution'].sum().reset_index()
    
    # Fusionner avec le DataFrame de résultats
    result_df = result_df.merge(score_by_cp, on='ID', how='left')
    result_df['SCORE'] = result_df['contribution'].fillna(0)
    result_df.drop(columns=['contribution'], inplace=True)
    
    # Normaliser les scores entre 0 et 1
    max_score = result_df['SCORE'].max()
    if max_score > 0:  # Éviter la division par zéro
        result_df['SCORE'] = result_df['SCORE'] / max_score

# Fusionner avec les géométries des codes postaux pour créer un GeoDataFrame final
result_gdf = code_postal_gdf[['ID', 'geometry']].merge(result_df, on='ID')

# Enregistrer les résultats
output_path = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/IncendieFeuxForêts/resultats_feux_clc.shp"
result_gdf.to_file(output_path)

print(f"Traitement terminé. Résultats enregistrés dans {output_path}")

# Créer un fichier CSV avec les résultats également
csv_output_path = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/IncendieFeuxForêts/resultats_feux_clc.csv"
result_df.to_csv(csv_output_path, index=False)
print(f"Résultats également enregistrés en CSV dans {csv_output_path}") 