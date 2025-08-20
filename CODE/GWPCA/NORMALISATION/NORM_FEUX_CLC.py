import os
import pandas as pd
import geopandas as gpd
from shapely.geometry import Polygon
import numpy as np

# Définir les chemins des fichiers
clc_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/IncendieFeuxForêts/FF_1_CLC"
code_postal_path = "/Users/noa/Desktop/PRISM/Data/MISC/Codes postaux - Communes/codes_postaux_region.shp"

# Définir les codes CLC à conserver et leurs coefficients
code_weights = {
    "312": 1.0,
    "313": 1.0, 
    "311": 0.9,
    "324": 0.8,
    "322": 0.7,
    "323": 0.7
}

# Charger les données des codes postaux
print("Chargement des codes postaux...")
code_postal_gdf = gpd.read_file(code_postal_path)
print(f"Nombre de codes postaux: {len(code_postal_gdf)}")

# Initialiser un dictionnaire pour stocker les résultats par code postal
results = {cp: 0 for cp in code_postal_gdf['ID'].unique()}

# Lister tous les fichiers shapefile des régions
region_files = [f for f in os.listdir(clc_dir) if f.endswith('.shp')]
print(f"Nombre de fichiers de régions trouvés: {len(region_files)}")

# Traiter chaque fichier de région
for i, region_file in enumerate(region_files):
    print(f"Traitement du fichier {i+1}/{len(region_files)}: {region_file}")
    region_path = os.path.join(clc_dir, region_file)
    
    # Charger le fichier shapefile de la région
    try:
        region_gdf = gpd.read_file(region_path)
        
        # Filtrer les polygones selon les codes demandés
        filtered_gdf = region_gdf[region_gdf['CODE_12'].astype(str).isin(code_weights.keys())].copy()
        
        if len(filtered_gdf) > 0:
            print(f"  {len(filtered_gdf)} polygones trouvés avec les codes requis")
            
            # S'assurer que les deux GeoDataFrames utilisent le même CRS
            if filtered_gdf.crs != code_postal_gdf.crs:
                filtered_gdf = filtered_gdf.to_crs(code_postal_gdf.crs)
            
            # Calculer l'aire de chaque polygone filtré
            filtered_gdf['area'] = filtered_gdf.geometry.area
            
            # Pour chaque code postal, calculer l'intersection avec les polygones filtrés
            for idx, cp_row in code_postal_gdf.iterrows():
                if idx % 100 == 0:
                    print(f"  Traitement du code postal {idx}/{len(code_postal_gdf)}")
                
                cp_geom = cp_row.geometry
                cp_code = cp_row['ID']
                cp_area = cp_geom.area
                
                # Calculer l'intersection pour chaque polygone filtré
                for _, forest_row in filtered_gdf.iterrows():
                    forest_geom = forest_row.geometry
                    
                    # Si les polygones se croisent
                    if cp_geom.intersects(forest_geom):
                        intersection = cp_geom.intersection(forest_geom)
                        intersection_area = intersection.area
                        
                        # Calculer la proportion de l'intersection par rapport à l'aire du code postal
                        proportion = intersection_area / cp_area
                        
                        # Appliquer le coefficient selon le code
                        code = str(forest_row['CODE_12'])
                        coefficient = code_weights[code]
                        
                        # Ajouter la contribution pondérée au résultat du code postal
                        results[cp_code] += proportion * coefficient
        else:
            print(f"  Aucun polygone trouvé avec les codes requis dans ce fichier")
    except Exception as e:
        print(f"Erreur lors du traitement du fichier {region_file}: {e}")

# Créer un DataFrame avec les résultats
result_df = pd.DataFrame({
    'ID': list(results.keys()),
    'SCORE': list(results.values())
})

# Normaliser les scores entre 0 et 1 si nécessaire
max_score = result_df['SCORE'].max()
if max_score > 0:  # Éviter la division par zéro
    result_df['SCORE'] = result_df['SCORE'] / max_score

# Fusionner avec les géométries des codes postaux pour créer un GeoDataFrame final
result_gdf = code_postal_gdf[['ID', 'geometry']].merge(result_df, on='ID')

# Enregistrer les résultats
output_path = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/IncendieFeuxForêts/resultats_feux_clc.shp"
result_gdf.to_file(output_path)

print(f"Traitement terminé. Résultats enregistrés dans {output_path}")
