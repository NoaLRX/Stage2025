import geopandas as gpd
import pandas as pd
import numpy as np
import rasterio
from rasterio.mask import mask
import os
import warnings
from tqdm import tqdm
warnings.filterwarnings('ignore')

print("Début du traitement d'agrégation des données d'imperméabilisation (IMP) par code postal...")

# Définition des chemins des fichiers
impermeabilisation_path = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Inondation/INO_1_IMP/IMD_2018_010m_03035_V2_0.tif"
codes_postaux_path = "/Users/noa/Desktop/PRISM/Data/MISC/Codes postaux - Communes/codes_postaux_region.shp"
output_path = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Inondation/INO_1_IMP/impermeabilisation_codes_postaux.gpkg"

# Vérification de l'existence des fichiers source
for file_path in [impermeabilisation_path, codes_postaux_path]:
    if not os.path.exists(file_path):
        raise FileNotFoundError(f"Le fichier {file_path} n'existe pas")

# 1. Lecture des données
print("Lecture des données...")
# Lire le raster d'imperméabilisation
with rasterio.open(impermeabilisation_path) as src:
    print(f"Raster d'imperméabilisation chargé: {src.width}x{src.height} pixels")
    print(f"Système de coordonnées: {src.crs}")
    raster_crs = src.crs

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

# Convertir les codes postaux au même CRS que le raster
gdf_codes_postaux = gdf_codes_postaux.to_crs(raster_crs)
print(f"CRS des codes postaux après conversion: {gdf_codes_postaux.crs}")

# Ajouter une colonne d'aire pour les codes postaux
gdf_codes_postaux['cp_area'] = gdf_codes_postaux.geometry.area

# 3. Calcul du score d'imperméabilisation pour chaque code postal
print("Calcul de l'imperméabilisation par code postal...")
print("Ce processus peut prendre du temps...")

# Préparer la liste des résultats
results = []

# Traiter chaque code postal
with rasterio.open(impermeabilisation_path) as src:
    for idx, row in tqdm(gdf_codes_postaux.iterrows(), total=len(gdf_codes_postaux)):
        try:
            # Extraire le polygone du code postal
            geom = [row.geometry.__geo_interface__]
            
            # Masquer le raster avec le polygone du code postal
            out_image, out_transform = mask(src, geom, crop=True, nodata=0)
            
            # Calculer les statistiques
            # La valeur du pixel dans ce raster représente le pourcentage d'imperméabilisation (0-100)
            valid_pixels = out_image[out_image > 0]
            
            if len(valid_pixels) > 0:
                # Calculer le pourcentage moyen d'imperméabilisation
                mean_value = np.mean(valid_pixels)
                
                # Ajouter aux résultats
                results.append({
                    'CODE_POST': row['CODE_POST'],
                    'cp_area': row['cp_area'],
                    'imp_mean': mean_value,
                    'valid_pixels': len(valid_pixels)
                })
            else:
                # Aucun pixel valide dans ce code postal
                results.append({
                    'CODE_POST': row['CODE_POST'],
                    'cp_area': row['cp_area'],
                    'imp_mean': 0,
                    'valid_pixels': 0
                })
        except Exception as e:
            print(f"Erreur pour le code postal {row['CODE_POST']}: {e}")
            # En cas d'erreur, ajouter un résultat nul
            results.append({
                'CODE_POST': row['CODE_POST'],
                'cp_area': row['cp_area'],
                'imp_mean': 0,
                'valid_pixels': 0
            })

# Créer un DataFrame avec les résultats
result_df = pd.DataFrame(results)
print(f"Résultats calculés pour {len(result_df)} codes postaux")

# 4. Normalisation du score
print("Normalisation des scores...")

# Renommer la colonne de score
result_df['score_impermeabilisation'] = result_df['imp_mean']

# Normalisation du score entre 0 et 1
if len(result_df) > 0 and result_df['score_impermeabilisation'].max() > 0:
    # Comme les valeurs sont déjà en pourcentage (0-100), diviser par 100
    result_df['score_normalise'] = result_df['score_impermeabilisation'] / 100
    
    # Limiter à 1 au maximum
    result_df['score_normalise'] = result_df['score_normalise'].clip(0, 1)
else:
    print("Attention: Aucun score d'imperméabilisation positif trouvé!")
    result_df['score_normalise'] = 0

# 5. Création du GeoDataFrame final
print("Création du GeoDataFrame final...")

# Créer un GeoDataFrame final avec tous les codes postaux
gdf_final = gdf_codes_postaux[['CODE_POST', 'geometry', 'cp_area']].copy()

# Fusionner avec les résultats
gdf_final = pd.merge(
    gdf_final,
    result_df[['CODE_POST', 'score_impermeabilisation', 'score_normalise']],
    on='CODE_POST',
    how='left'
)

# Remplacer les valeurs NaN par 0
gdf_final['score_impermeabilisation'] = gdf_final['score_impermeabilisation'].fillna(0)
gdf_final['score_normalise'] = gdf_final['score_normalise'].fillna(0)

print(f"GeoDataFrame final: {len(gdf_final)} codes postaux")

# 6. Afficher des statistiques
print("\nStatistiques sur le score d'imperméabilisation:")
print(gdf_final['score_impermeabilisation'].describe())

print("\nNombre de codes postaux par catégorie d'imperméabilisation:")
risk_categories = pd.cut(gdf_final['score_normalise'], 
                         bins=[0, 0.2, 0.4, 0.6, 0.8, 1.0],
                         labels=['Très faible', 'Faible', 'Moyen', 'Élevé', 'Très élevé'])
print(risk_categories.value_counts().sort_index())

# 7. Enregistrement des résultats
print(f"Enregistrement des résultats dans {output_path}...")
gdf_final.to_file(output_path, driver="GPKG")

print("Traitement terminé avec succès!")
