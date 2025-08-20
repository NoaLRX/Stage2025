import pandas as pd
import geopandas as gpd
import rasterio
from rasterio.mask import mask
import os
import numpy as np
from shapely.geometry import mapping
import glob
import warnings
warnings.filterwarnings('ignore')

# Chemins des fichiers
elevation_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Elevation_Mer"
codes_postaux_shp = "/Users/noa/Desktop/PRISM/Data/MISC/Codes postaux - Communes/codes_postaux_region.shp"
jointure_csv = "/Users/noa/Desktop/PRISM/Data/MISC/CONVERTED/test.csv"
output_gpkg = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Elevation_Mer/elevation_score_cp.gpkg"

# Vérifier que les fichiers et dossiers existent
if not os.path.exists(elevation_dir):
    raise FileNotFoundError(f"Le dossier {elevation_dir} n'existe pas")
if not os.path.exists(codes_postaux_shp):
    raise FileNotFoundError(f"Le fichier {codes_postaux_shp} n'existe pas")
if not os.path.exists(jointure_csv):
    raise FileNotFoundError(f"Le fichier {jointure_csv} n'existe pas")

# 1. Charger le shapefile des codes postaux
print("Chargement du shapefile des codes postaux...")
cp_gdf = gpd.read_file(codes_postaux_shp)
print(f"Nombre de polygones de codes postaux: {len(cp_gdf)}")
print(f"Colonnes du fichier des codes postaux: {cp_gdf.columns.tolist()}")

# Vérifier que la colonne ID existe (contenant le code postal)
if 'ID' not in cp_gdf.columns:
    raise ValueError("La colonne 'ID' n'existe pas dans le shapefile des codes postaux")

# S'assurer que les IDs sont au format numérique
cp_gdf['ID'] = cp_gdf['ID'].astype(int)

# 2. Créer une liste des fichiers raster d'élévation
print("\nRecherche des fichiers raster d'élévation...")
elevation_files = glob.glob(os.path.join(elevation_dir, "*.hgt"))
print(f"Nombre de fichiers raster trouvés: {len(elevation_files)}")

if len(elevation_files) == 0:
    raise FileNotFoundError(f"Aucun fichier raster (.hgt) trouvé dans {elevation_dir}")

# Fonction pour appliquer la règle de filtrage des scores
def appliquer_score_elevation(elevation):
    if elevation <= 1:
        return 100
    elif elevation > 2 and elevation < 3:
        return 25
    else:
        return 0

# 3. Calculer l'élévation par code postal
print("\nCalcul de l'élévation par code postal...")
results = []

# Convertir le CRS du GeoDataFrame pour qu'il corresponde aux données raster (WGS84)
cp_gdf_wgs84 = cp_gdf.to_crs("EPSG:4326")

# Traiter chaque code postal
for idx, cp_row in cp_gdf_wgs84.iterrows():
    if idx % 100 == 0:
        print(f"Traitement du code postal {idx+1}/{len(cp_gdf_wgs84)}")
    
    code_postal = cp_row['ID']
    geometry = cp_row['geometry']
    
    if geometry is None or geometry.is_empty:
        continue
    
    # Variables pour stocker les statistiques d'élévation
    elevations = []
    
    # Créer un masque pour le découpage du raster
    geom = [mapping(geometry)]
    
    # Parcourir tous les fichiers raster et extraire les données pour ce code postal
    for raster_file in elevation_files:
        try:
            with rasterio.open(raster_file) as src:
                # Vérifier si le raster et le polygone se chevauchent
                raster_bounds = src.bounds
                minx, miny, maxx, maxy = geometry.bounds
                
                # Si pas de chevauchement, passer au raster suivant
                if (maxx < raster_bounds.left or minx > raster_bounds.right or
                    maxy < raster_bounds.bottom or miny > raster_bounds.top):
                    continue
                
                # Découper le raster selon la géométrie du code postal
                # Utiliser un masque à valeur fixe au lieu de NaN pour éviter les problèmes avec int16
                masked, mask_transform = mask(src, geom, crop=True, all_touched=True, nodata=-32768)
                
                # Convertir en float pour pouvoir utiliser NaN
                masked_float = masked.astype(np.float32)
                
                # Remplacer les valeurs nodata par NaN
                masked_float[masked == -32768] = np.nan
                
                # Extraire les valeurs d'élévation (ignorer les valeurs NoData)
                valid_data = masked_float[0][~np.isnan(masked_float[0])]
                
                # Ajouter les valeurs valides à notre liste
                if valid_data.size > 0:
                    elevations.extend(valid_data.flatten())
        
        except Exception as e:
            print(f"Erreur lors du traitement du fichier {raster_file} pour le code postal {code_postal}: {e}")
            continue
    
    # Si nous avons des données d'élévation pour ce code postal
    if elevations:
        elevations = np.array(elevations)
        
        # Calculer les statistiques d'élévation
        min_elevation = np.min(elevations)
        max_elevation = np.max(elevations)
        mean_elevation = np.mean(elevations)
        median_elevation = np.median(elevations)
        std_elevation = np.std(elevations)
        
        # Calculer les pourcentages pour les seuils du score
        pct_below_1m = np.sum(elevations <= 1) / len(elevations) * 100
        pct_1m_to_2m = np.sum((elevations > 1) & (elevations <= 2)) / len(elevations) * 100
        pct_2m_to_3m = np.sum((elevations > 2) & (elevations < 3)) / len(elevations) * 100
        pct_above_3m = np.sum(elevations >= 3) / len(elevations) * 100
        
        # Appliquer le score à chaque point d'élévation
        scores = np.zeros_like(elevations)
        scores[elevations <= 1] = 100
        scores[(elevations > 2) & (elevations < 3)] = 25
        # Les autres restent à 0
        
        # Calculer le score moyen pondéré par la surface
        # C'est équivalent à la somme des pourcentages multipliés par les scores correspondants
        score = (100 * (pct_below_1m/100)) + (25 * (pct_2m_to_3m/100))
        
        # Ajouter les résultats
        results.append({
            'CODE_POST': code_postal,
            'MIN_ELEV': min_elevation,
            'MAX_ELEV': max_elevation,
            'MEAN_ELEV': mean_elevation,
            'MEDIAN_ELEV': median_elevation,
            'STD_ELEV': std_elevation,
            'PCT_BELOW_1M': pct_below_1m,
            'PCT_1M_TO_2M': pct_1m_to_2m,
            'PCT_2M_TO_3M': pct_2m_to_3m,
            'PCT_ABOVE_3M': pct_above_3m,
            'SCORE': score,
            'DATA_COUNT': len(elevations)
        })
    else:
        # Ajouter une entrée avec des valeurs 0 au lieu de NaN
        results.append({
            'CODE_POST': code_postal,
            'MIN_ELEV': 0,
            'MAX_ELEV': 0,
            'MEAN_ELEV': 0,
            'MEDIAN_ELEV': 0,
            'STD_ELEV': 0,
            'PCT_BELOW_1M': 0,
            'PCT_1M_TO_2M': 0,
            'PCT_2M_TO_3M': 0,
            'PCT_ABOVE_3M': 0,
            'SCORE': 0,
            'DATA_COUNT': 0
        })

# 4. Créer un DataFrame avec les résultats
print("\nCréation du DataFrame avec les résultats...")
df_results = pd.DataFrame(results)
print(f"Nombre de codes postaux avec des données d'élévation: {len(df_results[df_results['DATA_COUNT'] > 0])}")

# 5. Fusionner avec le shapefile original
print("\nFusion avec le shapefile original...")
gdf_final = cp_gdf.merge(df_results, left_on='ID', right_on='CODE_POST', how='left')

# Remplacer toutes les valeurs NaN par 0 dans les colonnes numériques
numeric_columns = ['MIN_ELEV', 'MAX_ELEV', 'MEAN_ELEV', 'MEDIAN_ELEV', 'STD_ELEV', 
                  'PCT_BELOW_1M', 'PCT_1M_TO_2M', 'PCT_2M_TO_3M', 'PCT_ABOVE_3M',
                  'SCORE', 'DATA_COUNT']
gdf_final[numeric_columns] = gdf_final[numeric_columns].fillna(0)

# Vérifier les correspondances manquantes
nb_sans_data = (gdf_final['DATA_COUNT'] == 0).sum()
print(f"Nombre de codes postaux sans données d'élévation: {nb_sans_data}")

# 6. Catégoriser l'exposition aux risques liés à l'élévation du niveau de la mer
def categoriser_risque_elevation(row):
    if row['DATA_COUNT'] == 0:
        return "Pas de données"
    if row['SCORE'] >= 75:
        return "Très haute exposition"
    elif row['SCORE'] >= 50:
        return "Haute exposition"
    elif row['SCORE'] >= 25:
        return "Exposition modérée"
    elif row['SCORE'] > 0:
        return "Faible exposition"
    else:
        return "Très faible exposition"

print("\nCatégorisation des risques liés à l'élévation...")
gdf_final['RISQUE_ELEV'] = gdf_final.apply(categoriser_risque_elevation, axis=1)

# Afficher la répartition des catégories de risque
print("\nRépartition des catégories de risque:")
print(gdf_final['RISQUE_ELEV'].value_counts())

# 7. Sauvegarder le résultat au format GeoPackage
print(f"\nEnregistrement du résultat dans {output_gpkg}...")
gdf_final.to_file(output_gpkg, driver="GPKG", layer="elevation_scores")

print("\nStatistiques sur l'élévation par code postal:")
print(f"Nombre total de codes postaux: {len(gdf_final)}")
print(f"Nombre de codes postaux avec des données d'élévation: {len(gdf_final[gdf_final['DATA_COUNT'] > 0])}")
print(f"Élévation moyenne minimale: {gdf_final[gdf_final['DATA_COUNT'] > 0]['MIN_ELEV'].min():.2f} mètres")
print(f"Élévation moyenne maximale: {gdf_final[gdf_final['DATA_COUNT'] > 0]['MAX_ELEV'].max():.2f} mètres")
print(f"Élévation moyenne nationale: {gdf_final[gdf_final['DATA_COUNT'] > 0]['MEAN_ELEV'].mean():.2f} mètres")

# Statistiques sur les scores
print("\nStatistiques sur les scores:")
print(f"Score moyen: {gdf_final[gdf_final['DATA_COUNT'] > 0]['SCORE'].mean():.2f}")
print(f"Score médian: {gdf_final[gdf_final['DATA_COUNT'] > 0]['SCORE'].median():.2f}")
print(f"Score maximal: {gdf_final[gdf_final['DATA_COUNT'] > 0]['SCORE'].max():.2f}")

print("\nTraitement terminé avec succès!")
