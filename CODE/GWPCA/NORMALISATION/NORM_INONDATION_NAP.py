import pandas as pd
import geopandas as gpd
import os
import glob
from tqdm import tqdm

# Chemins des fichiers et dossiers
base_path = "/Users/noa/Downloads/Inondation/INO.1.NAP/NAP"
output_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Inondation/INO_1_NAP"
codes_postaux_shp = "/Users/noa/Desktop/PRISM/Data/MISC/Codes postaux - Communes/codes_postaux_region.shp"

# Créer le dossier de sortie s'il n'existe pas
os.makedirs(output_dir, exist_ok=True)

# Chercher tous les dossiers de départements
print("Recherche des dossiers départementaux...")
dept_folders = [f for f in os.listdir(base_path) if os.path.isdir(os.path.join(base_path, f))]
print(f"Nombre de dossiers départementaux trouvés: {len(dept_folders)}")

# Fonction pour traiter un shapefile
def process_shapefile(file_path):
    try:
        # Charger le shapefile
        gdf = gpd.read_file(file_path)
        
        # Vérifier si la colonne 'CLASSE' existe
        if 'CLASSE' not in gdf.columns:
            print(f"Avertissement: La colonne 'CLASSE' n'existe pas dans {file_path}")
            return None
        
        # Créer la colonne 'CLASSE_NUM' selon les règles spécifiées
        gdf['CLASSE_NUM'] = 0  # Valeur par défaut
        
        # Cas 1: "Zones potentiellement sujettes aux débordements de nappe" -> 1
        mask1 = gdf['CLASSE'] == "Zones potentiellement sujettes aux débordements de nappe"
        gdf.loc[mask1, 'CLASSE_NUM'] = 1
        
        # Cas 2: "Zones potentiellement sujettes aux inondations de cave" -> 1
        mask2 = gdf['CLASSE'] == "Zones potentiellement sujettes aux inondations de cave"
        gdf.loc[mask2, 'CLASSE_NUM'] = 1
        
        # Cas 3: "Pas de débordement de nappe ni d'inondation de cave" -> 0 (déjà fait par défaut)
        
        return gdf
    except Exception as e:
        print(f"Erreur lors du traitement de {file_path}: {e}")
        return None

# Liste pour stocker les GeoDataFrames de chaque département
all_gdfs = []

# Traiter chaque département
print("Traitement des shapefiles par département...")
for dept_folder in tqdm(dept_folders):
    # Chercher le fichier shapefile dans le sous-dossier 'VECTEUR'
    vecteur_dir = os.path.join(base_path, dept_folder, "VECTEUR")
    
    if not os.path.exists(vecteur_dir):
        print(f"Avertissement: Le dossier VECTEUR n'existe pas pour {dept_folder}")
        continue
    
    # Chercher le fichier shapefile
    shp_files = glob.glob(os.path.join(vecteur_dir, "*.shp"))
    
    if not shp_files:
        print(f"Avertissement: Aucun fichier shapefile trouvé dans {vecteur_dir}")
        continue
    
    # Utiliser le premier fichier shapefile trouvé (généralement Re_Nappe_fr.shp)
    shp_file = shp_files[0]
    
    # Traiter le shapefile
    gdf = process_shapefile(shp_file)
    
    if gdf is not None:
        # Ajouter le nom du département comme attribut
        gdf['DEPARTEMENT'] = dept_folder
        all_gdfs.append(gdf)
        print(f"Département {dept_folder} traité avec succès: {len(gdf)} entités")

# Fusionner tous les GeoDataFrames
print("Fusion de tous les shapefiles...")
if all_gdfs:
    merged_gdf = pd.concat(all_gdfs, ignore_index=True)
    print(f"Nombre total d'entités après fusion: {len(merged_gdf)}")
    
    # Sauvegarder le fichier fusionné
    output_merged = os.path.join(output_dir, "inondation_nappe_france.gpkg")
    print(f"Enregistrement du fichier fusionné dans {output_merged}...")
    merged_gdf.to_file(output_merged, driver="GPKG")
    print("Fichier fusionné enregistré avec succès.")
else:
    print("Aucun shapefile valide trouvé. Impossible de créer le fichier fusionné.")
    exit(1)

# Charger le shapefile des codes postaux
print("Chargement du shapefile des codes postaux...")
cp_gdf = gpd.read_file(codes_postaux_shp)
print(f"Nombre de polygones de codes postaux: {len(cp_gdf)}")

# Vérifier et harmoniser les systèmes de coordonnées
print(f"CRS du fichier fusionné: {merged_gdf.crs}")
print(f"CRS du fichier des codes postaux: {cp_gdf.crs}")

# Reprojeter si nécessaire
if merged_gdf.crs != cp_gdf.crs:
    print("Reprojection des données...")
    merged_gdf = merged_gdf.to_crs(cp_gdf.crs)

# Calculer le risque par code postal
print("Calcul du risque d'inondation par nappe par code postal...")

# Initialiser le GeoDataFrame pour les résultats
postal_risk_gdf = cp_gdf.copy()
postal_risk_gdf = postal_risk_gdf.drop(columns=['MEN2010', 'POP2010', 'SURF'], errors='ignore')

# Calcul de l'intersection pour chaque code postal
results = []

for idx, cp_row in tqdm(postal_risk_gdf.iterrows(), total=len(postal_risk_gdf)):
    cp_geom = cp_row.geometry
    cp_id = cp_row['ID']
    
    # Trouver toutes les zones qui intersectent avec ce code postal
    intersect_mask = merged_gdf.intersects(cp_geom)
    intersect_gdf = merged_gdf[intersect_mask]
    
    if len(intersect_gdf) == 0:
        # Pas d'intersection, on met un risque de 0
        results.append({'ID': cp_id, 'RISK_NAP': 0.0})
        continue
    
    # Calculer l'intersection
    intersection_gdf = gpd.overlay(
        gpd.GeoDataFrame([cp_row], geometry='geometry', crs=cp_gdf.crs),
        intersect_gdf,
        how='intersection'
    )
    
    if len(intersection_gdf) == 0:
        results.append({'ID': cp_id, 'RISK_NAP': 0.0})
        continue
    
    # Calculer les aires
    intersection_gdf['area'] = intersection_gdf.geometry.area
    total_area = cp_geom.area
    
    # Calculer l'aire des zones à risque (CLASSE_NUM = 1)
    risk_area = intersection_gdf[intersection_gdf['CLASSE_NUM'] == 1]['area'].sum()
    
    # Calculer le pourcentage de la superficie à risque
    risk_pct = risk_area / total_area if total_area > 0 else 0.0
    
    results.append({'ID': cp_id, 'RISK_NAP': risk_pct})

# Créer un DataFrame avec les résultats
risk_df = pd.DataFrame(results)

# Joindre les résultats avec le GeoDataFrame des codes postaux
postal_risk_gdf = postal_risk_gdf.merge(risk_df, on='ID', how='left')

# Remplacer les valeurs manquantes par 0
postal_risk_gdf['RISK_NAP'] = postal_risk_gdf['RISK_NAP'].fillna(0.0)

# Sauvegarder le résultat
output_postal = os.path.join(output_dir, "risque_inondation_nappe_par_cp.gpkg")
print(f"Enregistrement des résultats par code postal dans {output_postal}...")
postal_risk_gdf.to_file(output_postal, driver="GPKG")

print("Statistiques sur le risque d'inondation par nappe par code postal:")
print(f"Nombre total de codes postaux: {len(postal_risk_gdf)}")
print(f"Nombre de codes postaux avec un risque > 0: {(postal_risk_gdf['RISK_NAP'] > 0).sum()}")
print(f"Risque maximum: {postal_risk_gdf['RISK_NAP'].max():.4f}")
print(f"Risque moyen: {postal_risk_gdf['RISK_NAP'].mean():.4f}")

print("Traitement terminé avec succès!")
