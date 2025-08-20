import os
import pandas as pd
import geopandas as gpd
from shapely.geometry import Point
import glob

# Chemins
input_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Mouvement2Terrain/MVT_1_MVT"
output_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Mouvement2Terrain/MVT_1_MVT"
postal_codes_path = "/Users/noa/Desktop/PRISM/Data/MISC/Codes postaux - Communes/codes_postaux_region.shp"

# Créer le dossier de sortie s'il n'existe pas
os.makedirs(output_dir, exist_ok=True)

def process_csv_files():
    # Récupérer tous les fichiers CSV dans le répertoire
    csv_files = glob.glob(os.path.join(input_dir, "*.csv"))
    print(f"Trouvé {len(csv_files)} fichiers CSV à traiter.")
    
    # Initialiser une liste vide pour stocker tous les enregistrements valides
    all_data = []
    
    # Traiter chaque fichier CSV
    for csv_file in csv_files:
        try:
            print(f"Traitement de {os.path.basename(csv_file)}...")
            # Lire le fichier CSV
            df = pd.read_csv(csv_file, sep=';', encoding='utf-8', low_memory=False)
            
            # Ignorer si le fichier est vide ou ne contient pas les colonnes requises
            if df.empty or 'xsaisi' not in df.columns or 'ysaisi' not in df.columns or 'idMvt' not in df.columns:
                print(f"Ignorer {os.path.basename(csv_file)} - Colonnes requises manquantes")
                continue
            
            # Sélectionner uniquement les colonnes nécessaires
            df = df[['idMvt', 'typeMvt', 'libelleType', 'xsaisi', 'ysaisi', 'commune', 'dateDebut']]
            
            # Convertir les coordonnées en numérique, en supprimant les lignes avec des coordonnées invalides
            df['xsaisi'] = pd.to_numeric(df['xsaisi'], errors='coerce')
            df['ysaisi'] = pd.to_numeric(df['ysaisi'], errors='coerce')
            df = df.dropna(subset=['xsaisi', 'ysaisi'])
            
            all_data.append(df)
            
        except Exception as e:
            print(f"Erreur lors du traitement de {os.path.basename(csv_file)}: {str(e)}")
    
    # Combiner tous les dataframes
    if all_data:
        combined_df = pd.concat(all_data, ignore_index=True)
        print(f"Les données combinées contiennent {len(combined_df)} enregistrements valides.")
        return combined_df
    else:
        print("Aucune donnée valide trouvée dans les fichiers CSV.")
        return None

def create_movement_geopackage(df):
    # Créer un GeoDataFrame avec la géométrie Point à partir des coordonnées
    # EPSG:27572 est la projection correcte pour les coordonnées "Lambert II étendu"
    geometry = [Point(x, y) for x, y in zip(df['xsaisi'], df['ysaisi'])]
    gdf = gpd.GeoDataFrame(df, geometry=geometry, crs="EPSG:27572")
    
    # Supprimer les colonnes de coordonnées d'origine
    gdf = gdf.drop(['xsaisi', 'ysaisi'], axis=1)
    
    # Reprojeter vers un CRS plus standard (EPSG:4326 - WGS84)
    gdf = gdf.to_crs("EPSG:4326")
    
    # Enregistrer en tant que geopackage
    output_gpkg = os.path.join(output_dir, "mouvements_terrain.gpkg")
    gdf.to_file(output_gpkg, driver="GPKG")
    print(f"Geopackage enregistré dans {output_gpkg}")
    
    return gdf

def aggregate_by_postal_code(movements_gdf):
    print("Comptage des mouvements par code postal...")
    
    # Lire le fichier shapefile des codes postaux
    postal_codes = gpd.read_file(postal_codes_path)
    
    # S'assurer que les deux GeoDataFrames sont dans le même CRS
    postal_codes = postal_codes.to_crs(movements_gdf.crs)
    
    print(f"Jointure spatiale entre {len(movements_gdf)} mouvements et {len(postal_codes)} zones de codes postaux")
    
    # Jointure spatiale pour compter les mouvements dans chaque zone de code postal
    joined = gpd.sjoin(movements_gdf, postal_codes, how="inner", predicate="within")
    
    # Compter les mouvements par code postal en utilisant 'ID' comme colonne de code postal
    movement_counts = joined.groupby('ID')[['idMvt']].count()
    movement_counts.columns = ['movement_count']
    
    # Fusionner les comptages avec les données de code postal
    postal_codes_with_counts = postal_codes.set_index('ID')
    postal_codes_with_counts = postal_codes_with_counts.join(movement_counts)
    postal_codes_with_counts = postal_codes_with_counts.reset_index()
    
    # Remplacer les comptages NaN par 0
    postal_codes_with_counts['movement_count'] = postal_codes_with_counts['movement_count'].fillna(0).astype(int)
    
    # Enregistrer le résultat en tant que geopackage
    output_counts = os.path.join(output_dir, "mouvements_par_code_postal.gpkg")
    postal_codes_with_counts.to_file(output_counts, driver="GPKG")
    print(f"Comptage des mouvements par code postal enregistré dans {output_counts}")
    
    return postal_codes_with_counts

def main():
    print("Démarrage du traitement des données de mouvements de terrain...")
    
    # Traiter tous les fichiers CSV et les combiner
    data = process_csv_files()
    if data is None or data.empty:
        print("Aucune donnée à traiter. Sortie.")
        return
    
    # Créer un geopackage avec les données combinées
    movement_gdf = create_movement_geopackage(data)
    
    # Agréger les mouvements par code postal
    aggregate_by_postal_code(movement_gdf)
    
    print("\nTraitement terminé !")

if __name__ == "__main__":
    main()
