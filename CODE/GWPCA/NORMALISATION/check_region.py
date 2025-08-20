import geopandas as gpd
import os

# Chemin vers le dossier des régions
clc_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/IncendieFeuxForêts/FF_1_CLC"

# Prendre le premier fichier shapefile
region_files = [f for f in os.listdir(clc_dir) if f.endswith('.shp')]
if region_files:
    region_path = os.path.join(clc_dir, region_files[0])
    print(f"Vérification du fichier: {region_files[0]}")
    
    # Charger le fichier
    region_gdf = gpd.read_file(region_path)
    
    # Afficher les colonnes
    print("Colonnes disponibles:")
    print(region_gdf.columns.tolist())
    
    # Afficher les premières lignes
    print("\nAperçu des données:")
    print(region_gdf.head())
    
    # Vérifier si CODE_12 existe et afficher ses valeurs uniques
    if 'CODE_12' in region_gdf.columns:
        print("\nValeurs uniques de CODE_12:")
        print(region_gdf['CODE_12'].unique())
    else:
        print("\nLa colonne CODE_12 n'existe pas. Voici les colonnes alternatives possibles:")
        for col in region_gdf.columns:
            if 'CODE' in col or 'code' in col:
                print(f"- {col}")
else:
    print("Aucun fichier shapefile trouvé.") 