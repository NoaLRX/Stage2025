import geopandas as gpd
import pandas as pd
import os

# Chemins des fichiers shapefile
communes_shp = "/Users/noa/Desktop/PRISM/Data/MISC/Autres polygones administratifs/COMMUNE.shp"
codes_postaux_shp = "/Users/noa/Desktop/PRISM/Data/MISC/Codes postaux - Communes/codes_postaux_region.shp"

# Vérifier que les fichiers existent
if not os.path.exists(communes_shp):
    raise FileNotFoundError(f"Le fichier {communes_shp} n'existe pas")
if not os.path.exists(codes_postaux_shp):
    raise FileNotFoundError(f"Le fichier {codes_postaux_shp} n'existe pas")

# Lire les fichiers shapefile
print("Lecture du fichier des communes...")
communes_gdf = gpd.read_file(communes_shp)
print(f"Colonnes dans le fichier communes: {communes_gdf.columns.tolist()}")
print(f"Nombre de communes: {len(communes_gdf)}")

print("\nLecture du fichier des codes postaux...")
codes_postaux_gdf = gpd.read_file(codes_postaux_shp)
print(f"Colonnes dans le fichier codes postaux: {codes_postaux_gdf.columns.tolist()}")
print(f"Nombre de codes postaux: {len(codes_postaux_gdf)}")

# S'assurer que les deux GeoDataFrames utilisent le même système de coordonnées
print("\nVérification des systèmes de coordonnées...")
print(f"CRS des communes: {communes_gdf.crs}")
print(f"CRS des codes postaux: {codes_postaux_gdf.crs}")

# Si nécessaire, transformer l'un des GeoDataFrames au CRS de l'autre
if communes_gdf.crs != codes_postaux_gdf.crs and communes_gdf.crs is not None and codes_postaux_gdf.crs is not None:
    print("Transformation des systèmes de coordonnées pour qu'ils correspondent...")
    codes_postaux_gdf = codes_postaux_gdf.to_crs(communes_gdf.crs)

# Approche simplifiée: on utilise le centroïde de chaque commune
print("\nCalcul des centroïdes des communes...")
# Créer une copie du GeoDataFrame des communes
communes_centroids = communes_gdf.copy()
# Remplacer les géométries par les centroïdes
communes_centroids['geometry'] = communes_centroids['geometry'].centroid

# Effectuer la jointure spatiale avec les centroïdes
print("\nEffectuer la jointure spatiale avec les centroïdes (plus rapide)...")
joined_gdf = gpd.sjoin(communes_centroids, codes_postaux_gdf, how="left", predicate="within")

# Afficher les colonnes après la jointure pour vérification
print(f"\nColonnes après la jointure: {joined_gdf.columns.tolist()}")

# Restaurer les géométries originales des communes
print("\nRestauration des géométries originales des communes...")
# Créer un dictionnaire pour mapper INSEE_COM aux géométries originales
geom_dict = dict(zip(communes_gdf["INSEE_COM"], communes_gdf.geometry))
# Remplacer les centroïdes par les géométries originales
joined_gdf["geometry"] = joined_gdf["INSEE_COM"].map(geom_dict)

# D'après l'output, les colonnes sont INSEE_COM, NOM, ..., ID_right, LIB, DEP, ...
# Sélectionner et renommer les colonnes selon les spécifications
result_gdf = joined_gdf[["INSEE_COM", "NOM", "ID_right", "LIB", "geometry"]]
result_gdf = result_gdf.rename(columns={
    "INSEE_COM": "Code_commune",
    "NOM": "Nom_commune", 
    "ID_right": "Code_postal",
    "LIB": "Nom_postal"
})

# Enregistrer le résultat
output_path = "/Users/noa/Desktop/PRISM/Data/MISC/communes_codes_postaux.shp"
print(f"\nEnregistrement du résultat dans {output_path}...")
result_gdf.to_file(output_path)

print(f"Terminé! {len(result_gdf)} communes traitées.")
print(f"Nombre de communes sans code postal trouvé: {result_gdf['Code_postal'].isna().sum()}")
