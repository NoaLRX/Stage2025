import pandas as pd
import geopandas as gpd
import numpy as np
import os

# Type de référence à utiliser pour l'agrégation
TYPE_REFERENCE = "POSTAL"  # Options: "POSTAL" ou "COMMUNE"

# Chemins des fichiers
alea_argile_shp = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/SécheresseRGA/AleaRG_Fxx_L93/ExpoArgile_Fxx_L93.shp"
communes_shp = "/Users/noa/Desktop/PRISM/Data/MISC/CONVERTED/test.shp"
postal_shp = "/Users/noa/Desktop/PRISM/Data/MISC/Codes postaux - Communes/codes_postaux_region.shp"

# Sélectionner le fichier de référence selon le type choisi
if TYPE_REFERENCE == "COMMUNE":
    reference_shp = communes_shp
    join_field = 'Code_commu'
    id_field = 'Code_commu'
    name_field = 'NOM'
    print(f"Utilisation de l'agrégation par COMMUNE")
else:  # TYPE_REFERENCE == "POSTAL"
    reference_shp = postal_shp
    join_field = 'ID'
    id_field = 'ID'
    name_field = 'LIB'
    print(f"Utilisation de l'agrégation par CODE POSTAL")

# Vérifier que les fichiers existent
if not os.path.exists(alea_argile_shp):
    raise FileNotFoundError(f"Le fichier {alea_argile_shp} n'existe pas")
if not os.path.exists(reference_shp):
    raise FileNotFoundError(f"Le fichier {reference_shp} n'existe pas")

# Lire le shapefile d'aléa argile
print("\nLecture du fichier shapefile d'aléa argile...")
alea_gdf = gpd.read_file(alea_argile_shp)
print(f"Colonnes du shapefile aléa: {alea_gdf.columns.tolist()}")
print(f"Nombre de polygones d'aléa: {len(alea_gdf)}")

# Vérifier si la colonne NIVEAU existe
if 'NIVEAU' not in alea_gdf.columns:
    raise ValueError("La colonne NIVEAU n'existe pas dans le shapefile d'aléa argile")

# Afficher les valeurs uniques de NIVEAU
niveau_values = alea_gdf['NIVEAU'].unique()
print(f"Valeurs uniques de NIVEAU: {niveau_values}")

# Convertir NIVEAU en numérique si ce n'est pas déjà le cas
if not pd.api.types.is_numeric_dtype(alea_gdf['NIVEAU']):
    try:
        # Essayer de convertir directement
        alea_gdf['NIVEAU_NUM'] = pd.to_numeric(alea_gdf['NIVEAU'])
    except:
        # Si échec, faire une conversion manuelle
        niveau_map = {}
        for i, val in enumerate(sorted(niveau_values)):
            niveau_map[val] = i + 1
        print(f"Conversion de NIVEAU en valeurs numériques: {niveau_map}")
        alea_gdf['NIVEAU_NUM'] = alea_gdf['NIVEAU'].map(niveau_map)
else:
    alea_gdf['NIVEAU_NUM'] = alea_gdf['NIVEAU']

# Calculer la surface de chaque polygone d'aléa
print("\nCalcul des surfaces des polygones d'aléa...")
# Vérifier que le CRS est en mètres
if alea_gdf.crs is None:
    print("Attention: Le CRS du shapefile d'aléa n'est pas défini. Utilisation de Lambert 93 par défaut.")
    alea_gdf.crs = "EPSG:2154"  # Lambert 93
elif alea_gdf.crs.is_geographic:  # Si en degrés (latitude/longitude)
    print("Conversion des coordonnées en Lambert 93 pour le calcul des surfaces...")
    alea_gdf = alea_gdf.to_crs("EPSG:2154")  # Lambert 93

# Calculer la surface en m²
alea_gdf['surface_m2'] = alea_gdf.geometry.area

# Lire le fichier shapefile de référence (communes ou codes postaux)
print(f"\nLecture du fichier shapefile de référence ({TYPE_REFERENCE})...")
reference_gdf = gpd.read_file(reference_shp)
print(f"Colonnes du shapefile: {reference_gdf.columns.tolist()}")
print(f"Nombre d'entités dans le shapefile: {len(reference_gdf)}")

# Vérifier que le CRS du fichier de référence est compatible avec celui des aléas
if reference_gdf.crs != alea_gdf.crs:
    print(f"Conversion du CRS du fichier de référence de {reference_gdf.crs} à {alea_gdf.crs}...")
    reference_gdf = reference_gdf.to_crs(alea_gdf.crs)

# Préparer un DataFrame pour stocker les résultats
results = []

# Pour chaque entité de référence (commune ou code postal)
print("\nCalcul de la pondération des aléas par entité...")
for idx, ref_entity in reference_gdf.iterrows():
    if idx % 100 == 0:
        print(f"Traitement de l'entité {idx+1}/{len(reference_gdf)}")
    
    # Sélectionner les polygones d'aléa qui intersectent cette entité
    intersecting_aleas = alea_gdf[alea_gdf.intersects(ref_entity.geometry)]
    
    if len(intersecting_aleas) == 0:
        # Aucun aléa dans cette entité
        results.append({
            'entity_id': ref_entity[id_field],
            'entity_name': ref_entity[name_field] if name_field in reference_gdf.columns else "",
            'niveau_moyen': 0,
            'surface_totale': 0,
            'geometry': ref_entity.geometry
        })
        continue
    
    # Calculer l'intersection entre chaque aléa et l'entité
    weighted_sum = 0
    total_area = 0
    
    for _, alea in intersecting_aleas.iterrows():
        # Calculer l'intersection
        intersection = alea.geometry.intersection(ref_entity.geometry)
        if not intersection.is_empty:
            # Calculer la surface de l'intersection
            intersection_area = intersection.area
            # Ajouter à la somme pondérée
            weighted_sum += alea['NIVEAU_NUM'] * intersection_area
            total_area += intersection_area
    
    # Calculer le niveau moyen pondéré
    niveau_moyen = weighted_sum / total_area if total_area > 0 else 0
    
    # Stocker les résultats
    results.append({
        'entity_id': ref_entity[id_field],
        'entity_name': ref_entity[name_field] if name_field in reference_gdf.columns else "",
        'niveau_moyen': niveau_moyen,
        'surface_totale': total_area,
        'geometry': ref_entity.geometry
    })

# Créer un GeoDataFrame avec les résultats
print("\nCréation du GeoDataFrame de résultats...")
result_gdf = gpd.GeoDataFrame(results, crs=reference_gdf.crs)

# Enregistrer le résultat
output_path = f"/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/SécheresseRGA/AleaRG_Fxx_L93/alea_argile_{TYPE_REFERENCE.lower()}.shp"
print(f"\nEnregistrement du résultat dans {output_path}...")
result_gdf.to_file(output_path)

print("Terminé!")

