import pandas as pd
import geopandas as gpd
import os
import numpy as np
from collections import Counter
import warnings
warnings.filterwarnings('ignore')

# Définition des chemins des fichiers
trait_cote_shp = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Erosion/N_evolution_trait_cote_fr_epsg2154_S.shp"
shape_codes_postaux = "/Users/noa/Desktop/PRISM/Data/MISC/Codes postaux - Communes/codes_postaux_region.shp"
output_path = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Erosion/codes_postaux_erosion_corrige.gpkg"

# Vérification de l'existence des fichiers source
for file_path in [trait_cote_shp, shape_codes_postaux]:
    if not os.path.exists(file_path):
        raise FileNotFoundError(f"Le fichier {file_path} n'existe pas")

# 1. Lecture des données
print("Lecture des données...")
# Lire le shapefile d'évolution du trait de côte
gdf_trait_cote = gpd.read_file(trait_cote_shp)
print(f"Données du trait de côte chargées: {len(gdf_trait_cote)} segments")
print(f"Colonnes du fichier trait de côte: {gdf_trait_cote.columns.tolist()}")

# Afficher quelques informations sur les données d'évolution du trait de côte
print("\nAperçu des valeurs d'évolution du trait de côte:")
print(gdf_trait_cote[['taux', 'amenagemen']].describe())

# Pour comprendre les données, afficher la répartition des valeurs spéciales
print("\nRépartition des valeurs spéciales:")
print(f"Nombre de segments avec taux = -9999: {(gdf_trait_cote['taux'] == -9999).sum()}")
print(f"Parmi eux, amenagement = 0: {((gdf_trait_cote['taux'] == -9999) & (gdf_trait_cote['amenagemen'] == 0)).sum()}")
print(f"Parmi eux, amenagement = 1: {((gdf_trait_cote['taux'] == -9999) & (gdf_trait_cote['amenagemen'] == 1)).sum()}")

# Lire le shapefile des codes postaux
gdf_codes_postaux = gpd.read_file(shape_codes_postaux)
print(f"Données des codes postaux chargées: {len(gdf_codes_postaux)} codes postaux")
print(f"Colonnes du fichier codes postaux: {gdf_codes_postaux.columns.tolist()}")

# Création d'une colonne CODE_POST dans le shapefile si elle n'existe pas
if 'CODE_POST' not in gdf_codes_postaux.columns:
    print("Utilisation de la colonne 'ID' comme code postal")
    gdf_codes_postaux['CODE_POST'] = gdf_codes_postaux['ID']

# Préparation des données
gdf_codes_postaux['CODE_POST'] = gdf_codes_postaux['CODE_POST'].astype(str)

# 2. Prétraitement des données du trait de côte
print("\nPrétraitement des données du trait de côte...")

# Ajouter une colonne de catégorie basée sur les règles de la légende
def categorize_taux(row):
    taux = row['taux']
    amenagemen = row['amenagemen']
    
    # Valeurs spéciales
    if taux == -9999 and amenagemen == 0:
        return "Pas de calcul (pas de donnée ou marqueur différent)"
    elif taux == -9999 and amenagemen == 1:
        return "Pas de calcul (ouvrage au niveau du profil de calcul)"
    
    # Catégories standard
    elif taux <= -3:
        return "Recul sup. à 3 m/an"
    elif -3 < taux <= -1.5:
        return "Recul entre 1,5 et 3 m/an"
    elif -1.5 < taux <= -0.5:
        return "Recul entre 0,5 et 1,5 m/an"
    elif -0.5 < taux < 0:
        return "Recul entre 0 et 0,5 m/an"
    elif taux == 0:
        return "Non perceptible"
    elif 0 < taux < 0.5:
        return "Avancée entre 0 et 0,5 m/an"
    elif 0.5 <= taux < 1.5:
        return "Avancée entre 0,5 et 1,5 m/an"
    elif 1.5 <= taux < 3:
        return "Avancée entre 1,5 et 3 m/an"
    elif taux >= 3:
        return "Avancée sup. à 3 m/an"
    else:
        return "Catégorie inconnue"

# Appliquer la catégorisation
gdf_trait_cote['categorie'] = gdf_trait_cote.apply(categorize_taux, axis=1)

# Couleurs associées aux catégories pour la visualisation
colors = {
    "Recul sup. à 3 m/an": "#A52A2A",  # Marron foncé
    "Recul entre 1,5 et 3 m/an": "#D2691E",  # Marron moyen
    "Recul entre 0,5 et 1,5 m/an": "#DEB887",  # Beige foncé
    "Recul entre 0 et 0,5 m/an": "#F5DEB3",  # Beige clair
    "Non perceptible": "#00FFFF",  # Cyan
    "Avancée entre 0 et 0,5 m/an": "#ADFF2F",  # Vert clair
    "Avancée entre 0,5 et 1,5 m/an": "#32CD32",  # Vert moyen
    "Avancée entre 1,5 et 3 m/an": "#228B22",  # Vert foncé
    "Avancée sup. à 3 m/an": "#006400",  # Vert très foncé
    "Pas de calcul (pas de donnée ou marqueur différent)": "#808080",  # Gris
    "Pas de calcul (ouvrage au niveau du profil de calcul)": "#000000"   # Noir
}

# Ajouter la couleur
gdf_trait_cote['couleur'] = gdf_trait_cote['categorie'].map(colors)

# Afficher la répartition des catégories
print("\nRépartition des catégories du trait de côte:")
print(gdf_trait_cote['categorie'].value_counts())

# 3. Préparation des données pour l'analyse spatiale
print("\nPréparation des données pour l'analyse spatiale...")

# S'assurer que les deux GeoDataFrames utilisent le même système de coordonnées
if gdf_trait_cote.crs != gdf_codes_postaux.crs:
    print("Transformation des systèmes de coordonnées...")
    try:
        gdf_trait_cote = gdf_trait_cote.to_crs(gdf_codes_postaux.crs)
    except Exception as e:
        print(f"Erreur lors de la transformation: {e}")
        print("Tentative de définition explicite du CRS...")
        if gdf_trait_cote.crs is None:
            print("Le CRS du trait de côte est None, tentative de définition à EPSG:2154 (Lambert-93)")
            gdf_trait_cote.crs = "EPSG:2154"  # Lambert-93, CRS français standard
            gdf_trait_cote = gdf_trait_cote.to_crs(gdf_codes_postaux.crs)

# Ajouter un buffer au trait de côte pour faciliter l'intersection
print("Création d'un buffer autour du trait de côte pour faciliter les intersections...")
# Augmenter la taille du buffer pour mieux capturer les zones côtières
gdf_trait_cote['geometry_buffered'] = gdf_trait_cote.geometry.buffer(300)  # 300 mètres de buffer

# Créer un GeoDataFrame avec la géométrie bufferisée pour l'intersection
gdf_trait_cote_buffer = gdf_trait_cote.copy()
gdf_trait_cote_buffer.geometry = gdf_trait_cote_buffer['geometry_buffered']

# 4. Identification des codes postaux côtiers
print("\nIdentification des codes postaux côtiers...")

# Créer un index spatial pour accélérer les calculs
gdf_codes_postaux_coastal = gdf_codes_postaux[gdf_codes_postaux.intersects(gdf_trait_cote_buffer.unary_union)]
print(f"Nombre de codes postaux côtiers identifiés: {len(gdf_codes_postaux_coastal)}")

# Si très peu de codes postaux côtiers sont trouvés, augmenter encore le buffer
if len(gdf_codes_postaux_coastal) < 100:  # Seuil arbitraire
    print("Peu de codes postaux côtiers détectés, augmentation du buffer...")
    gdf_trait_cote['geometry_buffered'] = gdf_trait_cote.geometry.buffer(500)  # 500 mètres de buffer
    gdf_trait_cote_buffer.geometry = gdf_trait_cote['geometry_buffered']
    gdf_codes_postaux_coastal = gdf_codes_postaux[gdf_codes_postaux.intersects(gdf_trait_cote_buffer.unary_union)]
    print(f"Nouveau nombre de codes postaux côtiers identifiés: {len(gdf_codes_postaux_coastal)}")

# 5. Calcul des intersections entre le trait de côte et les codes postaux
print("\nCalcul des intersections entre trait de côte et codes postaux...")
print("Ce processus peut prendre du temps...")

# Création d'un dictionnaire pour stocker les résultats
result_dict = {}

# Pour chaque code postal côtier
for idx, cp_row in gdf_codes_postaux_coastal.iterrows():
    if idx % 10 == 0:
        print(f"Traitement du code postal {idx}/{len(gdf_codes_postaux_coastal)}")
    
    code_postal = cp_row['CODE_POST']
    cp_geom = cp_row['geometry']
    
    # Trouver tous les segments du trait de côte qui intersectent ce code postal
    intersecting_segments = gdf_trait_cote[gdf_trait_cote_buffer.intersects(cp_geom)].copy()
    
    if intersecting_segments.empty:
        continue
    
    # Calculer l'intersection entre chaque segment et le code postal
    intersecting_segments['intersection'] = intersecting_segments.apply(
        lambda row: row['geometry_buffered'].intersection(cp_geom), axis=1
    )
    
    # Calculer la longueur de chaque intersection
    intersecting_segments['intersection_length'] = intersecting_segments.apply(
        lambda row: row['intersection'].length if hasattr(row['intersection'], 'length') else 0, axis=1
    )
    
    # Supprimer les intersections minuscules (qui peuvent être des artefacts)
    total_length = intersecting_segments['intersection_length'].sum()
    min_length_threshold = total_length * 0.01  # 1% de la longueur totale
    intersecting_segments = intersecting_segments[intersecting_segments['intersection_length'] > min_length_threshold]
    
    if intersecting_segments.empty:
        continue
    
    # Compter les occurrences de chaque catégorie, pondérées par la longueur
    category_length = {}
    for cat in colors.keys():
        cat_segments = intersecting_segments[intersecting_segments['categorie'] == cat]
        if not cat_segments.empty:
            category_length[cat] = cat_segments['intersection_length'].sum()
    
    if not category_length:
        continue
    
    # Trouver la catégorie majoritaire en longueur
    majority_category = max(category_length.items(), key=lambda x: x[1])[0]
    
    # Stocker d'autres statistiques intéressantes
    # Calculer la moyenne pondérée du taux d'évolution pour les segments valides (taux != -9999)
    valid_segments = intersecting_segments[intersecting_segments['taux'] != -9999]
    if not valid_segments.empty:
        weighted_sum = (valid_segments['taux'] * valid_segments['intersection_length']).sum()
        total_valid_length = valid_segments['intersection_length'].sum()
        weighted_avg = weighted_sum / total_valid_length if total_valid_length > 0 else np.nan
    else:
        weighted_avg = np.nan
    
    # Stocker les résultats
    result_dict[code_postal] = {
        'categorie': majority_category,
        'taux_evolution': weighted_avg,
        'longueur_totale': total_length,
        'nb_segments': len(intersecting_segments),
        'couleur': colors.get(majority_category, "#808080")
    }
    
    # Ajouter des données sur la composition détaillée
    for cat in colors.keys():
        if cat in category_length:
            result_dict[code_postal][f'longueur_{cat.replace(" ", "_").replace(",", "").replace(".", "").replace("(", "").replace(")", "")}'] = category_length[cat]
            result_dict[code_postal][f'pourcentage_{cat.replace(" ", "_").replace(",", "").replace(".", "").replace("(", "").replace(")", "")}'] = (category_length[cat] / total_length) * 100

print(f"Traitement terminé. {len(result_dict)} codes postaux côtiers traités.")

# 6. Création du dataframe résultat
print("\nCréation du dataframe résultat...")
df_result = pd.DataFrame.from_dict(result_dict, orient='index')
df_result.reset_index(inplace=True)
df_result.rename(columns={'index': 'Code_postal'}, inplace=True)

print(f"Résultat: {len(df_result)} codes postaux")
print("Aperçu des données:")
print(df_result[['Code_postal', 'categorie', 'taux_evolution', 'nb_segments']].head())

# 7. Jointure avec le shapefile des codes postaux
print("\nJointure avec le shapefile des codes postaux...")
gdf_final = pd.merge(
    gdf_codes_postaux,
    df_result,
    left_on='CODE_POST',
    right_on='Code_postal',
    how='left'
)

# Convertir en GeoDataFrame si nécessaire
if not isinstance(gdf_final, gpd.GeoDataFrame):
    gdf_final = gpd.GeoDataFrame(gdf_final, geometry='geometry', crs=gdf_codes_postaux.crs)

# 8. Traitement des codes postaux côtiers qui n'ont pas de valeur attribuée
missing_coastal = []
# Créer une version dissolue du trait de côte bufferisé pour détecter les zones côtières
coast_buffer_union = gdf_trait_cote_buffer.unary_union.buffer(200)

# Identifier les codes postaux qui sont côtiers mais n'ont pas de valeur d'érosion
for idx, row in gdf_final.iterrows():
    if pd.isna(row.get('categorie')):
        geom = row['geometry']
        # Vérifier si le code postal est côtier (intersecte le buffer côtier)
        if geom.intersects(coast_buffer_union):
            missing_coastal.append(row['CODE_POST'])

print(f"\nNombre de codes postaux côtiers sans valeur d'érosion: {len(missing_coastal)}")

if len(missing_coastal) > 0:
    print("Attribution des valeurs aux codes postaux côtiers manquants par voisinage...")
    
    # Identifer les codes postaux qui ont des valeurs
    gdf_with_values = gdf_final[gdf_final['categorie'].notna()].copy()
    
    # Pour chaque code postal côtier manquant
    for code_postal in missing_coastal:
        cp_row = gdf_final[gdf_final['CODE_POST'] == code_postal].iloc[0]
        cp_geom = cp_row['geometry']
        
        # Créer un buffer autour du polygone du code postal
        buffer_geom = cp_geom.buffer(5000)  # 5km buffer
        
        # Trouver les codes postaux voisins avec des valeurs
        neighbors = gdf_with_values[gdf_with_values.intersects(buffer_geom)].copy()
        
        if not neighbors.empty:
            # Calculer les distances aux voisins
            neighbors['distance'] = neighbors.apply(lambda row: row['geometry'].distance(cp_geom), axis=1)
            
            # Trier par distance
            neighbors = neighbors.sort_values('distance')
            
            # Prendre le voisin le plus proche
            nearest = neighbors.iloc[0]
            
            # Créer une entrée dans result_dict
            result_dict[code_postal] = {
                'categorie': nearest['categorie'],
                'taux_evolution': nearest['taux_evolution'],
                'longueur_totale': 0,  # Valeur par défaut
                'nb_segments': 0,  # Valeur par défaut
                'couleur': nearest['couleur'],
                'attribution': 'Par voisinage'  # Marquer comme attribué par voisinage
            }

# Mettre à jour df_result avec les nouvelles valeurs
if len(missing_coastal) > 0:
    df_result = pd.DataFrame.from_dict(result_dict, orient='index')
    df_result.reset_index(inplace=True)
    df_result.rename(columns={'index': 'Code_postal'}, inplace=True)
    
    # Recréer gdf_final avec les nouvelles données
    gdf_final = pd.merge(
        gdf_codes_postaux,
        df_result,
        left_on='CODE_POST',
        right_on='Code_postal',
        how='left'
    )
    
    # Convertir en GeoDataFrame si nécessaire
    if not isinstance(gdf_final, gpd.GeoDataFrame):
        gdf_final = gpd.GeoDataFrame(gdf_final, geometry='geometry', crs=gdf_codes_postaux.crs)

# 9. Afficher un résumé des catégories finales
print("\nRépartition des catégories d'évolution du trait de côte par code postal:")
print(gdf_final['categorie'].value_counts(dropna=False))

# 10. Enregistrement du résultat au format geopackage
print(f"\nEnregistrement du résultat dans {output_path}...")
# Création du dossier de sortie si nécessaire
os.makedirs(os.path.dirname(output_path), exist_ok=True)
gdf_final.to_file(output_path, driver="GPKG")

print("Traitement terminé avec succès!")
print(f"Le fichier geopackage a été créé avec {len(gdf_final)} codes postaux, dont {len(df_result)} avec des données d'évolution du trait de côte.") 