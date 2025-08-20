import pandas as pd
import geopandas as gpd
import os
import numpy as np
from collections import Counter
import warnings
warnings.filterwarnings('ignore')

# Définition des chemins des fichiers
communes_massifs_gpkg = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Mouvement2Terrain/MVT_2_MASSIF/communes_massifs.gpkg"
shape_codes_postaux = "/Users/noa/Desktop/PRISM/Data/MISC/Codes postaux - Communes/codes_postaux_region.shp"
output_path = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Mouvement2Terrain/MVT_2_MASSIF/codes_postaux_massifs_overlay.gpkg"

# Vérification de l'existence des fichiers source
for file_path in [communes_massifs_gpkg, shape_codes_postaux]:
    if not os.path.exists(file_path):
        raise FileNotFoundError(f"Le fichier {file_path} n'existe pas")

# 1. Lecture des données
print("Lecture des données...")
# Lire le fichier geopackage des communes avec les informations de massif
gdf_communes = gpd.read_file(communes_massifs_gpkg)
print(f"Données des communes chargées: {len(gdf_communes)} communes")

# Vérifier que les informations de massif sont présentes
if 'MASSIF' not in gdf_communes.columns:
    raise ValueError("La colonne MASSIF n'existe pas dans le fichier des communes")

# Lire le shapefile des codes postaux
gdf_codes_postaux = gpd.read_file(shape_codes_postaux)
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

# S'assurer que les deux GeoDataFrames utilisent le même système de coordonnées
if gdf_communes.crs != gdf_codes_postaux.crs:
    print("Transformation des systèmes de coordonnées...")
    gdf_communes = gdf_communes.to_crs(gdf_codes_postaux.crs)

# 3. Calcul de l'overlay spatial
print("Calcul de l'overlay spatial entre communes et codes postaux...")
print("Ce processus peut prendre du temps...")

# Ajouter une colonne d'aire pour les communes
gdf_communes['commune_area'] = gdf_communes.geometry.area

# Utilisation de l'opération overlay pour calculer les intersections
overlay = gpd.overlay(gdf_communes, gdf_codes_postaux, how='intersection')

# Calcul de l'aire de chaque intersection
overlay['intersection_area'] = overlay.geometry.area

print(f"Nombre d'intersections calculées: {len(overlay)}")
print("Colonnes après l'overlay:")
print(overlay.columns.tolist())

# 4. Agrégation des données par code postal
print("Agrégation des données par code postal...")

# Grouper par code postal et massif pour calculer la surface totale par massif
grouped = overlay.groupby(['CODE_POST', 'MASSIF'])['intersection_area'].sum().reset_index()
print(f"Nombre de groupes (code postal, massif): {len(grouped)}")

# Trouver le massif majoritaire pour chaque code postal
result_dict = {}

for code_postal in grouped['CODE_POST'].unique():
    # Filtrer les données pour ce code postal
    cp_data = grouped[grouped['CODE_POST'] == code_postal]
    
    # Trouver le massif avec la plus grande surface
    idx_max = cp_data['intersection_area'].idxmax()
    majority_massif = cp_data.loc[idx_max, 'MASSIF']
    
    # Extraire les communes principales (top 3 par surface d'intersection)
    cp_communes = overlay[overlay['CODE_POST'] == code_postal].sort_values('intersection_area', ascending=False)
    if len(cp_communes) > 0:
        top_communes = cp_communes.head(3)['LIBGEO'].tolist()
        communes_str = ', '.join(top_communes)
    else:
        communes_str = "Non renseigné"
    
    # Stocker les résultats
    result_dict[code_postal] = {
        'MASSIF': majority_massif,
        'LIBGEO': communes_str,
        'Nb_communes': len(cp_communes)
    }

print(f"Traitement terminé. {len(result_dict)} codes postaux traités.")

# 5. Création du dataframe résultat
print("Création du dataframe résultat...")
df_result = pd.DataFrame.from_dict(result_dict, orient='index')
df_result.reset_index(inplace=True)
df_result.rename(columns={'index': 'Code_postal'}, inplace=True)

print(f"Résultat: {len(df_result)} codes postaux")
print("Aperçu des données:")
print(df_result.head())

# 6. Vérification des codes postaux manquants
print("Vérification des codes postaux manquants...")
result_codes = set(df_result['Code_postal'])
all_codes = set(gdf_codes_postaux['CODE_POST'])
missing_codes = all_codes - result_codes

print(f"Nombre de codes postaux manquants: {len(missing_codes)}")
if missing_codes and len(missing_codes) < 20:
    print(f"Codes postaux manquants: {list(missing_codes)}")
elif missing_codes:
    print(f"Exemples de codes postaux manquants: {list(missing_codes)[:10]}")

# 7. Traitement des codes postaux manquants
if missing_codes:
    print("Traitement des codes postaux manquants...")
    
    # Créer un géodataframe des codes postaux manquants
    missing_cp = gdf_codes_postaux[gdf_codes_postaux['CODE_POST'].isin(missing_codes)].copy()
    
    # Pour chaque code postal manquant, trouver le plus proche voisin avec une valeur
    for idx, row in missing_cp.iterrows():
        code_postal = row['CODE_POST']
        geom = row.geometry
        
        # Créer un buffer autour du polygone du code postal
        buffer_geom = geom.buffer(1000)  # 1km buffer
        
        # Trouver les communes qui intersectent ce buffer
        intersecting_communes = gdf_communes[gdf_communes.intersects(buffer_geom)].copy()
        
        if not intersecting_communes.empty:
            # Calculer l'intersection entre chaque commune et le buffer
            intersecting_communes['buffer_intersection'] = intersecting_communes.apply(
                lambda r: r.geometry.intersection(buffer_geom).area, axis=1
            )
            
            # Grouper par massif et sommer les surfaces
            massif_areas = intersecting_communes.groupby('MASSIF')['buffer_intersection'].sum()
            
            # Prendre le massif avec la plus grande surface
            majority_massif = massif_areas.idxmax()
            
            # Top communes
            top_communes = intersecting_communes.sort_values('buffer_intersection', ascending=False).head(3)['LIBGEO'].tolist()
            communes_str = ', '.join(top_communes)
            
            # Ajouter au dictionnaire de résultats
            result_dict[code_postal] = {
                'MASSIF': majority_massif,
                'LIBGEO': f"Attribué par voisinage: {communes_str}",
                'Nb_communes': len(intersecting_communes)
            }
        else:
            # Si aucune commune ne se trouve dans le buffer, utiliser le plus proche voisin
            codes_with_massif = df_result['Code_postal'].tolist()
            
            # Créer un geodataframe des codes postaux avec des valeurs massif
            gdf_with_massif = gdf_codes_postaux[gdf_codes_postaux['CODE_POST'].isin(codes_with_massif)].copy()
            
            # Calculer la distance au plus proche voisin
            gdf_with_massif['distance'] = gdf_with_massif.geometry.distance(geom)
            
            # Prendre le code postal le plus proche
            nearest_idx = gdf_with_massif['distance'].idxmin()
            nearest_cp = gdf_with_massif.loc[nearest_idx, 'CODE_POST']
            
            # Attribuer le même massif que le voisin
            result_dict[code_postal] = {
                'MASSIF': result_dict[nearest_cp]['MASSIF'],
                'LIBGEO': f"Attribué par proximité avec {nearest_cp}",
                'Nb_communes': 0
            }

    # Mettre à jour le dataframe résultat
    df_result = pd.DataFrame.from_dict(result_dict, orient='index')
    df_result.reset_index(inplace=True)
    df_result.rename(columns={'index': 'Code_postal'}, inplace=True)
    
    print(f"Résultat après traitement des manquants: {len(df_result)} codes postaux")

# 8. Jointure avec le shapefile des codes postaux
print("Jointure avec le shapefile des codes postaux...")
gdf_final = pd.merge(
    gdf_codes_postaux,
    df_result,
    left_on='CODE_POST',
    right_on='Code_postal',
    how='left'
)

# Compter les codes postaux avec et sans correspondance
with_massif = gdf_final['MASSIF'].notna().sum()
without_massif = gdf_final['MASSIF'].isna().sum()
print(f"Codes postaux avec information de massif: {with_massif}")
print(f"Codes postaux sans information de massif: {without_massif}")

# 9. Traitement des éventuels codes postaux restants sans massif
if without_massif > 0:
    print(f"Il reste {without_massif} codes postaux sans massif. Attribution par voisinage...")
    
    # Nous devrions avoir un GeoDataFrame à ce stade
    if not isinstance(gdf_final, gpd.GeoDataFrame):
        gdf_final = gpd.GeoDataFrame(gdf_final, geometry='geometry', crs=gdf_codes_postaux.crs)
    
    # Identifier les polygones sans valeur MASSIF
    gdf_missing = gdf_final[gdf_final['MASSIF'].isna()].copy()
    gdf_with_massif = gdf_final[gdf_final['MASSIF'].notna()].copy()
    
    # Pour chaque polygone manquant, trouver les polygones voisins
    missing_indices = gdf_missing.index.tolist()
    filled_values = 0
    
    for idx in missing_indices:
        # Récupérer la géométrie du polygone sans MASSIF
        geom = gdf_final.loc[idx, 'geometry']
        code_postal = gdf_final.loc[idx, 'CODE_POST']
        
        # Trouver tous les polygones qui touchent ce polygone
        neighbors = gdf_with_massif[gdf_with_massif.touches(geom)]
        
        if len(neighbors) > 0:
            # S'il y a des voisins, prendre la valeur MASSIF la plus fréquente
            massifs = neighbors['MASSIF'].tolist()
            most_common_massif = Counter(massifs).most_common(1)[0][0]
            
            # Attribuer cette valeur au polygone manquant
            gdf_final.loc[idx, 'MASSIF'] = most_common_massif
            gdf_final.loc[idx, 'LIBGEO'] = "Attribué par voisinage direct"
            gdf_final.loc[idx, 'Nb_communes'] = 0
            filled_values += 1
        else:
            # Si pas de voisin direct, utiliser un buffer pour trouver des voisins proches
            buffer_geom = geom.buffer(5000)  # 5km buffer
            neighbors = gdf_with_massif[gdf_with_massif.intersects(buffer_geom)]
            
            if len(neighbors) > 0:
                massifs = neighbors['MASSIF'].tolist()
                most_common_massif = Counter(massifs).most_common(1)[0][0]
                
                gdf_final.loc[idx, 'MASSIF'] = most_common_massif
                gdf_final.loc[idx, 'LIBGEO'] = "Attribué par voisinage étendu"
                gdf_final.loc[idx, 'Nb_communes'] = 0
                filled_values += 1
            else:
                # Dernier recours: utiliser la valeur la plus fréquente
                most_common_massif = gdf_final['MASSIF'].mode()[0]
                gdf_final.loc[idx, 'MASSIF'] = most_common_massif
                gdf_final.loc[idx, 'LIBGEO'] = "Attribué par défaut"
                gdf_final.loc[idx, 'Nb_communes'] = 0
                filled_values += 1
    
    print(f"Codes postaux attribués par voisinage: {filled_values}")

# Vérification finale
with_massif = gdf_final['MASSIF'].notna().sum()
without_massif = gdf_final['MASSIF'].isna().sum()
print(f"Statut final - Codes postaux avec massif: {with_massif}, sans massif: {without_massif}")

# Remplir les éventuelles valeurs manquantes restantes
if without_massif > 0:
    print("Remplissage des dernières valeurs manquantes...")
    # Utiliser la valeur la plus fréquente
    most_common_massif = gdf_final['MASSIF'].mode()[0]
    gdf_final['MASSIF'].fillna(most_common_massif, inplace=True)
    gdf_final['LIBGEO'].fillna("Attribué par défaut", inplace=True)
    gdf_final['Nb_communes'].fillna(0, inplace=True)

# 10. Enregistrement du résultat au format geopackage
print(f"Enregistrement du résultat dans {output_path}...")
# Création du dossier de sortie si nécessaire
os.makedirs(os.path.dirname(output_path), exist_ok=True)
gdf_final.to_file(output_path, driver="GPKG")

print("Traitement terminé avec succès!")
print(f"Statistiques finales des massifs:")
print(gdf_final['MASSIF'].value_counts()) 