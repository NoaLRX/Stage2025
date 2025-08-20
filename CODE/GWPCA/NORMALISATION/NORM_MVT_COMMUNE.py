import pandas as pd
import geopandas as gpd
import os
from collections import Counter

# Définition des chemins des fichiers
excel_massifs = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Mouvement2Terrain/MVT_2_MASSIF/diffusion-zonages-massifs-cog2021.xls"
communes_shp = "/Users/noa/Desktop/PRISM/Data/MISC/Autres polygones administratifs/COMMUNE.shp"
codes_postaux_shp = "/Users/noa/Desktop/PRISM/Data/MISC/Codes postaux - Communes/codes_postaux_region.shp"
communes_cp_csv = "/Users/noa/Desktop/PRISM/Data/MISC/CONVERTED/test.csv"
output_communes_path = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Mouvement2Terrain/MVT_2_MASSIF/communes_massifs.gpkg"
output_cp_path = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Mouvement2Terrain/MVT_2_MASSIF/score_massifs_cp.gpkg"

# Vérification de l'existence des fichiers source
for file_path in [excel_massifs, communes_shp, codes_postaux_shp, communes_cp_csv]:
    if not os.path.exists(file_path):
        raise FileNotFoundError(f"Le fichier {file_path} n'existe pas")

# 1. Lecture des données
print("Lecture des données...")
# Lire le fichier Excel des massifs (première feuille) en sautant les 4 premières lignes
df_massifs = pd.read_excel(excel_massifs, sheet_name=0, skiprows=4)
print(f"Colonnes du fichier massifs: {df_massifs.columns.tolist()}")
print(f"Données de massifs chargées: {len(df_massifs)} communes")

# Renommer les colonnes pour faciliter l'utilisation
df_massifs = df_massifs.rename(columns={
    'Code Insee': 'CODGEO',
    'Libellé de la commune': 'LIBGEO',
    'Commune de massif': 'MASSIF'
})

# Supprimer la première ligne qui contient des en-têtes résiduels
if df_massifs.iloc[0]['CODGEO'] == 'CODGEO':
    print("Suppression de la première ligne contenant des en-têtes résiduels")
    df_massifs = df_massifs.iloc[1:].reset_index(drop=True)

# Lire le shapefile des communes
print("Chargement du shapefile des communes...")
gdf_communes = gpd.read_file(communes_shp)
print(f"Colonnes du shapefile des communes: {gdf_communes.columns.tolist()}")
print(f"Nombre de communes dans le shapefile: {len(gdf_communes)}")

# Charger le shapefile des codes postaux
print("Chargement du shapefile des codes postaux...")
gdf_cp = gpd.read_file(codes_postaux_shp)
print(f"Colonnes du shapefile des codes postaux: {gdf_cp.columns.tolist()}")
print(f"Nombre de codes postaux dans le shapefile: {len(gdf_cp)}")
print("Aperçu des premières lignes du shapefile des codes postaux:")
print(gdf_cp.head())

# Utiliser la colonne ID comme code postal dans le shapefile des codes postaux
gdf_cp['CODE_POSTAL'] = gdf_cp['ID']
print("Exemple de codes postaux extraits du shapefile:")
print(gdf_cp['CODE_POSTAL'].head().tolist())

# Charger le fichier CSV de correspondance communes - codes postaux
print("Chargement du fichier de correspondance communes - codes postaux...")
df_communes_cp = pd.read_csv(communes_cp_csv)
print(f"Colonnes du fichier de correspondance: {df_communes_cp.columns.tolist()}")
print(f"Nombre de correspondances: {len(df_communes_cp)}")
print("Aperçu des premières lignes du fichier de correspondance:")
print(df_communes_cp.head())

# 2. Jointure des données des massifs avec le shapefile des communes
print("Préparation de la jointure communes - massifs...")
# Identifier la colonne contenant le code INSEE dans le shapefile des communes
insee_col = None
potential_cols = ['INSEE_COM', 'CODE_INSEE', 'CODE_COMM', 'INSEE', 'CODGEO']

for col in potential_cols:
    if col in gdf_communes.columns:
        insee_col = col
        print(f"Colonne de code INSEE identifiée: {insee_col}")
        break

if insee_col is None:
    print("Colonnes disponibles dans le shapefile des communes:")
    print(gdf_communes.columns.tolist())
    # Essayer de déterminer visuellement quelle colonne contient le code INSEE
    print("Échantillon des premières valeurs pour chaque colonne:")
    for col in gdf_communes.columns:
        if gdf_communes[col].dtype == 'object':  # Chercher les colonnes de type string
            print(f"{col}: {gdf_communes[col].head().tolist()}")
    
    # Par défaut, utiliser la première colonne qui ressemble à un code INSEE
    for col in gdf_communes.columns:
        if gdf_communes[col].dtype == 'object' and 'CODE' in col or 'INSEE' in col:
            insee_col = col
            print(f"Utilisation de la colonne {insee_col} par défaut")
            break
    
    if insee_col is None:
        raise ValueError("Impossible de déterminer la colonne contenant le code INSEE")

# Convertir les codes INSEE en string pour assurer la compatibilité des types
df_massifs['CODGEO'] = df_massifs['CODGEO'].astype(str)
gdf_communes[insee_col] = gdf_communes[insee_col].astype(str)

# Effectuer la jointure entre communes et massifs
print("Jointure communes - massifs...")
gdf_communes_massifs = pd.merge(
    gdf_communes,
    df_massifs,
    left_on=insee_col,
    right_on='CODGEO',
    how='left'
)

# Compter les communes avec et sans correspondance de massif
matched = gdf_communes_massifs['MASSIF'].notna().sum()
unmatched = gdf_communes_massifs['MASSIF'].isna().sum()
print(f"Communes avec correspondance de massif: {matched}")
print(f"Communes sans correspondance de massif: {unmatched}")

# Remplir les valeurs manquantes
if unmatched > 0:
    print("Remplissage des valeurs manquantes...")
    gdf_communes_massifs['MASSIF'].fillna("Non défini", inplace=True)
    gdf_communes_massifs['LIBGEO'].fillna(gdf_communes_massifs['NOM'] if 'NOM' in gdf_communes_massifs.columns else "Non renseigné", inplace=True)

# Vérifier que le résultat est un GeoDataFrame
if not isinstance(gdf_communes_massifs, gpd.GeoDataFrame):
    gdf_communes_massifs = gpd.GeoDataFrame(gdf_communes_massifs, geometry='geometry', crs=gdf_communes.crs)

# Jointure avec le fichier de correspondance communes - codes postaux
print("Préparation de la jointure avec les codes postaux...")
# Normalisations des colonnes pour la jointure
df_communes_cp['Code_commu'] = df_communes_cp['Code_commu'].astype(str)
gdf_communes_massifs[insee_col] = gdf_communes_massifs[insee_col].astype(str)

# Jointure pour associer les codes postaux aux communes
print("Jointure communes - codes postaux...")
gdf_result = pd.merge(
    gdf_communes_massifs,
    df_communes_cp,
    left_on=insee_col,
    right_on='Code_commu',
    how='left'
)

# Compter les communes avec et sans correspondance de code postal
matched_cp = gdf_result['Code_posta'].notna().sum()
unmatched_cp = gdf_result['Code_posta'].isna().sum()
print(f"Communes avec correspondance de code postal: {matched_cp}")
print(f"Communes sans correspondance de code postal: {unmatched_cp}")

# Identifier les communes qui ne sont pas "hors-massif"
print("Identification des communes dans un massif...")
hors_massif_values = ["Non défini", "hors-massif", "Hors-massif"]
gdf_result['IN_MASSIF'] = ~gdf_result['MASSIF'].isin(hors_massif_values)

# Enregistrer le résultat détaillé au niveau des communes
print(f"Enregistrement du résultat détaillé par commune dans {output_communes_path}...")
os.makedirs(os.path.dirname(output_communes_path), exist_ok=True)
gdf_result.to_file(output_communes_path, driver="GPKG")

# Calculer le score par code postal (nombre de communes dans un massif)
print("Calcul du score SCORE_MASSIF par code postal...")
# Grouper par code postal et compter les communes dans un massif
score_massif = gdf_result.groupby('Code_posta')['IN_MASSIF'].sum().reset_index()
score_massif.rename(columns={'IN_MASSIF': 'SCORE_MASSIF', 'Code_posta': 'CODE_POSTAL'}, inplace=True)

# Convertir le code postal en string pour la jointure
score_massif['CODE_POSTAL'] = score_massif['CODE_POSTAL'].astype(str)
gdf_cp['CODE_POSTAL'] = gdf_cp['CODE_POSTAL'].astype(str)

# Jointure entre le score et les polygones des codes postaux
print("Jointure avec les polygones des codes postaux...")
gdf_cp_score = pd.merge(
    gdf_cp,
    score_massif,
    on='CODE_POSTAL',
    how='left'
)

# S'assurer que c'est bien un GeoDataFrame
gdf_cp_score = gpd.GeoDataFrame(gdf_cp_score, geometry='geometry', crs=gdf_cp.crs)

# Trier par score décroissant et remplacer les NaN par 0
gdf_cp_score['SCORE_MASSIF'].fillna(0, inplace=True)
gdf_cp_score['SCORE_MASSIF'] = gdf_cp_score['SCORE_MASSIF'].astype(int)
gdf_cp_score = gdf_cp_score.sort_values('SCORE_MASSIF', ascending=False)

print(f"Résultat final: {len(gdf_cp_score)} codes postaux")
print("Aperçu des scores par code postal (10 premiers):")
print(gdf_cp_score[['CODE_POSTAL', 'SCORE_MASSIF']].head(10))

# Création du fichier geopackage par code postal
print(f"Enregistrement du résultat agrégé par code postal dans {output_cp_path}...")
os.makedirs(os.path.dirname(output_cp_path), exist_ok=True)
gdf_cp_score.to_file(output_cp_path, driver="GPKG")

print("Traitement terminé avec succès!") 