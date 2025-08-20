import pandas as pd
import geopandas as gpd
import os
from collections import Counter

# Définition des chemins des fichiers
excel_massifs = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Mouvement2Terrain/MVT_2_MASSIF/diffusion-zonages-massifs-cog2021.xls"
jointure_csv = "/Users/noa/Desktop/PRISM/Data/MISC/CONVERTED/test.csv"
shape_codes_postaux = "/Users/noa/Desktop/PRISM/Data/MISC/Codes postaux - Communes/codes_postaux_region.shp"
output_path = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Mouvement2Terrain/MVT_2_MASSIF/codes_postaux_massifs.gpkg"

# Vérification de l'existence des fichiers source
for file_path in [excel_massifs, jointure_csv, shape_codes_postaux]:
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

# Lire le fichier CSV de jointure code commune - code postal
df_jointure = pd.read_csv(jointure_csv)
print(f"Colonnes du fichier jointure: {df_jointure.columns.tolist()}")
print(f"Données de jointure chargées: {len(df_jointure)} entrées")

# Vérifier si le nom de la colonne du code postal est correct et le renommer
df_jointure = df_jointure.rename(columns={'Code_posta': 'Code_postal'})
print("Colonnes après renommage dans jointure:")
print(df_jointure.columns.tolist())

# Lire le shapefile des codes postaux
gdf_codes_postaux = gpd.read_file(shape_codes_postaux)
print(f"Colonnes du fichier codes postaux: {gdf_codes_postaux.columns.tolist()}")
print(f"Données géographiques chargées: {len(gdf_codes_postaux)} codes postaux")

# Création d'une colonne CODE_POST dans le shapefile si elle n'existe pas
if 'CODE_POST' not in gdf_codes_postaux.columns:
    print(f"La colonne CODE_POST n'existe pas dans le shapefile.")
    print(f"Utilisation de la colonne 'ID' comme code postal")
    gdf_codes_postaux['CODE_POST'] = gdf_codes_postaux['ID']

# Conversion des types pour les jointures
gdf_codes_postaux['CODE_POST'] = gdf_codes_postaux['CODE_POST'].astype(str)

# 2. Jointure des données massifs avec la table de correspondance commune-code postal
print("Jointure des données...")
# Assurons-nous que les colonnes de jointure sont du même type
df_massifs['CODGEO'] = df_massifs['CODGEO'].astype(str)
df_jointure['Code_commu'] = df_jointure['Code_commu'].astype(str)

# Jointure entre massifs et codes postaux
df_merged = pd.merge(
    df_massifs, 
    df_jointure, 
    left_on='CODGEO', 
    right_on='Code_commu', 
    how='inner'
)

print(f"Après jointure: {len(df_merged)} lignes conservées")
print("Colonnes après jointure:")
print(df_merged.columns.tolist())

# Vérifions les types de données
print("Types de données après jointure:")
for col in df_merged.columns:
    print(f"{col}: {df_merged[col].dtype}")

# 3. Agrégation par code postal - Approche alternative
print("Agrégation des données par code postal...")

# Créer un dictionnaire pour stocker les résultats d'agrégation
result_dict = {}

# Traiter chaque code postal séparément
for code_postal, group in df_merged.groupby('Code_postal'):
    # Pour LIBGEO, on joint les valeurs uniques avec une virgule
    libgeos = set(group['LIBGEO'])
    joined_libgeo = ', '.join(libgeos)
    
    # Pour MASSIF, on prend la valeur la plus fréquente
    massifs = group['MASSIF'].tolist()
    count = Counter(massifs)
    majority_massif, _ = count.most_common(1)[0]
    
    # Stocker les résultats
    result_dict[code_postal] = {
        'LIBGEO': joined_libgeo,
        'MASSIF': majority_massif
    }

# Créer un dataframe à partir du dictionnaire de résultats
df_grouped = pd.DataFrame.from_dict(result_dict, orient='index')
df_grouped.reset_index(inplace=True)
df_grouped.rename(columns={'index': 'Code_postal'}, inplace=True)

print(f"Après agrégation: {len(df_grouped)} codes postaux uniques")

# 4. Analyse des codes postaux manquants
print("Analyse des codes postaux manquants...")
# Convertir les valeurs en chaînes pour la comparaison
gdf_codes_postaux_ids = set(gdf_codes_postaux['CODE_POST'].astype(str))
df_grouped_codes = set(df_grouped['Code_postal'].astype(str))

# Trouver les codes postaux présents dans le shapefile mais absents de nos données
missing_codes = gdf_codes_postaux_ids - df_grouped_codes
print(f"Nombre de codes postaux manquants: {len(missing_codes)}")
print(f"Exemples de codes postaux manquants: {list(missing_codes)[:10] if missing_codes else 'Aucun'}")

# Ajouter les codes postaux manquants avec une valeur par défaut pour MASSIF
default_massif = "Non défini"  # ou une autre valeur appropriée
missing_data = []

for code in missing_codes:
    missing_data.append({
        'Code_postal': code,
        'LIBGEO': "Non renseigné",
        'MASSIF': default_massif
    })

# Ajouter les codes postaux manquants au dataframe groupé
if missing_data:
    df_missing = pd.DataFrame(missing_data)
    df_grouped = pd.concat([df_grouped, df_missing], ignore_index=True)
    print(f"Après ajout des codes postaux manquants: {len(df_grouped)} codes postaux")

# 5. Jointure avec les données géographiques des codes postaux
print("Effectuer la jointure finale...")
# Assurer que les types sont cohérents
df_grouped['Code_postal'] = df_grouped['Code_postal'].astype(str)

# Utilisation d'une jointure à droite pour conserver tous les codes postaux du shapefile
gdf_result = pd.merge(
    df_grouped,
    gdf_codes_postaux,
    left_on='Code_postal',
    right_on='CODE_POST',
    how='right'
)

# Pour les codes postaux sans données de massif, remplir avec une valeur par défaut
if 'MASSIF' in gdf_result.columns:
    gdf_result['MASSIF'].fillna(default_massif, inplace=True)
    gdf_result['LIBGEO'].fillna("Non renseigné", inplace=True)

print(f"Résultat final: {len(gdf_result)} codes postaux")
print(f"Nombre de codes postaux avec MASSIF='Non défini': {(gdf_result['MASSIF'] == default_massif).sum()}")

# Conversion en GeoDataFrame si nécessaire
if not isinstance(gdf_result, gpd.GeoDataFrame):
    gdf_result = gpd.GeoDataFrame(gdf_result, geometry='geometry', crs=gdf_codes_postaux.crs)

# 6. Enregistrement du résultat au format geopackage
print(f"Enregistrement du résultat dans {output_path}...")
# Création du dossier de sortie si nécessaire
os.makedirs(os.path.dirname(output_path), exist_ok=True)
gdf_result.to_file(output_path, driver="GPKG")

print("Traitement terminé avec succès!")