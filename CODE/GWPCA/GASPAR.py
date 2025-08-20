import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LinearSegmentedColormap
import matplotlib as mpl
import matplotlib.patheffects as pe
import os

# Création du dossier de résultats
results_dir = "/Users/noa/Desktop/PRISM/Data/Resultats_CATNAT_MVT_Terrain_Unique"
os.makedirs(results_dir, exist_ok=True)
print(f"Dossier de résultats créé: {results_dir}")

# Chemins des fichiers
catnat_file = "/Users/noa/Downloads/gaspar(1)/catnat_gaspar.csv"
communes_file = "/Users/noa/Desktop/PRISM/Data/MISC/Autres polygones administratifs/COMMUNE.shp"
conversion_file = "/Users/noa/Desktop/PRISM/Data/MISC/CONVERTED/test.shp"
codes_postaux_file = "/Users/noa/Desktop/PRISM/Data/MISC/Codes postaux - Communes/codes_postaux_region.shp"

# Lecture des données CATNAT
print("Lecture des données CATNAT...")
catnat_df = pd.read_csv(catnat_file, low_memory=False)

# Afficher les informations sur les colonnes
print("\nColonnes des données CATNAT:")
print(catnat_df.columns.tolist())

# Identifier la colonne qui contient les informations sur le type de risque
print("\nRecherche de la colonne contenant les informations sur les mouvements de terrain...")
colonnes_risque = [col for col in catnat_df.columns if 'risque' in col.lower()]
print(f"Colonnes liées aux risques: {colonnes_risque}")

# Vérifier les valeurs uniques dans les colonnes potentielles
for col in colonnes_risque:
    unique_vals = catnat_df[col].dropna().unique()
    print(f"\nValeurs uniques dans {col} (échantillon):")
    print([val for val in unique_vals[:10] if isinstance(val, str)])

# Déterminer quelle colonne utiliser
colonne_risque = 'lib_risque_jo' if 'lib_risque_jo' in catnat_df.columns else 'lib_risque'
print(f"\nUtilisation de la colonne '{colonne_risque}' pour le filtrage des mouvements de terrain.")

# Définir l'aléa à rechercher - uniquement "Mouvement de Terrain"
print("\nFiltrage des événements 'Mouvement de Terrain' uniquement...")
alea_mvt_terrain = "Mouvement de Terrain"

# Filtrer les événements
mvt_terrain_df = catnat_df[catnat_df[colonne_risque] == alea_mvt_terrain]
print(f"Nombre d'événements de mouvements de terrain trouvés: {len(mvt_terrain_df)}")

# Compter les occurrences par commune
print("\nCalcul de la fréquence d'occurrence par commune...")
if 'cod_commune' in mvt_terrain_df.columns:
    commune_col = 'cod_commune'
else:
    # Chercher une colonne alternative contenant les codes communes
    commune_cols = [col for col in mvt_terrain_df.columns if 'commune' in col.lower() and 'cod' in col.lower()]
    commune_col = commune_cols[0] if commune_cols else None
    if not commune_col:
        raise ValueError("Impossible de trouver la colonne contenant les codes communes")
    print(f"Utilisation de la colonne '{commune_col}' pour les codes communes.")

freq_communes = mvt_terrain_df[commune_col].value_counts().reset_index()
freq_communes.columns = [commune_col, "frequence"]

# Préparation pour la jointure (ajouter des zéros devant les codes commune si nécessaire)
freq_communes["INSEE_COM"] = freq_communes[commune_col].astype(str).str.zfill(5)
print(f"Nombre de communes avec des événements de mouvements de terrain: {len(freq_communes)}")

# =========================================================
# Agrégation par code postal
# =========================================================
print("\n==================================================")
print("Agrégation par code postal")
print("==================================================")

# Charger le fichier de conversion Commune -> Code postal
print("\nChargement du fichier de conversion communes/codes postaux...")
conversion_gdf = gpd.read_file(conversion_file)
print(f"Nombre d'entrées dans le fichier de conversion: {len(conversion_gdf)}")
print("Aperçu des données de conversion:")
print(conversion_gdf[["Code_commu", "Code_posta", "Nom_commun"]].head())

# Préparation pour la jointure avec les données de fréquence par commune
print("\nJointure des codes postaux avec les données de fréquence...")
conversion_gdf["INSEE_COM"] = conversion_gdf["Code_commu"].astype(str).str.zfill(5)

# Joindre les données de fréquence par commune au fichier de conversion
print("Jointure des fréquences des communes avec la table de conversion...")
communecp_with_freq = conversion_gdf.merge(freq_communes[["INSEE_COM", "frequence"]], on="INSEE_COM", how="left")
communecp_with_freq["frequence"] = communecp_with_freq["frequence"].fillna(0)

# Agrégation par code postal
print("\nAgrégation des fréquences par code postal...")
# Groupement des fréquences par code postal
freq_by_cp = communecp_with_freq.groupby("Code_posta")["frequence"].sum().reset_index()
print(f"Nombre de codes postaux avec des données: {len(freq_by_cp)}")

# Sélectionner uniquement les codes postaux avec des mouvements de terrain
freq_by_cp_nonzero = freq_by_cp[freq_by_cp["frequence"] > 0]
print(f"Nombre de codes postaux avec des événements de mouvements de terrain: {len(freq_by_cp_nonzero)}")

# Statistiques descriptives des fréquences par code postal
print("\nStatistiques descriptives des fréquences d'événements de mouvements de terrain par code postal:")
cp_stats = freq_by_cp["frequence"].describe()
print(cp_stats)

# Sauvegarder les statistiques dans un fichier texte
cp_stats_file = os.path.join(results_dir, "statistiques_mvt_terrain_cp.txt")
with open(cp_stats_file, 'w') as f:
    f.write("Statistiques descriptives des fréquences d'événements de mouvements de terrain par code postal:\n")
    f.write(str(cp_stats))
print(f"Statistiques par code postal sauvegardées dans: {cp_stats_file}")

# Sauvegarder les données agrégées par code postal en CSV
freq_cp_csv = os.path.join(results_dir, "frequences_par_code_postal_mvt_terrain.csv")
freq_by_cp.to_csv(freq_cp_csv, index=False)
print(f"Données agrégées par code postal sauvegardées dans: {freq_cp_csv}")

# Chargement du shapefile des codes postaux
print("\nChargement des polygones des codes postaux...")
cp_polygons = gpd.read_file(codes_postaux_file)
print(f"Nombre de polygones de codes postaux: {len(cp_polygons)}")
print("Aperçu des données de codes postaux:")
print(cp_polygons.head(3))

# Vérifier le contenu de la colonne ID qui correspond aux codes postaux
print("\nAperçu des identifiants de codes postaux:")
print(cp_polygons["ID"].head(10))

# Conversion de ID en chaîne de caractères pour la jointure
cp_polygons["Code_posta"] = cp_polygons["ID"].astype(str)

# Joindre les données de fréquence aux polygones des codes postaux
print("\nJointure des fréquences avec les polygones des codes postaux...")
cp_map_data = cp_polygons.merge(freq_by_cp, on="Code_posta", how="left")
cp_map_data["frequence"] = cp_map_data["frequence"].fillna(0)

# Pour le rendu visuel, les zones sans données seront grises plutôt que blanches/transparentes
print("\nCréation de la carte par code postal avec les zones sans données en gris...")
cp_fig, cp_ax = plt.figure(figsize=(15, 10)), plt.gca()

# Créer une palette de couleurs type "température" (bleu-jaune-rouge)
colors = ['#0000FF', '#00FFFF', '#FFFF00', '#FF0000']  # Bleu, Cyan, Jaune, Rouge
cmap = LinearSegmentedColormap.from_list('temperature', colors)

# Créer une fonction de normalisation pour les couleurs (excluant les valeurs 0)
cp_vmin = cp_map_data[cp_map_data["frequence"] > 0]["frequence"].min()
cp_vmax = cp_map_data["frequence"].max()
cp_norm = mpl.colors.Normalize(vmin=cp_vmin, vmax=cp_vmax)

# Masque pour les codes postaux avec fréquence 0
cp_mask = cp_map_data["frequence"] == 0

# Afficher les codes postaux avec mouvements de terrain
cp_map_data[~cp_mask].plot(column="frequence", 
                          cmap=cmap, 
                          linewidth=0.1, 
                          ax=cp_ax, 
                          edgecolor="0.5",
                          norm=cp_norm,
                          legend=True,
                          legend_kwds={'label': "Fréquence de mouvements de terrain par code postal"})

# Afficher les codes postaux sans mouvements de terrain (en gris clair avec contour gris)
cp_map_data[cp_mask].plot(color="#EEEEEE", 
                         edgecolor="0.5", 
                         linewidth=0.1, 
                         ax=cp_ax,
                         label="Aucun mouvement de terrain")

# Ajouter un titre
plt.title("Fréquence d'occurrence des mouvements de terrain par code postal en France", fontsize=14)
plt.axis('off')

# Sauvegarder la carte
cp_output_png = os.path.join(results_dir, "carte_frequence_mvt_terrain_cp.png")
plt.savefig(cp_output_png, dpi=300, bbox_inches="tight")
print(f"\nCarte par code postal sauvegardée sous: {cp_output_png}")

# Exporter les données en CSV
cp_output_csv = os.path.join(results_dir, "frequence_mvt_terrain_cp.csv")
cp_map_data[["Code_posta", "LIB", "frequence"]].to_csv(cp_output_csv, index=False)
print(f"Données par code postal exportées en CSV sous: {cp_output_csv}")

# Exporter les données en GPKG
cp_output_gpkg = os.path.join(results_dir, "frequence_mvt_terrain_cp.gpkg")
cp_map_data.to_file(cp_output_gpkg, driver="GPKG")
print(f"Données par code postal exportées en GPKG sous: {cp_output_gpkg}")

print("\nProcessus terminé.")

# Afficher les fichiers générés dans le dossier
print("\nListe des fichiers générés:")
for file in os.listdir(results_dir):
    print(f" - {file}")
