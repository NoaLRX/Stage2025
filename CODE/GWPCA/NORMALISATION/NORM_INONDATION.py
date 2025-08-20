import pandas as pd
import geopandas as gpd
import os
import numpy as np

# Chemins des fichiers
catnat_csv = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Inondation/INO_2_CAT/catnat_gaspar.csv.csv"
jointure_csv = "/Users/noa/Desktop/PRISM/Data/MISC/CONVERTED/test.csv"
codes_postaux_shp = "/Users/noa/Desktop/PRISM/Data/MISC/Codes postaux - Communes/codes_postaux_region.shp"
output_shp = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Inondation/INO_2_CAT/inondations_par_cp.shp"

# Vérifier que les fichiers existent
for filepath in [catnat_csv, jointure_csv, codes_postaux_shp]:
    if not os.path.exists(filepath):
        raise FileNotFoundError(f"Le fichier {filepath} n'existe pas")

# 1. Charger et filtrer les données CATNAT (inondations)
print("Chargement des données CATNAT...")
catnat_df = pd.read_csv(catnat_csv, sep=";", encoding="latin1", low_memory=False)
print(f"Nombre total d'événements: {len(catnat_df)}")

# Filtrer pour ne garder que les inondations (commençant par "Inondations")
catnat_inondations = catnat_df[catnat_df["lib_risque_jo"].str.startswith("Inondations", na=False)]
print(f"Nombre d'événements d'inondation: {len(catnat_inondations)}")

# Compter le nombre d'inondations par commune
inondations_par_commune = catnat_inondations.groupby("cod_commune").size().reset_index(name="nb_inondations")
print(f"Nombre de communes touchées par des inondations: {len(inondations_par_commune)}")

# 2. Charger le fichier de correspondance commune-code postal
print("\nChargement du fichier de correspondance commune-code postal...")
jointure_df = pd.read_csv(jointure_csv)
print(f"Nombre de correspondances commune-code postal: {len(jointure_df)}")

# Nettoyer les codes communes (enlever les guillemets)
jointure_df["Code_commu"] = jointure_df["Code_commu"].str.replace('"', '')

# 3. Fusionner pour agréger par code postal
print("\nFusion des données d'inondation avec les codes postaux...")
# Assurons-nous que les types de données correspondent pour la jointure
inondations_par_commune["cod_commune"] = inondations_par_commune["cod_commune"].astype(str)
jointure_df["Code_commu"] = jointure_df["Code_commu"].astype(str)

# Effectuer la jointure
inondations_cp = pd.merge(
    inondations_par_commune,
    jointure_df,
    left_on="cod_commune",
    right_on="Code_commu",
    how="left"
)

# Vérifier les correspondances manquantes
nb_sans_cp = inondations_cp["Code_posta"].isna().sum()
print(f"Nombre de communes sans correspondance de code postal: {nb_sans_cp}")

# Agréger par code postal
inondations_cp_agg = inondations_cp.groupby(["Code_posta", "Nom_postal"], as_index=False)["nb_inondations"].sum()
print(f"Nombre de codes postaux avec des données d'inondation: {len(inondations_cp_agg)}")

# Afficher quelques exemples de codes postaux pour déboguer
print("\nExemples de codes postaux dans les données agrégées:")
print(inondations_cp_agg[["Code_posta", "nb_inondations"]].head(10))

# 4. Joindre avec le shapefile des codes postaux
print("\nChargement du shapefile des codes postaux...")
cp_gdf = gpd.read_file(codes_postaux_shp)
print(f"Nombre de polygones de codes postaux: {len(cp_gdf)}")

# Afficher quelques exemples d'IDs de codes postaux dans le shapefile
print("\nExemples d'IDs dans le shapefile:")
print(cp_gdf[["ID", "LIB"]].head(10))

# Analyser les premiers chiffres des codes postaux dans les deux ensembles de données
print("\nAnalyse des codes postaux:")
print("Dans les données d'inondation:")
print(inondations_cp_agg["Code_posta"].astype(str).str[:2].value_counts().head(10))
print("\nDans le shapefile:")
print(cp_gdf["ID"].astype(str).str[:2].value_counts().head(10))

# Convertir les codes postaux en entiers (car ils semblent être de formats différents)
inondations_cp_agg["Code_posta"] = inondations_cp_agg["Code_posta"].astype(int)
cp_gdf["ID"] = cp_gdf["ID"].astype(int)

# Effectuer la jointure spatiale
print("\nJointure des données d'inondation avec les polygones des codes postaux...")
resultat_gdf = cp_gdf.merge(
    inondations_cp_agg,
    left_on="ID",
    right_on="Code_posta",
    how="left"
)

# Vérifier combien de correspondances ont été trouvées après la jointure
nb_avec_inondations = resultat_gdf["nb_inondations"].notna().sum()
print(f"Nombre de codes postaux avec des données d'inondation après jointure: {nb_avec_inondations}")

# Si toujours aucune correspondance, essayons une approche différente
if nb_avec_inondations == 0:
    print("\nAucune correspondance trouvée avec l'approche standard, essai d'une méthode alternative...")
    
    # Créer une table de correspondance entre les codes postaux des deux ensembles
    # Prenons les 2 (ou 3) premiers chiffres comme base de correspondance (département)
    inondations_cp_agg["Dept"] = inondations_cp_agg["Code_posta"].astype(str).str[:2]
    cp_gdf["Dept"] = cp_gdf["ID"].astype(str).str[:2]
    
    # Joindre sur le département pour créer une relation approximative
    print("Jointure basée sur le département...")
    resultat_dept = cp_gdf.merge(
        inondations_cp_agg[["Dept", "nb_inondations"]].groupby("Dept").sum().reset_index(),
        on="Dept",
        how="left"
    )
    
    # Vérifier combien de correspondances ont été trouvées après la jointure par département
    print(f"Nombre de codes postaux avec des données d'inondation après jointure par département: {resultat_dept['nb_inondations'].notna().sum()}")
    
    # Si cette approche fonctionne, utilisons-la
    if resultat_dept["nb_inondations"].notna().sum() > 0:
        resultat_gdf = resultat_dept
        print("Utilisation de la jointure par département.")
    
# Remplacer les NaN par 0 pour les codes postaux sans inondation
resultat_gdf["nb_inondations"] = resultat_gdf["nb_inondations"].fillna(0)

# 5. Sauvegarder le résultat
print(f"\nEnregistrement du résultat dans {output_shp}...")
resultat_gdf.to_file(output_shp)

print("\nStatistiques sur les inondations par code postal:")
print(f"Nombre total de codes postaux: {len(resultat_gdf)}")
print(f"Nombre de codes postaux avec au moins une inondation: {(resultat_gdf['nb_inondations'] > 0).sum()}")
if len(resultat_gdf[resultat_gdf["nb_inondations"] > 0]) > 0:
    print(f"Nombre maximum d'inondations par code postal: {resultat_gdf['nb_inondations'].max()}")
    print(f"Nombre moyen d'inondations par code postal: {resultat_gdf['nb_inondations'].mean():.2f}")

print("\nTraitement terminé avec succès!") 