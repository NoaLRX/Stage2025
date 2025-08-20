#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import geopandas as gpd
import os

# Définition des chemins des fichiers
zones_protegees_path = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Biodiv/IND_BIO_1_1/N_ENP_PN_S_000.shp"
codes_postaux_path = "/Users/noa/Desktop/PRISM/Data/MISC/Codes postaux - Communes/codes_postaux_region.shp"
output_path = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Biodiv/IND_BIO_1_1/codes_postaux_zones_protegees.shp"

# Vérification de l'existence des fichiers
print("Vérification des fichiers d'entrée...")
if not os.path.exists(zones_protegees_path):
    print(f"ERREUR: Le fichier des zones protégées n'existe pas: {zones_protegees_path}")
    exit(1)
if not os.path.exists(codes_postaux_path):
    print(f"ERREUR: Le fichier des codes postaux n'existe pas: {codes_postaux_path}")
    exit(1)

# Chargement des données
print("Chargement des zones protégées...")
zones_protegees = gpd.read_file(zones_protegees_path)
print(f"Nombre de zones protégées: {len(zones_protegees)}")

print("Chargement des codes postaux...")
codes_postaux = gpd.read_file(codes_postaux_path)
print(f"Nombre de codes postaux: {len(codes_postaux)}")

# Vérifier que les géométries sont valides
print("Vérification et correction des géométries invalides...")
zones_protegees.geometry = zones_protegees.geometry.buffer(0)
codes_postaux.geometry = codes_postaux.geometry.buffer(0)

# S'assurer que les deux couches utilisent le même système de coordonnées
print("Uniformisation des systèmes de coordonnées...")
if zones_protegees.crs != codes_postaux.crs:
    zones_protegees = zones_protegees.to_crs(codes_postaux.crs)

# Suppression des colonnes non nécessaires
print("Préparation des données...")
cols_to_drop = ['SURF', 'POP2010', 'MEN2010']
for col in cols_to_drop:
    if col in codes_postaux.columns:
        codes_postaux.drop([col], axis=1, inplace=True)

# Initialiser la colonne ZONE_PROT à 0 (non protégé)
codes_postaux['ZONE_PROT'] = 0

# Effectuer un spatial join pour identifier les intersections
# On utilise 'intersects' comme prédicat et 'inner' comme type de jointure
print("Identification des codes postaux qui intersectent les zones protégées...")
spatial_join = gpd.sjoin(codes_postaux, zones_protegees, how='inner', predicate='intersects')

# Marquer les codes postaux qui intersectent avec au moins une zone protégée
if not spatial_join.empty:
    # Récupérer les index uniques des codes postaux qui intersectent
    intersect_indices = spatial_join.index.unique()
    print(f"Nombre de codes postaux intersectant des zones protégées: {len(intersect_indices)}")
    
    # Marquer ces codes postaux
    codes_postaux.loc[intersect_indices, 'ZONE_PROT'] = 1
else:
    print("ATTENTION: Aucun code postal n'intersecte de zone protégée!")

# Vérifier les résultats
protected_count = codes_postaux['ZONE_PROT'].sum()
total = len(codes_postaux)
print(f"Résultat: {protected_count}/{total} codes postaux ({protected_count/total*100:.2f}%) dans des zones protégées")

# Sauvegarder le résultat
print("Sauvegarde des résultats...")
codes_postaux.to_file(output_path)

print(f"Traitement terminé! Le fichier résultat a été sauvegardé à l'emplacement: {output_path}")
