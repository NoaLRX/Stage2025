#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import geopandas as gpd
import os
import glob
from pathlib import Path
import pandas as pd
import re

# Chemins des fichiers
base_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/SécheresseRGA"
output_gpkg = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/resultats_agregation.gpkg"

# Initialisation du DataFrame final
final_gdf = None

# Fonction pour extraire le scenario et le traiter correctement
def extract_scenario(filename):
    # Rechercher les motifs comme "2_6", "4_5", "8_5"
    match = re.search(r'(\d+)_(\d+)', filename)
    if match:
        return f"RCP{match.group(1)}_{match.group(2)}"
    elif "REF" in filename:
        return "REF"
    elif "H1" in filename:
        return "H1"
    elif "H2" in filename:
        return "H2"
    elif "H3" in filename:
        return "H3"
    else:
        return "UNKNOWN"

# Fonction générique pour traiter les données DRIAS
def process_drias_data(base_path, prefix_code, variable_name, column_prefix):
    print(f"Traitement de {variable_name} (Données DRIAS)...")
    data_dir = os.path.join(base_path, "Resultats")
    
    # Trouver tous les fichiers gpkg disponibles
    data_files = glob.glob(os.path.join(data_dir, "*.gpkg"))
    print(f"Fichiers {variable_name} trouvés: {data_files}")
    
    global final_gdf
    
    for data_file in data_files:
        # Extraire le scénario du nom de fichier
        filename = os.path.basename(data_file)
        scenario = extract_scenario(filename)
        
        print(f"Traitement de {filename}, scénario détecté: {scenario}")
        try:
            data_gdf = gpd.read_file(data_file)
            
            # Afficher les colonnes disponibles pour le debug
            print(f"Colonnes disponibles dans {data_file}: {list(data_gdf.columns)}")
            
            # Vérifier si les colonnes attendues existent
            rename_dict = {prefix_code: "code_postal"}
            
            # Identifier toutes les colonnes pertinentes commençant par le préfixe spécifié
            for col in data_gdf.columns:
                if col.startswith(column_prefix):
                    # Extraire la partie après le préfixe (comme H1, H2, H3, REF)
                    suffix = col.replace(f"{column_prefix}_", "")
                    if suffix == scenario:  # Format comme PREFIX_REF pour scenario REF
                        rename_dict[col] = f"{variable_name}_{scenario}"
                    elif suffix in ["H1", "H2", "H3"]:  # Format comme PREFIX_H1
                        rename_dict[col] = f"{variable_name}_{scenario}_{suffix}"
                    else:  # Autres cas, utiliser le nom de la colonne comme suffixe
                        rename_dict[col] = f"{variable_name}_{scenario}_{suffix}"
            
            # Si aucune colonne trouvée, passer au prochain fichier
            if len(rename_dict) <= 1:  # Seulement le code postal est présent
                print(f"Aucune colonne de données trouvée dans {data_file}")
                continue
            
            # Renommer les colonnes
            print(f"Colonnes à renommer: {rename_dict}")
            data_gdf = data_gdf.rename(columns=rename_dict)
            
            # Sélectionner uniquement les colonnes nécessaires qui existent
            columns_to_keep = ["code_postal"] + [v for k, v in rename_dict.items() if k != prefix_code]
            existing_columns = [col for col in columns_to_keep if col in data_gdf.columns]
            
            # S'assurer que code_postal est dans les colonnes
            if "code_postal" not in existing_columns:
                print(f"Colonne code_postal non trouvée dans {data_file}")
                continue
                
            data_gdf = data_gdf[existing_columns]
            
            # Fusionner avec le DataFrame final
            if final_gdf is None:
                # Pour le premier jeu de données, conserver la géométrie
                data_gdf_with_geom = gpd.read_file(data_file)
                data_gdf_with_geom = data_gdf_with_geom.rename(columns={prefix_code: "code_postal"})
                if "geometry" in data_gdf_with_geom.columns:
                    final_gdf = gpd.GeoDataFrame(data_gdf, geometry=data_gdf_with_geom["geometry"])
                else:
                    final_gdf = data_gdf
            else:
                final_gdf = final_gdf.merge(data_gdf, on="code_postal", how="left")
        except Exception as e:
            print(f"Erreur lors du traitement de {data_file}: {str(e)}")
            continue

# Fonction pour traiter les fichiers individuels (non-DRIAS)
def process_single_file(file_path, id_column, value_column, new_column_name):
    print(f"Traitement de {new_column_name}...")
    global final_gdf
    
    try:
        if file_path.endswith('.shp'):
            data_gdf = gpd.read_file(file_path)
        else:
            data_gdf = gpd.read_file(file_path)
        
        # Renommer les colonnes
        data_gdf = data_gdf.rename(columns={id_column: "code_postal", value_column: new_column_name})
        
        # Sélectionner les colonnes nécessaires
        columns_to_keep = ["code_postal", new_column_name]
        if "geometry" in data_gdf.columns and final_gdf is None:
            columns_to_keep.append("geometry")
            
        data_gdf = data_gdf[columns_to_keep]
        
        # Fusionner avec le DataFrame final
        if final_gdf is None:
            final_gdf = data_gdf
        else:
            final_gdf = final_gdf.merge(data_gdf[["code_postal", new_column_name]], on="code_postal", how="left")
    except Exception as e:
        print(f"Erreur lors du traitement de {file_path}: {str(e)}")

# Traitement des variables SécheresseRGA
print("=== Traitement du dossier SécheresseRGA ===")

# RGA_2_CAT
process_single_file(
    os.path.join(base_dir, "RGA_2_CAT/postal_secheresse.shp"), 
    "ID", 
    "nb_sechere", 
    "RGA_2_CAT"
)

# RGA_1_SSWI (Données DRIAS)
process_drias_data(
    os.path.join(base_dir, "RGA_1_SSWI"),
    "CODE_C",
    "RGA_1_SSWI",
    "NORSWIAV"
)

# RGA_1_RGA
process_single_file(
    os.path.join(base_dir, "RGA_1_RGA/alea_argile_postal.shp"),
    "entity_id",
    "niveau_moy",
    "RGA_1_RGA"
)

# RGA_1_JPLUIE (Données DRIAS)
process_drias_data(
    os.path.join(base_dir, "RGA_1_JPLUIE"),
    "CODE_C",
    "RGA_1_JPLUIE",
    "NORRR1MM"
)

# Traitement des variables SécheresseBrute
print("=== Traitement du dossier SécheresseBrute ===")
base_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/SécheresseBrute"

# SEC_1_SPI (Données DRIAS)
process_drias_data(
    os.path.join(base_dir, "SEC_1_SPI"),
    "CODE_C",
    "SEC_1_SPI",
    "NORTPSPI"
)

# SEC_1_SURFACE (Données DRIAS)
process_drias_data(
    os.path.join(base_dir, "SEC_1_SURFACE"),
    "CODE_C",
    "SEC_1_SURFACE",
    "NORIS700"
)

# Traitement des variables TGN
print("=== Traitement du dossier TGN ===")
base_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/TGN"

# TGN_1_VENF (Données DRIAS)
process_drias_data(
    os.path.join(base_dir, "TGN_1_VENF"),
    "CODE_C",
    "TGN_1_VENF",
    "NORFFQ98"
)

# Traitement des variables IncendieFeuxForêts
print("=== Traitement du dossier IncendieFeuxForêts ===")
base_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/IncendieFeuxForêts"

# FF_2_IFM40 (Données DRIAS)
process_drias_data(
    os.path.join(base_dir, "FF_2_IFM40"),
    "CODE_C",
    "FF_2_IFM40",
    "NORIFM40"
)

# FF_1_CLC
process_single_file(
    os.path.join(base_dir, "FF_1_CLC/resultats_feux_clc.shp"),
    "ID",
    "SCORE",
    "FF_1_CLC"
)

# Traitement des variables Vagues2Chaleur
print("=== Traitement du dossier Vagues2Chaleur ===")
base_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Vagues2Chaleur"

# VCH_1_HIST (Données DRIAS)
process_drias_data(
    os.path.join(base_dir, "VCH_1_HIST"),
    "CODE_C",
    "VCH_1_HIST",
    "NORTXHWD"
)

# VCH_1_NTROP (Données DRIAS)
process_drias_data(
    os.path.join(base_dir, "VCH_1_NTROP"),
    "CODE_C",
    "VCH_1_NTROP",
    "NORTR"
)

# Traitement des variables Vagues2Froid
print("=== Traitement du dossier Vagues2Froid ===")
base_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Vagues2Froid"

# VFR_1_HIST (Données DRIAS)
process_drias_data(
    os.path.join(base_dir, "VFR_1_HIST"),
    "CODE_C",
    "VFR_1_HIST",
    "NORTNCWD"
)

# Traitement des variables Vagues2Gel
print("=== Traitement du dossier Vagues2Gel ===")
base_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Vagues2Gel"

# VGEL_1_HIST (Données DRIAS)
process_drias_data(
    base_dir,
    "CODE_C",
    "VGEL_1_HIST",
    "NORTNFD"
)

# Traitement des variables Mouvement2Terrain
print("=== Traitement du dossier Mouvement2Terrain ===")
base_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Mouvement2Terrain"

# MVT_2_MASSIF
process_single_file(
    os.path.join(base_dir, "MVT_2_MASSIF/score_massifs_cp.gpkg"),
    "ID",
    "SCORE_MASSIF",
    "MVT_2_MASSIF"
)

# MVT_1_MVT
process_single_file(
    os.path.join(base_dir, "MVT_1_MVT/mouvements_par_code_postal.gpkg"),
    "ID",
    "movement_count",
    "MVT_1_MVT"
)

# MVT_1_CAV
process_single_file(
    os.path.join(base_dir, "MVT_1_CAV/cavites_par_code_postal_l2e.gpkg"),
    "ID",
    "cavity_count",
    "MVT_1_CAV"
)

# Traitement des variables Modification_Air
print("=== Traitement du dossier Modification_Air ===")
base_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Modification_Air"

# MTA_1_ETMJ (Données DRIAS)
process_drias_data(
    base_dir,
    "CODE_C",
    "MTA_1_ETMJ",
    "ATAV"
)

# Traitement des variables Modification_Vent
print("=== Traitement du dossier Modification_Vent ===")
base_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Modification_Vent"

# MRV_1_VENF (Données DRIAS)
process_drias_data(
    os.path.join(base_dir, "MRV_1_VENF"),
    "CODE_C",
    "MRV_1_VENF",
    "NORFFQ98"
)

# Traitement des variables Modification_Precipitation
print("=== Traitement du dossier Modification_Precipitation ===")
base_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Modification_Precipitation"

# MRP_1_NPJ (Données DRIAS)
process_drias_data(
    os.path.join(base_dir, "MRP_1_NJP"),
    "CODE_C",
    "MRP_1_NPJ",
    "ARR1MM"
)

# MRP_1_ECP (Données DRIAS)
process_drias_data(
    os.path.join(base_dir, "MRP_1_ECP"),
    "CODE_C",
    "MRP_1_ECP",
    "ARR"
)

# Traitement des variables Erosion
print("=== Traitement du dossier Erosion ===")
base_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Erosion"

# EDL_1_ECO
process_single_file(
    os.path.join(base_dir, "codes_postaux_erosion_corrige.gpkg"),
    "ID",
    "taux_evolution",
    "EDL_1_ECO"
)

# Traitement des variables Inondation
print("=== Traitement du dossier Inondation ===")
base_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Inondation"

# INO_2_CAT
process_single_file(
    os.path.join(base_dir, "INO_2_CAT/postal_inondation.shp"), 
    "ID",
    "nb_inondat",
    "INO_2_CAT"
)

# INO_1_SUB
process_single_file(
    os.path.join(base_dir, "INO_1_SUB/inondation_codes_postaux.gpkg"),
    "CODE_POST",
    "score_normalise",
    "INO_1_SUB"
)

# INO_1_RUI
process_single_file(
    os.path.join(base_dir, "inondation_rui_dco_codes_postaux.gpkg"),
    "CODE_POST",
    "score_rui",
    "INO_1_RUI"
)

# INO_1_DCO
process_single_file(
    os.path.join(base_dir, "inondation_rui_dco_codes_postaux.gpkg"),
    "CODE_POST",
    "score_dco",
    "INO_1_DCO"
)

# INO_1_PLUIEXT (Données DRIAS)
process_drias_data(
    os.path.join(base_dir, "INO_1_PLUIEXT"),
    "CODE_C",
    "INO_1_PLUIEXT",
    "NORRR99"
)

# INO_1_NAP
process_single_file(
    os.path.join(base_dir, "INO_1_NAP/risque_inondation_nappe_par_cp.gpkg"),
    "ID",
    "RISK_NAP",
    "INO_1_NAP"
)

# INO_1_IMP
process_single_file(
    os.path.join(base_dir, "INO_1_IMP/impermeabilisation_codes_postaux.gpkg"),
    "CODE_POST",
    "score_normalise",
    "INO_1_IMP"
)

# Traitement des variables Elevation_Mer
print("=== Traitement du dossier Elevation_Mer ===")
base_dir = "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Elevation_Mer"

# ENM_1_ALT
process_single_file(
    os.path.join(base_dir, "elevation_score_cp.gpkg"),
    "ID",
    "SCORE",
    "ENM_1_ALT"
)

# Enregistrer le résultat final
print(f"Nombre de lignes dans le résultat final: {len(final_gdf) if final_gdf is not None else 0}")
print(f"Colonnes dans le résultat final: {list(final_gdf.columns) if final_gdf is not None else []}")
print(f"Enregistrement des résultats dans {output_gpkg}...")

if final_gdf is not None:
    final_gdf.to_file(output_gpkg, driver="GPKG")
    print("Agrégation terminée avec succès!")
else:
    print("Erreur: Aucune donnée n'a été agrégée!")
