# =============== PARAMÈTRES À CONFIGURER ===============
# Chemins des fichiers
# Chemin du fichier DRIAS au format TEXTE (.txt) OU dossier contenant plusieurs fichiers
chemins_entree = [
    "/Users/noa/Desktop/PRISM/Scripts/DRIAS_INTERACTIVE/Data/CUMUL_PRECIP_ETE",
]
# Type de référence à utiliser
TYPE_REFERENCE = "POSTAL"  # Options: "POSTAL" ou "COMMUNE"

# Chemin du fichier SHP des codes postaux
postal_path = "/Users/noa/Desktop/PRISM/Data/MISC/Codes postaux - Communes/codes_postaux_region.shp"

# Chemin du fichier SHP des communes
communes_path = "/Users/noa/Desktop/PRISM/Data/MISC/CONVERTED/test.shp"

# Chemin du fichier SHP "département"
departements_path = "/Users/noa/Desktop/PRISM/Data/MISC/Autres polygones administratifs/DEPARTEMENT.shp"

# Options de traitement
GENERER_VERIFICATION = False  # True pour OUI, False pour NON - Générer les fichiers de vérification
GENERER_CARTES = False        # True pour OUI, False pour NON - Générer les cartes
GENERER_CSV = False           # True pour OUI, False pour NON - Générer les fichiers CSV
CALCUL_DEPARTEMENT = False     # True pour OUI, False pour NON - Effectuer les calculs par département
TRAITER_DOSSIER_COMPLET = True  # True pour traiter tous les fichiers .txt du dossier, False pour traiter un seul fichier

# Si TRAITER_DOSSIER_COMPLET est False, spécifier le fichier individuel à traiter
fichier_individuel = "/Users/noa/Desktop/TESTING/INDICATEURS_SAISONNIERS_ETE/DRIAS_ETE_REFERENCE.txt"
# =====================================================
# =========== NE PAS MODIFIER ===========
# =====================================================
import pandas as pd
import geopandas as gpd
import numpy as np
import matplotlib.pyplot as plt
import random
import os
from shapely.geometry import Point
from matplotlib.colors import LinearSegmentedColormap
import matplotlib.patheffects as pe
import re

# Fonction pour traiter un seul fichier
def traiter_fichier(fichier_entree, reference_path, departements_path):
    print("\n" + "="*80)
    print(f"TRAITEMENT DU FICHIER: {fichier_entree}")
    print("="*80 + "\n")
    
    fichier_sortie = fichier_entree.replace(".txt", "_clean.csv")
    
    # Nettoyage du fichier CSV
    # Vérifier si le fichier existe
    if not os.path.exists(fichier_entree):
        print(f"Erreur: Le fichier {fichier_entree} n'existe pas.")
        return False
    
    try:
        print(f"Lecture du fichier: {fichier_entree}")
        
        # Lire tout le contenu du fichier
        with open(fichier_entree, 'r', encoding='utf-8') as file:
            content = file.read()
            
        # Afficher les 300 premiers caractères pour le débogage
        print("Début du fichier (300 premiers caractères):")
        print(content[:300])
        
        # Diviser le contenu en lignes
        lines = content.splitlines()
        print(f"Nombre de lignes dans le fichier: {len(lines)}")
        
        # Variables pour stocker l'en-tête et les données
        header = ""
        data_lines = []
        
        # On cherche d'abord la ligne contenant "Format des enregistrements"
        format_line_index = -1
        for i, line in enumerate(lines):
            if "Format des enregistrements" in line:
                format_line_index = i
                print(f"Ligne 'Format des enregistrements' trouvée à l'index {i}: {line}")
                break
        
        if format_line_index == -1:
            print("ERREUR: Ligne 'Format des enregistrements' non trouvée!")
            # Essayons une approche alternative
            for i, line in enumerate(lines):
                if ";" in line and line.startswith("#"):
                    print(f"Ligne d'en-tête potentielle trouvée à l'index {i}: {line}")
                    header = line.replace("#", "").strip()
                    break
        else:
            # Chercher la ligne d'en-tête après "Format des enregistrements"
            for i in range(format_line_index + 1, min(format_line_index + 5, len(lines))):
                if i < len(lines) and ";" in lines[i] and lines[i].startswith("#"):
                    print(f"Ligne d'en-tête trouvée à l'index {i}: {lines[i]}")
                    header = lines[i].replace("#", "").strip()
                    break
        
        # Si on a trouvé l'en-tête, on cherche les lignes de données
        if header:
            print(f"En-tête trouvée: {header}")
            # Renommer PÃ©riode en Saison dans l'en-tête
            header = header.replace("PÃ©riode", "Saison").replace("Période", "Saison")
            print(f"En-tête après renommage: {header}")
            
            # Chercher les lignes de données (celles contenant des points-virgules mais ne commençant pas par #)
            for i, line in enumerate(lines):
                if ";" in line and not line.startswith("#") and len(line.strip()) > 0:
                    data_lines.append(line.strip())
                    if len(data_lines) <= 3:  # Afficher seulement les 3 premières lignes pour le débogage
                        print(f"Ligne de données trouvée: {line.strip()}")
            
            print(f"Nombre total de lignes de données trouvées: {len(data_lines)}")
        else:
            print("ERREUR: Impossible de trouver la ligne d'en-tête!")
            # Essayons une autre méthode - chercher directement les lignes avec des nombres et des points-virgules
            for i, line in enumerate(lines):
                if ";" in line and any(c.isdigit() for c in line) and not line.startswith("#"):
                    if not data_lines:
                        print("Recherche de lignes de données sans en-tête...")
                    data_lines.append(line.strip())
                    if len(data_lines) <= 3:
                        print(f"Ligne de données potentielle: {line.strip()}")
            
            if data_lines:
                print(f"Trouvé {len(data_lines)} lignes de données sans en-tête.")
                # Essayer de déduire l'en-tête à partir des lignes de données
                if ";" in data_lines[0]:
                    num_columns = data_lines[0].count(";")
                    print(f"Création d'un en-tête générique avec {num_columns+1} colonnes")
                    header = ";".join([f"Col{i}" for i in range(1, num_columns+2)])
                    print(f"En-tête générée: {header}")
        
        # Écrire le fichier de sortie si on a des données
        if header and data_lines:
            with open(fichier_sortie, 'w', encoding='utf-8') as output_file:
                output_file.write(header + "\n")
                for line in data_lines:
                    output_file.write(line + "\n")
            
            print(f"Succès! Fichier nettoyé enregistré dans {fichier_sortie}")
        else:
            print("Échec: Impossible de créer le fichier de sortie (en-tête ou données manquantes)")
            return False
    
        # Chemin du fichier nettoyé
        csv_path = fichier_sortie
        
        # Création des noms de fichiers de sortie basés sur le fichier d'entrée
        base_dir = os.path.dirname(csv_path)
        base_filename = os.path.splitext(os.path.basename(csv_path))[0]
        
        # Création des dossiers de sortie uniquement si nécessaire
        resultats_dir = os.path.join(base_dir, "Resultats")
        os.makedirs(resultats_dir, exist_ok=True)
        
        if GENERER_VERIFICATION:
            verification_dir = os.path.join(base_dir, "Verification")
            os.makedirs(verification_dir, exist_ok=True)
            
        if GENERER_CARTES:
            cartes_dir = os.path.join(base_dir, "Cartes")
            os.makedirs(cartes_dir, exist_ok=True)
        
        # Chargement des données
        print("Chargement du CSV...")
        df = pd.read_csv(csv_path, sep=";")
        print(f"CSV chargé avec {len(df)} lignes")
        
        # Renommer la colonne PÃ©riode en Saison si elle existe
        if 'PÃ©riode' in df.columns:
            df.rename(columns={'PÃ©riode': 'Saison'}, inplace=True)
            print("Colonne 'PÃ©riode' renommée en 'Saison'")
        elif 'Période' in df.columns:
            df.rename(columns={'Période': 'Saison'}, inplace=True)
            print("Colonne 'Période' renommée en 'Saison'")
        
        # Mettre à jour les valeurs de la colonne Saison pour être plus explicites
        if 'Saison' in df.columns:
            # Créer un mapping pour les saisons
            saison_mapping = {1: 'Hiver', 2: 'Été'}
            
            # Vérifier si df['Saison'] contient des entiers ou des chaînes
            if df['Saison'].dtype == 'int64' or df['Saison'].dtype == 'float64':
                # Appliquer le mapping
                df['Saison'] = df['Saison'].map(saison_mapping)
                print("Valeurs de la colonne 'Saison' converties: 1 -> 'Hiver', 2 -> 'Été'")
        
        # Extraction des scénarios et variables disponibles
        scenarios = df['Saison'].unique() if 'Saison' in df.columns else []
        print(f"Scénarios trouvés: {scenarios}")
        
        # Détection automatique des colonnes de données numériques (variables climatiques)
        # Exclure les colonnes connues qui ne sont pas des variables climatiques
        colonnes_a_exclure = ['Point', 'Latitude', 'Longitude', 'Contexte', 'Période', 'PÃ©riode', 'Saison']
        
        # Filtrer les colonnes numériques et exclure les colonnes "Unnamed"
        colonnes_variables = [col for col in df.columns 
                             if col not in colonnes_a_exclure 
                             and df[col].dtype in [np.float64, np.int64]
                             and not col.startswith('Unnamed')]
        
        print(f"Variables climatiques détectées: {colonnes_variables}")
        
        # Si aucune variable n'est détectée, on sort avec une erreur
        if not colonnes_variables:
            raise ValueError("Aucune variable climatique numérique détectée dans le jeu de données")
        
        print("Chargement du fichier de référence...")
        reference_gdf = gpd.read_file(reference_path)
        print(f"Entités chargées : {len(reference_gdf)} entités")
        
        # Créer un GeoDataFrame en utilisant Longitude et Latitude
        geometry = [Point(xy) for xy in zip(df['Longitude'], df['Latitude'])]
        gdf = gpd.GeoDataFrame(df, geometry=geometry)
        
        # Définir le CRS et reprojeter vers Lambert 93
        gdf.crs = "EPSG:4326"  # WGS 84 (standard pour lat/long)
        gdf_lambert = gdf.to_crs("EPSG:2154")  # Lambert 93
        
        # Créer la grille de 4000m x 4000m
        buffer_gdf = gdf_lambert.copy()
        buffer_gdf['geometry'] = gdf_lambert.buffer(4000, cap_style=3, join_style=2)
        
        # Assurons-nous que le fichier de référence a le même CRS
        if reference_gdf.crs != buffer_gdf.crs:
            print(f"Reprojection du fichier de référence de {reference_gdf.crs} vers {buffer_gdf.crs}")
            reference_gdf = reference_gdf.to_crs(buffer_gdf.crs)
        
        # Ajouter une colonne d'index original pour la traçabilité
        reference_gdf['index_original'] = reference_gdf.index
        
        # Dictionnaire pour stocker les GeoDataFrames résultants pour chaque scénario et variable
        resultats_communes = {}
        
        # Fonction pour créer une palette de couleurs de type température
        def create_temperature_cmap():
            colors = [
                '#1E5CB3',  # bleu foncé
                '#176FC1',  # bleu
                '#0A8ED8',  # bleu clair
                '#03A1E6',  # cyan
                '#1BCBF2',  # turquoise
                '#00ff80',  # vert-bleu
                '#00ff40',  # vert clair
                '#00ff00',  # vert
                '#40ff00',  # vert-jaune
                '#80ff00',  # jaune-vert
                '#bfff00',  # jaune clair
                '#ffff00',  # jaune
                '#ffbf00',  # orange clair
                '#ff8000',  # orange
                '#ff4000',  # orange-rouge
                '#ff0000'   # rouge
            ]
            return LinearSegmentedColormap.from_list('rainbow', colors)
        
        # Boucle sur chaque scénario et chaque variable
        for scenario in scenarios:
            print(f"\n==== Traitement du scénario: {scenario} ====")
            
            # Filtrer les données pour le scénario actuel
            grille_scenario = buffer_gdf[buffer_gdf['Saison'] == scenario]
            print(f"Grille filtrée pour le scénario {scenario}: {len(grille_scenario)} entités")
            
            # Créer un GeoDataFrame pour chaque variable dans ce scénario
            for variable in colonnes_variables:
                print(f"\n--- Traitement de la variable: {variable} ---")
                
                # Vérifier si la variable existe et contient des données numériques valides
                if variable not in grille_scenario.columns:
                    print(f"La variable {variable} n'existe pas dans le jeu de données")
                    continue
                    
                if not np.issubdtype(grille_scenario[variable].dtype, np.number) or grille_scenario[variable].isnull().all():
                    print(f"La variable {variable} ne contient pas de données numériques valides")
                    continue
                
                # Créer une copie du GeoDataFrame des références pour ce scénario et cette variable
                communes_resultat = reference_gdf.copy()
                
                # Nom de la colonne pour stocker le résultat
                colonne_resultat = f'{variable}_{scenario}'
                communes_resultat[colonne_resultat] = np.nan
                
                # Liste pour stocker les détails des vérifications
                verification_details = []
                
                # Sélectionner aléatoirement des communes pour vérification détaillée (5 communes)
                nb_verifications = min(5, len(communes_resultat))
                indices_verification = random.sample(range(len(communes_resultat)), nb_verifications)
                
                print(f"Calcul de la moyenne pondérée pour {len(communes_resultat)} entités...")
                print(f"Vérification détaillée pour {len(indices_verification)} entités")
                
                # Pour chaque entité (commune ou code postal)
                for idx, commune in communes_resultat.iterrows():
                    if idx % 100 == 0:
                        print(f"Traitement de l'entité {idx}/{len(communes_resultat)}")
                    
                    # Variables pour calculer la moyenne pondérée
                    total_pondéré = 0
                    total_poids = 0
                    
                    # Récupérer la géométrie et l'aire de l'entité
                    geom_commune = commune.geometry
                    aire_commune = geom_commune.area
                    
                    # Liste pour stocker les détails de calcul si c'est une entité à vérifier
                    details_calcul = []
                    faire_verification = idx in indices_verification and GENERER_VERIFICATION
                    
                    # Pour chaque cellule de la grille qui intersecte cette entité
                    cellules_intersectees = grille_scenario[grille_scenario.intersects(geom_commune)]
                    
                    for _, cellule in cellules_intersectees.iterrows():
                        # Calculer l'intersection
                        intersection = geom_commune.intersection(cellule.geometry)
                        aire_intersection = intersection.area
                        
                        # Calculer le poids (proportion de l'entité dans cette cellule)
                        poids = aire_intersection / aire_commune
                        
                        # Ajouter à la somme pondérée
                        total_pondéré += cellule[variable] * poids
                        total_poids += poids
                        
                        # Si c'est une entité à vérifier, stocker les détails
                        if faire_verification:
                            details_calcul.append({
                                'id_cellule': cellule.name,
                                'valeur': cellule[variable],
                                'aire_intersection': aire_intersection,
                                'poids': poids,
                                'contribution': cellule[variable] * poids
                            })
                    
                    # Calculer et stocker la moyenne pondérée
                    if total_poids > 0:
                        valeur_moyenne = total_pondéré / total_poids
                        communes_resultat.at[idx, colonne_resultat] = valeur_moyenne
                        
                        # Si c'est une entité à vérifier, stocker tous les détails
                        if faire_verification:
                            nom_commune = commune.get('NOM', commune.get('nom', f'Commune_{idx}'))
                            verification_details.append({
                                'idx': idx,
                                'nom_commune': nom_commune,
                                'aire_commune': aire_commune,
                                'nb_cellules': len(cellules_intersectees),
                                'valeur_moyenne': valeur_moyenne,
                                'details': details_calcul
                            })
                            
                            # Ajouter des informations d'identification pour le débogage
                            if 'index_original' in commune:
                                verification_details[-1]['index_original'] = commune['index_original']
                            if 'INSEE_COM' in commune:
                                verification_details[-1]['INSEE_COM'] = commune['INSEE_COM']
                            if 'code_insee' in commune:
                                verification_details[-1]['code_insee'] = commune['code_insee']
                
                # Créer un rapport de vérification pour cette variable et ce scénario
                if GENERER_VERIFICATION and verification_details:
                    rapport_path = os.path.join(verification_dir, f"verification_calcul_{variable}_{scenario}.txt")
                    with open(rapport_path, 'w') as f:
                        f.write(f"RAPPORT DE VÉRIFICATION DU CALCUL DE {variable} MOYEN - SCÉNARIO {scenario}\n")
                        f.write("=" * 70 + "\n\n")
                        
                        for verif in verification_details:
                            f.write(f"COMMUNE: {verif['nom_commune']} (index_original: {verif['idx']})\n")
                            f.write(f"Aire totale: {verif['aire_commune']:.2f} m²\n")
                            f.write(f"Nombre de cellules intersectées: {verif['nb_cellules']}\n")
                            f.write(f"Valeur moyenne calculée: {verif['valeur_moyenne']:.4f}\n\n")
                            
                            f.write("Détails du calcul:\n")
                            f.write("-" * 80 + "\n")
                            f.write(f"{'ID Cellule':<12} {variable:<10} {'Aire Inter.':<15} {'Poids':<10} {'Contribution':<15}\n")
                            
                            total_verification = 0
                            sum_poids = 0
                            for detail in verif['details']:
                                f.write(f"{detail['id_cellule']:<12} {detail['valeur']:<10.2f} {detail['aire_intersection']:<15.2f} "
                                       f"{detail['poids']:<10.4f} {detail['contribution']:<15.4f}\n")
                                total_verification += detail['contribution']
                                sum_poids += detail['poids']
                            
                            f.write("-" * 80 + "\n")
                            f.write(f"Somme des poids: {sum_poids:.4f}\n")
                            f.write(f"Total des contributions: {total_verification:.4f}\n")
                            f.write(f"Moyenne pondérée: {total_verification:.4f}\n")
                            f.write("\n" + "=" * 70 + "\n\n")
                    
                    print(f"Rapport de vérification créé: {rapport_path}")
                
                # Visualiser les résultats pour cette variable et ce scénario
                if GENERER_VERIFICATION and verification_details:
                    fig, ax = plt.subplots(figsize=(12, 12))
                    cmap = create_temperature_cmap()
                    
                    # Déterminer les valeurs min/max pour la légende
                    vmin = communes_resultat[colonne_resultat].min()
                    vmax = communes_resultat[colonne_resultat].max()
                    
                    # Tracer les communes avec la valeur moyenne
                    communes_resultat.plot(column=colonne_resultat, cmap=cmap, legend=True, ax=ax,
                                         vmin=vmin, vmax=vmax,
                                         legend_kwds={'shrink': 0.6, 'aspect': 20, 'label': f'Valeur moyenne de {variable}'})
                    
                    # Mettre en évidence les communes utilisées pour la vérification
                    for idx in indices_verification:
                        communes_resultat.iloc[[idx]].plot(ax=ax, facecolor='none', edgecolor='black', linewidth=2)
                    
                    plt.title(f'Valeur moyenne de {variable} par commune - Scénario {scenario}\n(communes vérifiées en bordure noire)')
                    plt.savefig(os.path.join(verification_dir, f"communes_{variable}_{scenario}_moyen.png"), dpi=300)
                    plt.close()
                
                # Créer des images de vérification pour les communes sélectionnées
                if GENERER_VERIFICATION and verification_details:
                    for verif in verification_details:
                        fig, ax = plt.subplots(figsize=(10, 10))
                        
                        # Récupérer la commune
                        commune = communes_resultat.iloc[verif['idx']]
                        
                        # Dessiner la commune
                        communes_resultat.iloc[[verif['idx']]].plot(ax=ax, color='lightgrey', edgecolor='black')
                        
                        # Dessiner les cellules qui intersectent la commune
                        cellules_ids = [d['id_cellule'] for d in verif['details']]
                        if cellules_ids:  # Vérifier que la liste n'est pas vide
                            grille_scenario.loc[cellules_ids].plot(ax=ax, column=variable, cmap=cmap, alpha=0.5)
                            
                            # Ajouter des étiquettes pour chaque cellule
                            for detail in verif['details']:
                                if detail['id_cellule'] in grille_scenario.index:
                                    cell = grille_scenario.loc[detail['id_cellule']]
                                    centroid = cell.geometry.centroid
                                    plt.annotate(f"ID: {detail['id_cellule']}\n{variable}: {cell[variable]:.2f}\nPoids: {detail['poids']:.3f}",
                                                (centroid.x, centroid.y), ha='center', 
                                                bbox=dict(boxstyle="round,pad=0.3", fc="white", alpha=0.7))
                        
                        # Ajout de l'ID de la commune dans le titre
                        commune_id_display = verif['idx'] + 1
                        plt.title(f"Vérification pour la commune {commune_id_display}\n{verif['nom_commune']}\n{variable} ({scenario}) - Valeur moyenne: {verif['valeur_moyenne']:.4f}")
                        
                        output_img = os.path.join(verification_dir, f"verification_commune_{commune_id_display}_{verif['nom_commune'].replace(' ', '_')}_{variable}_{scenario}.png")
                        plt.savefig(output_img, dpi=300)
                        plt.close()
                        print(f"Image de vérification créée: {output_img}")
                
                # Créer la carte PDF pour cette variable et ce scénario
                if GENERER_CARTES:
                    print(f"Création de la carte pour {variable} - Scénario {scenario}...")
                    fig, ax = plt.subplots(figsize=(11, 8.5))  # Format US Letter
                    
                    # Déterminer les valeurs min/max pour la légende
                    vmin = communes_resultat[colonne_resultat].min()
                    vmax = communes_resultat[colonne_resultat].max()
                    
                    # Tracer les communes avec la valeur moyenne
                    communes_resultat.plot(column=colonne_resultat, cmap=cmap, 
                                         vmin=vmin, vmax=vmax, ax=ax, legend=True,
                                         legend_kwds={'shrink': 0.6, 'aspect': 20, 
                                                      'label': f'Valeur moyenne de {variable} - {scenario}'})
                    
                    # Ajouter les frontières des communes
                    communes_resultat.boundary.plot(ax=ax, linewidth=0.2, color='black')
                    
                    # Titre en gros caractères
                    plt.title(f'Valeur moyenne de {variable} par commune - Scénario {scenario}', 
                              fontsize=18, fontweight='bold')
                    plt.tight_layout()
                    
                    # Enregistrer au format PDF
                    communes_pdf = os.path.join(cartes_dir, f"carte_COMMUNE_{variable}_{scenario}.pdf")
                    plt.savefig(communes_pdf, format='pdf', dpi=300, bbox_inches='tight')
                    plt.close()
                    print(f"Carte créée: {communes_pdf}")
                
                # Stocker ce GeoDataFrame dans le dictionnaire des résultats
                resultats_communes[(scenario, variable)] = communes_resultat
            
            # Créer également une carte de la grille SAFRAN pour ce scénario
            if GENERER_CARTES:
                print(f"Création des cartes SAFRAN pour le scénario {scenario}...")
                for variable in colonnes_variables:
                    if variable not in grille_scenario.columns:
                        continue
                        
                    fig, ax = plt.subplots(figsize=(11, 8.5))  # Format US Letter
                    cmap = create_temperature_cmap()
                    
                    # Déterminer les valeurs min/max pour la légende
                    vmin = grille_scenario[variable].min()
                    vmax = grille_scenario[variable].max()
                    
                    # Tracer la grille SAFRAN
                    grille_scenario.plot(column=variable, cmap=cmap, alpha=0.8, 
                                       vmin=vmin, vmax=vmax, ax=ax, legend=True,
                                       legend_kwds={'shrink': 0.6, 'aspect': 20, 'label': f'Valeur de {variable}'})
                    
                    # Titre en gros caractères
                    plt.title(f'Distribution spatiale des valeurs {variable} - Scénario {scenario}', 
                              fontsize=18, fontweight='bold')
                    plt.tight_layout()
                    
                    # Enregistrer au format PDF
                    safran_pdf = os.path.join(cartes_dir, f"carte_SAFRAN_{variable}_{scenario}.pdf")
                    plt.savefig(safran_pdf, format='pdf', dpi=300, bbox_inches='tight')
                    plt.close()
                    print(f"Carte SAFRAN créée: {safran_pdf}")
        
        # Créer un fichier unique contenant toutes les variables et tous les scénarios
        print("\nPréparation des données pour le fichier final...")
        
        # Création d'un DataFrame combiné avec toutes les variables et scénarios
        combined_gdf = None
        combined_dep_gdf = None
        
        if resultats_communes:
            # Traitement des communes
            base_gdf = list(resultats_communes.values())[0]
            
            # Créer un GeoDataFrame avec uniquement l'identifiant de commune et la géométrie
            colonnes_base = ['index_original', 'geometry']
            
            # Ajouter les colonnes NOM ou nom si elles existent
            for col in ['NOM', 'nom', 'INSEE_COM', 'code_insee']:
                if col in base_gdf.columns:
                    colonnes_base.append(col)
            
            combined_gdf = base_gdf[colonnes_base].copy()
            
            # Ajouter toutes les colonnes de valeur de chaque GeoDataFrame
            for (scenario, variable), communes_gdf in resultats_communes.items():
                colonne_resultat = f'{variable}_{scenario}'
                if colonne_resultat in communes_gdf.columns:
                    combined_gdf[colonne_resultat] = communes_gdf[colonne_resultat]
            
            # Traitement des départements uniquement si l'option est activée
            if CALCUL_DEPARTEMENT:
                # Traitement des départements
                print("\nChargement des départements...")
                departements_gdf = gpd.read_file(departements_path)
                departements_jointure = gpd.read_file(departements_path)
                
                # Vérifier et sélectionner les colonnes d'intérêt pour les départements
                colonnes_requises_dep = ["geometry"]
                colonnes_a_renommer_dep = {}
                
                # Vérification et mapping des colonnes potentielles pour les départements
                if "NOM" in departements_jointure.columns:
                    colonnes_requises_dep.append("NOM")
                elif "Nom" in departements_jointure.columns:
                    colonnes_requises_dep.append("Nom")
                    colonnes_a_renommer_dep["Nom"] = "NOM"
                
                if "INSEE_DEP" in departements_jointure.columns:
                    colonnes_requises_dep.append("INSEE_DEP")
                elif "CODE_DEPT" in departements_jointure.columns:
                    colonnes_requises_dep.append("CODE_DEPT")
                    colonnes_a_renommer_dep["CODE_DEPT"] = "INSEE_DEP"
                
                # Sélectionner uniquement les colonnes existantes
                colonnes_existantes_dep = [col for col in colonnes_requises_dep if col in departements_jointure.columns]
                departements_jointure = departements_jointure[colonnes_existantes_dep].copy()
                
                # Renommer les colonnes selon le mapping défini
                departements_jointure = departements_jointure.rename(columns=colonnes_a_renommer_dep)
                
                # Assurer que les départements ont le même CRS que la grille
                if departements_jointure.crs != buffer_gdf.crs:
                    departements_jointure = departements_jointure.to_crs(buffer_gdf.crs)
                
                # Créer un GeoDataFrame pour les départements
                colonnes_dep = ['geometry']
                if 'NOM' in departements_jointure.columns:
                    colonnes_dep.append('NOM')
                if 'INSEE_DEP' in departements_jointure.columns:
                    colonnes_dep.append('INSEE_DEP')
                
                combined_dep_gdf = departements_jointure[colonnes_dep].copy()
                
                # Calculer les moyennes pondérées pour chaque département
                print("Calcul des moyennes pondérées par département...")
                for scenario in scenarios:
                    print(f"\nTraitement du scénario: {scenario}")
                    grille_scenario = buffer_gdf[buffer_gdf['Saison'] == scenario]
                    
                    for variable in colonnes_variables:
                        if variable not in grille_scenario.columns:
                            continue
                        
                        print(f"Calcul des moyennes pour la variable: {variable}")
                        colonne_resultat = f'{variable}_{scenario}'
                        combined_dep_gdf[colonne_resultat] = np.nan
                        
                        # Traiter tous les départements d'un coup pour cette variable
                        for idx, departement in combined_dep_gdf.iterrows():
                            if idx % 10 == 0:  # Afficher la progression tous les 10 départements
                                print(f"  Progression: département {idx+1}/{len(combined_dep_gdf)}")
                            valeur_moyenne = calculer_moyenne_ponderee_departement(
                                grille_scenario, departement, variable, scenario)
                            combined_dep_gdf.at[idx, colonne_resultat] = valeur_moyenne
                        
                        # Générer des cartes pour les départements si l'option est activée
                        if GENERER_CARTES:
                            print(f"Création de la carte départementale pour {variable} - Scénario {scenario}...")
                            fig, ax = plt.subplots(figsize=(11, 8.5))  # Format US Letter
                            cmap = create_temperature_cmap()
                            
                            # Déterminer les valeurs min/max pour la légende
                            vmin = combined_dep_gdf[colonne_resultat].min()
                            vmax = combined_dep_gdf[colonne_resultat].max()
                            
                            # Tracer les départements avec la valeur moyenne
                            combined_dep_gdf.plot(column=colonne_resultat, cmap=cmap, 
                                               vmin=vmin, vmax=vmax, ax=ax, legend=True,
                                               legend_kwds={'shrink': 0.6, 'aspect': 20, 
                                                          'label': f'Valeur moyenne de {variable} - {scenario}'})
                            
                            # Ajouter les frontières des départements
                            combined_dep_gdf.boundary.plot(ax=ax, linewidth=0.2, color='black')
                            
                            # Titre en gros caractères
                            plt.title(f'Valeur moyenne de {variable} par département - Scénario {scenario}', 
                                    fontsize=18, fontweight='bold')
                            plt.tight_layout()
                            
                            # Enregistrer au format PDF
                            dep_pdf = os.path.join(cartes_dir, f"carte_DEPARTEMENT_{variable}_{scenario}.pdf")
                            plt.savefig(dep_pdf, format='pdf', dpi=300, bbox_inches='tight')
                            plt.close()
                            print(f"Carte départementale créée: {dep_pdf}")
            
            # Ajout d'une étape de jointure spatiale
            print("\nRéalisation de la jointure spatiale et création du fichier final...")
            
            # Charger le fichier des entités avec les colonnes d'intérêt
            try:
                # Charger le fichier SHP des références (communes ou codes postaux)
                if TYPE_REFERENCE == "COMMUNE":
                    unites_jointure = gpd.read_file(communes_path)
                    # Mapping des colonnes pour le format commune
                    colonnes_requises = ["geometry"]
                    colonnes_a_renommer = {}
                    
                    # Vérification et mapping des colonnes potentielles pour les communes
                    if "NOM" in unites_jointure.columns:
                        colonnes_requises.append("NOM")
                        colonnes_a_renommer["NOM"] = "LIB"
                    elif "nom" in unites_jointure.columns:
                        colonnes_requises.append("nom")
                        colonnes_a_renommer["nom"] = "LIB"
                    
                    if "INSEE_COM" in unites_jointure.columns:
                        colonnes_requises.append("INSEE_COM")
                        colonnes_a_renommer["INSEE_COM"] = "CODE_C"
                    elif "Code_commu" in unites_jointure.columns:
                        colonnes_requises.append("Code_commu")
                        colonnes_a_renommer["Code_commu"] = "CODE_C"
                    elif "code_insee" in unites_jointure.columns:
                        colonnes_requises.append("code_insee")
                        colonnes_a_renommer["code_insee"] = "CODE_C"
                    
                    if "DEP" in unites_jointure.columns:
                        colonnes_requises.append("DEP")
                    elif "CODE_DEPT" in unites_jointure.columns:
                        colonnes_requises.append("CODE_DEPT")
                        colonnes_a_renommer["CODE_DEPT"] = "DEP"
                else:  # TYPE_REFERENCE == "POSTAL"
                    unites_jointure = gpd.read_file(postal_path)
                    # Mapping des colonnes pour le format code postal
                    colonnes_requises = ["geometry"]
                    colonnes_a_renommer = {}
                    
                    # Vérification et mapping des colonnes potentielles pour les codes postaux
                    if "LIB" in unites_jointure.columns:
                        colonnes_requises.append("LIB")
                    elif "NOM" in unites_jointure.columns:
                        colonnes_requises.append("NOM")
                        colonnes_a_renommer["NOM"] = "LIB"
                    
                    if "ID" in unites_jointure.columns:
                        colonnes_requises.append("ID")
                        colonnes_a_renommer["ID"] = "CODE_C"
                    
                    if "DEP" in unites_jointure.columns:
                        colonnes_requises.append("DEP")
                
                # Sélectionner uniquement les colonnes existantes
                colonnes_existantes = [col for col in colonnes_requises if col in unites_jointure.columns]
                unites_jointure = unites_jointure[colonnes_existantes].copy()
                
                # Renommer les colonnes selon le mapping défini
                unites_jointure = unites_jointure.rename(columns=colonnes_a_renommer)
                
                # Effectuer la jointure spatiale avec le fichier combiné
                if combined_gdf is not None:
                    # Assurez-vous que les deux GeoDataFrames ont le même CRS
                    if unites_jointure.crs != combined_gdf.crs:
                        unites_jointure = unites_jointure.to_crs(combined_gdf.crs)
                    
                    print("Exécution de la jointure spatiale...")
                    
                    # Option 1: utiliser sjoin avec un seul point par entité
                    unites_point = unites_jointure.copy()
                    unites_point['geometry'] = unites_point.geometry.centroid
                    
                    # Effectuer la jointure spatiale avec les centroides
                    result_jointure = gpd.sjoin_nearest(unites_point, combined_gdf, how="left", max_distance=5000)
                    print(f"Jointure effectuée avec {len(result_jointure)} résultats")
                    
                    # Restaurer la géométrie originale
                    result_jointure['geometry'] = unites_jointure.geometry
                    
                    # Vérifier pour des communes sans correspondance
                    missing = result_jointure[result_jointure['index_right'].isna()]
                    if len(missing) > 0:
                        print(f"Attention: {len(missing)} communes n'ont pas trouvé de correspondance")
                        
                        # Essayer avec une distance plus grande pour les communes sans correspondance
                        if len(missing) > 0:
                            print("Tentative avec une distance plus grande pour les communes sans correspondance...")
                            missing_communes = unites_point[unites_point.index.isin(missing.index)]
                            second_attempt = gpd.sjoin_nearest(missing_communes, combined_gdf, how="left", max_distance=10000)
                            
                            # Mettre à jour les valeurs manquantes
                            for idx in second_attempt.index:
                                if not pd.isna(second_attempt.loc[idx, 'index_right']):
                                    for col in result_jointure.columns:
                                        if col in second_attempt.columns and col != 'geometry':
                                            result_jointure.loc[idx, col] = second_attempt.loc[idx, col]
                    
                    # Supprimer les colonnes redondantes
                    result_jointure = result_jointure.loc[:, ~result_jointure.columns.str.contains('index_|^idx$')]
                    
                    # Sauvegarder le résultat final pour les communes/codes postaux
                    jointure_output = os.path.join(resultats_dir, f"{base_filename}_FINAL_RESULTS_{TYPE_REFERENCE}.gpkg")
                    result_jointure.to_file(jointure_output, driver="GPKG")
                    print(f"Fichier final pour {TYPE_REFERENCE} sauvegardé: {jointure_output}")
                    
                    # Générer un fichier CSV si nécessaire
                    if GENERER_CSV:
                        # Créer une version CSV sans la géométrie
                        csv_columns = [col for col in result_jointure.columns if col != 'geometry']
                        csv_output = os.path.join(resultats_dir, f"{base_filename}_FINAL_RESULTS_{TYPE_REFERENCE}.csv")
                        result_jointure[csv_columns].to_csv(csv_output, index=False)
                        print(f"Fichier CSV final pour {TYPE_REFERENCE} sauvegardé: {csv_output}")
                
                # Sauvegarder les résultats pour les départements si l'option est activée
                if CALCUL_DEPARTEMENT and combined_dep_gdf is not None:
                    # Simplifier les géométries des départements pour réduire la taille du fichier
                    print("Simplification des géométries des départements pour réduire la taille du fichier...")
                    combined_dep_gdf_simplified = combined_dep_gdf.copy()
                    
                    # Paramètre de tolérance pour la simplification (en mètres)
                    tolerance = 100  # Ajuster si nécessaire
                    
                    # Simplifier la géométrie des départements
                    combined_dep_gdf_simplified['geometry'] = combined_dep_gdf_simplified.geometry.simplify(tolerance, preserve_topology=True)
                    
                    # Vérifier la réduction de taille
                    taille_avant = sum(combined_dep_gdf.geometry.apply(lambda g: len(g.wkt)))
                    taille_apres = sum(combined_dep_gdf_simplified.geometry.apply(lambda g: len(g.wkt)))
                    reduction = (1 - taille_apres/taille_avant) * 100 if taille_avant > 0 else 0
                    print(f"Réduction de la taille des géométries: {reduction:.2f}% (tolérance: {tolerance}m)")
                    
                    # Si la réduction n'est pas suffisante, essayer avec une tolérance plus grande
                    if reduction < 50 and taille_avant > 10000000:  # Si moins de 50% de réduction et la taille est grande
                        tolerance = 200  # Doubler la tolérance
                        print(f"Augmentation de la tolérance à {tolerance}m pour une meilleure réduction...")
                        combined_dep_gdf_simplified['geometry'] = combined_dep_gdf.geometry.simplify(tolerance, preserve_topology=True)
                        
                        taille_apres = sum(combined_dep_gdf_simplified.geometry.apply(lambda g: len(g.wkt)))
                        reduction = (1 - taille_apres/taille_avant) * 100 if taille_avant > 0 else 0
                        print(f"Nouvelle réduction: {reduction:.2f}%")
                    
                    # Sauvegarder avec les géométries simplifiées
                    dep_output = os.path.join(resultats_dir, f"{base_filename}_FINAL_RESULTS_DEPARTEMENTS_{TYPE_REFERENCE}.gpkg")
                    combined_dep_gdf_simplified.to_file(dep_output, driver="GPKG")
                    print(f"Fichier final pour les départements sauvegardé: {dep_output}")
                    
                    if GENERER_CSV:
                        # Créer également une version CSV sans la géométrie
                        csv_columns_dep = [col for col in combined_dep_gdf_simplified.columns if col != 'geometry']
                        csv_dep_output = os.path.join(resultats_dir, f"{base_filename}_FINAL_RESULTS_DEPARTEMENTS_{TYPE_REFERENCE}.csv")
                        combined_dep_gdf_simplified[csv_columns_dep].to_csv(csv_dep_output, index=False)
                        print(f"Fichier CSV final pour les départements sauvegardé: {csv_dep_output}")
            
            except Exception as e:
                import traceback
                print(f"Erreur lors de la jointure spatiale: {e}")
                traceback.print_exc()
        
        print("\nTraitement terminé avec succès!")
        return True
    
    except Exception as e:
        import traceback
        print(f"Erreur lors du traitement: {e}")
        traceback.print_exc()
        return False

def calculer_moyenne_ponderee_departement(grille_scenario, departement, variable, scenario):
    """Calcule la moyenne pondérée pour un département de manière optimisée."""
    try:
        # Créer un masque pour les cellules qui intersectent le département
        cellules_intersectees = grille_scenario[grille_scenario.intersects(departement.geometry)]
        
        if len(cellules_intersectees) == 0:
            return np.nan
        
        # Calculer toutes les intersections d'un coup
        intersections = gpd.overlay(
            gpd.GeoDataFrame({'geometry': [departement.geometry]}, crs=grille_scenario.crs),
            cellules_intersectees,
            how='intersection'
        )
        
        if len(intersections) == 0:
            return np.nan
        
        # Calculer les aires des intersections
        aires_intersection = intersections.geometry.area
        
        # Calculer les poids (proportions des intersections)
        aire_departement = departement.geometry.area
        poids = aires_intersection / aire_departement
        
        # Calculer la moyenne pondérée
        valeurs = intersections[variable]
        moyenne_ponderee = np.sum(valeurs * poids) / np.sum(poids)
        
        return moyenne_ponderee
        
    except Exception as e:
        print(f"Erreur lors du calcul de la moyenne pondérée: {e}")
        return np.nan

# Code principal pour traiter un fichier ou un dossier complet
if __name__ == "__main__":
    # Sélectionner le chemin de référence selon le type choisi
    reference_path = communes_path if TYPE_REFERENCE == "COMMUNE" else postal_path
    print(f"Utilisation de la référence: {TYPE_REFERENCE} avec le fichier: {reference_path}")
    
    if TRAITER_DOSSIER_COMPLET:
        # Traiter chaque dossier d'entrée
        for chemin_entree in chemins_entree:
            print(f"Traitement du dossier: {chemin_entree}")
            if os.path.isdir(chemin_entree):
                # Lister tous les fichiers .txt dans le dossier
                fichiers_txt = [os.path.join(chemin_entree, f) for f in os.listdir(chemin_entree) if f.endswith('.txt')]
                print(f"Nombre de fichiers .txt trouvés: {len(fichiers_txt)}")
                
                # Traiter chaque fichier
                for fichier in fichiers_txt:
                    traiter_fichier(fichier, reference_path, departements_path)
            else:
                print(f"Erreur: {chemin_entree} n'est pas un dossier valide.")
    else:
        print(f"Traitement du fichier individuel: {fichier_individuel}")
        traiter_fichier(fichier_individuel, reference_path, departements_path)