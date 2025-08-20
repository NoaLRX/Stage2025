#!/usr/bin/env Rscript

# Script pour créer des données DRIAS normalisées (pourcentage de variation par rapport à REF)
# Ce script générera des versions normalisées des données pour les variables spécifiées

# Charger les bibliothèques nécessaires
library(sf)
library(dplyr)
library(stringr)
library(readxl)
library(openxlsx)

# Définir les variables à normaliser
variables_to_normalize <- c(
  "NORTAV", "NORSD", "NORTX35", "NORTR", "NORTXHWD", "NORTNCWD", 
  "NORTNFD", "NORRR", "NORRR1MM", "NORFFQ98", "NORFF98"
)

# Fonction pour créer le dossier DRIAS_NORM s'il n'existe pas
setup_directories <- function() {
  # Créer les répertoires principaux
  main_dirs <- c(
    "Data/DRIAS_NORM",
    "Data/DRIAS_NORM/INDICATEURS_ANNUELS_HORIZONS",
    "Data/DRIAS_NORM/INDICATEURS_ANNUELS_HORIZONS/Resultats",
    "Data/DRIAS_NORM/INDICATEURS_SAISONNIERS_ETE",
    "Data/DRIAS_NORM/INDICATEURS_SAISONNIERS_ETE/Resultats",
    "Data/DRIAS_NORM/FEUX_INDICATEURS_ANNUELS_HORIZONS",
    "Data/DRIAS_NORM/FEUX_INDICATEURS_ANNUELS_HORIZONS/Resultats",
    "Data/DRIAS_NORM/AGRI_INDICATEURS_ANNUELS_HORIZONS",
    "Data/DRIAS_NORM/AGRI_INDICATEURS_ANNUELS_HORIZONS/Resultats"
  )
  
  for (dir in main_dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      cat("Répertoire créé:", dir, "\n")
    }
  }
}

# Fonction pour normaliser un fichier GPKG
normalize_gpkg_file <- function(input_file, output_file, variables) {
  cat("Traitement du fichier:", input_file, "\n")
  
  tryCatch({
    # Charger le fichier
    data <- st_read(input_file, quiet = TRUE)
    
    # Vérifier s'il s'agit d'un fichier de référence ou d'un scénario
    if (grepl("REFERENCE", basename(input_file))) {
      # Pour les fichiers REF, simplement copier
      cat("Fichier de référence détecté, copie directe...\n")
      st_write(data, output_file, quiet = TRUE, delete_dsn = TRUE)
    } else {
      # Pour les scénarios, trouver le fichier REF correspondant
      # Gérer les deux formats de noms de fichiers (avec ou sans '2020')
      if (grepl("DRIAS_2020_", basename(input_file))) {
        ref_file <- str_replace(input_file, "(2_6|4_5|8_5)", "REFERENCE")
      } else {
        ref_file <- str_replace(input_file, "(2_6|4_5|8_5)", "REFERENCE")
      }
      
      if (!file.exists(ref_file)) {
        cat("ERREUR: Fichier de référence non trouvé:", ref_file, "\n")
        return(FALSE)
      }
      
      # Charger le fichier de référence
      ref_data <- st_read(ref_file, quiet = TRUE)
      
      # Vérifier que les deux fichiers ont les mêmes entités (communes ou départements)
      if (nrow(data) != nrow(ref_data)) {
        cat("ATTENTION: Nombre d'entités différent entre le fichier de référence et le scénario!\n")
        # Tentative de jointure par identifiant spatial
        data_with_id <- data %>% mutate(temp_id = row_number())
        ref_data_with_id <- ref_data %>% mutate(temp_id = row_number())
      } else {
        # Même nombre d'entités, on suppose qu'elles sont dans le même ordre
        data_with_id <- data %>% mutate(temp_id = row_number())
        ref_data_with_id <- ref_data %>% mutate(temp_id = row_number())
      }
      
      # Créer une copie de travail
      normalized_data <- data
      
      # Pour chaque variable à normaliser
      for (var in variables) {
        # Pour chaque horizon (H1, H2, H3)
        for (horizon in c("H1", "H2", "H3")) {
          # Construire les noms de colonnes
          scenario_col <- paste0(var, "_", horizon)
          ref_col <- paste0(var, "_REF")
          
          # Vérifier si les colonnes existent
          if (scenario_col %in% colnames(data) && ref_col %in% colnames(ref_data)) {
            cat("Normalisation de", scenario_col, "par rapport à", ref_col, "\n")
            
            # Créer un dataframe temporaire pour la jointure
            temp_scenario <- data_with_id %>% 
              st_drop_geometry() %>% 
              select(temp_id, !!scenario_col)
            
            temp_ref <- ref_data_with_id %>% 
              st_drop_geometry() %>% 
              select(temp_id, !!ref_col)
            
            # Joindre les données
            temp_joined <- left_join(temp_scenario, temp_ref, by = "temp_id")
            
            # Calculer le pourcentage de variation
            # Formule: ((scenario - ref) / ref) * 100
            temp_joined$normalized <- ifelse(
              abs(temp_joined[[ref_col]]) > 0.001,  # Éviter la division par zéro
              ((temp_joined[[scenario_col]] - temp_joined[[ref_col]]) / abs(temp_joined[[ref_col]])) * 100,
              NA  # Si ref est proche de zéro, utiliser NA
            )
            
            # Copier les valeurs normalisées dans le jeu de données final
            for (i in 1:nrow(normalized_data)) {
              if (i <= nrow(temp_joined)) {
                normalized_data[[scenario_col]][i] <- temp_joined$normalized[i]
              }
            }
          } else {
            if (!(scenario_col %in% colnames(data))) {
              cat("ATTENTION: Colonne", scenario_col, "non trouvée dans le fichier scénario\n")
            }
            if (!(ref_col %in% colnames(ref_data))) {
              cat("ATTENTION: Colonne", ref_col, "non trouvée dans le fichier de référence\n")
            }
          }
        }
      }
      
      # Enregistrer le fichier normalisé
      st_write(normalized_data, output_file, quiet = TRUE, delete_dsn = TRUE)
    }
    
    cat("Fichier normalisé créé:", output_file, "\n")
    return(TRUE)
  }, error = function(e) {
    cat("ERREUR lors de la normalisation:", e$message, "\n")
    return(FALSE)
  })
}

# Fonction pour normaliser un fichier Excel
normalize_excel_file <- function(input_file, output_file, variables) {
  cat("Traitement du fichier Excel:", input_file, "\n")
  
  tryCatch({
    # Charger le fichier
    data <- read_excel(input_file)
    
    # Vérifier s'il s'agit d'un fichier de référence ou d'un scénario
    if (grepl("REFERENCE", basename(input_file))) {
      # Pour les fichiers REF, simplement copier
      cat("Fichier de référence détecté, copie directe...\n")
      write.xlsx(data, output_file, overwrite = TRUE)
    } else {
      # Pour les scénarios, trouver le fichier REF correspondant
      # Gérer les deux formats de noms de fichiers (avec ou sans '2020')
      if (grepl("DRIAS_2020_", basename(input_file))) {
        ref_file <- str_replace(input_file, "(2_6|4_5|8_5)", "REFERENCE")
      } else {
        ref_file <- str_replace(input_file, "(2_6|4_5|8_5)", "REFERENCE")
      }
      
      if (!file.exists(ref_file)) {
        cat("ERREUR: Fichier de référence non trouvé:", ref_file, "\n")
        return(FALSE)
      }
      
      # Charger le fichier de référence
      ref_data <- read_excel(ref_file)
      
      # Créer une copie de travail
      normalized_data <- data
      
      # Pour chaque variable à normaliser
      for (var in variables) {
        # Pour chaque horizon (H1, H2, H3)
        for (horizon in c("H1", "H2", "H3")) {
          # Construire les noms de colonnes
          scenario_col <- paste0(var, "_", horizon)
          ref_col <- paste0(var, "_REF")
          
          # Vérifier si les colonnes existent
          if (scenario_col %in% colnames(data) && ref_col %in% colnames(ref_data)) {
            cat("Normalisation de", scenario_col, "par rapport à", ref_col, "\n")
            
            # Normaliser directement si même structure
            if ("CODE_C" %in% colnames(data) && "CODE_C" %in% colnames(ref_data) || 
                "CODE_INSEE" %in% colnames(data) && "CODE_INSEE" %in% colnames(ref_data)) {
              
              # Déterminer quelle colonne utiliser pour la jointure
              join_col <- if ("CODE_C" %in% colnames(data)) "CODE_C" else "CODE_INSEE"
              
              # Créer des dataframes temporaires pour la jointure
              temp_scenario <- data %>% select(!!join_col, !!scenario_col)
              temp_ref <- ref_data %>% select(!!join_col, !!ref_col)
              
              # Joindre les données
              temp_joined <- left_join(temp_scenario, temp_ref, by = join_col)
              
              # Calculer le pourcentage de variation
              temp_joined$normalized <- ifelse(
                abs(temp_joined[[ref_col]]) > 0.001,  # Éviter la division par zéro
                ((temp_joined[[scenario_col]] - temp_joined[[ref_col]]) / abs(temp_joined[[ref_col]])) * 100,
                NA  # Si ref est proche de zéro, utiliser NA
              )
              
              # Mettre à jour les données normalisées
              normalized_data[[scenario_col]] <- temp_joined$normalized
            } else {
              # Si pas de colonne d'identifiant commune, normaliser par position
              for (i in 1:nrow(normalized_data)) {
                if (i <= nrow(ref_data)) {
                  ref_value <- ref_data[[ref_col]][i]
                  scenario_value <- data[[scenario_col]][i]
                  
                  if (!is.na(ref_value) && !is.na(scenario_value) && abs(ref_value) > 0.001) {
                    normalized_data[[scenario_col]][i] <- ((scenario_value - ref_value) / abs(ref_value)) * 100
                  } else {
                    normalized_data[[scenario_col]][i] <- NA
                  }
                }
              }
            }
          } else {
            if (!(scenario_col %in% colnames(data))) {
              cat("ATTENTION: Colonne", scenario_col, "non trouvée dans le fichier scénario\n")
            }
            if (!(ref_col %in% colnames(ref_data))) {
              cat("ATTENTION: Colonne", ref_col, "non trouvée dans le fichier de référence\n")
            }
          }
        }
      }
      
      # Enregistrer le fichier normalisé
      write.xlsx(normalized_data, output_file, overwrite = TRUE)
    }
    
    cat("Fichier Excel normalisé créé:", output_file, "\n")
    return(TRUE)
  }, error = function(e) {
    cat("ERREUR lors de la normalisation Excel:", e$message, "\n")
    return(FALSE)
  })
}

# Fonction principale de traitement pour un dossier
process_directory <- function(src_dir, dest_dir, variables) {
  # Créer le dossier de destination s'il n'existe pas
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }
  
  # Filtrer pour garder uniquement les fichiers COMMUNES pour INDICATEURS_ANNUELS_HORIZONS
  if (grepl("INDICATEURS_ANNUELS_HORIZONS", src_dir)) {
    # Lister les fichiers GPKG au format COMMUNES
    gpkg_files <- list.files(src_dir, pattern = "COMMUNES\\.gpkg$", full.names = TRUE)
    
    # Lister les fichiers Excel au format COMMUNES
    excel_files <- list.files(src_dir, pattern = "COMMUNES\\.xlsx$", full.names = TRUE)
  } else {
    # Pour les autres dossiers, prendre tous les fichiers
    gpkg_files <- list.files(src_dir, pattern = "\\.gpkg$", full.names = TRUE)
    excel_files <- list.files(src_dir, pattern = "\\.xlsx$", full.names = TRUE)
  }
  
  # Normaliser les fichiers GPKG
  for (file in gpkg_files) {
    output_file <- file.path(dest_dir, basename(file))
    normalize_gpkg_file(file, output_file, variables)
  }
  
  # Normaliser les fichiers Excel
  for (file in excel_files) {
    output_file <- file.path(dest_dir, basename(file))
    normalize_excel_file(file, output_file, variables)
  }
}

# Fonction principale
main <- function() {
  cat("Démarrage de la normalisation des données DRIAS...\n")
  
  # Créer les répertoires nécessaires
  setup_directories()
  
  # Traiter les différents dossiers de données
  source_dirs <- c(
    "Data/INDICATEURS_ANNUELS_HORIZONS/Resultats",
    "Data/INDICATEURS_SAISONNIERS_ETE/Resultats",
    "Data/FEUX_INDICATEURS_ANNUELS_HORIZONS/Resultats",
    "Data/AGRI_INDICATEURS_ANNUELS_HORIZONS/Resultats"
  )
  
  dest_dirs <- c(
    "Data/DRIAS_NORM/INDICATEURS_ANNUELS_HORIZONS/Resultats",
    "Data/DRIAS_NORM/INDICATEURS_SAISONNIERS_ETE/Resultats",
    "Data/DRIAS_NORM/FEUX_INDICATEURS_ANNUELS_HORIZONS/Resultats",
    "Data/DRIAS_NORM/AGRI_INDICATEURS_ANNUELS_HORIZONS/Resultats"
  )
  
  # Traiter chaque dossier
  for (i in 1:length(source_dirs)) {
    if (dir.exists(source_dirs[i])) {
      cat("\nTraitement du dossier:", source_dirs[i], "\n")
      process_directory(source_dirs[i], dest_dirs[i], variables_to_normalize)
    } else {
      cat("ATTENTION: Dossier source non trouvé:", source_dirs[i], "\n")
    }
  }
  
  cat("\nNormalisation des données DRIAS terminée!\n")
}

# Exécuter la fonction principale
main() 