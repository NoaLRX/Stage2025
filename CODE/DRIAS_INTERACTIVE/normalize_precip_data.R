#!/usr/bin/env Rscript

# Script pour normaliser les données de précipitations été et hiver par rapport à REF
# Ce script traite uniquement les données dans CUMUL_PRECIP_HIVER et CUMUL_PRECIP_ETE

# Charger les bibliothèques nécessaires
library(sf)
library(dplyr)
library(stringr)

# Fonction pour créer les dossiers de destination
setup_directories <- function() {
  # Créer les répertoires principaux
  main_dirs <- c(
    "Data/DRIAS_NORM",
    "Data/DRIAS_NORM/CUMUL_PRECIP_HIVER",
    "Data/DRIAS_NORM/CUMUL_PRECIP_HIVER/Resultats",
    "Data/DRIAS_NORM/CUMUL_PRECIP_ETE",
    "Data/DRIAS_NORM/CUMUL_PRECIP_ETE/Resultats"
  )
  
  for (dir in main_dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      cat("Répertoire créé:", dir, "\n")
    }
  }
}

# Fonction pour normaliser un fichier GPKG de précipitations
normalize_precip_gpkg <- function(input_file, output_file) {
  cat("Traitement du fichier:", input_file, "\n")
  
  # Ajouter des logs spécifiques pour le scénario 8.5
  is_scenario_85 <- grepl("8_5", basename(input_file))
  if (is_scenario_85) {
    cat("***** FICHIER SCÉNARIO 8.5 DÉTECTÉ *****\n")
  }
  
  tryCatch({
    # Charger le fichier
    data <- st_read(input_file, quiet = TRUE)
    
    # Vérifier s'il s'agit d'un fichier de référence ou d'un scénario
    if (grepl("REF", basename(input_file))) {
      # Pour les fichiers REF, simplement copier
      cat("Fichier de référence détecté, copie directe...\n")
      st_write(data, output_file, quiet = TRUE, delete_dsn = TRUE)
    } else {
      # Pour les scénarios, trouver le fichier REF correspondant
      ref_file <- str_replace(input_file, "(2_6|4_5|8_5)", "REF")
      
      if (!file.exists(ref_file)) {
        cat("ERREUR: Fichier de référence non trouvé:", ref_file, "\n")
        return(FALSE)
      }
      
      # Charger le fichier de référence
      ref_data <- st_read(ref_file, quiet = TRUE)
      
      # Afficher les noms de colonnes pour debug
      cat("Colonnes dans le fichier de scénario:", paste(colnames(data), collapse = ", "), "\n")
      cat("Colonnes dans le fichier de référence:", paste(colnames(ref_data), collapse = ", "), "\n")
      
      # Vérifier que les deux fichiers ont les mêmes entités
      if (nrow(data) != nrow(ref_data)) {
        cat("ATTENTION: Nombre d'entités différent entre le fichier de référence et le scénario!\n")
      }
      
      # Créer une copie de travail
      normalized_data <- data
      
      # Déterminer la colonne de référence
      ref_col <- "NORRR_REF"  # Nom attendu par défaut
      if (!(ref_col %in% colnames(ref_data))) {
        # Rechercher une colonne alternative contenant "REF" et "NORRR"
        possible_ref_cols <- colnames(ref_data)[grepl("REF", colnames(ref_data), ignore.case = TRUE) & 
                                              grepl("NORRR", colnames(ref_data), ignore.case = TRUE)]
        
        if (length(possible_ref_cols) > 0) {
          ref_col <- possible_ref_cols[1]  # Utiliser la première colonne trouvée
          cat("Utilisation de la colonne alternative pour la référence:", ref_col, "\n")
        } else {
          # Si toujours pas trouvé, essayer de trouver une colonne avec "REF" et pouvant contenir des données de précipitation
          possible_ref_cols <- colnames(ref_data)[grepl("REF", colnames(ref_data), ignore.case = TRUE)]
          if (length(possible_ref_cols) > 0) {
            # Choisir la colonne qui correspond le plus probablement à celle qu'on cherche
            for (col in possible_ref_cols) {
              # Vérifier si c'est une colonne numérique
              if (is.numeric(ref_data[[col]])) {
                ref_col <- col
                cat("Utilisation de la colonne numérique avec REF pour la référence:", ref_col, "\n")
                break
              }
            }
          }
        }
      }
      
      # Pour chaque horizon (H1, H2, H3)
      for (horizon in c("H1", "H2", "H3")) {
        # Logs spécifiques pour H3 et scénario 8.5
        detailed_debug <- is_scenario_85 && horizon == "H3"
        
        if (detailed_debug) {
          cat("\n***** TRAITEMENT DÉTAILLÉ POUR SCÉNARIO 8.5 ET HORIZON H3 *****\n")
        }
        
        # Construire les noms de colonnes de scénario
        scenario_col <- paste0("NORRR_", horizon)
        
        # Vérifier si les colonnes existent
        if (scenario_col %in% colnames(data) && ref_col %in% colnames(ref_data)) {
          cat("Normalisation de", scenario_col, "par rapport à", ref_col, "\n")
          
          # Joindre par CODE_C
          if ("CODE_C" %in% colnames(data) && "CODE_C" %in% colnames(ref_data)) {
            # Préparation des données pour la jointure
            temp_scenario <- data %>% 
              st_drop_geometry() %>% 
              select(CODE_C, !!scenario_col)
            
            temp_ref <- ref_data %>% 
              st_drop_geometry() %>% 
              select(CODE_C, !!ref_col)
            
            # Joindre les données
            temp_joined <- left_join(temp_scenario, temp_ref, by = "CODE_C")
            
            # Pour debug - examiner un échantillon avant normalisation
            sample_size <- min(5, nrow(temp_joined))
            if (detailed_debug) sample_size <- min(20, nrow(temp_joined))
            
            cat("Échantillon avant normalisation (", sample_size, " lignes):\n")
            for (i in 1:sample_size) {
              cat("CODE_C:", temp_joined$CODE_C[i], 
                  ", Valeur scénario (", scenario_col, "):", temp_joined[[scenario_col]][i],
                  ", Valeur REF (", ref_col, "):", temp_joined[[ref_col]][i], "\n")
            }
            
            # Pour le scénario 8.5 et H3, rechercher Plabennec pour comparaison avec été
            if (detailed_debug) {
              cat("\nRecherche de Plabennec ou des communes comportant 'Plabenn' dans leur code:\n")
              plabennec_indices <- grep("29160", temp_joined$CODE_C)
              if (length(plabennec_indices) > 0) {
                for (idx in plabennec_indices) {
                  cat("COMMUNE PLABENNEC TROUVÉE - CODE_C:", temp_joined$CODE_C[idx], 
                     ", Valeur scénario:", temp_joined[[scenario_col]][idx],
                     ", Valeur REF:", temp_joined[[ref_col]][idx], "\n")
                }
              } else {
                cat("Aucune commune 'Plabennec' trouvée avec code 29160\n")
                
                # Rechercher des codes commençant par 29 (Finistère)
                finistere_indices <- grep("^29", temp_joined$CODE_C)
                if (length(finistere_indices) > 0) {
                  cat("Exemples de communes du Finistère (premiers 5):\n")
                  for (idx in finistere_indices[1:min(5, length(finistere_indices))]) {
                    cat("CODE_C:", temp_joined$CODE_C[idx], 
                       ", Valeur scénario:", temp_joined[[scenario_col]][idx],
                       ", Valeur REF:", temp_joined[[ref_col]][idx], "\n")
                  }
                }
              }
            }
            
            # Calculer correctement le pourcentage de variation
            # CORRECTION: On utilise abs(temp_joined[[ref_col]]) au dénominateur pour éviter les problèmes avec les valeurs négatives
            temp_joined$normalized <- ifelse(
              abs(temp_joined[[ref_col]]) > 0.001,  # Éviter la division par zéro
              ((temp_joined[[scenario_col]] - temp_joined[[ref_col]]) / abs(temp_joined[[ref_col]])) * 100,
              NA  # Si ref est proche de zéro, utiliser NA
            )
            
            # Debug supplémentaire pour le scénario 8.5 et H3
            if (detailed_debug) {
              # Vérifier les valeurs aberrantes (par exemple > 300%)
              extreme_values <- which(temp_joined$normalized > 300)
              if (length(extreme_values) > 0) {
                cat("\nVALEURS NORMALISÉES EXTRÊMES (>300%) DÉTECTÉES pour", length(extreme_values), "communes:\n")
                for (idx in extreme_values[1:min(10, length(extreme_values))]) {
                  cat("CODE_C:", temp_joined$CODE_C[idx], 
                     ", % Variation:", temp_joined$normalized[idx], 
                     ", Calcul: ((", temp_joined[[scenario_col]][idx], " - ", temp_joined[[ref_col]][idx], 
                     ") / |", temp_joined[[ref_col]][idx], "|) * 100\n")
                }
              }
              
              # Vérifier la distribution des valeurs
              norm_values <- temp_joined$normalized[!is.na(temp_joined$normalized)]
              cat("\nSTATISTIQUES DES VALEURS NORMALISÉES:\n")
              cat("Min:", min(norm_values, na.rm=TRUE), 
                 ", Max:", max(norm_values, na.rm=TRUE),
                 ", Moyenne:", mean(norm_values, na.rm=TRUE),
                 ", Médiane:", median(norm_values, na.rm=TRUE), "\n")
              cat("Quantiles (5%, 25%, 75%, 95%):", 
                 quantile(norm_values, probs=c(0.05, 0.25, 0.75, 0.95), na.rm=TRUE), "\n")
            }
            
            # Debug - afficher quelques résultats après normalisation
            cat("Échantillon après normalisation (", sample_size, " lignes):\n")
            for (i in 1:sample_size) {
              cat("CODE_C:", temp_joined$CODE_C[i], 
                  ", % Variation:", temp_joined$normalized[i], 
                  ", Calcul: ((", temp_joined[[scenario_col]][i], " - ", temp_joined[[ref_col]][i], 
                  ") / |", temp_joined[[ref_col]][i], "|) * 100\n")
            }
            
            # Mettre à jour les données normalisées
            for (i in 1:nrow(normalized_data)) {
              code_c <- normalized_data$CODE_C[i]
              matched_row <- which(temp_joined$CODE_C == code_c)
              if (length(matched_row) > 0) {
                normalized_data[[scenario_col]][i] <- temp_joined$normalized[matched_row[1]]
                
                # Debug pour les lignes mises à jour
                if (i <= sample_size) {
                  cat("Mise à jour ligne", i, ": CODE_C =", code_c, 
                      ", Nouvelle valeur normalisée =", normalized_data[[scenario_col]][i], "\n")
                }
              }
            }
          } else {
            # Si pas de CODE_C, joindre par position (en supposant le même ordre)
            for (i in 1:min(nrow(normalized_data), nrow(ref_data))) {
              scenario_value <- data[[scenario_col]][i]
              ref_value <- ref_data[[ref_col]][i]
              
              if (!is.na(scenario_value) && !is.na(ref_value) && abs(ref_value) > 0.001) {
                # CORRECTION: Utiliser abs(ref_value) au dénominateur
                normalized_data[[scenario_col]][i] <- ((scenario_value - ref_value) / abs(ref_value)) * 100
                
                # Debug pour l'échantillon
                if (i <= 5 || (detailed_debug && i <= 20)) {
                  cat("Position", i, ": Valeur scénario =", scenario_value, 
                      ", Valeur REF =", ref_value, 
                      ", % Variation =", normalized_data[[scenario_col]][i], "\n")
                }
              } else {
                normalized_data[[scenario_col]][i] <- NA
              }
            }
          }
          
          cat("Colonne", scenario_col, "normalisée\n")
        } else {
          if (!(scenario_col %in% colnames(data))) {
            cat("ATTENTION: Colonne", scenario_col, "non trouvée dans le fichier scénario\n")
          }
          if (!(ref_col %in% colnames(ref_data))) {
            cat("ATTENTION: Colonne", ref_col, "non trouvée dans le fichier de référence\n")
          }
        }
      }
      
      # Enregistrer le fichier normalisé
      st_write(normalized_data, output_file, quiet = TRUE, delete_dsn = TRUE)
      cat("Fichier normalisé créé:", output_file, "\n")
    }
    
    return(TRUE)
  }, error = function(e) {
    cat("ERREUR lors de la normalisation:", e$message, "\n")
    return(FALSE)
  })
}

# Fonction pour traiter un dossier
process_directory <- function(src_dir, dest_dir) {
  # Créer le dossier de destination s'il n'existe pas
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }
  
  # Lister tous les fichiers GPKG
  gpkg_files <- list.files(src_dir, pattern = "\\.gpkg$", full.names = TRUE)
  
  # Normaliser les fichiers
  for (file in gpkg_files) {
    output_file <- file.path(dest_dir, basename(file))
    normalize_precip_gpkg(file, output_file)
  }
}

# Fonction principale
main <- function() {
  cat("Démarrage de la normalisation des données de précipitations...\n")
  
  # Créer les répertoires nécessaires
  setup_directories()
  
  # Traiter les dossiers de précipitations hiver et été
  source_dirs <- c(
    "Data/CUMUL_PRECIP_HIVER/Resultats",
    "Data/CUMUL_PRECIP_ETE/Resultats"
  )
  
  dest_dirs <- c(
    "Data/DRIAS_NORM/CUMUL_PRECIP_HIVER/Resultats",
    "Data/DRIAS_NORM/CUMUL_PRECIP_ETE/Resultats"
  )
  
  # Traiter chaque dossier
  for (i in 1:length(source_dirs)) {
    if (dir.exists(source_dirs[i])) {
      cat("\nTraitement du dossier:", source_dirs[i], "\n")
      process_directory(source_dirs[i], dest_dirs[i])
    } else {
      cat("ATTENTION: Dossier source non trouvé:", source_dirs[i], "\n")
    }
  }
  
  cat("\nNormalisation des données de précipitations terminée!\n")
}

# Exécuter la fonction principale
main() 