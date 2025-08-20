#!/usr/bin/env Rscript

# Script pour précharger toutes les données dans le cache

library(sf)
library(dplyr)
cat("Chargement des bibliothèques terminé\n")

# Chemins des dossiers
path_indicateurs_saisonniers <- "Data/INDICATEURS_SAISONNIERS_ETE/Resultats/"
path_indicateurs_annuels <- "Data/INDICATEURS_ANNUELS_HORIZONS/Resultats/"
path_feux_indicateurs <- "Data/FEUX_INDICATEURS_ANNUELS_HORIZONS/Resultats/"
path_agri_indicateurs <- "Data/AGRI_INDICATEURS_ANNUELS_HORIZONS/Resultats/"
path_cumul_precip_ete <- "Data/CUMUL_PRECIP_ETE/Resultats/"
path_cumul_precip_hiver <- "Data/CUMUL_PRECIP_HIVER/Resultats/"
path_indicateurs_saisonniers_norm <- "Data/DRIAS_NORM/INDICATEURS_SAISONNIERS_ETE/Resultats/"
path_indicateurs_annuels_norm <- "Data/DRIAS_NORM/INDICATEURS_ANNUELS_HORIZONS/Resultats/"
path_feux_indicateurs_norm <- "Data/DRIAS_NORM/FEUX_INDICATEURS_ANNUELS_HORIZONS/Resultats/"
path_agri_indicateurs_norm <- "Data/DRIAS_NORM/AGRI_INDICATEURS_ANNUELS_HORIZONS/Resultats/"
path_cumul_precip_ete_norm <- "Data/DRIAS_NORM/CUMUL_PRECIP_ETE/Resultats/"
path_cumul_precip_hiver_norm <- "Data/DRIAS_NORM/CUMUL_PRECIP_HIVER/Resultats/"
path_cache <- "Data/cache"

# Créer le dossier de cache s'il n'existe pas
if (!dir.exists(path_cache)) {
  dir.create(path_cache, recursive = TRUE)
}

# Fonction pour mettre en cache un fichier
cache_file <- function(file_path) {
  # Identifier si le fichier est normalisé
  is_normalized <- grepl("DRIAS_NORM", file_path)
  norm_suffix <- if(is_normalized) "_NORM" else ""
  
  # Créer un nom de fichier unique pour le cache
  cache_file_name <- paste0(gsub("[^a-zA-Z0-9]", "_", basename(file_path)), norm_suffix)
  cache_file_path <- file.path(path_cache, paste0(cache_file_name, ".rds"))
  
  # Vérifier si le fichier cache existe déjà et est à jour
  if (file.exists(cache_file_path)) {
    if (file.info(cache_file_path)$mtime > file.info(file_path)$mtime) {
      cat(sprintf("Fichier déjà en cache et à jour: %s\n", basename(file_path)))
      return(TRUE)
    }
  }
  
  # Si pas de cache valide, charger et traiter les données
  cat(sprintf("Mise en cache du fichier: %s\n", basename(file_path)))
  tryCatch({
    # Lecture avec transformation EPSG:4326 (WGS84) pour Leaflet
    data <- st_read(file_path, quiet = TRUE)
    
    # Ajouter un index pour la jointure
    data$index_original <- seq_len(nrow(data))
    
    # Vérifier et transformer la projection si nécessaire
    if (!is.na(st_crs(data)$wkt) && st_crs(data)$epsg != 4326) {
      data <- st_transform(data, 4326)
    } else if (is.na(st_crs(data)$wkt)) {
      # Si la projection n'est pas définie, assigner une projection (souvent Lambert-93 pour la France)
      data <- st_set_crs(data, 2154)
      data <- st_transform(data, 4326)
    }
    
    # Sauvegarder dans le cache
    saveRDS(data, cache_file_path)
    cat(sprintf("✅ Fichier mis en cache avec succès: %s\n", basename(cache_file_path)))
    
    return(TRUE)
  }, error = function(e) {
    cat(sprintf("❌ Erreur lors de la mise en cache: %s - %s\n", basename(file_path), e$message))
    return(FALSE)
  })
}

# Fonction pour traiter tous les fichiers GPKG dans un dossier
process_folder <- function(folder_path, pattern = "\\.gpkg$") {
  if (!dir.exists(folder_path)) {
    cat(sprintf("Le dossier n'existe pas: %s\n", folder_path))
    return(0)
  }
  
  files <- list.files(folder_path, pattern = pattern, full.names = TRUE)
  
  if (length(files) == 0) {
    cat(sprintf("Aucun fichier GPKG trouvé dans: %s\n", folder_path))
    return(0)
  }
  
  cat(sprintf("\n--- Traitement de %d fichiers dans %s ---\n", length(files), folder_path))
  
  success_count <- 0
  for (file in files) {
    if (cache_file(file)) {
      success_count <- success_count + 1
    }
  }
  
  return(success_count)
}

# Fonction principale pour précharger toutes les données
precharge_donnees <- function() {
  total_success <- 0
  cat("\n==== PRÉCHARGEMENT DES DONNÉES STANDARD ====\n")
  
  # Traiter les dossiers standard
  total_success <- total_success + process_folder(path_indicateurs_saisonniers)
  total_success <- total_success + process_folder(path_indicateurs_annuels)
  total_success <- total_success + process_folder(path_feux_indicateurs)
  total_success <- total_success + process_folder(path_agri_indicateurs)
  total_success <- total_success + process_folder(path_cumul_precip_ete)
  total_success <- total_success + process_folder(path_cumul_precip_hiver)
  
  cat("\n==== PRÉCHARGEMENT DES DONNÉES NORMALISÉES ====\n")
  
  # Vérifier si les dossiers normalisés existent
  if (dir.exists(path_cumul_precip_ete_norm) && dir.exists(path_cumul_precip_hiver_norm)) {
    cat("Les dossiers de données normalisées existent déjà.\n")
  } else {
    cat("Génération des données normalisées pour les précipitations...\n")
    # Exécuter le script de normalisation
    tryCatch({
      source("normalize_precip_data.R")
      cat("✅ Données normalisées générées avec succès!\n")
    }, error = function(e) {
      cat("❌ Erreur lors de la génération des données normalisées:", e$message, "\n")
    })
  }
  
  # Traiter les dossiers normalisés
  total_success <- total_success + process_folder(path_indicateurs_saisonniers_norm)
  total_success <- total_success + process_folder(path_indicateurs_annuels_norm)
  total_success <- total_success + process_folder(path_feux_indicateurs_norm)
  total_success <- total_success + process_folder(path_agri_indicateurs_norm)
  total_success <- total_success + process_folder(path_cumul_precip_ete_norm)
  total_success <- total_success + process_folder(path_cumul_precip_hiver_norm)
  
  cat(sprintf("\n==== RÉSUMÉ ====\n%d fichiers mis en cache avec succès.\n", total_success))
  cat(sprintf("Le cache se trouve dans: %s\n", normalizePath(path_cache)))
}

# Exécuter le préchargement
cat("Démarrage du préchargement des données...\n")
precharge_donnees()
cat("Préchargement terminé!\n") 