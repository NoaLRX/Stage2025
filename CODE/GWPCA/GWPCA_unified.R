#!/usr/bin/env Rscript

# ===========================================================================
# SCRIPT GWPCA POUR INDICATEURS PRISM
# ===========================================================================
# Ce script exécute une Analyse en Composantes Principales Géographiquement Pondérée
# (GWPCA) sur des données géospatiales avec filtrage par type d'indicateur et scénario.
# 
# CONFIGURATION: Modifier les paramètres ci-dessous selon l'analyse souhaitée
# UTILISATION: Rscript GWPCA_unified.R
#             ou
#             VAR_TYPE="SécheresseBrute" RCP="4_5" HORIZON="H1" Rscript GWPCA_unified.R

# Chargement des bibliothèques
suppressMessages({
  library(sf)
  library(dplyr)
  library(GWmodel)
  library(RColorBrewer)
})

# Installation de GWmodel si nécessaire
if (!requireNamespace("GWmodel", quietly = TRUE)) {
  install.packages("GWmodel")
}

# ===========================================================================
# PARAMÈTRES À MODIFIER
# ===========================================================================

# Permettre l'utilisation de variables d'environnement pour les paramètres
# 1. Choix de la variable d'intérêt
VAR_TYPE <- if (Sys.getenv("VAR_TYPE") != "") Sys.getenv("VAR_TYPE") else "SécheresseBrute"
cat(paste("Type de variable sélectionné:", VAR_TYPE, "\n"))

# 2. Choix du scénario climatique
RCP <- if (Sys.getenv("RCP") != "") Sys.getenv("RCP") else "8_5"
cat(paste("Scénario climatique RCP sélectionné:", RCP, "\n"))

# 3. Choix de l'horizon temporel
HORIZON <- if (Sys.getenv("HORIZON") != "") Sys.getenv("HORIZON") else "H1"
cat(paste("Horizon temporel sélectionné:", HORIZON, "\n"))

# 4. Paramètres avancés
DEBUG <- FALSE  # Afficher des informations de débogage
ADAPTIVE_BW_PERCENT <- 0.1  # Pourcentage d'observations pour la bande passante (0.1 = 10%)
MAX_COMPONENTS <- 3  # Nombre maximum de composantes à extraire

# 5. Options de bande passante
USE_FIXED_BANDWIDTH <- TRUE  # TRUE pour utiliser un nombre fixe de voisins, FALSE pour calculer la bande passante optimale
FIXED_NEIGHBORS <- 100  # Nombre de plus proches voisins à utiliser si USE_FIXED_BANDWIDTH = TRUE
BW_SELECTION_CRITERION <- "CV"  # Critère de sélection de la bande passante : "CV" (Cross-Validation/PRESS), "AIC" ou "BIC"

# ===========================================================================
# PRÉPARATION
# ===========================================================================

# Pour reproductibilité
#set.seed(123)

# Fonction pour détecter les variables à variance nulle
has_zero_variance <- function(x) {
  return(var(x, na.rm = TRUE) == 0)
}

# Créer le pattern de recherche pour le type de variable
var_prefix_pattern <- switch(VAR_TYPE,
  "RGA" = "^RGA(?!_1_SSWI|_1_RGA|_2_CAT)",
  "Inondation" = "^INO",
  "Erosion" = "^EDL", # SEULEMENT UNE VARIABLE DONC IMPOSSIBLE
  "IncendieFeuxForets" = "^FF",
  "Mouvement2Terrain" = "^MVT",
  "Modification_Air" = "^MTA",
  "SécheresseRGA" = "^RGA_",
  "SécheresseBrute" = "^SEC",
  "Vagues2Chaleur" = "^VCH",
  "Vagues2Froid" = "^VFR", # SEULEMENT UNE VARIABLE DONC IMPOSSIBLE
  "Vagues2Gel" = "^VGEL",
  "Modification_Precipitation" = "^MRP",
  "Modification_Vent" = "^MRV",
  "Biodiv" = "^BIO",
  "TGN" = "^TGN", # SEULEMENT UNE VARIABLE DONC IMPOSSIBLE
  "Elevation_Mer" = "^ENM" # SEULEMENT UNE VARIABLE DONC IMPOSSIBLE
)

# Chemins des fichiers
data_path <- "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/resultats_agregation.gpkg"
results_dir <- "/Users/noa/Desktop/PRISM/Resultats/GWPCA_Unified"

# Création des dossiers
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
scenario_folder <- if (RCP == "REF") {
  file.path(results_dir, paste0(VAR_TYPE, "_RCP", RCP))
} else {
  file.path(results_dir, paste0(VAR_TYPE, "_RCP", RCP, "_", HORIZON))
}
if (!dir.exists(scenario_folder)) dir.create(scenario_folder)

# Création du sous-dossier pour les résultats normalisés
normalized_folder <- file.path(scenario_folder, "normalized")
if (!dir.exists(normalized_folder)) {
  dir.create(normalized_folder, recursive = TRUE)
  if (!dir.exists(normalized_folder)) {
    cat("ATTENTION: Impossible de créer le dossier:", normalized_folder, "\n")
    cat("Tentative de création avec permissions explicites...\n")
    dir.create(normalized_folder, recursive = TRUE, mode = "0777")
  }
}
cat("Dossier pour résultats normalisés:", normalized_folder, "\n")

# ===========================================================================
# CHARGEMENT ET FILTRAGE DES DONNÉES
# ===========================================================================

# Lecture des données spatiales
data <- st_read(data_path, quiet = TRUE)

# Filtrer pour exclure les codes postaux 29242 et 29990
data <- data[!(data$code_postal %in% c("29242", "29990")), ]
cat("Codes postaux 29242 et 29990 exclus de l'analyse\n")
cat("Nombre d'observations restantes:", nrow(data), "\n")

# Filtrer les colonnes selon le type sélectionné
ino_vars <- grep(var_prefix_pattern, names(data), value = TRUE)
cat("Variables disponibles:", length(ino_vars), "\n")

# Filtrer les variables selon le scénario et l'horizon choisis
filtered_vars <- character(0)

# Traitement spécial pour SécheresseBrute
if (VAR_TYPE == "SécheresseBrute") {
  # Identifier le pattern exact pour le scénario et l'horizon pour SécheresseBrute
  rcp_format <- ""
  
  if (RCP == "8_5") rcp_format <- "RCP8_5"
  else if (RCP == "4_5") rcp_format <- "RCP4_5"
  else if (RCP == "2_6") rcp_format <- "RCP2_6"
  else if (RCP == "REF") rcp_format <- "REF"
  
  cat("Recherche de variables SécheresseBrute avec pattern:", paste0(rcp_format, "_", HORIZON), "\n")
  
  # Filtrage plus précis pour les variables de SécheresseBrute (SEC_1_SPI et SEC_1_SURFACE)
  for (var in ino_vars) {
    # Pour SécheresseBrute, vérifier si le nom de variable contient le RCP et l'HORIZON exacts
    if (RCP == "REF") {
      if (grepl("_REF$", var)) {
        filtered_vars <- c(filtered_vars, var)
      }
    } else if (grepl(paste0("_", rcp_format, "_", HORIZON, "$"), var)) {
      filtered_vars <- c(filtered_vars, var)
    }
  }
  
  cat("Variables SécheresseBrute trouvées après filtrage précis:", length(filtered_vars), "\n")
  if (length(filtered_vars) > 0) {
    cat("Variables SécheresseBrute sélectionnées:", paste(filtered_vars, collapse = " "), "\n")
  } else {
    cat("AVERTISSEMENT: Aucune variable SécheresseBrute trouvée pour le scénario", rcp_format, "et l'horizon", HORIZON, "\n")
    cat("Variables disponibles:", paste(ino_vars, collapse = " "), "\n")
    stop("Aucune variable correspondant au filtrage. Vérifiez les paramètres RCP et HORIZON.")
  }
} else {
  # Traitement standard pour les autres types de variables
  for (var in ino_vars) {
    # Vérifier si la variable a un scénario ou un horizon
    # Format peut être différent selon le type de variable
    has_scenario <- FALSE
    has_horizon <- FALSE
    
    if (VAR_TYPE == "SécheresseRGA") {
      # Pour SécheresseRGA, le format est RGA_1_SSWI_RCP8_5_H1
      has_scenario <- grepl("RCP[0-9]_[0-9]|_REF", var)
      has_horizon <- grepl("_H[1-3]|REF", var)
    } else {
      # Format standard pour les autres variables
      has_scenario <- grepl("RCP40_[0-9]|_REF", var)
      has_horizon <- grepl("_H[1-3]|REF", var)
    }
    
    if (!has_scenario && !has_horizon) {
      # Garder les variables sans scénario ni horizon
      filtered_vars <- c(filtered_vars, var)
    } else if (has_scenario && has_horizon) {
      # Pour les variables avec scénario et horizon, vérifier s'ils correspondent
      if (RCP == "REF") {
        if (grepl("_REF$", var)) {
          filtered_vars <- c(filtered_vars, var)
        }
      } else {
        # Ajuster selon le format de la base
        rcp_db_format <- ""
        
        if (VAR_TYPE == "SécheresseRGA") {
          # Format spécifique pour SécheresseRGA
          if (RCP == "8_5") rcp_db_format <- "RCP8_5"
          else if (RCP == "4_5") rcp_db_format <- "RCP4_5"
          else if (RCP == "2_6") rcp_db_format <- "RCP2_6"
          else rcp_db_format <- paste0("RCP", RCP)
        } else {
          # Format standard pour les autres variables
          if (RCP == "8_5") rcp_db_format <- "RCP40_8"
          else if (RCP == "4_5") rcp_db_format <- "RCP40_4"
          else if (RCP == "2_6") rcp_db_format <- "RCP40_2"
          else rcp_db_format <- paste0("RCP", RCP)
        }
        
        if (grepl(rcp_db_format, var) && grepl(paste0("_", HORIZON), var)) {
          filtered_vars <- c(filtered_vars, var)
        }
      }
    } else if (has_scenario && !has_horizon) {
      # Pour les variables avec seulement un scénario, vérifier s'il correspond
      if (RCP == "REF") {
        if (grepl("_REF$", var)) {
          filtered_vars <- c(filtered_vars, var)
        }
      } else {
        # Ajuster selon le format de la base
        rcp_db_format <- ""
        
        if (VAR_TYPE == "SécheresseRGA") {
          # Format spécifique pour SécheresseRGA
          if (RCP == "8_5") rcp_db_format <- "RCP8_5"
          else if (RCP == "4_5") rcp_db_format <- "RCP4_5"
          else if (RCP == "2_6") rcp_db_format <- "RCP2_6"
          else rcp_db_format <- paste0("RCP", RCP)
        } else {
          # Format standard pour les autres variables
          if (RCP == "8_5") rcp_db_format <- "RCP40_8"
          else if (RCP == "4_5") rcp_db_format <- "RCP40_4"
          else if (RCP == "2_6") rcp_db_format <- "RCP40_2"
          else rcp_db_format <- paste0("RCP", RCP)
        }
        
        if (grepl(rcp_db_format, var)) {
          filtered_vars <- c(filtered_vars, var)
        }
      }
    } else if (!has_scenario && has_horizon) {
      # Pour les variables avec seulement un horizon, vérifier s'il correspond
      if (RCP == "REF") {
        if (grepl("REF", var)) {
          filtered_vars <- c(filtered_vars, var)
        }
      } else {
        if (grepl(paste0("_", HORIZON), var)) {
          filtered_vars <- c(filtered_vars, var)
        }
      }
    }
  }
}

cat("Variables sélectionnées après filtrage:", length(filtered_vars), "\n")

# Vérifier qu'il reste suffisamment de variables pour l'analyse
if (length(filtered_vars) < 2) {
  stop("Pas assez de variables après filtrage pour effectuer une ACP (minimum 2 requises)")
}

# Créer un sous-ensemble avec uniquement les variables filtrées et la géométrie
filtered_data <- data[, c(filtered_vars, "geom")]

# Vérifier la présence de valeurs manquantes ou infinies
numeric_data <- st_set_geometry(filtered_data, NULL)
na_counts <- sapply(numeric_data, function(x) sum(is.na(x)))
cat("Nombre de NA par variable:\n")
print(na_counts)

inf_counts <- sapply(numeric_data, function(x) sum(!is.finite(x) & !is.na(x)))
cat("Nombre de valeurs infinies par variable:\n")
print(inf_counts)

# Traiter les valeurs problématiques si nécessaire
if(sum(na_counts) > 0 || sum(inf_counts) > 0) {
  cat("Des valeurs problématiques ont été détectées. Nettoyage des données...\n")
  
  # Remplacer les valeurs infinies par NA
  for(col in names(numeric_data)) {
    numeric_data[[col]][!is.finite(numeric_data[[col]]) & !is.na(numeric_data[[col]])] <- NA
  }
  
  # Imputer les valeurs manquantes avec la moyenne
  for(col in names(numeric_data)) {
    if(sum(is.na(numeric_data[[col]])) > 0) {
      col_mean <- mean(numeric_data[[col]], na.rm = TRUE)
      numeric_data[[col]][is.na(numeric_data[[col]])] <- col_mean
      cat("Variable", col, ": valeurs NA remplacées par la moyenne (", col_mean, ")\n")
    }
  }
}

# Gestion des variables à variance nulle (constantes)
zero_var_cols <- sapply(numeric_data, has_zero_variance)
if (any(zero_var_cols)) {
  cat("Variables à variance nulle détectées et supprimées:", sum(zero_var_cols), "\n")
  numeric_data <- numeric_data[, !zero_var_cols, drop = FALSE]
}

# Vérification du nombre minimal de variables
if (ncol(numeric_data) < 2) {
  stop("Pas assez de variables pour l'analyse GWPCA (minimum 2 requises)")
}

# Standardisation des données
data_scaled <- scale(as.matrix(numeric_data))
data_scaled_df <- as.data.frame(data_scaled)

# Vérifier à nouveau après standardisation
if(any(!is.finite(data_scaled))) {
  cat("Des valeurs non finies sont toujours présentes après standardisation.\n")
  data_scaled[!is.finite(data_scaled)] <- 0
  cat("Les valeurs non finies ont été remplacées par zéro.\n")
  data_scaled_df <- as.data.frame(data_scaled)
}

# Extraction des coordonnées géographiques
coords <- st_coordinates(st_centroid(filtered_data$geom))

# ===========================================================================
# EXÉCUTION DES ANALYSES PCA ET GWPCA
# ===========================================================================

# Analyse en Composantes Principales standard (pour comparaison)
pca_std <- prcomp(data_scaled_df, scale. = FALSE)

# Préparation des données spatiales pour GWmodel
sp_data <- SpatialPointsDataFrame(
  coords, 
  data = data_scaled_df, 
  proj4string = CRS(st_crs(filtered_data)$proj4string)
)

# Détermination de la bande passante selon l'option choisie
if (USE_FIXED_BANDWIDTH) {
  # Utiliser un nombre fixe de voisins
  bw_gw_pca <- FIXED_NEIGHBORS
  cat(paste("Utilisation d'une bande passante fixe avec", FIXED_NEIGHBORS, "plus proches voisins\n"))
} else {
  # Calculer la bande passante optimale selon le critère choisi
  cat(paste("Calcul de la bande passante optimale avec le critère", BW_SELECTION_CRITERION, "...\n"))
  
  if (BW_SELECTION_CRITERION == "CV") {
    # Méthode Cross-Validation standard (PRESS - Prediction Residual Error Sum of Squares)
    bw_gw_pca <- bw.gwpca(sp_data, 
                         vars = colnames(sp_data@data),
                         k = min(ncol(sp_data@data), MAX_COMPONENTS),
                         robust = FALSE, 
                         adaptive = TRUE)
    cat(paste("Bande passante optimale (CV) calculée:", bw_gw_pca, "\n"))
  } else if (BW_SELECTION_CRITERION == "AIC") {
    # Utilisation de l'AIC (Akaike Information Criterion)
    # Pour GWmodel, nous devons créer notre propre fonction d'optimisation basée sur AIC
    bw_min <- 5  # Bande passante minimale (au moins 5 voisins)
    bw_max <- nrow(sp_data) * 0.5  # Maximum à 50% des observations
    
    # Fonction d'optimisation AIC pour GWPCA
    # Note: Dans GWmodel, il n'y a pas de fonction directe pour AIC, donc nous créons une approximation
    aic_values <- numeric(10)  # Test de 10 valeurs de bande passante
    bw_values <- seq(bw_min, bw_max, length.out = 10)
    
    for (i in 1:length(bw_values)) {
      bw <- bw_values[i]
      tryCatch({
        temp_gwpca <- gwpca(sp_data, 
                           vars = colnames(sp_data@data),
                           bw = bw,
                           k = min(ncol(sp_data@data), MAX_COMPONENTS),
                           adaptive = TRUE,
                           robust = FALSE)
        
        # Calculer un AIC approximatif
        # Pour GWPCA: AIC = n*log(RSS/n) + 2*k où k est le nb de paramètres et RSS la somme des carrés résiduels
        n <- nrow(sp_data)
        p <- ncol(sp_data@data)
        k <- p * min(ncol(sp_data@data), MAX_COMPONENTS)  # nombre de paramètres
        
        # Obtenir la variance non expliquée comme approximation de la RSS
        explained_var <- sum(temp_gwpca$var[, 1:min(ncol(sp_data@data), MAX_COMPONENTS)])
        total_var <- sum(temp_gwpca$var)
        unexplained_var <- total_var - explained_var
        
        # Calcul de l'AIC
        aic_values[i] <- n * log(unexplained_var/n) + 2*k + n*log(2*pi) + n
      }, error = function(e) {
        aic_values[i] <- Inf  # En cas d'erreur, assigner une valeur infinie
      })
    }
    
    # Trouver la bande passante avec l'AIC minimum
    best_index <- which.min(aic_values)
    bw_gw_pca <- bw_values[best_index]
    cat(paste("Bande passante optimale (AIC) calculée:", bw_gw_pca, "\n"))
    
  } else if (BW_SELECTION_CRITERION == "BIC") {
    # Utilisation du BIC (Bayesian Information Criterion)
    # Pour GWmodel, nous devons créer notre propre fonction d'optimisation basée sur BIC
    bw_min <- 5  # Bande passante minimale (au moins 5 voisins)
    bw_max <- nrow(sp_data) * 0.5  # Maximum à 50% des observations
    
    # Fonction d'optimisation BIC pour GWPCA
    bic_values <- numeric(10)  # Test de 10 valeurs de bande passante
    bw_values <- seq(bw_min, bw_max, length.out = 10)
    
    for (i in 1:length(bw_values)) {
      bw <- bw_values[i]
      tryCatch({
        temp_gwpca <- gwpca(sp_data, 
                           vars = colnames(sp_data@data),
                           bw = bw,
                           k = min(ncol(sp_data@data), MAX_COMPONENTS),
                           adaptive = TRUE,
                           robust = FALSE)
        
        # Calculer un BIC approximatif
        # Pour GWPCA: BIC = n*log(RSS/n) + log(n)*k où k est le nb de paramètres
        n <- nrow(sp_data)
        p <- ncol(sp_data@data)
        k <- p * min(ncol(sp_data@data), MAX_COMPONENTS)  # nombre de paramètres
        
        # Obtenir la variance non expliquée comme approximation de la RSS
        explained_var <- sum(temp_gwpca$var[, 1:min(ncol(sp_data@data), MAX_COMPONENTS)])
        total_var <- sum(temp_gwpca$var)
        unexplained_var <- total_var - explained_var
        
        # Calcul du BIC
        bic_values[i] <- n * log(unexplained_var/n) + log(n)*k + n*log(2*pi) + n
      }, error = function(e) {
        bic_values[i] <- Inf  # En cas d'erreur, assigner une valeur infinie
      })
    }
    
    # Trouver la bande passante avec le BIC minimum
    best_index <- which.min(bic_values)
    bw_gw_pca <- bw_values[best_index]
    cat(paste("Bande passante optimale (BIC) calculée:", bw_gw_pca, "\n"))
  } else {
    # Méthode par défaut si critère non reconnu
    cat("Critère de sélection non reconnu, utilisation de la Cross-Validation par défaut\n")
    bw_gw_pca <- bw.gwpca(sp_data, 
                         vars = colnames(sp_data@data),
                         k = min(ncol(sp_data@data), MAX_COMPONENTS),
                         robust = FALSE, 
                         adaptive = TRUE)
    cat(paste("Bande passante optimale (CV) calculée:", bw_gw_pca, "\n"))
  }
}

# Exécution de la GWPCA avec gestion d'erreur
gwpca_result <- tryCatch({
  gwpca(
    sp_data, 
    vars = names(data_scaled_df),
    bw = bw_gw_pca,
    k = min(ncol(sp_data@data), MAX_COMPONENTS),
    adaptive = TRUE,  # TRUE car nous utilisons un nombre de voisins adaptatif
    robust = FALSE
  )
}, error = function(e) {
  # Réessayer avec moins de composantes si problème
  cat("Erreur dans GWPCA avec", min(ncol(sp_data@data), MAX_COMPONENTS), "composantes. Tentative avec 2 composantes...\n")
  tryCatch({
    gwpca(
      sp_data, 
      vars = names(data_scaled_df),
      bw = bw_gw_pca,
      k = 2,
      adaptive = TRUE,
      robust = FALSE
    )
  }, error = function(e2) {
    cat("Erreur lors de la deuxième tentative:", e2$message, "\n")
    return(NULL)
  })
})

# Vérifier si l'analyse a échoué
if (is.null(gwpca_result)) {
  stop("L'analyse GWPCA a échoué")
}

# Obtenir le nombre de composantes pour les étapes ultérieures
k_components <- min(ncol(data_scaled_df), MAX_COMPONENTS)
if (!is.null(gwpca_result$k)) {
  k_components <- gwpca_result$k
  cat("Nombre de composantes dans le résultat GWPCA:", k_components, "\n")
}

# ===========================================================================
# TRAITEMENT DES RÉSULTATS
# ===========================================================================

# Fonctions pour extraire les résultats selon la version de GWmodel
extract_local_variance <- function(gwpca_obj) {
  if (is.list(gwpca_obj$var) && !is.null(gwpca_obj$var$local)) {
    return(gwpca_obj$var$local)
  } else if (!is.null(dim(gwpca_obj$var)) && length(dim(gwpca_obj$var)) >= 2) {
    return(gwpca_obj$var)
  } else {
    return(matrix(0, nrow=nrow(filtered_data), ncol=k_components))
  }
}

extract_local_loadings <- function(gwpca_obj, comp_num=1) {
  if (is.list(gwpca_obj$loadings) && !is.null(gwpca_obj$loadings$local)) {
    if (length(dim(gwpca_obj$loadings$local)) == 3) {
      return(gwpca_obj$loadings$local[, , comp_num])
    } else {
      return(matrix(0, nrow=nrow(filtered_data), ncol=ncol(data_scaled_df)))
    }
  } else if (!is.null(dim(gwpca_obj$loadings)) && length(dim(gwpca_obj$loadings)) == 3) {
    return(gwpca_obj$loadings[, , comp_num])
  } else {
    return(matrix(0, nrow=nrow(filtered_data), ncol=ncol(data_scaled_df)))
  }
}

# Extraction de la variance locale
local_var <- extract_local_variance(gwpca_result)

# Calcul de la proportion de variance expliquée
prop_var <- function(local_var_matrix, n_components) {
  return((rowSums(local_var_matrix[, 1:n_components, drop=FALSE]) / 
           rowSums(local_var_matrix)) * 100)
}

# Calcul et ajout de la variance expliquée aux données
for (i in 1:min(k_components, ncol(local_var))) {
  var_col_name <- paste0("var_explained_pc", i)
  filtered_data[[var_col_name]] <- prop_var(local_var, i)
}

# Récupération des loadings pour PC1
loadings_pc1 <- extract_local_loadings(gwpca_result, 1)

# Calcul des variables dominantes
win_items <- max.col(abs(loadings_pc1))
filtered_data$win_item_pc1 <- win_items
filtered_data$win_item_name_pc1 <- names(data_scaled_df)[win_items]

# ===========================================================================
# SORTIE DES RÉSULTATS
# ===========================================================================

# 1. Sauvegarde des résultats en GeoPackage
result_gpkg <- file.path(scenario_folder, paste0(VAR_TYPE, "_GWPCA_RCP", RCP, 
                                               ifelse(RCP != "REF", paste0("_", HORIZON), ""), 
                                               ".gpkg"))
st_write(filtered_data, result_gpkg, delete_dsn = TRUE)

# Fonction pour créer les cartes de variance expliquée
create_var_map <- function(pc_num, cumulative = FALSE) {
  # Déterminer le titre et le nom de fichier
  if (cumulative) {
    title_prefix <- paste0("Variance expliquée PC1 à PC", pc_num)
    file_prefix <- paste0("Variance_PC1toPC", pc_num)
    var_col_name <- paste0("var_explained_pc", pc_num)  # Déjà cumulatif
  } else {
    title_prefix <- paste0("Variance expliquée PC", pc_num)
    file_prefix <- paste0("Variance_PC", pc_num)
    
    # Pour PC1, nous utilisons directement var_explained_pc1
    # Pour PC > 1, nous calculons la différence entre la variance cumulée
    if (pc_num == 1) {
      var_col_name <- "var_explained_pc1"
    } else {
      # Calculer la variance individuelle pour cette composante
      pc_prev <- pc_num - 1
      filtered_data[[paste0("var_explained_pc", pc_num, "_indiv")]] <- 
        filtered_data[[paste0("var_explained_pc", pc_num)]] - 
        filtered_data[[paste0("var_explained_pc", pc_prev)]]
      var_col_name <- paste0("var_explained_pc", pc_num, "_indiv")
    }
  }
  
  # Nom de fichier complet
  var_file <- file.path(scenario_folder, paste0(VAR_TYPE, "_", file_prefix, "_RCP", RCP, 
                                              ifelse(RCP != "REF", paste0("_", HORIZON), ""), 
                                              ".png"))
  # Créer la carte
  png(var_file, width = 2400, height = 1800, res = 300)
  par(mar = c(1, 1, 2, 8))
  
  # Paramètres de la carte
  var_explained <- filtered_data[[var_col_name]]
  var_breaks <- seq(min(var_explained, na.rm = TRUE), 
                   max(var_explained, na.rm = TRUE), 
                   length.out = 10)
  
  # Palette de couleurs
  var_pal <- colorRampPalette(c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", 
                              "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f"))(length(var_breaks)-1)
  
  # Tracé de la carte
  plot(st_geometry(filtered_data), 
       main = paste(VAR_TYPE, "-", title_prefix, "- RCP", RCP, 
                  ifelse(RCP != "REF", paste("Horizon", HORIZON), "")),
       border = "grey",
       lwd = 0.1)
  
  # Coloration selon la variance expliquée
  for (j in 1:(length(var_breaks)-1)) {
    mask <- var_explained >= var_breaks[j] & var_explained <= var_breaks[j+1]
    if (any(mask)) {
      plot(st_geometry(filtered_data)[mask], 
           col = var_pal[j], 
           add = TRUE, 
           border = "grey", 
           lwd = 0.1)
    }
  }
  
  # Légende
  legend("right", 
         legend = round(var_breaks[-length(var_breaks)], 1), 
         fill = var_pal,
         title = "Variance expliquée (%)",
         cex = 0.8,
         bty = "n",
         xpd = TRUE)
  
  dev.off()
  cat("Carte créée:", var_file, "\n")
}

# Fonction pour créer la carte de moyenne géométrique
create_geometric_mean_map <- function() {
  # Pour la moyenne géométrique, nous devons avoir des valeurs positives
  # Conversion des données standardisées en valeurs positives (décalage + 1)
  positive_data <- data_scaled + abs(min(data_scaled, na.rm = TRUE)) + 1
  
  # Calcul de la moyenne géométrique pour chaque observation
  # Moyenne géométrique = racine n-ième du produit des n valeurs
  geometric_means <- apply(positive_data, 1, function(x) {
    exp(mean(log(x), na.rm = TRUE))
  })
  
  # Re-normalisation des moyennes entre 0 et 1
  min_geo_mean <- min(geometric_means, na.rm = TRUE)
  max_geo_mean <- max(geometric_means, na.rm = TRUE)
  normalized_geo_means <- (geometric_means - min_geo_mean) / (max_geo_mean - min_geo_mean)
  
  # Ajouter la colonne au jeu de données
  filtered_data$geometric_mean <- normalized_geo_means
  
  # Nom du fichier
  geo_mean_file <- file.path(scenario_folder, paste0(VAR_TYPE, "_Moyenne_Geometrique_RCP", RCP, 
                                                   ifelse(RCP != "REF", paste0("_", HORIZON), ""), 
                                                   ".png"))
  # Créer la carte
  png(geo_mean_file, width = 2400, height = 1800, res = 300)
  par(mar = c(1, 1, 2, 8))
  
  # Paramètres de la carte
  geo_mean_values <- filtered_data$geometric_mean
  geo_mean_breaks <- seq(min(geo_mean_values, na.rm = TRUE), 
                        max(geo_mean_values, na.rm = TRUE), 
                        length.out = 10)
  
  # Palette de couleurs
  geo_mean_pal <- colorRampPalette(c("#2166ac", "#4393c3", "#92c5de", "#d1e5f0", 
                                   "#fddbc7", "#f4a582", "#d6604d", "#b2182b"))(length(geo_mean_breaks)-1)
  
  # Tracé de la carte
  plot(st_geometry(filtered_data), 
       main = paste(VAR_TYPE, "- Moyenne géométrique des variables - RCP", RCP, 
                  ifelse(RCP != "REF", paste("Horizon", HORIZON), "")),
       border = "grey",
       lwd = 0.1)
  
  # Coloration selon la moyenne géométrique
  for (j in 1:(length(geo_mean_breaks)-1)) {
    mask <- geo_mean_values >= geo_mean_breaks[j] & geo_mean_values <= geo_mean_breaks[j+1]
    if (any(mask)) {
      plot(st_geometry(filtered_data)[mask], 
           col = geo_mean_pal[j], 
           add = TRUE, 
           border = "grey", 
           lwd = 0.1)
    }
  }
  
  # Légende
  legend("right", 
         legend = round(geo_mean_breaks[-length(geo_mean_breaks)], 2), 
         fill = geo_mean_pal,
         title = "Moyenne géométrique\n(normalisée)",
         cex = 0.8,
         bty = "n",
         xpd = TRUE)
  
  dev.off()
  cat("Carte de moyenne géométrique créée:", geo_mean_file, "\n")
}

# 2. Créer la carte de variance expliquée Axe 1
create_var_map(1, cumulative = FALSE)

# 3. Créer la carte de variance expliquée Axe 2
if (k_components >= 2) {
  create_var_map(2, cumulative = FALSE)
}

# 4. Créer la carte de variance expliquée Axe 1 à 2
if (k_components >= 2) {
  create_var_map(2, cumulative = TRUE)
}

# 5. Créer la carte de la variable gagnante sur l'Axe 1
# Créer un fichier pour la carte des variables dominantes sur PC1
win_map_file <- file.path(scenario_folder, paste0(VAR_TYPE, "_WinItem_PC1_RCP", RCP, 
                                                ifelse(RCP != "REF", paste0("_", HORIZON), ""), 
                                                ".png"))

# Créer la carte des variables dominantes
png(win_map_file, width = 2400, height = 1800, res = 300)
par(mar = c(1, 1, 2, 8))

# Obtenir les noms uniques des variables dominantes et créer une palette de couleurs
unique_win_items <- unique(filtered_data$win_item_name_pc1)
n_colors <- length(unique_win_items)
win_pal <- colorRampPalette(brewer.pal(min(9, n_colors), "Set1"))(n_colors)

# Tracé de la carte
plot(st_geometry(filtered_data), 
     main = paste(VAR_TYPE, "- Variables dominantes sur PC1 - RCP", RCP, 
                ifelse(RCP != "REF", paste("Horizon", HORIZON), "")),
     border = "grey",
     lwd = 0.1)

# Coloration selon la variable dominante
for (i in 1:length(unique_win_items)) {
  win_name <- unique_win_items[i]
  mask <- filtered_data$win_item_name_pc1 == win_name
  if (any(mask)) {
    plot(st_geometry(filtered_data)[mask], 
         col = win_pal[i], 
         add = TRUE, 
         border = "grey", 
         lwd = 0.1)
  }
}

# Légende
legend("right", 
       legend = unique_win_items, 
       fill = win_pal,
       title = "Variable dominante PC1",
       cex = 0.7,
       bty = "n",
       xpd = TRUE)

dev.off()
cat("Carte des variables dominantes sur PC1 créée:", win_map_file, "\n")

# 6. Créer carte de moyenne géométrique
create_geometric_mean_map()

# Fin du script
cat("Traitement terminé. Cartes générées dans:", scenario_folder, "\n")