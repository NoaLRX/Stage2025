#!/usr/bin/env Rscript

# Script simplifié pour créer des cartes GWPCA directement, sans dépendre du script GWPCA.R
cat("GWPCA et génération simplifiée de cartes\n")

# Charger les bibliothèques de base
library(sf)
library(dplyr)
if (!requireNamespace("GWmodel", quietly = TRUE)) {
  install.packages("GWmodel")
}
library(GWmodel)
library(RColorBrewer)

# Paramètres pour le RCP et l'horizon
args <- commandArgs(trailingOnly = TRUE)
rcp <- "2_6"    # Valeur par défaut
horizon <- "H1" # Valeur par défaut
if (length(args) >= 1) rcp <- args[1]
if (length(args) >= 2) horizon <- args[2]

cat("Utilisation des paramètres: RCP", rcp, "Horizon", horizon, "\n")

# Chemins
data_path <- "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/resultats_agregation.gpkg"
output_dir <- "/Users/noa/Desktop/PRISM/Resultats/Cartes_GWPCA"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Lire les données
cat("Lecture des données...\n")
data <- st_read(data_path)

# Filtrer les variables selon le RCP et l'horizon
all_vars <- names(data)
all_vars <- all_vars[all_vars != "geom"]  # Exclure la géométrie

# Sélectionner les variables RGA avec le RCP et l'horizon choisis
if (rcp == "REF") {
  pattern <- "_REF$"
} else {
  pattern <- paste0("RCP", rcp, ".*", horizon, "$")
}

scenario_vars <- grep(pattern, all_vars, value = TRUE)
rga_vars <- grep("^RGA", all_vars, value = TRUE)
non_scenario_vars <- rga_vars[!rga_vars %in% scenario_vars]

cat("Variables RGA sans scénario trouvées:", length(non_scenario_vars), "\n")
cat("Variables avec RCP", rcp, ifelse(rcp != "REF", paste("et horizon", horizon), ""), "trouvées:", length(scenario_vars), "\n")

# Sélectionner toutes les variables pour l'analyse
analysis_vars <- c(non_scenario_vars, scenario_vars)
cat("Total de variables pour l'analyse:", length(analysis_vars), "\n")

# Filtrer les données
filtered_data <- data %>% select(all_of(c(analysis_vars, "geom")))

# Extraire les données numériques pour l'ACP
numeric_data <- st_set_geometry(filtered_data, NULL)
numeric_cols <- sapply(numeric_data, is.numeric)
numeric_data <- numeric_data[, numeric_cols, drop = FALSE]

cat("Variables numériques pour l'analyse:", ncol(numeric_data), "\n")

# Vérifier qu'il y a suffisamment de variables
if (ncol(numeric_data) < 2) {
  stop("Pas assez de variables numériques. Minimum 2 requis.")
}

# Coordonnées géographiques
coords <- st_coordinates(st_centroid(filtered_data$geom))

# Standardiser les données
data_scaled <- scale(as.matrix(numeric_data))

# Vérifier les NA
if(any(is.na(data_scaled))) {
  cat("Suppression des lignes avec des NA...\n")
  complete_cases <- complete.cases(data_scaled)
  data_scaled <- data_scaled[complete_cases,]
  coords <- coords[complete_cases,]
  filtered_data <- filtered_data[complete_cases,]
}

# Créer un SpatialPointsDataFrame pour GWmodel
if (!requireNamespace("sp", quietly = TRUE)) {
  install.packages("sp")
}
library(sp)

spdf <- SpatialPointsDataFrame(
  coords = coords, 
  data = as.data.frame(data_scaled),
  proj4string = CRS(st_crs(filtered_data)$proj4string)
)

# Calculer la bande passante pour GWPCA
cat("Calcul de la bande passante pour GWPCA...\n")
bw <- nrow(spdf) * 0.1  # 10% des points comme bande passante adaptative

# Exécuter GWPCA avec une approche robuste pour éviter les erreurs
cat("Exécution de GWPCA...\n")
tryCatch({
  gwpca_result <- gwpca(
    spdf, 
    vars = colnames(spdf@data),
    bw = bw,
    k = min(ncol(spdf@data), 3),  # Maximum 3 composantes
    adaptive = TRUE,
    robust = FALSE
  )
  
  # Calculer la variance expliquée
  var_explained <- rowSums(gwpca_result$var[, 1:min(3, ncol(spdf@data))]) / rowSums(gwpca_result$var) * 100
  filtered_data$var_explained <- var_explained
  
  # Extraire les chargements de la PC1 et trouver la variable dominante
  loadings_pc1 <- gwpca_result$loadings[, , 1]
  win_items <- max.col(abs(loadings_pc1))
  filtered_data$win_item <- win_items
  
  # Créer une carte simple de la variance expliquée
  var_file <- file.path(output_dir, paste0("Variance_RCP", rcp, "_", horizon, ".png"))
  cat("Création de la carte de variance expliquée...\n")
  png(var_file, width = 2400, height = 1800, res = 300)
  par(mar = c(1, 1, 2, 8))  # Ajuster les marges
  var_breaks <- seq(min(var_explained, na.rm = TRUE), 
                    max(var_explained, na.rm = TRUE), 
                    length.out = 8)
  var_colors <- colorRampPalette(c("lightblue", "blue", "darkblue"))(7)
  plot(filtered_data["var_explained"], 
       main = paste("Variance expliquée - RCP", rcp, ifelse(rcp != "REF", paste("Horizon", horizon), "")),
       breaks = var_breaks, 
       pal = var_colors, 
       border = "grey",
       lwd = 0.1,
       key.pos = 4)
  dev.off()
  cat("Carte de variance sauvegardée:", var_file, "\n")
  
  # Créer une carte simple des variables dominantes
  win_file <- file.path(output_dir, paste0("Variables_dominantes_RCP", rcp, "_", horizon, ".png"))
  cat("Création de la carte des variables dominantes...\n")
  png(win_file, width = 2400, height = 1800, res = 300)
  par(mar = c(1, 1, 2, 12))  # Augmenter la marge droite pour la légende
  n_vars <- ncol(numeric_data)
  win_colors <- brewer.pal(min(9, max(3, n_vars)), "Set1")
  if (n_vars > 9) {
    win_colors <- colorRampPalette(win_colors)(n_vars)
  }
  
  # Titre
  win_title <- paste("Variable dominante sur PC1 - RCP", rcp, 
                   ifelse(rcp != "REF", paste("Horizon", horizon), ""))
  
  # Faire le plot sans légende d'abord
  plot(filtered_data["win_item"], 
       main = win_title,
       col = win_colors,
       border = "grey",
       lwd = 0.1,
       key.pos = NULL)  # Désactiver la légende par défaut
  
  # Ajouter une légende manuelle
  var_names <- colnames(numeric_data)
  legend("right", 
         legend = var_names, 
         fill = win_colors[1:length(var_names)],
         title = "Variable dominante",
         cex = 0.8,
         bty = "n",
         xpd = TRUE, 
         inset = c(-0.25, 0))
  
  dev.off()
  cat("Carte des variables dominantes sauvegardée:", win_file, "\n")
  
}, error = function(e) {
  cat("ERREUR dans l'analyse GWPCA:", e$message, "\n")
  
  # Créer une carte basique même en cas d'erreur
  dummy_file <- file.path(output_dir, paste0("Carte_base_RCP", rcp, "_", horizon, ".png"))
  png(dummy_file, width = 1800, height = 1200, res = 300)
  plot(filtered_data$geom, main = paste("Données RCP", rcp, ifelse(rcp != "REF", paste("Horizon", horizon), "")))
  dev.off()
  cat("Carte de base sauvegardée:", dummy_file, "\n")
})

cat("Traitement terminé!\n") 