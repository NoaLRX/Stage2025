#!/usr/bin/env Rscript

# Script pour corriger les problèmes des cartes générées par GWPCA.R

# Traiter les arguments de la ligne de commande
args <- commandArgs(trailingOnly = TRUE)

# Définir les valeurs par défaut et traiter les arguments
rcp <- "2_6"    # Valeur par défaut
horizon <- "H1" # Valeur par défaut

# Vérifier s'il y a des arguments et les traiter
if (length(args) >= 1) {
  rcp <- args[1]
}
if (length(args) >= 2) {
  horizon <- args[2]
}

# Afficher les paramètres utilisés
cat("Utilisation des paramètres: RCP", rcp, "Horizon", horizon, "\n")

# Charger les bibliothèques nécessaires
library(sf)
library(RColorBrewer)
library(ggplot2)

# Définir les chemins des données et résultats
# Chemin du dossier des résultats
results_dir <- "/Users/noa/Desktop/PRISM/Resultats"
scenario_folder <- file.path(results_dir, paste0("GWPCA_RCP", rcp, "_", horizon))

# Chemin du fichier GeoPackage
gpkg_file <- file.path(scenario_folder, paste0("GWPCA_RCP", rcp, "_", horizon, ".gpkg"))

# Vérifier si le fichier existe
if (!file.exists(gpkg_file)) {
  stop("Le fichier GeoPackage n'existe pas. Exécutez d'abord GWPCA.R.")
}

# Lire les données
cat("Lecture des données...\n")
data <- st_read(gpkg_file)

# Extraire les noms des variables numériques (sans la géométrie et les colonnes spéciales)
var_cols <- names(data)[!names(data) %in% c("geom", "var_explained", "win_item")]
var_cols <- var_cols[!grepl("^loading_", var_cols)]

# 1. Corriger la carte de variance expliquée
var_plot_file <- file.path(scenario_folder, paste0("GWPCA_variance_RCP", rcp, "_", horizon, ".png"))

cat("Création de la carte de variance expliquée...\n")
# Titre principal
var_title <- paste("Pourcentage de variance expliquée\n",
                 "RCP", rcp, "Horizon", horizon)

# Utiliser ggplot2 pour la visualisation
p1 <- ggplot(data) +
  geom_sf(aes(fill = var_explained), color = "grey50", lwd = 0.1) +
  scale_fill_gradientn(
    colours = colorRampPalette(c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"))(100),
    name = "% Variance\nexpliquée"
  ) +
  ggtitle(var_title) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "right"
  )

# Sauvegarder en PNG haute résolution
ggsave(var_plot_file, plot = p1, width = 10, height = 8, dpi = 300)
cat("Carte de variance sauvegardée dans:", var_plot_file, "\n")

# 2. Corriger la carte des variables dominantes en utilisant la méthode standard de plot
win_plot_file <- file.path(scenario_folder, paste0("GWPCA_winning_vars_RCP", rcp, "_", horizon, ".png"))

cat("Création de la carte des variables dominantes...\n")

# Titre principal
win_title <- paste("Variable ayant le plus fort poids (loading) sur la PC1\n", 
                 "RCP", rcp, "Horizon", horizon)

# Créer un facteur pour les noms de variables
data$win_item_name <- factor(
  data$win_item,
  levels = 1:length(var_cols),
  labels = var_cols
)

# Créer une palette de couleurs qualitative
n_vars <- length(var_cols)
if (n_vars <= 9) {
  my_colors <- brewer.pal(max(3, n_vars), "Set1")
} else {
  my_colors <- colorRampPalette(brewer.pal(9, "Set1"))(n_vars)
}

# Utiliser ggplot2 pour la visualisation
p2 <- ggplot(data) +
  geom_sf(aes(fill = win_item_name), color = "grey50", lwd = 0.1) +
  scale_fill_manual(
    values = my_colors,
    name = "Variable dominante"
  ) +
  ggtitle(win_title) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "right",
    legend.text = element_text(size = 9)
  )

# Sauvegarder en PNG haute résolution
ggsave(win_plot_file, plot = p2, width = 10, height = 8, dpi = 300)
cat("Carte des variables dominantes sauvegardée dans:", win_plot_file, "\n")

# Méthode alternative - utilisation d'une carte plus simple avec la fonction plot
if (!is.null(dev.list())) dev.off()
win_plot_file2 <- file.path(scenario_folder, paste0("GWPCA_winning_vars_simple_RCP", rcp, "_", horizon, ".png"))
png(win_plot_file2, width = 3000, height = 2400, res = 300)
par(mar = c(5, 4, 4, 10)) # Augmenter la marge droite pour la légende
plot(data["win_item"], main = win_title, col = my_colors[1:n_vars], 
     key.pos = NULL, border = "grey50", lwd = 0.1) # Pas de légende par défaut
legend("right", legend = var_cols, fill = my_colors[1:n_vars], 
       title = "Variable dominante", xpd = TRUE, inset = c(-0.2, 0), cex = 0.8)
dev.off()
cat("Carte simple des variables dominantes sauvegardée dans:", win_plot_file2, "\n")

cat("Correction des cartes terminée!\n") 