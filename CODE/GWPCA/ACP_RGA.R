#!/usr/bin/env Rscript

# ========================================================================
# Script d'analyse en composantes principales (ACP)
# Basé sur la méthode du fichier 'Fichier R - Dossier ACP copie.R'
# Avec gestion simplifiée des données via jointure directe par code commune
# ========================================================================

# ---- Chargement des packages nécessaires ----
library(sf)
library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(ggpubr)
library(ggcorrplot)
library(gridExtra)
library(cowplot)
library(corrplot)
library(outliers)
library(EnvStats)
library(tseries)
library(stats)
library(lmtest)
library(lattice)
library(ggrepel)
library(RColorBrewer)

# ---- Définition des paramètres utilisateur ----
# Options possibles :
# - Horizon : "REF", "H1", "H2", "H3"
# - Scénario : "REF", "2_6", "4_5", "8_5"

# Définition des choix (à modifier par l'utilisateur)
HORIZON_CHOISI <- "H3"    # Choisir parmi "REF", "H1", "H2", "H3"
SCENARIO_CHOISI <- "8_5"  # Choisir parmi "REF", "2_6", "4_5", "8_5"

message(paste0("Analyse pour horizon ", HORIZON_CHOISI, " et scénario ", SCENARIO_CHOISI))

# ---- Création des dossiers de sortie ----
BASE_DIR <- "/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/SécheresseRGA"
output_dir <- file.path(BASE_DIR, paste0("Resultats_ACP_", SCENARIO_CHOISI, "_", HORIZON_CHOISI))

# Création du dossier s'il n'existe pas
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ---- Définition des chemins vers les fichiers de données ----
postal_secheresse_shp <- file.path(BASE_DIR, "RGA_2_CAT/postal_secheresse.shp")
alea_argile_postal_shp <- file.path(BASE_DIR, "AleaRG_Fxx_L93/alea_argile_postal.shp")

# Définition des fichiers NORSWIAV
norswiav_file <- file.path(BASE_DIR, 
                          "NORSWIAV/Resultats", 
                          paste0("NORSWIAV_", SCENARIO_CHOISI, "_clean_FINAL_RESULTS_COMMUNES.gpkg"))

# Définition des fichiers NORRR1MM
norrr1mm_file <- file.path(BASE_DIR, 
                          "NORRR1MM/Resultats", 
                          paste0("NORRR1MM_", SCENARIO_CHOISI, "_clean_FINAL_RESULTS_COMMUNES.gpkg"))

# ---- Lecture et préparation des données ----
message("Lecture des données spatiales...")

# Lecture des fichiers de base
postal_secheresse <- st_read(postal_secheresse_shp, quiet = TRUE)
message("Postal secheresse chargé: ", nrow(postal_secheresse), " entités")

alea_argile_postal <- st_read(alea_argile_postal_shp, quiet = TRUE)
message("Alea argile chargé: ", nrow(alea_argile_postal), " entités")

# Vérification de l'existence des fichiers d'indicateurs climatiques
if (!file.exists(norswiav_file)) {
  stop(paste("Le fichier NORSWIAV pour le scénario", SCENARIO_CHOISI, "n'existe pas:", norswiav_file))
}

if (!file.exists(norrr1mm_file)) {
  stop(paste("Le fichier NORRR1MM pour le scénario", SCENARIO_CHOISI, "n'existe pas:", norrr1mm_file))
}

# Lecture des indicateurs climatiques
norswiav_data <- st_read(norswiav_file, quiet = TRUE)
message("NORSWIAV chargé: ", nrow(norswiav_data), " entités")

norrr1mm_data <- st_read(norrr1mm_file, quiet = TRUE)
message("NORRR1MM chargé: ", nrow(norrr1mm_data), " entités")

# ---- Création d'un jeu de données unifié par code commune ----
message("Création d'un jeu de données unifié par code commune...")

# Standardisation des noms de colonnes pour les codes commune
postal_secheresse$CODE_COMMUNE <- as.character(postal_secheresse$ID)
alea_argile_postal$CODE_COMMUNE <- as.character(alea_argile_postal$entity_id)
norswiav_data$CODE_COMMUNE <- as.character(norswiav_data$CODE_C)
norrr1mm_data$CODE_COMMUNE <- as.character(norrr1mm_data$CODE_C)

# Extraire la géométrie et les noms des communes (LIB) qui serviront de base
geometry_base <- postal_secheresse %>%
  select(CODE_COMMUNE, LIB, geometry)

# Préparation des données de sécheresse
secheresse_vars <- postal_secheresse %>%
  st_drop_geometry() %>%
  select(CODE_COMMUNE, nb_sechere)

# Préparation des données d'aléa argile
argile_vars <- alea_argile_postal %>%
  st_drop_geometry() %>%
  select(CODE_COMMUNE, niveau_moy)

# Préparation des indicateurs NORSWIAV pour l'horizon choisi
if (HORIZON_CHOISI == "REF") {
  norswiav_col <- "NORSWIAV_REF"
} else {
  norswiav_col <- paste0("NORSWIAV_", HORIZON_CHOISI)
}

# Vérifier si la colonne existe
if (!(norswiav_col %in% names(norswiav_data))) {
  stop(paste("La colonne", norswiav_col, "n'existe pas dans le fichier NORSWIAV"))
}

# Sélection et renommage avec horizon et scénario
norswiav_vars <- norswiav_data %>%
  st_drop_geometry() %>%
  select(CODE_COMMUNE, !!norswiav_col) %>%
  rename_with(~ paste0("norswiav_", SCENARIO_CHOISI, "_", HORIZON_CHOISI), .cols = norswiav_col)

# Préparation des indicateurs NORRR1MM pour l'horizon choisi
if (HORIZON_CHOISI == "REF") {
  norrr1mm_col <- "NORRR1MM_REF"
} else {
  norrr1mm_col <- paste0("NORRR1MM_", HORIZON_CHOISI)
}

# Vérifier si la colonne existe
if (!(norrr1mm_col %in% names(norrr1mm_data))) {
  stop(paste("La colonne", norrr1mm_col, "n'existe pas dans le fichier NORRR1MM"))
}

# Sélection et renommage avec horizon et scénario
norrr1mm_vars <- norrr1mm_data %>%
  st_drop_geometry() %>%
  select(CODE_COMMUNE, !!norrr1mm_col) %>%
  rename_with(~ paste0("norrr1mm_", SCENARIO_CHOISI, "_", HORIZON_CHOISI), .cols = norrr1mm_col)

# Création du jeu de données global par jointure directe sur CODE_COMMUNE
data_global <- geometry_base %>%
  left_join(secheresse_vars, by = "CODE_COMMUNE") %>%
  left_join(argile_vars, by = "CODE_COMMUNE") %>%
  left_join(norswiav_vars, by = "CODE_COMMUNE") %>%
  left_join(norrr1mm_vars, by = "CODE_COMMUNE")

# Gestion des valeurs manquantes
data_global <- data_global %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Affichage des informations sur le jeu de données global
message("Jeu de données global créé: ", nrow(data_global), " entités et ", ncol(data_global), " colonnes")
message("Colonnes du jeu de données global: ", paste(names(data_global), collapse=", "))

# Extraire les données numériques pour l'analyse (sans la géométrie et le code commune)
data_analysis <- data_global %>%
  st_drop_geometry() %>%
  select(-CODE_COMMUNE, -LIB) %>%
  select(where(is.numeric))

# ---- ANALYSE EN COMPOSANTES PRINCIPALES ----

# Vérification des dimensions du jeu de données
message("Dimensions du jeu de données pour l'ACP: ", nrow(data_analysis), " x ", ncol(data_analysis))

# Assurons-nous qu'il n'y a pas de valeurs NA qui pourraient bloquer l'ACP
na_count <- sum(is.na(data_analysis))
if (na_count > 0) {
  message("ATTENTION: ", na_count, " valeurs NA détectées dans les données. Remplacement par la moyenne.")
  data_analysis <- data_analysis %>%
    mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
}

# Exécution de l'ACP avec prcomp (plus rapide que PCA)
message("Exécution de l'ACP avec prcomp() (plus rapide)...")
start_time <- Sys.time()

# Version avec prcomp (plus rapide que PCA de FactoMineR)
pca_result <- prcomp(data_analysis, scale. = TRUE)
message("ACP terminée en ", round(difftime(Sys.time(), start_time, units = "secs"), 2), " secondes")

# Au lieu de convertir l'objet prcomp, utilisons directement les fonctions factoextra
message("Préparation des visualisations...")

# Extraction directe des coordonnées, contributions et cosinus carrés
# Sauvegarde des coordonnées des variables
coordonnees <- pca_result$rotation
write.csv(coordonnees, file.path(output_dir, "coordonnees_variables.csv"), row.names = TRUE)

# Sauvegarde des coordonnées des individus (scores)
scores <- pca_result$x
# Ajout des noms des communes
scores_df <- as.data.frame(scores)
scores_df$CODE_COMMUNE <- data_global$CODE_COMMUNE
scores_df$LIB <- data_global$LIB
write.csv(scores_df, file.path(output_dir, "scores_individus.csv"), row.names = FALSE)

# ---- CERCLES DE CORRÉLATIONS POUR DIFFÉRENTES PAIRES D'AXES ----
# 1. Cercle des corrélations pour les axes 1 et 2
correlation_plot_1_2 <- fviz_pca_var(pca_result, 
                         axes = c(1, 2),
                         col.var = "contrib",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE,
                         title = paste("Cercle des corrélations (axes 1-2) -", 
                                SCENARIO_CHOISI, "horizon", HORIZON_CHOISI))
ggsave(file.path(output_dir, "cercle_correlations_1_2.pdf"), correlation_plot_1_2, width=10, height=8)
ggsave(file.path(output_dir, "cercle_correlations_1_2.png"), correlation_plot_1_2, width=10, height=8, dpi=300)

# 2. Cercle des corrélations pour les axes 1 et 3
correlation_plot_1_3 <- fviz_pca_var(pca_result, 
                         axes = c(1, 3),
                         col.var = "contrib",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE,
                         title = paste("Cercle des corrélations (axes 1-3) -", 
                                SCENARIO_CHOISI, "horizon", HORIZON_CHOISI))
ggsave(file.path(output_dir, "cercle_correlations_1_3.pdf"), correlation_plot_1_3, width=10, height=8)
ggsave(file.path(output_dir, "cercle_correlations_1_3.png"), correlation_plot_1_3, width=10, height=8, dpi=300)

# 3. Cercle des corrélations pour les axes 2 et 3
correlation_plot_2_3 <- fviz_pca_var(pca_result, 
                         axes = c(2, 3),
                         col.var = "contrib",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE,
                         title = paste("Cercle des corrélations (axes 2-3) -", 
                                SCENARIO_CHOISI, "horizon", HORIZON_CHOISI))
ggsave(file.path(output_dir, "cercle_correlations_2_3.pdf"), correlation_plot_2_3, width=10, height=8)
ggsave(file.path(output_dir, "cercle_correlations_2_3.png"), correlation_plot_2_3, width=10, height=8, dpi=300)

# ---- MATRICE DES CORRELATIONS ----
Tableau_Correlation <- round(cor(data_analysis, use="complete.obs"), 2)

correlation_matrix_plot <- ggcorrplot(Tableau_Correlation, hc.order = TRUE, lab = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726")) +
  labs(title = paste("Matrice des corrélations -", 
                      SCENARIO_CHOISI, "horizon", HORIZON_CHOISI))
ggsave(file.path(output_dir, "matrice_correlations.pdf"), correlation_matrix_plot, width=11, height=9)
ggsave(file.path(output_dir, "matrice_correlations.png"), correlation_matrix_plot, width=11, height=9, dpi=300)

# ---- HISTOGRAMME DES VALEURS PROPRES ----
eigenvalue_plot <- fviz_eig(pca_result, choice = c("variance", "eigenvalue"), 
         geom = c("bar", "line"), barfill = "royalblue1",
         barcolor = "grey35", linecolor = "black", 
         ncp = min(16, ncol(data_analysis)), addlabels = TRUE,
         main=paste("Histogramme des valeurs propres -", 
                      SCENARIO_CHOISI, "horizon", HORIZON_CHOISI))
ggsave(file.path(output_dir, "histogramme_valeurs_propres.pdf"), eigenvalue_plot, width=10, height=6)
ggsave(file.path(output_dir, "histogramme_valeurs_propres.png"), eigenvalue_plot, width=10, height=6, dpi=300)

# ---- COORDONNÉES, CONTRIBUTIONS ET COSINUS CARRÉS DES VARIABLES ----
coordonnees <- round(pca_result$rotation, 2)
write.csv(coordonnees, file.path(output_dir, "coordonnees_variables.csv"), row.names = TRUE)

contributions <- round(100 * (pca_result$rotation^2) / (colSums(pca_result$rotation^2)), 2)
write.csv(contributions, file.path(output_dir, "contributions_variables.csv"), row.names = TRUE)

cosinus <- round(pca_result$rotation^2, 2)
write.csv(cosinus, file.path(output_dir, "cosinus_carres_variables.csv"), row.names = TRUE)

# Moyenne des cosinus carrés
cos2_moyenne <- round(rbind(cosinus, MOYENNE_COS2=colSums(cosinus/dim(cosinus)[1])), 2)

# Utiliser png au lieu de pdf pour la corrplot
png(file.path(output_dir, "cosinus_carres_moyens.png"), width=1000, height=600, res=120)
corrplot(cos2_moyenne,	
         is.corr=FALSE,
         method='circle',
         color=palette(),
         title="Tableau des cosinus carrés",
         addCoef.col=TRUE,
         tl.col="black",
         number.cex=0.7)
dev.off()

# ---- PROJECTION DES 20 INDIVIDUS LES PLUS CONTRIBUTIFS PAR AXE ----
# Créer un sous-répertoire pour les cartes
maps_dir <- file.path(output_dir, "maps")
if (!dir.exists(maps_dir)) {
  dir.create(maps_dir)
}

# Extraire les contributions des individus pour chaque axe
ind_contrib <- get_pca_ind(pca_result)$contrib

# Pour chaque axe, on va identifier les 20 individus les plus contributifs
# et créer une carte et un graphique
for (axis_num in 1:3) {
  # Identifier les 20 individus les plus contributifs pour cet axe
  top20_ind_axis <- order(-ind_contrib[, axis_num])[1:20]
  
  # Créer un dataframe spécifique pour les 20 communes les plus contributives
  # Initialiser toutes les communes avec valeur 0 (non contributives)
  data_global_axis <- data_global
  data_global_axis$contributif <- 0
  
  # Marquer uniquement les 20 communes les plus contributives avec valeur 1
  data_global_axis$contributif[top20_ind_axis] <- 1
  
  # Convertir en facteur pour contrôler l'affichage
  data_global_axis$contributif <- factor(data_global_axis$contributif, 
                                        levels = c(0, 1),
                                        labels = c("Non", "Oui"))
  
  # Ajouter le score sur cet axe pour référence
  data_global_axis$score <- pca_result$x[, axis_num]
  
  # Carte des 20 communes les plus contributives pour cet axe
  png(file.path(maps_dir, paste0("carte_top20_PC", axis_num, ".png")), 
      width=1200, height=800, res=120)
  
  # Utiliser une échelle de couleur claire pour les non-contributives, et rouge vif pour les contributives
  plot(data_global_axis["contributif"], 
       main = paste0("Les 20 communes les plus contributives à l'axe ", axis_num, 
                     " - ", SCENARIO_CHOISI, " horizon ", HORIZON_CHOISI),
       pal = c("lightgrey", "red"),
       key.pos = 4,  # Légende en bas à droite
       key.width = lcm(5),
       key.length = 1)
  dev.off()
  
  # Version alternative avec trame
  png(file.path(maps_dir, paste0("carte_top20_PC", axis_num, "_alt.png")), 
      width=1200, height=800, res=120)
  
  # Dessiner d'abord toutes les communes en gris clair
  plot(st_geometry(data_global_axis), 
       col = "lightgrey", 
       border = "darkgrey",
       main = paste0("Les 20 communes les plus contributives à l'axe ", axis_num, 
                    " - ", SCENARIO_CHOISI, " horizon ", HORIZON_CHOISI),
       lwd = 0.2)
  
  # Superposer uniquement les 20 communes les plus contributives en rouge
  plot(st_geometry(data_global_axis[top20_ind_axis,]), 
       col = "red", 
       border = "black", 
       add = TRUE,
       lwd = 0.3)
  
  # Ajouter une légende
  legend("bottomright", 
         legend = c("20 communes les plus contributives", "Autres communes"),
         fill = c("red", "lightgrey"),
         border = c("black", "darkgrey"),
         bty = "n")
  dev.off()
  
  # Carte des scores pour cet axe
  pc_col <- paste0("PC", axis_num)
  png(file.path(maps_dir, paste0("carte_scores_PC", axis_num, ".png")), 
      width=1200, height=800, res=120)
  plot(data_global_axis["score"], 
       main = paste0("Scores sur l'axe PC", axis_num, 
                    " - ", SCENARIO_CHOISI, " horizon ", HORIZON_CHOISI),
       breaks = "quantile")
  dev.off()
  
  # Carte combinée: scores avec les 20 communes superposées
  png(file.path(maps_dir, paste0("carte_scores_top20_PC", axis_num, ".png")), 
      width=1200, height=800, res=120)
  # D'abord afficher les scores
  plot(data_global_axis["score"], 
       main = paste0("Scores sur l'axe PC", axis_num, " et les 20 communes les plus contributives", 
                    " - ", SCENARIO_CHOISI, " horizon ", HORIZON_CHOISI),
       breaks = "quantile")
  
  # Superposer le contour des 20 communes les plus contributives
  plot(st_geometry(data_global_axis[top20_ind_axis,]), 
       border = "red", 
       add = TRUE,
       lwd = 1.5)
  
  # Ajouter une légende pour les contours
  legend("bottomright", 
         legend = "20 communes les plus contributives",
         lty = 1,
         col = "red",
         lwd = 1.5,
         bty = "n")
  dev.off()
  
  # Carte montrant UNIQUEMENT les scores des 20 communes les plus contributives
  png(file.path(maps_dir, paste0("carte_scores_uniquement_top20_PC", axis_num, ".png")), 
      width=1200, height=800, res=120)
  
  # Préparer un fond de carte avec toutes les communes en gris clair
  plot(st_geometry(data_global_axis), 
       col = "lightgrey", 
       border = "darkgrey",
       main = paste0("Scores sur l'axe PC", axis_num, " - UNIQUEMENT les 20 communes les plus contributives", 
                    " - ", SCENARIO_CHOISI, " horizon ", HORIZON_CHOISI),
       lwd = 0.1)
       
  # Créer une palette de couleurs pour les scores des communes contributives
  score_range <- range(data_global_axis$score[top20_ind_axis])
  breaks <- seq(score_range[1], score_range[2], length.out = 7)
  score_colors <- colorRampPalette(c("blue", "purple", "red", "orange", "yellow"))(6)
  
  # Déterminer les couleurs pour chaque commune contributive
  score_values <- data_global_axis$score[top20_ind_axis]
  color_indices <- cut(score_values, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  
  # Ajouter uniquement les 20 communes contributives avec couleur selon score
  for (i in 1:length(top20_ind_axis)) {
    plot(st_geometry(data_global_axis[top20_ind_axis[i],]), 
         col = score_colors[color_indices[i]],
         border = "black",
         add = TRUE,
         lwd = 0.5)
  }
  
  # Ajouter une légende pour les scores
  legend_labels <- round(breaks, 2)
  legend_colors <- score_colors
  
  legend("bottomright",
         legend = paste(legend_labels[-length(legend_labels)], "-", legend_labels[-1]),
         fill = legend_colors,
         title = paste("Scores PC", axis_num),
         border = "black",
         bty = "n")
  
  dev.off()
  
  # Carte interactive des 20 communes les plus contributives avec leurs scores
  if (requireNamespace("mapview", quietly = TRUE)) {
    # Créer un sous-ensemble pour les communes contributives
    top20_sf <- data_global_axis[top20_ind_axis,]
    # Renommer la colonne score pour une meilleure lisibilité
    names(top20_sf)[which(names(top20_sf) == "score")] <- paste0("Score_PC", axis_num)
    
    # Créer une carte interactive et la sauvegarder
    map_file <- file.path(maps_dir, paste0("carte_interactive_top20_PC", axis_num, ".html"))
    
    tryCatch({
      m <- mapview::mapview(top20_sf, zcol = paste0("Score_PC", axis_num), 
                           layer.name = paste0("Scores PC", axis_num))
      mapview::mapshot(m, file = map_file)
      message(paste("Carte interactive créée:", map_file))
    }, error = function(e) {
      message("Impossible de créer la carte interactive: ", e$message)
    })
  }
  
  # Sauvegarder les 20 communes les plus contributives pour cet axe
  top20_communes_axis <- data.frame(
    CODE_COMMUNE = data_global$CODE_COMMUNE[top20_ind_axis],
    COMMUNE = data_global$LIB[top20_ind_axis],
    SCORE = pca_result$x[top20_ind_axis, axis_num],
    CONTRIBUTION = ind_contrib[top20_ind_axis, axis_num]
  )
  write.csv(top20_communes_axis, 
            file.path(maps_dir, paste0("top20_communes_PC", axis_num, ".csv")), 
            row.names = FALSE)
}

# ---- PROJECTIONS STYLISÉES DES INDIVIDUS (STYLE SIMILAIRE À L'EXEMPLE) ----
# Extraire les contributions des individus pour les trois premiers axes
ind_contrib <- get_pca_ind(pca_result)$contrib

# Identifier les 20 individus les plus contributifs pour axes 1-2
top20_ind_axes12 <- order(-rowSums(ind_contrib[, 1:2]))[1:20]

# Extraire les scores (coordonnées) des individus
pca_scores <- as.data.frame(pca_result$x)

# Ajouter des informations d'identification
pca_scores$commune <- data_global$LIB
pca_scores$code <- data_global$CODE_COMMUNE
pca_scores$contrib12 <- rowSums(ind_contrib[, 1:2])
pca_scores$is_top20 <- FALSE
pca_scores$is_top20[top20_ind_axes12] <- TRUE

# Créer des étiquettes avec juste le nom de la commune (sans code)
pca_scores$label <- pca_scores$commune

# 1. Projection PC1 vs PC2 (style similaire à l'exemple)
p1_2_styled <- ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  # Grille en fond
  geom_hline(yintercept = seq(-10, 10, by = 0.5), color = "lightgray", linetype = "dashed", alpha = 0.3) +
  geom_vline(xintercept = seq(-10, 10, by = 0.5), color = "lightgray", linetype = "dashed", alpha = 0.3) +
  # Axes à zéro
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  # Points pour tous les individus
  geom_point(data = pca_scores[!pca_scores$is_top20, ], 
             color = "darkblue", alpha = 0.3, size = 1) +
  # Points pour les top 20 (en violet comme dans l'exemple)
  geom_point(data = pca_scores[pca_scores$is_top20, ], 
             color = "purple", size = 2) +
  # Étiquettes pour les top 20 communes uniquement
  geom_text_repel(data = pca_scores[pca_scores$is_top20, ],
                 aes(label = label), 
                 color = "purple", 
                 size = 4, 
                 fontface = "bold",
                 max.overlaps = 40,
                 box.padding = 0.5) +
  # Personnalisation
  labs(title = paste("Projection des 20 individus les plus contributifs sur PC1-PC2"),
       subtitle = paste("Scénario", SCENARIO_CHOISI, "- Horizon", HORIZON_CHOISI),
       x = "PC1", y = "PC2") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#FAFAFA", color = NA)) +
  # Limites fixes pour les axes
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits = c(min(pca_scores$PC1)*1.2, max(pca_scores$PC1)*1.2),
                    breaks = seq(-10, 10, by = 2.5)) +
  scale_y_continuous(limits = c(min(pca_scores$PC2)*1.2, max(pca_scores$PC2)*1.2),
                    breaks = seq(-10, 10, by = 2.5))

ggsave(file.path(output_dir, "projection_styled_PC1_PC2.pdf"), p1_2_styled, width=11, height=9)
ggsave(file.path(output_dir, "projection_styled_PC1_PC2.png"), p1_2_styled, width=11, height=9, dpi=300)

# 2. Projection PC1 vs PC3 (style similaire à l'exemple)
p1_3_styled <- ggplot(pca_scores, aes(x = PC1, y = PC3)) +
  # Grille en fond
  geom_hline(yintercept = seq(-10, 10, by = 0.5), color = "lightgray", linetype = "dashed", alpha = 0.3) +
  geom_vline(xintercept = seq(-10, 10, by = 0.5), color = "lightgray", linetype = "dashed", alpha = 0.3) +
  # Axes à zéro
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  # Points pour tous les individus
  geom_point(data = pca_scores[!pca_scores$is_top20, ], 
             color = "darkblue", alpha = 0.3, size = 1) +
  # Points pour les top 20 (en violet comme dans l'exemple)
  geom_point(data = pca_scores[pca_scores$is_top20, ], 
             color = "purple", size = 2) +
  # Étiquettes pour les top 20 communes uniquement
  geom_text_repel(data = pca_scores[pca_scores$is_top20, ],
                 aes(label = label), 
                 color = "purple", 
                 size = 4, 
                 fontface = "bold",
                 max.overlaps = 40,
                 box.padding = 0.5) +
  # Personnalisation
  labs(title = paste("Projection des 20 individus les plus contributifs sur PC1-PC3"),
       subtitle = paste("Scénario", SCENARIO_CHOISI, "- Horizon", HORIZON_CHOISI),
       x = "PC1", y = "PC3") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#FAFAFA", color = NA)) +
  # Limites fixes pour les axes
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits = c(min(pca_scores$PC1)*1.2, max(pca_scores$PC1)*1.2),
                    breaks = seq(-10, 10, by = 2.5)) +
  scale_y_continuous(limits = c(min(pca_scores$PC3)*1.2, max(pca_scores$PC3)*1.2),
                    breaks = seq(-10, 10, by = 2.5))

ggsave(file.path(output_dir, "projection_styled_PC1_PC3.pdf"), p1_3_styled, width=11, height=9)
ggsave(file.path(output_dir, "projection_styled_PC1_PC3.png"), p1_3_styled, width=11, height=9, dpi=300)

# 3. Projection PC2 vs PC3 (style similaire à l'exemple)
p2_3_styled <- ggplot(pca_scores, aes(x = PC2, y = PC3)) +
  # Grille en fond
  geom_hline(yintercept = seq(-10, 10, by = 0.5), color = "lightgray", linetype = "dashed", alpha = 0.3) +
  geom_vline(xintercept = seq(-10, 10, by = 0.5), color = "lightgray", linetype = "dashed", alpha = 0.3) +
  # Axes à zéro
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  # Points pour tous les individus
  geom_point(data = pca_scores[!pca_scores$is_top20, ], 
             color = "darkblue", alpha = 0.3, size = 1) +
  # Points pour les top 20 (en violet comme dans l'exemple)
  geom_point(data = pca_scores[pca_scores$is_top20, ], 
             color = "purple", size = 2) +
  # Étiquettes pour les top 20 communes uniquement
  geom_text_repel(data = pca_scores[pca_scores$is_top20, ],
                 aes(label = label), 
                 color = "purple", 
                 size = 4, 
                 fontface = "bold",
                 max.overlaps = 40,
                 box.padding = 0.5) +
  # Personnalisation
  labs(title = paste("Projection des 20 individus les plus contributifs sur PC2-PC3"),
       subtitle = paste("Scénario", SCENARIO_CHOISI, "- Horizon", HORIZON_CHOISI),
       x = "PC2", y = "PC3") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#FAFAFA", color = NA)) +
  # Limites fixes pour les axes
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits = c(min(pca_scores$PC2)*1.2, max(pca_scores$PC2)*1.2),
                    breaks = seq(-10, 10, by = 2.5)) +
  scale_y_continuous(limits = c(min(pca_scores$PC3)*1.2, max(pca_scores$PC3)*1.2),
                    breaks = seq(-10, 10, by = 2.5))

ggsave(file.path(output_dir, "projection_styled_PC2_PC3.pdf"), p2_3_styled, width=11, height=9)
ggsave(file.path(output_dir, "projection_styled_PC2_PC3.png"), p2_3_styled, width=11, height=9, dpi=300)

# ---- BIPLOT (INDIVIDUS ET VARIABLES) POUR DIFFÉRENTES PAIRES D'AXES ----
# Préparation des données pour les biplots personnalisés
var_coord <- pca_result$rotation
var_data <- as.data.frame(var_coord)
var_data$var <- rownames(var_data)

# 1. Biplot pour les axes 1 et 2 - Approche directe avec ggplot2
biplot_1_2 <- ggplot() +
  # Points des individus (toutes les communes en gris clair)
  geom_point(data = pca_scores, aes(x = PC1, y = PC2), color = "gray80", alpha = 0.3) +
  # Points des individus contributifs
  geom_point(data = pca_scores[top20_ind_axes12, ], aes(x = PC1, y = PC2), color = "steelblue", size = 3) +
  # Étiquettes des communes contributives
  geom_text_repel(data = pca_scores[top20_ind_axes12, ], aes(x = PC1, y = PC2, label = commune), 
                 size = 3, color = "black", max.overlaps = 40) +
  # Flèches des variables
  geom_segment(data = var_data, aes(x = 0, y = 0, xend = PC1*5, yend = PC2*5), 
               arrow = arrow(length = unit(0.2, "cm")), color = "red", alpha = 0.8) +
  # Étiquettes des variables
  geom_text_repel(data = var_data, aes(x = PC1*5.2, y = PC2*5.2, label = var), 
                 color = "darkred", size = 3) +
  # Personnalisation
  theme_minimal() +
  labs(title = paste("Biplot (axes 1-2) -", SCENARIO_CHOISI, "horizon", HORIZON_CHOISI)) +
  coord_fixed(ratio = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50")

ggsave(file.path(output_dir, "biplot_1_2.pdf"), biplot_1_2, width=11, height=9)
ggsave(file.path(output_dir, "biplot_1_2.png"), biplot_1_2, width=11, height=9, dpi=300)

# 2. Biplot pour les axes 1 et 3
biplot_1_3 <- ggplot() +
  # Points des individus (toutes les communes en gris clair)
  geom_point(data = pca_scores, aes(x = PC1, y = PC3), color = "gray80", alpha = 0.3) +
  # Points des individus contributifs
  geom_point(data = pca_scores[top20_ind_axes12, ], aes(x = PC1, y = PC3), color = "steelblue", size = 3) +
  # Étiquettes des communes contributives
  geom_text_repel(data = pca_scores[top20_ind_axes12, ], aes(x = PC1, y = PC3, label = commune), 
                 size = 3, color = "black", max.overlaps = 40) +
  # Flèches des variables
  geom_segment(data = var_data, aes(x = 0, y = 0, xend = PC1*5, yend = PC3*5), 
               arrow = arrow(length = unit(0.2, "cm")), color = "red", alpha = 0.8) +
  # Étiquettes des variables
  geom_text_repel(data = var_data, aes(x = PC1*5.2, y = PC3*5.2, label = var), 
                 color = "darkred", size = 3) +
  # Personnalisation
  theme_minimal() +
  labs(title = paste("Biplot (axes 1-3) -", SCENARIO_CHOISI, "horizon", HORIZON_CHOISI)) +
  coord_fixed(ratio = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50")

ggsave(file.path(output_dir, "biplot_1_3.pdf"), biplot_1_3, width=11, height=9)
ggsave(file.path(output_dir, "biplot_1_3.png"), biplot_1_3, width=11, height=9, dpi=300)

# 3. Biplot pour les axes 2 et 3
biplot_2_3 <- ggplot() +
  # Points des individus (toutes les communes en gris clair)
  geom_point(data = pca_scores, aes(x = PC2, y = PC3), color = "gray80", alpha = 0.3) +
  # Points des individus contributifs
  geom_point(data = pca_scores[top20_ind_axes12, ], aes(x = PC2, y = PC3), color = "steelblue", size = 3) +
  # Étiquettes des communes contributives
  geom_text_repel(data = pca_scores[top20_ind_axes12, ], aes(x = PC2, y = PC3, label = commune), 
                 size = 3, color = "black", max.overlaps = 40) +
  # Flèches des variables
  geom_segment(data = var_data, aes(x = 0, y = 0, xend = PC2*5, yend = PC3*5), 
               arrow = arrow(length = unit(0.2, "cm")), color = "red", alpha = 0.8) +
  # Étiquettes des variables
  geom_text_repel(data = var_data, aes(x = PC2*5.2, y = PC3*5.2, label = var), 
                 color = "darkred", size = 3) +
  # Personnalisation
  theme_minimal() +
  labs(title = paste("Biplot (axes 2-3) -", SCENARIO_CHOISI, "horizon", HORIZON_CHOISI)) +
  coord_fixed(ratio = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50")

ggsave(file.path(output_dir, "biplot_2_3.pdf"), biplot_2_3, width=11, height=9)
ggsave(file.path(output_dir, "biplot_2_3.png"), biplot_2_3, width=11, height=9, dpi=300)

# ---- SAUVEGARDE DES SCORES DANS LE FICHIER GPKG ----
message("Sauvegarde des scores de l'ACP dans le fichier GPKG...")

# Ajout des scores aux données spatiales
data_global$pc1 <- pca_result$x[,1]
data_global$pc2 <- pca_result$x[,2]
if(ncol(pca_result$x) >= 3) data_global$pc3 <- pca_result$x[,3]

# Ajout des métadonnées
data_global$scenario <- SCENARIO_CHOISI
data_global$horizon <- HORIZON_CHOISI

# Ajout des variables les plus contributives pour PC1 et PC2
# Calculer les contributions des variables pour chaque composante
var_contributions <- get_pca_var(pca_result)$contrib
# Identifier les 3 variables les plus contributives pour PC1
top_contrib_pc1 <- rownames(var_contributions)[order(-var_contributions[,1])][1:3]
data_global$top_var_pc1 <- paste(top_contrib_pc1, collapse=", ")

# Identifier les 3 variables les plus contributives pour PC2
top_contrib_pc2 <- rownames(var_contributions)[order(-var_contributions[,2])][1:3]
data_global$top_var_pc2 <- paste(top_contrib_pc2, collapse=", ")

# Création des cartes des scores
png(file.path(output_dir, "carte_pc1.png"), width=1200, height=800, res=120)
plot(data_global["pc1"], 
     main = paste("Scores PC1 -", SCENARIO_CHOISI, "horizon", HORIZON_CHOISI))
dev.off()

png(file.path(output_dir, "carte_pc2.png"), width=1200, height=800, res=120)
plot(data_global["pc2"], 
     main = paste("Scores PC2 -", SCENARIO_CHOISI, "horizon", HORIZON_CHOISI))
dev.off()

# Sauvegarde des résultats dans un fichier GPKG
st_write(data_global, file.path(output_dir, "resultats_acp.gpkg"), 
         append = FALSE, quiet = TRUE)

# Sauvegarde séparée des scores sous forme de tableau CSV
scores_df <- as.data.frame(pca_result$x[,1:min(5, ncol(pca_result$x))])
scores_df$CODE_COMMUNE <- data_global$CODE_COMMUNE
scores_df$LIB <- data_global$LIB
write.csv(scores_df, file.path(output_dir, "scores_acp.csv"), row.names = FALSE)

# ---- FIN DU SCRIPT ----
message("\nAnalyse terminée. Résultats sauvegardés dans: ", output_dir)
message("Lien vers les fichiers de sortie: ", normalizePath(output_dir))

# Rendre le script exécutable
if (.Platform$OS.type == "unix") {
  this_script <- commandArgs(trailingOnly = FALSE)
  script_path <- sub("--file=", "", this_script[grep("--file=", this_script)])
  system(paste("chmod +x", script_path))
}

