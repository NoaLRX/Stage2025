source("scripts/library.R")
df <- st_read("data/DF_FINAL_AVEC_CATNAT.gpkg")
df_CSV <- read.csv("data/DF_FINAL_AVEC_CATNAT.csv")

# On retire les NA
sapply(df, function(y) sum(length(which(is.na(y)))))
df[is.na(df)] <- 0

# Format des données
str(df)
df$code_commune <- as.numeric(df$code_commune)
df$code_dep <- as.numeric(df$code_dep)
df$annee_constuction <- as.numeric(df$annee_constuction)
df$age_batiment <- 2025 - df$annee_constuction

# Création d'une variable €/m2
df$prix_m2 <- df$prix / df$surface_habitable

appart <- df %>%
  filter(type == "appartement")

appart_csv <- df_CSV %>%
  filter(type == "appartement")

maison <- df %>%
  filter(type == "maison")

maison_csv <- df_CSV %>%
  filter(type == "maison")


# Modèles MAISONS --------------------------------------------------------------
# Ajouter des effets fixes départementaux ou communaux
options(scipen = 999)
fit_dpe <- lm(
  prix ~
    relevel(factor(dpe), ref = "D") +
      age_batiment +
      surface_habitable +
      surface_terrain +
      nb_etages +
      nb_pieces +
      DIST_VERT_KM +
      DIST_BLEU_KM +
      DIST_ROUGE_KM +
      n_vide +
      revenu_median +
      part_proprio +
      # Partie CATNAT
      inond +
      mvt_terrain +
      secheresse +
      grele +
      tempete +
      # mvt_sech+
      inond_nappe +
      glissement +
      # mvt_hors_sech+
      eboulement +
      seisme +
      # effondrement+
      # lave_torr+
      # avalanche+
      choc_vagues +
      # gliss_effond+
      poids_neige +
      # vent_cyclo+
      # raz_maree+
      # inond_vagues+
      # gliss_eboul+
      coulee_boue +
      # seismes+
      # choc_vagues2+
      # divers+
      # volcan+
      relevel(factor(code_dep), ref = "29"), # Effets fixes départementaux
  data = maison
)
vif(fit_dpe)
summary(fit_dpe)


# Avec le prix en LOG
options(scipen = 999)
fit_dpe <- lm(
  log(prix) ~
    relevel(factor(dpe), ref = "D") +
      age_batiment +
      log(surface_habitable) +
      surface_terrain +
      # nb_etages +
      nb_pieces +
      DIST_VERT_KM +
      DIST_BLEU_KM +
      DIST_ROUGE_KM +
      n_vide +
      revenu_median +
      part_proprio +
      # Partie CATNAT
      inond +
      mvt_terrain +
      secheresse +
      grele +
      # tempete +
      # mvt_sech+
      inond_nappe +
      glissement +
      # mvt_hors_sech+
      eboulement +
      # seisme +
      # effondrement+
      # lave_torr+
      # avalanche+
      choc_vagues +
      # gliss_effond+
      poids_neige +
      # vent_cyclo+
      # raz_maree+
      # inond_vagues+
      # gliss_eboul+
      coulee_boue +
      # seismes+
      # choc_vagues2+
      # divers+
      # volcan+
      relevel(factor(code_dep), ref = "29"), # Effets fixes départementaux
  data = maison
)
summary(fit_dpe)

# Graphique des résultats DPE --------------------------------------------------
# Créer le dataframe avec vos résultats
dpe_results <- data.frame(
  dpe = c("A", "B", "C", "D", "E", "F", "G"),
  coefficient = c(
    0.0023944738,
    0.0810976304,
    0.0323589656,
    0,
    -0.0757244020,
    -0.1582247242,
    -0.2475220671
  ),
  std_error = c(
    0.0121660943,
    0.0108441235,
    0.0060689697,
    0,
    0.0057256075,
    0.0069921010,
    0.0080210839
  ),
  pvalue = c(
    0.844,
    0.0000000000000763,
    0.0000000975891162,
    NA,
    0.0000000000000002,
    0.0000000000000002,
    0.0000000000000002
  )
)

# Convertir les coefficients en pourcentage
dpe_results$percentage <- dpe_results$coefficient * 100

# Calculer les intervalles de confiance (95%)
dpe_results$ci_lower <- (dpe_results$coefficient -
  1.96 * dpe_results$std_error) *
  100
dpe_results$ci_upper <- (dpe_results$coefficient +
  1.96 * dpe_results$std_error) *
  100

# Définir les couleurs pour chaque classe DPE
dpe_colors <- c(
  "A" = "#1f7a3c",
  "B" = "#4caf50",
  "C" = "#8bc34a",
  "D" = "#ffeb3b",
  "E" = "#ff9800",
  "F" = "#ff5722",
  "G" = "#d32f2f"
)

# Créer le graphique
pdf("pdf/impact_dpe.pdf", width = 12, height = 8)
ggplot(
  dpe_results,
  aes(x = reorder(dpe, -coefficient), y = percentage, fill = dpe)
) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(
    aes(
      label = ifelse(
        percentage >= 0,
        paste0("+", round(percentage, 1), "%"),
        paste0(round(percentage, 1), "%")
      )
    ),
    hjust = ifelse(dpe_results$percentage >= 0, -0.1, 1.1),
    vjust = 0.5,
    size = 4,
    fontface = "bold",
    color = "black"
  ) +
  scale_fill_manual(values = dpe_colors) +
  coord_flip() +
  labs(
    title = "Impact sur le prix de mise en vente des maisons",
    subtitle = "Variation par rapport au DPE D (référence)",
    x = NULL,
    y = "Variation du prix (%)",
    fill = "DPE"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray60"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 11),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  scale_x_discrete(limits = c("G", "F", "E", "D", "C", "B", "A")) +
  geom_hline(
    yintercept = 0,
    linetype = "solid",
    color = "black",
    linewidth = 0.5
  )

dev.off()


# Graphique avec les autres variables ------------------------------------------
library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Créer le dataframe avec les résultats des autres variables
variables_results <- data.frame(
  variable = c(
    "Âge du bâtiment",
    "Surface habitable",
    "Surface terrain",
    "Nombre d'étages",
    "Nombre de pièces",
    "Éloignement espaces verts",
    "Éloignement espaces bleus",
    "Éloignement espaces rouges",
    "Logements vacants",
    "Revenu médian",
    "Part propriétaires",
    "Catastrophes naturelles"
  ),
  coefficient = c(
    -0.0012926429,
    0.0037378445,
    0.0000075159,
    0.0061292648,
    0.0807294047,
    -0.0271521121,
    -0.0235397427,
    0.0031947694,
    0.0000662848,
    0.0001278393,
    -0.0062105904,
    -0.0117929036
  ),
  std_error = c(
    0.0000379549,
    0.0000554388,
    0.0000004129,
    0.0039715053,
    0.0018031327,
    0.0023855981,
    0.0004980380,
    0.0005450279,
    0.0000021062,
    0.0000012043,
    0.0002531358,
    0.0005269226
  ),
  pvalue = c(
    0.0000000000000002,
    0.0000000000000002,
    0.0000000000000002,
    0.123,
    0.0000000000000002,
    0.0000000000000002,
    0.0000000000000002,
    0.0000000046086863,
    0.0000000000000002,
    0.0000000000000002,
    0.0000000000000002,
    0.0000000000000002
  ),
  unite = c(
    "par année",
    "par m²",
    "par m²",
    "par étage",
    "par pièce",
    "par km",
    "par km",
    "par km",
    "par logement vacant",
    "par €",
    "par point %",
    "effet binaire"
  )
)

# Convertir les coefficients en pourcentages
variables_results$percentage <- variables_results$coefficient * 100

# Créer une palette de couleurs du rouge foncé au vert foncé
n_colors <- nrow(variables_results)
# Créer une palette gradient basée sur les valeurs de pourcentage
variables_results$couleur <- colorRampPalette(c(
  "#8B0000",
  "#DC143C",
  "#FFA500",
  "#FFFF00",
  "#90EE90",
  "#32CD32",
  "#006400"
))(n_colors)[rank(variables_results$percentage)]


# Version alternative avec annotations des unités
pdf("pdf/impact_autre_variables.pdf", width = 16, height = 10)
ggplot(
  variables_results,
  aes(x = reorder(variable, percentage), y = percentage, fill = couleur)
) +
  geom_col(width = 0.7, alpha = 0.8) +
  geom_text(
    aes(
      label = paste0(
        ifelse(percentage >= 0, "+", ""),
        round(percentage, 3),
        "%\n(",
        unite,
        ")"
      )
    ),
    hjust = ifelse(variables_results$percentage >= 0, -0.1, 1.1),
    vjust = 0.5,
    size = 3,
    fontface = "bold",
    color = "black"
  ) +
  scale_fill_identity() +
  coord_flip() +
  labs(
    title = "Impact des caractéristiques sur le prix de vente des maisons",
    subtitle = "Variation en % du prix par unité de la variable",
    x = NULL,
    y = "Variation du prix (%)",
    caption = "Espaces BLEUS : Littoral (mer, océan)\nEspaces VERTS: forêts, végétation, plages, équipements sportifs)\nEspaces ROUGES : Axes de transport (routes, voies ferrées, aéroports) et nuisances (décharges, extractions, marais)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray60"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.caption = element_text(size = 9, color = "gray60", hjust = 0)
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "solid",
    color = "black",
    linewidth = 0.5
  )
dev.off()


# Modèles APPARTS --------------------------------------------------------------
# Ajouter des effets fixes départementaux ou communaux
# Avec le prix en LOG
options(scipen = 99)
fit_dpe <- lm(
  log(prix) ~
    relevel(factor(dpe), ref = "D") +
      age_batiment +
      log(surface_habitable) +
      # surface_terrain +
      nb_etages +
      nb_pieces +
      DIST_VERT_KM +
      DIST_BLEU_KM +
      DIST_ROUGE_KM +
      n_vide +
      revenu_median +
      part_proprio +

      # Partie CATNAT
      inond +
      mvt_terrain +
      secheresse +
      grele +
      # tempete +
      # mvt_sech +
      # inond_nappe +
      glissement +
      # mvt_hors_sech +
      # eboulement +
      # seisme +
      # effondrement +
      # lave_torr +
      # avalanche +
      choc_vagues +
      # gliss_effond +
      poids_neige +
      # vent_cyclo +
      # raz_maree +
      # inond_vagues +
      # gliss_eboul +
      # coulee_boue +
      # seismes +
      # choc_vagues2 +
      # divers +
      # volcan,
      relevel(factor(code_dep), ref = "29"), # Effets fixes départementaux
  data = appart
)
summary(fit_dpe)


# Graphique des résultats DPE pour les appartements ----------------------------
# Créer le dataframe avec les résultats pour les appartements
dpe_results <- data.frame(
  dpe = c("A", "B", "C", "D", "E", "F", "G"),
  coefficient = c(
    0.102936828,
    0.175958975,
    0.116475819,
    0,
    -0.042545897,
    -0.081902334,
    -0.146003284
  ),
  std_error = c(
    0.107251452,
    0.038910683,
    0.011486766,
    0,
    0.011622177,
    0.018347433,
    0.025507553
  ),
  pvalue = c(
    0.337188,
    0.00000617560484417,
    0.0000000000000002,
    NA,
    0.000252,
    0.00000811239956965,
    0.00000001063599493
  )
)

# Convertir les coefficients en pourcentage
dpe_results$percentage <- dpe_results$coefficient * 100

# Calculer les intervalles de confiance (95%)
dpe_results$ci_lower <- (dpe_results$coefficient -
  1.96 * dpe_results$std_error) *
  100
dpe_results$ci_upper <- (dpe_results$coefficient +
  1.96 * dpe_results$std_error) *
  100

# Définir les couleurs pour chaque classe DPE
dpe_colors <- c(
  "A" = "#1f7a3c",
  "B" = "#4caf50",
  "C" = "#8bc34a",
  "D" = "#ffeb3b",
  "E" = "#ff9800",
  "F" = "#ff5722",
  "G" = "#d32f2f"
)

# Créer le graphique DPE
pdf("pdf/impact_dpe_appartements.pdf", width = 12, height = 8)
ggplot(
  dpe_results,
  aes(x = reorder(dpe, -coefficient), y = percentage, fill = dpe)
) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(
    aes(
      label = ifelse(
        percentage >= 0,
        paste0("+", round(percentage, 1), "%"),
        paste0(round(percentage, 1), "%")
      )
    ),
    hjust = ifelse(dpe_results$percentage >= 0, -0.1, 1.1),
    vjust = 0.5,
    size = 4,
    fontface = "bold",
    color = "black"
  ) +
  scale_fill_manual(values = dpe_colors) +
  coord_flip() +
  labs(
    title = "Impact sur le prix de mise en vente des appartements",
    subtitle = "Variation par rapport au DPE D (référence)",
    x = NULL,
    y = "Variation du prix (%)",
    fill = "DPE"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray60"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 11),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  scale_x_discrete(limits = c("G", "F", "E", "D", "C", "B", "A")) +
  geom_hline(
    yintercept = 0,
    linetype = "solid",
    color = "black",
    linewidth = 0.5
  )

dev.off()


# Graphique avec les autres variables pour les appartements --------------------
# Créer le dataframe avec les résultats des autres variables pour les appartements
variables_results <- data.frame(
  variable = c(
    "Âge du bâtiment",
    "Surface habitable",
    "Nombre de pièces",
    "Éloignement espaces verts",
    "Éloignement espaces bleus",
    "Éloignement espaces rouges",
    "Logements vacants",
    "Revenu médian",
    "Part propriétaires",
    "Catastrophes naturelles"
  ),
  coefficient = c(
    -0.000486043,
    0.000371986,
    0.266786493,
    -0.036071443,
    -0.042547754,
    0.010238126,
    0.000034124,
    0.000100060,
    -0.005645006,
    -0.012276569
  ),
  std_error = c(
    0.000077272,
    0.000062025,
    0.004027503,
    0.006499450,
    0.001679968,
    0.001551352,
    0.000002648,
    0.000003310,
    0.000724688,
    0.001369264
  ),
  pvalue = c(
    0.00000000032739810,
    0.00000000205765245,
    0.0000000000000002,
    0.00000002913149290,
    0.0000000000000002,
    0.00000000004283649,
    0.0000000000000002,
    0.0000000000000002,
    0.00000000000000723,
    0.0000000000000002
  ),
  unite = c(
    "par année",
    "par m²",
    "par pièce",
    "par km",
    "par km",
    "par km",
    "par logement vacant",
    "par €",
    "par point %",
    "effet binaire"
  )
)

# Convertir les coefficients en pourcentages
variables_results$percentage <- variables_results$coefficient * 100

# Créer une palette de couleurs du rouge foncé au vert foncé
n_colors <- nrow(variables_results)
# Créer une palette gradient basée sur les valeurs de pourcentage
variables_results$couleur <- colorRampPalette(c(
  "#8B0000",
  "#DC143C",
  "#FFA500",
  "#FFFF00",
  "#90EE90",
  "#32CD32",
  "#006400"
))(n_colors)[rank(variables_results$percentage)]

# Créer le graphique des autres variables
pdf("pdf/impact_autre_variables_appartements.pdf", width = 16, height = 10)
ggplot(
  variables_results,
  aes(x = reorder(variable, percentage), y = percentage, fill = couleur)
) +
  geom_col(width = 0.7, alpha = 0.8) +
  geom_text(
    aes(
      label = paste0(
        ifelse(percentage >= 0, "+", ""),
        round(percentage, 3),
        "%\n(",
        unite,
        ")"
      )
    ),
    hjust = ifelse(variables_results$percentage >= 0, -0.1, 1.1),
    vjust = 0.5,
    size = 3,
    fontface = "bold",
    color = "black"
  ) +
  scale_fill_identity() +
  coord_flip() +
  labs(
    title = "Impact des caractéristiques sur le prix de vente des appartements",
    subtitle = "Variation en % du prix par unité de la variable",
    x = NULL,
    y = "Variation du prix (%)",
    caption = "Espaces BLEUS : Littoral (mer, océan)\nEspaces VERTS: forêts, végétation, plages, équipements sportifs)\nEspaces ROUGES : Axes de transport (routes, voies ferrées, aéroports) et nuisances (décharges, extractions, marais)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray60"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.caption = element_text(size = 9, color = "gray60", hjust = 0)
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "solid",
    color = "black",
    linewidth = 0.5
  )
dev.off()
