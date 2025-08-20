source("scripts/library.R")
df <- st_read("data/DF_FINAL.gpkg")
df_CSV <- read.csv("data/DF_FINAL.csv")

dim(df)
# On retire les NA
sapply(df, function(y) sum(length(which(is.na(y)))))
df[is.na(df)] <- 0
dim(df)

# Format des données
# Créer les variables dummy avec D comme référence
df$dpe <- factor(df$dpe, levels = c("D", "A", "B", "C", "E", "F", "G"))
df$code_commune <- as.numeric(df$code_commune)
df$code_dep <- as.numeric(df$code_dep)
df$annee_constuction <- as.numeric(df$annee_constuction)
df$age_batiment <- 2025 - df$annee_constuction

# Création d'une variable €/m2
df$prix_m2 <- df$prix / df$surface_habitable

appart <- df %>%
  filter(type == "appartement", code_commune == "29019")

appart_csv <- df_CSV %>%
  filter(type == "appartement", code_commune == "29019")

maison <- df %>%
  filter(type == "maison", code_commune == "29019")

maison_csv <- df_CSV %>%
  filter(type == "maison", code_commune == "29019")

data <- df %>%
  filter(code_commune == "29019")

# Exportation pour faire les cartes en QGIS
# st_write(df, "data/brest.gpkg")
# st_write(appart, "data/appart.gpkg")
# st_write(maison, "data/maison.gpkg")

# VENTES ----
# 1. Évolution des ventes appartements vs maisons -------------------------------
ventes_evolution <- data %>%
  mutate(
    mois_annee = floor_date(date, "month"),
    type_clean = ifelse(type == "maison", "Maisons", "Appartements")
  ) %>%
  group_by(mois_annee, type_clean) %>%
  summarise(nb_ventes = n(), .groups = "drop") %>%
  filter(!is.na(mois_annee))

# Graphique évolution des ventes
pdf("pdf/BREST_evolution_ventes_type.pdf", width = 14, height = 8)
ggplot(
  ventes_evolution,
  aes(x = mois_annee, y = nb_ventes, color = type_clean)
) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.7) +

  # Ajouter les lignes verticales
  geom_vline(
    xintercept = as.Date("2020-12-17"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = as.Date("2021-03-31"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = as.Date("2021-07-01"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +

  # Ajouter les annotations avec position relative
  geom_text(
    data = data.frame(
      x = as.Date("2020-12-17"),
      y = Inf,
      label = "Annonce\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = 1.1,
    vjust = 1.2,
    inherit.aes = FALSE
  ) +

  geom_text(
    data = data.frame(
      x = as.Date("2021-03-31"),
      y = Inf,
      label = "Arrêtés\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = -0.1,
    vjust = 1,
    inherit.aes = FALSE
  ) +

  geom_text(
    data = data.frame(
      x = as.Date("2021-07-01"),
      y = Inf,
      label = "Entrée en vigueur\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = -0.1,
    vjust = 2.5,
    inherit.aes = FALSE
  ) +

  scale_color_manual(
    values = c("Appartements" = "#2E86AB", "Maisons" = "chartreuse4")
  ) +
  labs(
    # title = "Évolution des ventes immobilières par type de bien",
    # subtitle = "Nombre de transactions par mois",
    x = "Période",
    y = "Nombre de ventes",
    color = "Type de bien"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray60"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")
dev.off()


# DPE ----
# 2. Évolution des ventes par DPE pour appartements et maisons ----------------
ventes_dpe_evolution <- data %>%
  mutate(
    mois_annee = floor_date(date, "month"),
    type_clean = ifelse(type == "maison", "Maisons", "Appartements")
  ) %>%
  filter(!is.na(dpe), !is.na(mois_annee)) %>%
  group_by(mois_annee, type_clean, dpe) %>%
  summarise(nb_ventes = n(), .groups = "drop")

# STAT_ventes_dpe_evolution <- ventes_dpe_evolution %>%
#   drop("geom")

# Couleurs DPE
dpe_colors <- c(
  "A" = "#1f7a3c",
  "B" = "#4caf50",
  "C" = "#8bc34a",
  "D" = "#ffeb3b",
  "E" = "#ff9800",
  "F" = "#ff5722",
  "G" = "#d32f2f"
)

# Graphique pour les appartements
plot_appart <- ggplot(
  ventes_dpe_evolution %>% filter(type_clean == "Appartements"),
  aes(x = mois_annee, y = nb_ventes, color = dpe)
) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point(size = 1.5, alpha = 0.7) +

  # Ajouter les lignes verticales
  geom_vline(
    xintercept = as.Date("2020-12-17"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = as.Date("2021-03-31"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = as.Date("2021-07-01"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +

  # Ajouter les annotations avec position relative
  geom_text(
    data = data.frame(
      x = as.Date("2020-12-17"),
      y = Inf,
      label = "Annonce\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = 1.1,
    vjust = 1.2,
    inherit.aes = FALSE
  ) +

  geom_text(
    data = data.frame(
      x = as.Date("2021-03-31"),
      y = Inf,
      label = "Arrêtés\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = -0.1,
    vjust = 1,
    inherit.aes = FALSE
  ) +

  geom_text(
    data = data.frame(
      x = as.Date("2021-07-01"),
      y = Inf,
      label = "Entrée en vigueur\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = -0.1,
    vjust = 2.5,
    inherit.aes = FALSE
  ) +

  scale_color_manual(values = dpe_colors) +
  labs(
    title = "Appartements",
    x = NULL,
    y = "Nombre de ventes",
    color = "DPE"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 10),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")

# Graphique pour les maisons
plot_maison <- ggplot(
  ventes_dpe_evolution %>% filter(type_clean == "Maisons"),
  aes(x = mois_annee, y = nb_ventes, color = dpe)
) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point(size = 1.5, alpha = 0.7) +

  # Ajouter les lignes verticales
  geom_vline(
    xintercept = as.Date("2020-12-17"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = as.Date("2021-03-31"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = as.Date("2021-07-01"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +

  # Ajouter les annotations avec position relative
  geom_text(
    data = data.frame(
      x = as.Date("2020-12-17"),
      y = Inf,
      label = "Annonce\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = 1.1,
    vjust = 1.2,
    inherit.aes = FALSE
  ) +

  geom_text(
    data = data.frame(
      x = as.Date("2021-03-31"),
      y = Inf,
      label = "Arrêtés\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = -0.1,
    vjust = 1,
    inherit.aes = FALSE
  ) +

  geom_text(
    data = data.frame(
      x = as.Date("2021-07-01"),
      y = Inf,
      label = "Entrée en vigueur\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = -0.1,
    vjust = 2.5,
    inherit.aes = FALSE
  ) +

  scale_color_manual(values = dpe_colors) +
  labs(
    title = "Maisons",
    x = "Période",
    y = "Nombre de ventes",
    color = "DPE"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 10),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")

# Combiner les deux graphiques
pdf("pdf/BREST_evolution_ventes_dpe_type.pdf", width = 14, height = 8)
grid.arrange(plot_appart, plot_maison, ncol = 1)
dev.off()


# 3. Distribution DPE globale (histogramme en %) ------------------------------
dpe_distribution <- data %>%
  filter(!is.na(dpe)) %>%
  count(dpe) %>%
  mutate(pourcentage = n / sum(n) * 100) %>%
  arrange(match(dpe, c("A", "B", "C", "D", "E", "F", "G")))

pdf("pdf/BREST_distribution_dpe_global.pdf", width = 12, height = 8)
ggplot(
  dpe_distribution,
  aes(
    x = factor(dpe, levels = c("A", "B", "C", "D", "E", "F", "G")),
    y = pourcentage,
    fill = factor(dpe, levels = c("A", "B", "C", "D", "E", "F", "G")) # Ajout du factor avec levels
  )
) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(
    aes(label = paste0(round(pourcentage, 1), "%")),
    vjust = -0.5,
    size = 4,
    fontface = "bold",
    color = "black"
  ) +

  scale_fill_manual(values = dpe_colors) +
  labs(
    # title = "Distribution des classes DPE",
    # subtitle = "Répartition en pourcentage de l'ensemble des biens",
    x = "Classe DPE",
    y = "Pourcentage (%)",
    fill = "DPE"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray60"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, max(dpe_distribution$pourcentage) * 1.1)
  )
dev.off()


# 4. Distribution DPE par type de bien (appartements vs maisons) --------------
dpe_distribution_type <- data %>%
  filter(!is.na(dpe)) %>%
  mutate(type_clean = ifelse(type == "maison", "Maisons", "Appartements")) %>%
  group_by(type_clean, dpe) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(type_clean) %>%
  mutate(pourcentage = n / sum(n) * 100) %>%
  ungroup() %>%
  arrange(type_clean, match(dpe, c("A", "B", "C", "D", "E", "F", "G")))

pdf("pdf/BREST_distribution_dpe_par_type.pdf", width = 14, height = 8)
ggplot(
  dpe_distribution_type,
  aes(
    x = factor(dpe, levels = c("A", "B", "C", "D", "E", "F", "G")),
    y = pourcentage,
    fill = factor(dpe, levels = c("A", "B", "C", "D", "E", "F", "G")) # Ajout du factor avec levels
  )
) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(
    aes(label = paste0(round(pourcentage, 1), "%")),
    vjust = -0.5,
    size = 3.5,
    fontface = "bold",
    color = "black"
  ) +
  scale_fill_manual(values = dpe_colors) +
  facet_wrap(~type_clean, ncol = 2, scales = "free_y") +
  labs(
    # title = "Distribution des classes DPE par type de bien",
    # subtitle = "Répartition en pourcentage par type de logement",
    x = "Classe DPE",
    y = "Pourcentage (%)",
    fill = "DPE"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray60"),
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 11),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)) # 10% d'espace en haut
  )
dev.off()


df <- data %>%
  mutate(
    annee_mois = floor_date(date, "month"),
    annee = year(date),
    mois = month(date)
  )

# Graphique 1: Évolution des prix moyens appartements vs maisons
prix_type_evolution <- df %>%
  group_by(annee_mois, type) %>%
  summarise(
    prix_moyen = mean(prix, na.rm = TRUE),
    prix_moyen_m2 = mean(prix_m2, na.rm = TRUE),
    .groups = "drop"
  )

g1 <- ggplot(
  prix_type_evolution,
  aes(x = annee_mois, y = prix_moyen, color = type)
) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +

  # Ajouter les lignes verticales
  geom_vline(
    xintercept = as.Date("2020-12-17"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = as.Date("2021-03-31"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = as.Date("2021-07-01"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +

  # Ajouter les annotations avec position relative
  geom_text(
    data = data.frame(
      x = as.Date("2020-12-17"),
      y = Inf,
      label = "Annonce\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = 1.1,
    vjust = 1.2,
    inherit.aes = FALSE
  ) +

  geom_text(
    data = data.frame(
      x = as.Date("2021-03-31"),
      y = Inf,
      label = "Arrêtés\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = -0.1,
    vjust = 1,
    inherit.aes = FALSE
  ) +

  geom_text(
    data = data.frame(
      x = as.Date("2021-07-01"),
      y = Inf,
      label = "Entrée en vigueur\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = -0.1,
    vjust = 2.5,
    inherit.aes = FALSE
  ) +
  scale_color_manual(
    values = c("appartement" = "#2E86AB", "maison" = "chartreuse4")
  ) +

  scale_y_continuous(
    labels = scales::dollar_format(prefix = "", suffix = "k€", scale = 1e-3)
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  labs(
    # title = "Évolution des prix moyens : Appartements vs Maisons",
    # subtitle = "Prix moyen par type de bien immobilier",
    x = "Période",
    y = "Prix moyen (€)",
    color = "Type de bien"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

print(g1)

# Graphique 2: Évolution des prix moyens par classe DPE
prix_dpe_evolution <- df %>%
  filter(!is.na(dpe) & dpe != "") %>%
  group_by(annee_mois, dpe) %>%
  summarise(
    prix_moyen = mean(prix, na.rm = TRUE),
    prix_moyen_m2 = mean(prix_m2, na.rm = TRUE),
    .groups = "drop"
  )

# Ordre des classes DPE de A à G
prix_dpe_evolution$dpe <- factor(
  prix_dpe_evolution$dpe,
  levels = c("A", "B", "C", "D", "E", "F", "G")
)

g2 <- ggplot(
  prix_dpe_evolution,
  aes(x = annee_mois, y = prix_moyen, color = dpe)
) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +

  # Ajouter les lignes verticales
  geom_vline(
    xintercept = as.Date("2020-12-17"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = as.Date("2021-03-31"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = as.Date("2021-07-01"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +

  # Ajouter les annotations avec position relative
  geom_text(
    data = data.frame(
      x = as.Date("2020-12-17"),
      y = Inf,
      label = "Annonce\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = 1.1,
    vjust = 1.2,
    inherit.aes = FALSE
  ) +

  geom_text(
    data = data.frame(
      x = as.Date("2021-03-31"),
      y = Inf,
      label = "Arrêtés\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = -0.1,
    vjust = 1,
    inherit.aes = FALSE
  ) +

  geom_text(
    data = data.frame(
      x = as.Date("2021-07-01"),
      y = Inf,
      label = "Entrée en vigueur\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = -0.1,
    vjust = 2.5,
    inherit.aes = FALSE
  ) +

  scale_y_continuous(
    labels = scales::dollar_format(prefix = "", suffix = "k€", scale = 1e-3)
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  scale_color_manual(
    values = c(
      "A" = "#00AA00",
      "B" = "#55BB00",
      "C" = "#AACC00",
      "D" = "#FFDD00",
      "E" = "#FFAA00",
      "F" = "#FF6600",
      "G" = "#FF0000"
    )
  ) +
  labs(
    # title = "Évolution des prix moyens par classe DPE",
    # subtitle = "Prix moyen selon la performance énergétique",
    x = "Période",
    y = "Prix moyen (€)",
    color = "Classe DPE"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

print(g2)

# Graphique 3: Évolution des prix moyens par classe DPE et type de bien
prix_dpe_type_evolution <- df %>%
  filter(!is.na(dpe) & dpe != "") %>%
  group_by(annee_mois, dpe, type) %>%
  summarise(
    prix_moyen = mean(prix, na.rm = TRUE),
    prix_moyen_m2 = mean(prix_m2, na.rm = TRUE),
    .groups = "drop"
  )

prix_dpe_type_evolution$dpe <- factor(
  prix_dpe_type_evolution$dpe,
  levels = c("A", "B", "C", "D", "E", "F", "G")
)

g3 <- ggplot(
  prix_dpe_type_evolution,
  aes(x = annee_mois, y = prix_moyen, color = dpe)
) +
  geom_line(size = 1) +
  geom_point(size = 1.2) +

  # Ajouter les lignes verticales
  geom_vline(
    xintercept = as.Date("2020-12-17"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = as.Date("2021-03-31"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = as.Date("2021-07-01"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +

  # Ajouter les annotations avec position relative
  geom_text(
    data = data.frame(
      x = as.Date("2020-12-17"),
      y = Inf,
      label = "Annonce\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = 1.1,
    vjust = 1.2,
    inherit.aes = FALSE
  ) +

  geom_text(
    data = data.frame(
      x = as.Date("2021-03-31"),
      y = Inf,
      label = "Arrêtés\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = -0.1,
    vjust = 1,
    inherit.aes = FALSE
  ) +

  geom_text(
    data = data.frame(
      x = as.Date("2021-07-01"),
      y = Inf,
      label = "Entrée en vigueur\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = -0.1,
    vjust = 2.5,
    inherit.aes = FALSE
  ) +

  facet_wrap(~type, scales = "free_y") +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "", suffix = "k€", scale = 1e-3)
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  scale_color_manual(
    values = c(
      "A" = "#00AA00",
      "B" = "#55BB00",
      "C" = "#AACC00",
      "D" = "#FFDD00",
      "E" = "#FFAA00",
      "F" = "#FF6600",
      "G" = "#FF0000"
    )
  ) +
  labs(
    # title = "Évolution des prix moyens par classe DPE et type de bien",
    # subtitle = "Comparaison appartements vs maisons selon la performance énergétique",
    x = "Période",
    y = "Prix moyen (€)",
    color = "Classe DPE"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold")
  )

print(g3)


# Graphique 3A: Prix moyens par classe DPE - Appartements
prix_appartements <- prix_dpe_type_evolution %>% filter(type == "appartement")

g3a <- ggplot(
  prix_appartements,
  aes(x = annee_mois, y = prix_moyen, color = dpe)
) +
  geom_line(size = 1) +
  geom_point(size = 2) +

  # Ajouter les lignes verticales
  geom_vline(
    xintercept = as.Date("2020-12-17"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = as.Date("2021-03-31"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = as.Date("2021-07-01"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +

  # Ajouter les annotations avec position relative
  geom_text(
    data = data.frame(
      x = as.Date("2020-12-17"),
      y = Inf,
      label = "Annonce\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = 1.1,
    vjust = 1.2,
    inherit.aes = FALSE
  ) +

  geom_text(
    data = data.frame(
      x = as.Date("2021-03-31"),
      y = Inf,
      label = "Arrêtés\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = -0.1,
    vjust = 1,
    inherit.aes = FALSE
  ) +

  geom_text(
    data = data.frame(
      x = as.Date("2021-07-01"),
      y = Inf,
      label = "Entrée en vigueur\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = -0.1,
    vjust = 2.5,
    inherit.aes = FALSE
  ) +

  scale_y_continuous(
    labels = scales::dollar_format(prefix = "", suffix = "k€", scale = 1e-3)
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  scale_color_manual(
    values = c(
      "A" = "#00AA00",
      "B" = "#55BB00",
      "C" = "#AACC00",
      "D" = "#FFDD00",
      "E" = "#FFAA00",
      "F" = "#FF6600",
      "G" = "#FF0000"
    )
  ) +
  labs(
    # title = "Évolution des prix moyens par classe DPE - Appartements",
    # subtitle = "Prix moyen des appartements selon la performance énergétique",
    x = "Période",
    y = "Prix moyen (€)",
    color = "Classe DPE"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

print(g3a)

# Graphique 3B: Prix moyens par classe DPE - Maisons
prix_maisons <- prix_dpe_type_evolution %>% filter(type == "maison")

g3b <- ggplot(prix_maisons, aes(x = annee_mois, y = prix_moyen, color = dpe)) +
  geom_line(size = 1) +
  geom_point(size = 2) +

  # Ajouter les lignes verticales
  geom_vline(
    xintercept = as.Date("2020-12-17"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = as.Date("2021-03-31"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = as.Date("2021-07-01"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +

  # Ajouter les annotations avec position relative
  geom_text(
    data = data.frame(
      x = as.Date("2020-12-17"),
      y = Inf,
      label = "Annonce\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = 1.1,
    vjust = 1.2,
    inherit.aes = FALSE
  ) +

  geom_text(
    data = data.frame(
      x = as.Date("2021-03-31"),
      y = Inf,
      label = "Arrêtés\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = -0.1,
    vjust = 1,
    inherit.aes = FALSE
  ) +

  geom_text(
    data = data.frame(
      x = as.Date("2021-07-01"),
      y = Inf,
      label = "Entrée en vigueur\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = -0.1,
    vjust = 2.5,
    inherit.aes = FALSE
  ) +

  scale_y_continuous(
    labels = scales::dollar_format(prefix = "", suffix = "k€", scale = 1e-3)
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  scale_color_manual(
    values = c(
      "A" = "#00AA00",
      "B" = "#55BB00",
      "C" = "#AACC00",
      "D" = "#FFDD00",
      "E" = "#FFAA00",
      "F" = "#FF6600",
      "G" = "#FF0000"
    )
  ) +
  labs(
    # title = "Évolution des prix moyens par classe DPE - Maisons",
    # subtitle = "Prix moyen des maisons selon la performance énergétique",
    x = "Période",
    y = "Prix moyen (€)",
    color = "Classe DPE"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

print(g3b)

# Graphique bonus: Évolution des prix au m² par classe DPE et type
g4 <- ggplot(
  prix_dpe_type_evolution,
  aes(x = annee_mois, y = prix_moyen_m2, color = dpe)
) +
  geom_line(size = 1) +
  geom_point(size = 1.2) +

  # Ajouter les lignes verticales
  geom_vline(
    xintercept = as.Date("2020-12-17"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = as.Date("2021-03-31"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = as.Date("2021-07-01"),
    color = "darkgrey",
    linetype = "dashed",
    size = 1,
    alpha = 0.8
  ) +

  # Ajouter les annotations avec position relative
  geom_text(
    data = data.frame(
      x = as.Date("2020-12-17"),
      y = Inf,
      label = "Annonce\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = 1.1,
    vjust = 1.2,
    inherit.aes = FALSE
  ) +

  geom_text(
    data = data.frame(
      x = as.Date("2021-03-31"),
      y = Inf,
      label = "Arrêtés\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = -0.1,
    vjust = 1,
    inherit.aes = FALSE
  ) +

  geom_text(
    data = data.frame(
      x = as.Date("2021-07-01"),
      y = Inf,
      label = "Entrée en vigueur\nLoi DPE 2021"
    ),
    aes(x = x, y = y, label = label),
    color = "darkgrey",
    size = 3,
    fontface = "bold",
    hjust = -0.1,
    vjust = 2.5,
    inherit.aes = FALSE
  ) +

  facet_wrap(~type, scales = "free_y") +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "", suffix = "€/m²", scale = 1)
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  scale_color_manual(
    values = c(
      "A" = "#00AA00",
      "B" = "#55BB00",
      "C" = "#AACC00",
      "D" = "#FFDD00",
      "E" = "#FFAA00",
      "F" = "#FF6600",
      "G" = "#FF0000"
    )
  ) +
  labs(
    # title = "Évolution des prix au m² par classe DPE et type de bien",
    # subtitle = "Prix au m² selon la performance énergétique",
    x = "Période",
    y = "Prix au m² (€)",
    color = "Classe DPE"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold")
  )

print(g4)


# Fonction pour créer un graphique pour un type spécifique
create_dpe_graph <- function(data, type_filter, title_suffix) {
  # Filtrer les données pour le type spécifique
  data_filtered <- data %>% filter(type == type_filter)

  ggplot(data_filtered, aes(x = annee_mois, y = prix_moyen_m2, color = dpe)) +
    geom_line(size = 1) +
    geom_point(size = 1.2) +

    # Ajouter les lignes verticales
    geom_vline(
      xintercept = as.Date("2020-12-17"),
      color = "darkgrey",
      linetype = "dashed",
      size = 1,
      alpha = 0.8
    ) +
    geom_vline(
      xintercept = as.Date("2021-03-31"),
      color = "darkgrey",
      linetype = "dashed",
      size = 1,
      alpha = 0.8
    ) +
    geom_vline(
      xintercept = as.Date("2021-07-01"),
      color = "darkgrey",
      linetype = "dashed",
      size = 1,
      alpha = 0.8
    ) +

    # Ajouter les annotations avec position relative
    geom_text(
      data = data.frame(
        x = as.Date("2020-12-17"),
        y = Inf,
        label = "Annonce\nLoi DPE 2021"
      ),
      aes(x = x, y = y, label = label),
      color = "darkgrey",
      size = 3,
      fontface = "bold",
      hjust = 1.1,
      vjust = 1.2,
      inherit.aes = FALSE
    ) +

    geom_text(
      data = data.frame(
        x = as.Date("2021-03-31"),
        y = Inf,
        label = "Arrêtés\nLoi DPE 2021"
      ),
      aes(x = x, y = y, label = label),
      color = "darkgrey",
      size = 3,
      fontface = "bold",
      hjust = -0.1,
      vjust = 1,
      inherit.aes = FALSE
    ) +

    geom_text(
      data = data.frame(
        x = as.Date("2021-07-01"),
        y = Inf,
        label = "Entrée en vigueur\nLoi DPE 2021"
      ),
      aes(x = x, y = y, label = label),
      color = "darkgrey",
      size = 3,
      fontface = "bold",
      hjust = -0.1,
      vjust = 2.5,
      inherit.aes = FALSE
    ) +

    scale_y_continuous(
      labels = scales::dollar_format(prefix = "", suffix = "€/m²", scale = 1)
    ) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
    scale_color_manual(
      values = c(
        "A" = "#00AA00",
        "B" = "#55BB00",
        "C" = "#AACC00",
        "D" = "#FFDD00",
        "E" = "#FFAA00",
        "F" = "#FF6600",
        "G" = "#FF0000"
      )
    ) +
    labs(
      # title = paste("Évolution des prix au m² par classe DPE -", title_suffix),
      # subtitle = "Prix au m² selon la performance énergétique",
      x = "Période",
      y = "Prix au m² (€)",
      color = "Classe DPE"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
}

# Créer les deux graphiques séparés
# Supposons que vos types sont "Appartement" et "Maison" - ajustez selon vos données
types_uniques <- unique(prix_dpe_type_evolution$type)

# Graphique 1 : Premier type
g4_type1 <- create_dpe_graph(
  prix_dpe_type_evolution,
  types_uniques[1],
  types_uniques[1]
)
print(g4_type1)

# Graphique 2 : Deuxième type
g4_type2 <- create_dpe_graph(
  prix_dpe_type_evolution,
  types_uniques[2],
  types_uniques[2]
)
print(g4_type2)


# Sauvegarde des graphiques (optionnel)
ggsave(
  "pdf/BREST_evolution_prix_type.pdf",
  g1,
  width = 14,
  height = 8,
  dpi = 300
)
ggsave(
  "pdf/BREST_evolution_prix_dpe.pdf",
  g2,
  width = 14,
  height = 8,
  dpi = 300
)
# ggsave("pdf/evolution_prix_dpe_type.pdf", g3, width = 14, height = 10, dpi = 300)
ggsave(
  "pdf/BREST_evolution_prix_dpe_type_appartements.pdf",
  g3a,
  width = 14,
  height = 8,
  dpi = 300
)
ggsave(
  "pdf/BREST_evolution_prix_dpe_type_maisons.pdf",
  g3b,
  width = 14,
  height = 8,
  dpi = 300
)
ggsave(
  "pdf/BREST_evolution_prix_m2_dpe_type.pdf",
  g4,
  width = 14,
  height = 8,
  dpi = 300
)
ggsave(
  "pdf/BREST_evolution_prix_m2_dpe_type_maisons.pdf",
  g4_type1,
  width = 14,
  height = 8,
  dpi = 300
)
ggsave(
  "pdf/BREST_evolution_prix_m2_dpe_type_appartements.pdf",
  g4_type2,
  width = 14,
  height = 8,
  dpi = 300
)


# BOXPLOTS ET HISTOGRAMMES ------
# PRIX
## APPARTEMENTS
g1 <- appart %>%
  ggplot(aes(x = prix)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.alpha = 0.2,
    fill = "red",
    alpha = 0.2
  ) +
  theme_minimal() +
  coord_flip()

g2 <- appart %>%
  ggplot(aes(x = prix)) +
  geom_density(adjust = 1, alpha = 0.2, fill = "red") +
  theme_minimal()

combined_plot <- g1 + g2
ggsave(
  "pdf/BREST_prix_distributions_apparts.pdf",
  combined_plot,
  width = 12,
  height = 6,
  units = "in"
)

## MAISONS
g1 <- maison %>%
  ggplot(aes(x = prix)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.alpha = 0.2,
    fill = "blue",
    alpha = 0.2
  ) +
  theme_minimal() +
  coord_flip()

g2 <- maison %>%
  ggplot(aes(x = prix)) +
  geom_density(adjust = 1, alpha = 0.2, fill = "blue") +
  theme_minimal()

combined_plot <- g1 + g2
ggsave(
  "pdf/BREST_prix_distributions_maisons.pdf",
  combined_plot,
  width = 12,
  height = 6,
  units = "in"
)


# SURFACE HABITABLE
## APPARTEMENTS
g1 <- appart %>%
  ggplot(aes(x = surface_habitable)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.alpha = 0.2,
    fill = "red",
    alpha = 0.2
  ) +
  theme_minimal() +
  coord_flip()

g2 <- appart %>%
  ggplot(aes(x = surface_habitable)) +
  geom_density(adjust = 1, alpha = 0.2, fill = "red") +
  theme_minimal()

combined_plot <- g1 + g2
ggsave(
  "pdf/BREST_surface_habitable_distributions_apparts.pdf",
  combined_plot,
  width = 12,
  height = 6,
  units = "in"
)

## MAISONS
g1 <- maison %>%
  ggplot(aes(x = surface_habitable)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.alpha = 0.2,
    fill = "blue",
    alpha = 0.2
  ) +
  theme_minimal() +
  coord_flip()

g2 <- maison %>%
  ggplot(aes(x = surface_habitable)) +
  geom_density(adjust = 1, alpha = 0.2, fill = "blue") +
  theme_minimal()

combined_plot <- g1 + g2
ggsave(
  "pdf/BREST_surface_habitable_distributions_maisons.pdf",
  combined_plot,
  width = 12,
  height = 6,
  units = "in"
)


# SURFACE TERRAIN
## APPARTEMENTS
g1 <- appart %>%
  ggplot(aes(x = surface_terrain)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.alpha = 0.2,
    fill = "red",
    alpha = 0.2
  ) +
  theme_minimal() +
  coord_flip()

g2 <- appart %>%
  ggplot(aes(x = surface_terrain)) +
  geom_density(adjust = 1, alpha = 0.2, fill = "red") +
  theme_minimal()

combined_plot <- g1 + g2
ggsave(
  "pdf/BREST_surface_terrain_distributions_apparts.pdf",
  combined_plot,
  width = 12,
  height = 6,
  units = "in"
)

## MAISONS
g1 <- maison %>%
  ggplot(aes(x = surface_terrain)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.alpha = 0.2,
    fill = "blue",
    alpha = 0.2
  ) +
  theme_minimal() +
  coord_flip()

g2 <- maison %>%
  ggplot(aes(x = surface_terrain)) +
  geom_density(adjust = 1, alpha = 0.2, fill = "blue") +
  theme_minimal()

combined_plot <- g1 + g2
ggsave(
  "pdf/BREST_surface_terrain_distributions_maisons.pdf",
  combined_plot,
  width = 12,
  height = 6,
  units = "in"
)


# NB ETAGES
## APPARTEMENTS
g1 <- appart %>%
  ggplot(aes(x = nb_etages)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.alpha = 0.2,
    fill = "red",
    alpha = 0.2
  ) +
  theme_minimal() +
  coord_flip()

g2 <- appart %>%
  ggplot(aes(x = nb_etages)) +
  geom_histogram(bins = 20, alpha = 0.2, fill = "red", color = "black") +
  theme_minimal()

combined_plot <- g1 + g2
ggsave(
  "pdf/BREST_nb_etages_distributions_apparts.pdf",
  combined_plot,
  width = 12,
  height = 6,
  units = "in"
)

## MAISONS
g1 <- maison %>%
  ggplot(aes(x = nb_etages)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.alpha = 0.2,
    fill = "blue",
    alpha = 0.2
  ) +
  theme_minimal() +
  coord_flip()

g2 <- maison %>%
  ggplot(aes(x = nb_etages)) +
  geom_histogram(bins = 20, alpha = 0.2, fill = "blue", color = "black") +
  theme_minimal()

combined_plot <- g1 + g2
ggsave(
  "pdf/BREST_nb_etages_distributions_maisons.pdf",
  combined_plot,
  width = 12,
  height = 6,
  units = "in"
)


# NB PIECES
## APPARTEMENTS
g1 <- appart %>%
  ggplot(aes(x = nb_etages)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.alpha = 0.2,
    fill = "red",
    alpha = 0.2
  ) +
  theme_minimal() +
  coord_flip()

g2 <- appart %>%
  ggplot(aes(x = nb_etages)) +
  geom_histogram(bins = 20, alpha = 0.2, fill = "red", color = "black") +
  theme_minimal()

combined_plot <- g1 + g2
ggsave(
  "pdf/BREST_nb_etages_distributions_apparts.pdf",
  combined_plot,
  width = 12,
  height = 6,
  units = "in"
)

## MAISONS
g1 <- maison %>%
  ggplot(aes(x = nb_etages)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.alpha = 0.2,
    fill = "blue",
    alpha = 0.2
  ) +
  theme_minimal() +
  coord_flip()

g2 <- maison %>%
  ggplot(aes(x = nb_etages)) +
  geom_histogram(bins = 20, alpha = 0.2, fill = "blue", color = "black") +
  theme_minimal()

combined_plot <- g1 + g2
ggsave(
  "pdf/BREST_nb_etages_distributions_maisons.pdf",
  combined_plot,
  width = 12,
  height = 6,
  units = "in"
)


# AGE BATIMENTS
## APPARTEMENTS
g1 <- appart %>%
  ggplot(aes(x = age_batiment)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.alpha = 0.2,
    fill = "red",
    alpha = 0.2
  ) +
  theme_minimal() +
  coord_flip()

g2 <- appart %>%
  ggplot(aes(x = age_batiment)) +
  geom_histogram(bins = 20, alpha = 0.2, fill = "red", color = "black") +
  theme_minimal()

combined_plot <- g1 + g2
ggsave(
  "pdf/BREST_age_batiment_distributions_apparts.pdf",
  combined_plot,
  width = 12,
  height = 6,
  units = "in"
)

## MAISONS
g1 <- maison %>%
  ggplot(aes(x = age_batiment)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.alpha = 0.2,
    fill = "blue",
    alpha = 0.2
  ) +
  theme_minimal() +
  coord_flip()

g2 <- maison %>%
  ggplot(aes(x = age_batiment)) +
  geom_histogram(bins = 20, alpha = 0.2, fill = "blue", color = "black") +
  theme_minimal()

combined_plot <- g1 + g2
ggsave(
  "pdf/BREST_age_batiment_distributions_maisons.pdf",
  combined_plot,
  width = 12,
  height = 6,
  units = "in"
)


# NOMBRE D'ETAGES --------
## APPARTEMENTS
g1 <- appart %>%
  ggplot(aes(x = nb_etages)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.alpha = 0.2,
    fill = "red",
    alpha = 0.2
  ) +
  theme_minimal() +
  coord_flip()

g2 <- appart %>%
  ggplot(aes(x = nb_etages)) +
  geom_histogram(bins = 20, alpha = 0.2, fill = "red", color = "black") +
  theme_minimal()

combined_plot <- g1 + g2
ggsave(
  "pdf/BREST_nb_etages_distributions_apparts.pdf",
  combined_plot,
  width = 12,
  height = 6,
  units = "in"
)

## MAISONS
g1 <- maison %>%
  ggplot(aes(x = nb_etages)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.alpha = 0.2,
    fill = "blue",
    alpha = 0.2
  ) +
  theme_minimal() +
  coord_flip()

g2 <- maison %>%
  ggplot(aes(x = nb_etages)) +
  geom_histogram(bins = 20, alpha = 0.2, fill = "blue", color = "black") +
  theme_minimal()

combined_plot <- g1 + g2
ggsave(
  "pdf/BREST_nb_etages_distributions_maisons.pdf",
  combined_plot,
  width = 12,
  height = 6,
  units = "in"
)

# NOMBRE DE PIECES --------
## APPARTEMENTS
g1 <- appart %>%
  ggplot(aes(x = nb_pieces)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.alpha = 0.2,
    fill = "red",
    alpha = 0.2
  ) +
  theme_minimal() +
  coord_flip()

g2 <- appart %>%
  ggplot(aes(x = nb_pieces)) +
  geom_histogram(bins = 20, alpha = 0.2, fill = "red", color = "black") +
  theme_minimal()

combined_plot <- g1 + g2
ggsave(
  "pdf/BREST_nb_pieces_distributions_apparts.pdf",
  combined_plot,
  width = 12,
  height = 6,
  units = "in"
)

## MAISONS
g1 <- maison %>%
  ggplot(aes(x = nb_pieces)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.alpha = 0.2,
    fill = "blue",
    alpha = 0.2
  ) +
  theme_minimal() +
  coord_flip()

g2 <- maison %>%
  ggplot(aes(x = nb_pieces)) +
  geom_histogram(bins = 20, alpha = 0.2, fill = "blue", color = "black") +
  theme_minimal()

combined_plot <- g1 + g2
ggsave(
  "pdf/BREST_nb_pieces_distributions_maisons.pdf",
  combined_plot,
  width = 12,
  height = 6,
  units = "in"
)

# DISTANCE VERT KM --------
## APPARTEMENTS
g1 <- appart %>%
  ggplot(aes(x = DIST_VERT_KM)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.alpha = 0.2,
    fill = "red",
    alpha = 0.2
  ) +
  theme_minimal() +
  coord_flip()

g2 <- appart %>%
  ggplot(aes(x = DIST_VERT_KM)) +
  geom_histogram(bins = 20, alpha = 0.2, fill = "red", color = "black") +
  theme_minimal()

combined_plot <- g1 + g2
ggsave(
  "pdf/BREST_DIST_VERT_KM_distributions_apparts.pdf",
  combined_plot,
  width = 12,
  height = 6,
  units = "in"
)

## MAISONS
g1 <- maison %>%
  ggplot(aes(x = DIST_VERT_KM)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.alpha = 0.2,
    fill = "blue",
    alpha = 0.2
  ) +
  theme_minimal() +
  coord_flip()

g2 <- maison %>%
  ggplot(aes(x = DIST_VERT_KM)) +
  geom_histogram(bins = 20, alpha = 0.2, fill = "blue", color = "black") +
  theme_minimal()

combined_plot <- g1 + g2
ggsave(
  "pdf/BREST_DIST_VERT_KM_distributions_maisons.pdf",
  combined_plot,
  width = 12,
  height = 6,
  units = "in"
)

# DISTANCE BLEU KM --------
## APPARTEMENTS
g1 <- appart %>%
  ggplot(aes(x = DIST_BLEU_KM)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.alpha = 0.2,
    fill = "red",
    alpha = 0.2
  ) +
  theme_minimal() +
  coord_flip()

g2 <- appart %>%
  ggplot(aes(x = DIST_BLEU_KM)) +
  geom_histogram(bins = 20, alpha = 0.2, fill = "red", color = "black") +
  theme_minimal()

combined_plot <- g1 + g2
ggsave(
  "pdf/BREST_DIST_BLEU_KM_distributions_apparts.pdf",
  combined_plot,
  width = 12,
  height = 6,
  units = "in"
)

## MAISONS
g1 <- maison %>%
  ggplot(aes(x = DIST_BLEU_KM)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.alpha = 0.2,
    fill = "blue",
    alpha = 0.2
  ) +
  theme_minimal() +
  coord_flip()

g2 <- maison %>%
  ggplot(aes(x = DIST_BLEU_KM)) +
  geom_histogram(bins = 20, alpha = 0.2, fill = "blue", color = "black") +
  theme_minimal()

combined_plot <- g1 + g2
ggsave(
  "pdf/BREST_DIST_BLEU_KM_distributions_maisons.pdf",
  combined_plot,
  width = 12,
  height = 6,
  units = "in"
)

# DISTANCE ROUGE KM --------
## APPARTEMENTS
g1 <- appart %>%
  ggplot(aes(x = DIST_ROUGE_KM)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.alpha = 0.2,
    fill = "red",
    alpha = 0.2
  ) +
  theme_minimal() +
  coord_flip()

g2 <- appart %>%
  ggplot(aes(x = DIST_ROUGE_KM)) +
  geom_histogram(bins = 20, alpha = 0.2, fill = "red", color = "black") +
  theme_minimal()

combined_plot <- g1 + g2
ggsave(
  "pdf/BREST_DIST_ROUGE_KM_distributions_apparts.pdf",
  combined_plot,
  width = 12,
  height = 6,
  units = "in"
)

## MAISONS
g1 <- maison %>%
  ggplot(aes(x = DIST_ROUGE_KM)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.alpha = 0.2,
    fill = "blue",
    alpha = 0.2
  ) +
  theme_minimal() +
  coord_flip()

g2 <- maison %>%
  ggplot(aes(x = DIST_ROUGE_KM)) +
  geom_histogram(bins = 20, alpha = 0.2, fill = "blue", color = "black") +
  theme_minimal()

combined_plot <- g1 + g2
ggsave(
  "pdf/BREST_DIST_ROUGE_KM_distributions_maisons.pdf",
  combined_plot,
  width = 12,
  height = 6,
  units = "in"
)

# MATRICE DES CORRELATIONS ----
## MAISONS ----
vars <- c(
  "prix",
  "age_batiment",
  "DIST_ROUGE_KM",
  "DIST_BLEU_KM",
  "DIST_VERT_KM",
  "nb_etages",
  "nb_pieces",
  "surface_terrain",
  "surface_habitable"
)

# On drop les geometry
maison_numeric <- st_drop_geometry(maison)
# Calcul de la matrice des corrélations
correlation_matrix <- cor(maison_numeric[vars])

# Affichage de la matrice des corrélations

corrplot(
  correlation_matrix,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 45
)

pdf(
  "pdf/correlation_matrix_maisons.pdf",
  width = 8,
  height = 5,
  family = "Times"
)
corrplot(
  correlation_matrix,
  method = "color", # Méthode de visualisation
  type = "upper", # Afficher uniquement la partie supérieure
  tl.col = "black", # Couleur du texte
  tl.srt = 45, # Rotation du texte

  # Amélioration des couleurs
  col = rev(COL2("RdYlBu", 200)), # Palette de couleurs Rouge-Bleu plus douce

  # Amélioration du texte
  tl.cex = 0.7, # Taille du texte plus petite

  # Ajout d'un titre
  # title = "Matrice de corrélation",

  # Amélioration des cercles/carrés
  addCoef.col = "black", # Ajouter les coefficients
  number.cex = 1, # Taille des coefficients

  # Autres personnalisations
  diag = FALSE, # Ne pas afficher la diagonale
  addgrid.col = "gray90" # Couleur de la grille plus légère
)
dev.off()


## APPARTS ----
# On drop les geometry
appart_numeric <- st_drop_geometry(appart)
# Calcul de la matrice des corrélations
correlation_matrix <- cor(appart_numeric[vars])

# Affichage de la matrice des corrélations

corrplot(
  correlation_matrix,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 45
)

pdf(
  "pdf/correlation_matrix_apparts.pdf",
  width = 8,
  height = 5,
  family = "Times"
)
corrplot(
  correlation_matrix,
  method = "color", # Méthode de visualisation
  type = "upper", # Afficher uniquement la partie supérieure
  tl.col = "black", # Couleur du texte
  tl.srt = 45, # Rotation du texte

  # Amélioration des couleurs
  col = rev(COL2("RdYlBu", 200)), # Palette de couleurs Rouge-Bleu plus douce

  # Amélioration du texte
  tl.cex = 0.7, # Taille du texte plus petite

  # Ajout d'un titre
  # title = "Matrice de corrélation",

  # Amélioration des cercles/carrés
  addCoef.col = "black", # Ajouter les coefficients
  number.cex = 1, # Taille des coefficients

  # Autres personnalisations
  diag = FALSE, # Ne pas afficher la diagonale
  addgrid.col = "gray90" # Couleur de la grille plus légère
)
dev.off()

# STATISTIQUES DESCRIPTIVES
prop.table(summary(appart$dpe)) * 100
prop.table(summary(maison$dpe)) * 100

summary(appart$prix)
summary(maison$prix)

summary(appart$surface_habitable)
summary(maison$surface_habitable)

summary(appart$surface_terrain)
summary(maison$surface_terrain)

summary(appart$nb_etages)
summary(maison$nb_etages)

summary(appart$nb_pieces)
summary(maison$nb_pieces)

summary(appart$age_batiment)
summary(maison$age_batiment)

summary(appart$DIST_VERT_KM)
summary(maison$DIST_VERT_KM)

summary(appart$DIST_BLEU_KM)
summary(maison$DIST_BLEU_KM)

summary(appart$DIST_ROUGE_KM)
summary(maison$DIST_ROUGE_KM)


summary(appart$age_batiment)
summary(maison$age_batiment)
