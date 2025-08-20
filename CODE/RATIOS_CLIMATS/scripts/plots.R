# Représentation graphique de la charge globale par aléa
## Version GLOBALE
options(scipen = 999)
lgv %>%
  group_by(date_surv, alea) %>% # Grouper par date ET alea
  summarise(charge_j = sum(charge), .groups = 'drop') %>%
  ggplot(aes(x = date_surv, y = charge_j, color = alea)) +
  geom_line(alpha = 0.7, size = 1) +
  theme_minimal() +
  labs(
    title = "Evolution de la charge des sinistres",
    subtitle = "Base Suravenir Assurance",
    x = "Date",
    y = "Charge"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )


## Version MAISONS
lgv %>%
  filter(type_habitation == "Maison") %>%
  group_by(date_surv, alea) %>% # Grouper par date ET alea
  summarise(charge_j = sum(charge), .groups = 'drop') %>%
  ggplot(aes(x = date_surv, y = charge_j, color = alea)) +
  geom_line(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Evolution de la charge des sinistres des maisons",
    subtitle = "Base Suravenir Assurance",
    x = "Date",
    y = "Charge"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

## Version APPARTEMENTS
lgv %>%
  filter(type_habitation == "Appartement") %>%
  group_by(date_surv, alea) %>% # Grouper par date ET alea
  summarise(charge_j = sum(charge), .groups = 'drop') %>%
  ggplot(aes(x = date_surv, y = charge_j, color = alea)) +
  geom_line(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Evolution de la charge des sinistres des appartements",
    subtitle = "Base Suravenir Assurance",
    x = "Date",
    y = "Charge"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )


# PLOTS GLOBAUX
lgv_inon_y$ALEA <- "Inondation"
lgv_sech_y$ALEA <- "Sécheresse"
lgv_temp_y$ALEA <- "Tempête"

# Combiner les jeux de données
lgv_globale <- bind_rows(lgv_inon_y, lgv_sech_y, lgv_temp_y)

# Graphique des moyennes globales LGV_Y par aléa
ggplot(lgv_globale, aes(x = YEAR, y = LGV_Y, color = ALEA, group = ALEA)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Ratio de perte par aléa climatique",
    x = "Année",
    y = "Ratio Sinistre / Valeur du bien (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )


# PLOTS MAISONS
# Ajouter la colonne "ALEA"
lgv_inon_y_m$ALEA <- "Inondation"
lgv_sech_y_m$ALEA <- "Sécheresse"
lgv_temp_y_m$ALEA <- "Tempête"

# Combiner
lgv_maisons <- bind_rows(lgv_inon_y_m, lgv_sech_y_m, lgv_temp_y_m)

# Graphique
ggplot(lgv_maisons, aes(x = YEAR, y = LGV_Y, color = ALEA, group = ALEA)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Ratio de perte par aléa climatique - Maisons",
    x = "Année",
    y = "Ratio Sinistre / Valeur du bien (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )


# PLOTS APPARTS
# Ajouter la colonne "ALEA"
lgv_inon_y_a$ALEA <- "Inondation"
lgv_sech_y_a$ALEA <- "Sécheresse"
lgv_temp_y_a$ALEA <- "Tempête"

# Combiner
lgv_apparts <- bind_rows(lgv_inon_y_a, lgv_sech_y_a, lgv_temp_y_a)

# Graphique
ggplot(lgv_apparts, aes(x = YEAR, y = LGV_Y, color = ALEA, group = ALEA)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Ratio de perte par aléa climatique - Appartements",
    x = "Année",
    y = "Ratio Sinistre / Valeur du bien (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
