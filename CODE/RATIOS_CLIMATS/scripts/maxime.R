source("scripts/packages.R")
source("scripts/import.R")

# Regarder la LGV moyenne par niveau PRISM

# Inondations
df %>%
  mutate(LGV = (charge / valeur_fonciere) * 100) %>%
  filter(alea == "Inondation" & LGV < 150) %>%
  select(charge, LGV, prism_inon) %>%
  group_by(prism_inon) %>%
  summarise(
    LGV_médiane = median(LGV),
    LGV_moyenne = mean(LGV),
    LGV_max = max(LGV),
    charge_mediane = median(charge),
    charge_moyenne = mean(charge),
    charge_max = max(charge),
    n = n(),
    .groups = "drop"
  )

# Sécheresses
df %>%
  mutate(LGV = (charge / valeur_fonciere) * 100) %>%
  filter(alea == "Sécheresse" & LGV < 200) %>%
  select(charge, LGV, prism_rga) %>%
  group_by(prism_rga) %>%
  summarise(
    LGV_médiane = median(LGV),
    LGV_moyenne = mean(LGV),
    LGV_max = max(LGV),
    charge_mediane = median(charge),
    charge_moyenne = mean(charge),
    charge_max = max(charge),
    n = n(),
    .groups = "drop"
  )


# Tempêtes
df %>%
  mutate(LGV = (charge / valeur_fonciere) * 100) %>%
  filter(alea == "Tempête" & LGV < 150) %>%
  select(charge, LGV, prism_temp) %>%
  group_by(prism_temp) %>%
  summarise(
    LGV_médiane = median(LGV),
    LGV_moyenne = mean(LGV),
    LGV_max = max(LGV),
    charge_mediane = median(charge),
    charge_moyenne = mean(charge),
    charge_max = max(charge),
    n = n(),
    .groups = "drop"
  )
