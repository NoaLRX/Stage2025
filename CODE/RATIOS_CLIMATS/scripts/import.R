# Import de la fusion CMA-SURAVENIR
df <- read.csv2("data/fusion_CMA_SURAVENIR.csv")

df2 <- df %>%
  mutate(
    valeur = as.numeric(valeur),
    charge = as.numeric(charge),
    date_surv = as.Date(date_surv, format = "%Y%m%d")
  ) %>%
  mutate(
    valeur_fonciere = as.numeric(valeur),
    date_surv = as.Date(as.character(df$date_surv), format = "%Y%m%d"),
  ) %>%
  select(-X) %>%
  rename(
    type_logement = Type_logement
  ) %>%
  mutate(
    type_logement = recode(type_logement, `1` = "maison", `2` = "appartement")
  )


df <- df2
str(df)

# Import des données PRISM
prism <- read_excel(
  "data/PRISM_REF.xlsx",
  sheet = "Referentiel_FR_Code_postal",
  skip = 1
)

prism2 <- prism %>%
  select(
    "Code_postal_officiel",
    "Risque physique Aigu\nInondation",
    "Risque physique Aigu\nSécheresse RGA",
    "Risque physique Aigu\nTempête-Grêle-\nNeige"
  ) %>%
  mutate(
    code_postal = `Code_postal_officiel`,
    prism_inon = `Risque physique Aigu\nInondation`,
    prism_rga = `Risque physique Aigu\nSécheresse RGA`,
    prism_temp = `Risque physique Aigu\nTempête-Grêle-\nNeige`
  ) %>%
  drop_na()

prism <- prism2 %>%
  select(code_postal, prism_inon, prism_rga, prism_temp) %>%
  mutate(
    code_postal = as.integer(prism2$code_postal)
  ) %>%
  distinct


# Fusion entre FUSION et PRISM
df <- merge(df, prism, by = "code_postal", all.x = TRUE)
dim(df) # 24172 x 9


# Diviser en trois dataset distincts
inon <- df %>%
  filter(alea == "Inondation") %>%
  select(-alea)

sech <- df %>%
  filter(alea == "Sécheresse") %>%
  select(-alea)

temp <- df %>%
  filter(alea == "Tempête") %>%
  select(-alea)
