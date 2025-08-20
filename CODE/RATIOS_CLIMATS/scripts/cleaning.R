source("scripts/packages.R")
# Lecture des fichiers NZBA

# Pour avoir l'addresse et le type de logement
suptrac <- read.csv2(
  "data/Risques physiques/données input NZBA/suptrac9534_X2SAP962_20250709-cmb.csv"
)
# Pour avoir la valeur du bien
tbl <- read.csv2(
  "data/Risques physiques/données traitement NZBA/tblValeurBien2025-06-30.csv"
)

suptra_v2 <- suptrac %>%
  select(Num_contrat_sous, Adresse_emprunteur, Type_logement, Date_acceptation)

tbl_v2 <- tbl %>% select(Num_contrat_sous, valeurBienRetenue)

df <- merge(suptra_v2, tbl_v2, by = "Num_contrat_sous", all = TRUE)
dim(df) # 856 014 x 4

# Nettoyage complet des données
df_v2 <- df %>%
  mutate(across(
    where(is.character),
    ~ ifelse(trimws(.) %in% c("", "-"), NA, trimws(.))
  ))

df_v3 <- df_v2 %>%
  select(
    Adresse_emprunteur,
    valeurBienRetenue,
    Type_logement,
    Date_acceptation
  ) %>%
  mutate(
    Date_acceptation = as.Date(
      as.character(df_v2$Date_acceptation),
      format = "%Y%m%d"
    )
  ) %>%
  drop_na() %>%
  distinct()

dim(df_v3) # 246 984 x 4 : On perd 20 000 données si on prend le type de logement

# Import des adresses traitées par le géocodeur
lot1 <- read.csv2("data/adresse_a_traiter_lot1.geocoded.csv")
lot2 <- read.csv2("data/adresse_a_traiter_lot2.geocoded.csv")
lot3 <- read.csv2("data/adresse_a_traiter_lot3.geocoded.csv")

# Fusion des fichiers
lots <- rbind(lot1, lot2, lot3)
table(lots$result_status) # ok : 265 304

lots_clean <- lots %>%
  select(x, result_label, result_status, result_postcode) %>%
  filter(result_status == "ok") %>%
  select(-result_status) %>%
  rename(
    Adresse_emprunteur = x
  )

# Fusion entre les adresses CMA et géocodée
dim(df_v3) # 246 894 x 2
dim(lots_clean) # 265 304 x 3

df_v3$Adresse_emprunteur <- trimws(gsub("\\s+", " ", df_v3$Adresse_emprunteur))
df_c <- merge(df_v3, lots_clean, by = "Adresse_emprunteur", all.x = TRUE)
dim(df_c) # 461 983 x 5

df <- df_c %>%
  select(
    result_label,
    valeurBienRetenue,
    result_postcode,
    Type_logement,
    Date_acceptation
  ) %>%
  drop_na() %>%
  distinct()

dim(df) # 246 397 x 5

# Chargement de la base Suravenir Assurances sur les sinistres
sinistre <- read.csv2("data/base_sinistres_96_25.csv")
dim(sinistre) # 128 248 x 13

# Filtre sur les adresses non trouvées
sinistre2 <- sinistre %>%
  select(adresse_ban, charge, date_surv, filiere) %>%
  mutate(
    date_surv = as.Date(as.character(date_surv), format = "%Y%m%d")
  ) %>%
  filter(adresse_ban != "_Non_Trouvée_" & charge > 0)

dim(sinistre2) # 71 009 x 4

length(intersect(df$result_label, sinistre2$adresse_ban)) # 12 131

# Import de l'indice immobilier
indice <- read_excel("data/FR_PX_IMMO_reuters.xlsx", skip = 1)
indice_v2 <- indice %>%
  select("...4", "Indices des prix des logements anciens (INSEE)") %>%
  rename(
    indice = "Indices des prix des logements anciens (INSEE)",
    indice_date = "...4"
  ) %>%
  drop_na()


# Fusion entre la base Suravenir Assurances et la base propre
fusion <- df %>%
  inner_join(sinistre2, by = c("result_label" = "adresse_ban")) %>%
  distinct()

dim(fusion) # 21 741 x 8

fusion2 <- fusion %>%
  rename(
    adresse = result_label,
    valeur = valeurBienRetenue,
    alea = filiere,
    code_postal = result_postcode
  )

#' Correction
# Fonction pour trouver l'indice le plus proche
get_indice <- function(date_cible, df_indice) {
  date_cible <- as.Date(date_cible)
  df_indice$indice_date <- as.Date(df_indice$indice_date)

  indices_ant <- df_indice[df_indice$indice_date <= date_cible, ]

  if (nrow(indices_ant) == 0) {
    return(df_indice$indice[1])
  } else {
    return(indices_ant[which.max(indices_ant$indice_date), ]$indice)
  }
}

# Correction des valeurs
fusion_corrigee <- fusion2 %>%
  rowwise() %>%
  mutate(
    indice_acceptation = get_indice(Date_acceptation, indice_v2),
    indice_survenance = get_indice(date_surv, indice_v2),
    facteur_correction = indice_survenance / indice_acceptation,
    valeur_corrigee = valeur * facteur_correction
  ) %>%
  ungroup()

# Résultat
head(fusion_corrigee[c(
  "Date_acceptation",
  "date_surv",
  "valeur",
  "valeur_corrigee"
)])

fusion_final <- fusion_corrigee %>%
  select(
    -c(
      Date_acceptation,
      indice_acceptation,
      indice_survenance,
      facteur_correction,
      valeur
    )
  ) %>%
  rename(
    valeur = valeur_corrigee
  )
str(fusion_final)

write.csv2(fusion2, "data/fusion_CMA_SURAVENIR.csv")
