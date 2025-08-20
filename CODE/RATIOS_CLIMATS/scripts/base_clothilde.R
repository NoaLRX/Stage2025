source("scripts/packages.R")

# Import du fichier d'extraction
base <- read_excel(
  "data/extraction _PD_DPE_20241231_vdef1.xlsx",
  sheet = "extraction GUIMOADR-114",
  skip = 2
)
head(base)
dim(base)

# On va récupérer le niveau PRISM au code postal
# Import des données PRISM
prism <- read_excel(
  "data/PRISM_REF.xlsx",
  sheet = "Referentiel_FR_Code_postal",
  skip = 1
)

prism2 <- prism %>%
  select(
    "Code_postal_officiel",
    "Risque physique Chronique",
    "Risque physique Aigu"
  ) %>%
  mutate(
    code_postal = `Code_postal_officiel`,
    prism_chronique = `Risque physique Chronique`,
    prism_aigu = `Risque physique Aigu`,
    prism_total = floor((prism_chronique + prism_aigu) / 2)
  ) %>%
  select(
    -c(
      "Code_postal_officiel",
      "Risque physique Chronique",
      "Risque physique Aigu"
    )
  )
nrow(prism2) # 40 205

# Aggrégation au code postal
prism3 <- prism2 %>%
  group_by(code_postal) %>%
  summarise(
    prism_chronique = mean(prism_chronique),
    prism_aigu = mean(prism_aigu),
    prism_total = mean(prism_total)
  )
nrow(prism3) # 6333

# Fusion entre la base et le niveau PRISM
df <- merge(
  base,
  prism3,
  by.x = "final_cd_post",
  by.y = "code_postal",
  all.x = TRUE
)
#' On a beaucoup de NA étant donné que tout les codes postaux de la base ne sont
#' pas disponibles dans PRISM

sum(is.na(df)) # 1 049 272
colSums(is.na(df))
round(colMeans(is.na(df)) * 100, 1)

# Obtention de la valeur du bien et le type de bien
# Pour avoir l'addresse et le type de logement
suptrac <- read.csv2(
  "data/Risques physiques/données input NZBA/suptrac9534_X2SAP962_20250709-cmb.csv"
)
# Pour avoir la valeur du bien
tbl <- read.csv2(
  "data/Risques physiques/données traitement NZBA/tblValeurBien2025-06-30.csv"
)

suptra_v2 <- suptrac %>%
  select(Num_contrat_sous, Type_logement)

tbl_v2 <- tbl %>% select(Num_contrat_sous, valeurBienRetenue)

suptra_tbl <- merge(suptra_v2, tbl_v2, by = "Num_contrat_sous", all = TRUE)
dim(suptra_tbl) # 856 014 x 3

# On va nettoyer la base : on considère les cellules vides comme des NA
suptra_tbl2 <- suptra_tbl %>%
  mutate(across(
    where(is.character),
    ~ ifelse(trimws(.) %in% c("", "-"), NA, trimws(.))
  )) %>%
  distinct() # On retire les doublons

dim(suptra_tbl2) # 834514 x 3

# On a des contrats qui existent dans suptrac mais pas dans tbl
# On a des contrats où on a aucune données (valeur du bien, type de logemenet)
round(colMeans(is.na(suptra_tbl2)) * 100, 1)
# Num_contrat_sous     Type_logement valeurBienRetenue
#               0.0              36.6              29.3

dim(df) # 349 237 x 32

# Fusion entre les données valeurs de bien, type de logement, niveau PRISM et la BDD
df2 <- merge(
  df,
  suptra_tbl2,
  by.x = "idt_contrat",
  by.y = "Num_contrat_sous",
  all.x = TRUE
)
dim(df2) # 349 237 x 34
round(colMeans(is.na(df2)) * 100, 1)
#' NA:
#' - PRISM ~ 7,2 %
#' - Type de logement ~ 31,3 %
#' - Valeur du bien ~ 21,1 %

# On drop les contrats en NA
df3 <- df2 %>%
  drop_na(idt_contrat)
dim(df3) # 326 630 x 34

round(colMeans(is.na(df3)) * 100, 1)
#' NA:
#' - PRISM ~ 0,8 %
#' - Type de logement ~ 26, 5%
#' - Valeur du bien ~ 15,7 %

# Import de la valeur verte
v_verte <- read_excel("data/OVERLAY_DPE_NOA.xlsx")
head(v_verte)

# Import du fichier permettant de faire la jointure entre CP et région
cp_reg <- read.csv("data/20230823-communes-departement-region.csv")
cp_reg <- cp_reg %>%
  select(code_postal, nom_region)

cp_reg[c(152, 16191), ]
#       code_postal              nom_region
# 152          1590    Auvergne-Rhône-Alpes
# 16191        1590 Bourgogne-Franche-Comté

#' On a des codes postaux dans différentes régions simultanément, il faut
#' modifier ça sinon cela va créer des doublons

# Calculer les fréquences et pourcentages par code postal
cp_reg_freq <- cp_reg %>%
  group_by(code_postal) %>%
  count(nom_region) %>%
  mutate(
    total = sum(n),
    pourcentage = n / total * 100
  ) %>%
  arrange(code_postal, desc(pourcentage))

# Appliquer la règle : garder si > 50%, sinon prendre la plus fréquente
cp_reg_final <- cp_reg_freq %>%
  group_by(code_postal) %>%
  filter(
    pourcentage > 50 | # Garder si > 50%
      row_number() == 1 # Sinon garder la plus fréquente (1ère ligne après tri desc)
  ) %>%
  select(code_postal, nom_region) %>%
  ungroup()


# On fusionne notre base avec la valeur verte
df4 <- merge(
  df3, # on prend df3, dont on a supprimé les contrats en NA
  cp_reg_final,
  by.x = "final_cd_post",
  by.y = "code_postal",
  all.x = TRUE
)
dim(df3) # 326 630 x 34
dim(df4) # 326 630 x 35

# Modifier la colonne "type de bien" pour clarifier
unique(df4$Type_logement)

df5 <- df4 %>%
  mutate(
    Type_logement = case_when(
      Type_logement == "1" ~ "maison",
      Type_logement == "2" ~ "appartement",
      Type_logement == "CHARLES RENAUD  56620 CLEGUER" ~ NA_character_,
      TRUE ~ Type_logement # Garde les autres valeurs inchangées (dont NA existants)
    )
  ) %>%
  rename(dpe = `final_cd_classe_conso énergétique`)

# Créer l'overlay DPE en fonction de la valeur verte
unique(df5$nom_region) # En fonction des régions
unique(df5$Type_logement) # En fonction du type de logement
unique(df5$dpe) # En fonction de l'étiquette DPE

# Fonction pour nettoyer et convertir les pourcentages
clean_pct <- function(x) {
  x <- gsub("%", "", x)
  x <- gsub("\\+", "", x)
  x[x == "-" | x == "0"] <- "0"
  as.numeric(x) / 100
}

# Préparer la table de référence
v_verte_clean <- v_verte %>%
  mutate(
    `Type de bien` = case_when(
      `Type de bien` == "Appartements" ~ "appartement",
      `Type de bien` == "Maison" ~ "maison"
    ),
    across(A:G, clean_pct)
  ) %>%
  pivot_longer(cols = A:G, names_to = "dpe", values_to = "impact")

# Calculer l'impact dans df5
df6 <- df5 %>%
  left_join(
    v_verte_clean,
    by = c(
      "nom_region" = "Région",
      "Type_logement" = "Type de bien",
      "dpe" = "dpe"
    )
  )
dim(df6)

round(colMeans(is.na(df6)) * 100, 1)
#' NOMBRE DE NA:
#'  - DPE ~ 0 %
#'  - PRISM ~ 0.8 %
#'  - Type de logement ~ 26,5 %
#'  - Valeur du bien ~ 15,7 %
#'  - Nom de la région ~ 1 %
#'  - Impact ~ 32,1 %

# Création de 2 colonnes "impact_DPE" et "impact_physique"

df7 <- df6 %>%
  rename(impact_DPE = impact) %>%
  mutate(
    impact_DPE = impact_DPE * 100,
    impact_Physique = ifelse(prism_total == 4, -0.05, 0) * 100
  )

dim(df7) # 326 630 x 37

# Création d'une base de données CRE
cre <- df7 %>%
  filter(cd_ty_imbobilier == "CRE")
dim(cre) # 3697 x 37

round(colMeans(is.na(cre)) * 100, 1)
#' NOMBRE DE NA:
#'  - Impact_DPE ~ 30 %
#'  - Impact Physique ~ 0.2 %

# Création d'une base de données RRE
rre <- df7 %>%
  filter(cd_ty_imbobilier == "RRE")
dim(rre) # 322 933 x 37

round(colMeans(is.na(rre)) * 100, 1)
#' NOMBRE DE NA:
#'  - Impact_DPE ~ 32 %
#'  - Impact Physique ~ 0.8 %

write.csv2(cre, "data/CRE_overlay_final.csv")
write.csv2(rre, "data/RRE_overlay_final.csv")
