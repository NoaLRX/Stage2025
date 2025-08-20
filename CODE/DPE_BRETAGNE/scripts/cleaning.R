source("scripts/library.R")
# Importation de la base
## Département 29--------------------------------------------------------------
BDNB <- st_read("data/open_data_millesime_2024-10-a_dep29_gpkg/gpkg/bdnb.gpkg",
                layer = "batiment_groupe_compile")
colnames(BDNB)

# Nettoyage de la base
BDNB_CLEAN <- BDNB %>%
  select(c("code_departement_insee",
           "libelle_commune_insee",
           "code_commune_insee",
           "libelle_commune_insee",
           "libelle_commune_insee",
           "libelle_adr_principale_ban",
           "ffo_bat_annee_construction",
           "dpe_mix_arrete_classe_bilan_dpe",
           "dpe_mix_arrete_surface_habitable_logement",
           # "dvf_open_surface_bati_mutee_residencielle_collective",
           # "dvf_open_surface_bati_mutee_residencielle_individuelle",
           "dvf_open_surface_terrain_mutee",
           "ffo_bat_nb_niveau",
           "dvf_open_nb_piece_principale",
           "usage_principal_bdnb_open",
           "dvf_open_valeur_fonciere",
           "dvf_open_date_mutation"))%>%
  filter(dvf_open_date_mutation > as.Date("2020-01-01"))

# Nettoyage de NA pour chaque colonnes
sapply(BDNB_CLEAN, function(x) sum(is.na(x)))
df29 <- na.omit(BDNB_CLEAN)
nrow(df29) # 19900
unique(df29$code_departement_insee)


## Département 22--------------------------------------------------------------
BDNB <- st_read("data/open_data_millesime_2024-10-a_dep22_gpkg/gpkg/bdnb.gpkg",
                layer = "batiment_groupe_compile")
colnames(BDNB)

# Nettoyage de la base
BDNB_CLEAN <- BDNB %>%
  select(c("code_departement_insee",
           "libelle_commune_insee",
           "code_commune_insee",
           "libelle_commune_insee",
           "libelle_commune_insee",
           "libelle_adr_principale_ban",
           "ffo_bat_annee_construction",
           "dpe_mix_arrete_classe_bilan_dpe",
           "dpe_mix_arrete_surface_habitable_logement",
           # "dvf_open_surface_bati_mutee_residencielle_collective",
           # "dvf_open_surface_bati_mutee_residencielle_individuelle",
           "dvf_open_surface_terrain_mutee",
           "ffo_bat_nb_niveau",
           "dvf_open_nb_piece_principale",
           "usage_principal_bdnb_open",
           "dvf_open_valeur_fonciere",
           "dvf_open_date_mutation"))%>%
  filter(dvf_open_date_mutation > as.Date("2020-01-01"))

# Nettoyage de NA pour chaque colonnes
sapply(BDNB_CLEAN, function(x) sum(is.na(x)))
df22 <- na.omit(BDNB_CLEAN)
nrow(df22) # 13598
unique(df22$code_departement_insee)



## Département 35--------------------------------------------------------------
BDNB <- st_read("data/open_data_millesime_2024-10-a_dep35_gpkg/gpkg/bdnb.gpkg",
                layer = "batiment_groupe_compile")
colnames(BDNB)

# Nettoyage de la base
BDNB_CLEAN <- BDNB %>%
  select(c("code_departement_insee",
           "libelle_commune_insee",
           "code_commune_insee",
           "libelle_commune_insee",
           "libelle_commune_insee",
           "libelle_adr_principale_ban",
           "ffo_bat_annee_construction",
           "dpe_mix_arrete_classe_bilan_dpe",
           "dpe_mix_arrete_surface_habitable_logement",
           # "dvf_open_surface_bati_mutee_residencielle_collective",
           # "dvf_open_surface_bati_mutee_residencielle_individuelle",
           "dvf_open_surface_terrain_mutee",
           "ffo_bat_nb_niveau",
           "dvf_open_nb_piece_principale",
           "usage_principal_bdnb_open",
           "dvf_open_valeur_fonciere",
           "dvf_open_date_mutation"))%>%
  filter(dvf_open_date_mutation > as.Date("2020-01-01"))

# Nettoyage de NA pour chaque colonnes
sapply(BDNB_CLEAN, function(x) sum(is.na(x)))
df35 <- na.omit(BDNB_CLEAN)
nrow(df35) # 20022
unique(df35$code_departement_insee)


## Département 56--------------------------------------------------------------
BDNB <- st_read("data/open_data_millesime_2024-10-a_dep56_gpkg/gpkg/bdnb.gpkg",
                layer = "batiment_groupe_compile")
colnames(BDNB)

# Nettoyage de la base
BDNB_CLEAN <- BDNB %>%
  select(c("code_departement_insee",
           "libelle_commune_insee",
           "code_commune_insee",
           "libelle_commune_insee",
           "libelle_commune_insee",
           "libelle_adr_principale_ban",
           "ffo_bat_annee_construction",
           "dpe_mix_arrete_classe_bilan_dpe",
           "dpe_mix_arrete_surface_habitable_logement",
           # "dvf_open_surface_bati_mutee_residencielle_collective",
           # "dvf_open_surface_bati_mutee_residencielle_individuelle",
           "dvf_open_surface_terrain_mutee",
           "ffo_bat_nb_niveau",
           "dvf_open_nb_piece_principale",
           "usage_principal_bdnb_open",
           "dvf_open_valeur_fonciere",
           "dvf_open_date_mutation"))%>%
  filter(dvf_open_date_mutation > as.Date("2020-01-01"))

# Nettoyage de NA pour chaque colonnes
sapply(BDNB_CLEAN, function(x) sum(is.na(x)))
df56 <- na.omit(BDNB_CLEAN)
nrow(df56) # 15397
unique(df56$code_departement_insee)


# Fusion des données------------------------------------------------------------
bdnb_bzh <- bind_rows(df22, df29, df35, df56)
unique(bdnb_bzh$code_departement_insee)
st_write(bdnb_bzh, "data/bdnb_bretagne_2024.gpkg", layer = "bdnb_bzh", driver = "GPKG")



df <- st_read("data/bdnb_bretagne_2024.gpkg")

df <- df %>%
  rename(code_dep = code_departement_insee,
         code_commune = code_commune_insee,
         commune = libelle_commune_insee,
         adresse = libelle_adr_principale_ban,
         annee_constuction = ffo_bat_annee_construction,
         dpe = dpe_mix_arrete_classe_bilan_dpe,
         surface_habitable = dpe_mix_arrete_surface_habitable_logement,
         surface_terrain = dvf_open_surface_terrain_mutee,
         nb_etages = ffo_bat_nb_niveau,
         nb_pieces = dvf_open_nb_piece_principale,
         type = usage_principal_bdnb_open,
         prix = dvf_open_valeur_fonciere,
         date = dvf_open_date_mutation)%>%
  filter(type %in% c("Résidentiel individuel",
                     "Résidentiel collectif"))%>%
  mutate(type = 
           case_when(
             type == "Résidentiel individuel" ~ "maison",
             type == "Résidentiel collectif" ~ "appartement",
             TRUE ~ type
           ))

unique(df$type)

vacant <- read_excel("data/logements_vacants.xlsx",
                     skip = 4)

vacant <- vacant %>%
  rename(
    code_commune = codgeo,
    commune = libgeo,
    annee = an, 
    n_vide = p_logvac
  )%>%
  filter(
    annee == "2021"
  )

# LOGEMENT VACANTS -------------------------------------------------------------
df <- df %>%
  left_join(vacant %>% select(code_commune, n_vide), by = "code_commune") %>%
  mutate(n_vide = if_else(is.na(n_vide), 0, n_vide))

df$nb_etages <- as.numeric(df$nb_etages)
df$nb_pieces <- as.numeric(df$nb_pieces)



st_write(df, "data/bdnb_bretagne_2024_CLEAN.gpkg", layer = "bdnb_bzh", driver = "GPKG")
write.csv(st_drop_geometry(df), "data/bdnb_bretagne_2024_CLEAN.csv", row.names = FALSE)



# BASE CORINE LAND COVER -------------------------------------------------------
CLC <- st_read("/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/IncendieFeuxForêts/FF_1_CLC/CLC12_RBRE_RGF.shp")
colnames(CLC)

CLC_2 <- CLC %>%
  filter(
    CODE_12 %in% c("122", "124", "131", "132", "141", "142", "311", "312", "313", 
                   "321", "322", "323", "324", "331", "411", "412", "511", "512", 
                   "521", "522", "523", "523")
  )



## CALCUL ENTITEE ---------------------------------------------------------------
# Distance d'analyse maximale (en mètres) - on peut l'augmenter si besoin
distance_analyse <- 30000  # Augmenté pour être sûr de trouver des entités

# 1. Préparation des données Corine Land Cover avec catégorisation
CLC_categorise <- CLC %>%
  filter(
    CODE_12 %in% c("122", "124", "131", "132", "141", "142", "311", "312", "313", 
                   "321", "322", "323", "324", "331", "411", "412", "511", "512", 
                   "521", "522", "523")
  ) %>%
  mutate(
    couleur = case_when(
      # VERT : zones vertes
      CODE_12 %in% c("141", "142", "311", "312", "313", "321", "322", "323", "324", "331") ~ "VERT",
      
      # BLEU : zones bleues  
      CODE_12 %in% c("511", "512", "521", "522", "523") ~ "BLEU",
      
      # ROUGE : nuisances
      CODE_12 %in% c("122", "124", "131", "132", "411", "412") ~ "ROUGE",
      
      TRUE ~ "AUTRE"
    )
  ) %>%
  filter(couleur != "AUTRE")  # On garde seulement les catégories définies

# 2. S'assurer que les deux jeux de données utilisent le même système de coordonnées
if(st_crs(df) != st_crs(CLC_categorise)) {
  cat("Attention: systèmes de coordonnées différents, reprojection en cours...\n")
  CLC_categorise <- st_transform(CLC_categorise, st_crs(df))
}

# 3. Séparer les entités par couleur pour optimiser les calculs
CLC_VERT <- CLC_categorise %>% filter(couleur == "VERT")
CLC_BLEU <- CLC_categorise %>% filter(couleur == "BLEU")
CLC_ROUGE <- CLC_categorise %>% filter(couleur == "ROUGE")

# 4. Fonction pour calculer la distance minimale à une couleur donnée
calculer_distance_minimale <- function(points_geom, entites_couleur) {
  if(nrow(entites_couleur) == 0) {
    return(rep(NA, length(points_geom)))
  }
  
  # Calculer la matrice de distances
  distances_matrix <- st_distance(points_geom, entites_couleur)
  
  # Prendre la distance minimale pour chaque point (en mètres)
  distances_min <- apply(distances_matrix, 1, min)
  
  # Convertir en kilomètres
  return(as.numeric(distances_min) / 1000)
}

# 5. Calcul des distances minimales
cat("Début du calcul des distances minimales pour", nrow(df), "bâtiments...\n")

# Calcul pour chaque couleur
cat("Calcul des distances aux zones VERTES...\n")
df$DIST_VERT_KM <- calculer_distance_minimale(df, CLC_VERT)

cat("Calcul des distances aux zones BLEUES...\n")
df$DIST_BLEU_KM <- calculer_distance_minimale(df, CLC_BLEU)

cat("Calcul des distances aux zones ROUGES...\n")
df$DIST_ROUGE_KM <- calculer_distance_minimale(df, CLC_ROUGE)

cat("Calcul terminé !\n")

# 6. Résumé des résultats
cat("\n=== RÉSUMÉ DES DISTANCES ===\n")
cat("Distance moyenne aux zones VERTES:", round(mean(df$DIST_VERT_KM, na.rm = TRUE), 2), "km\n")
cat("Distance moyenne aux zones BLEUES:", round(mean(df$DIST_BLEU_KM, na.rm = TRUE), 2), "km\n")
cat("Distance moyenne aux zones ROUGES:", round(mean(df$DIST_ROUGE_KM, na.rm = TRUE), 2), "km\n")

cat("\nNombre de bâtiments sans zone VERTE à proximité:", sum(is.na(df$DIST_VERT_KM)), "\n")
cat("Nombre de bâtiments sans zone BLEUE à proximité:", sum(is.na(df$DIST_BLEU_KM)), "\n")
cat("Nombre de bâtiments sans zone ROUGE à proximité:", sum(is.na(df$DIST_ROUGE_KM)), "\n")

# 7. Optionnel : visualisation des distributions
if(require(ggplot2, quietly = TRUE)) {
  library(tidyr)
  
  # Préparer les données pour le graphique
  distances_long <- df %>%
    st_drop_geometry() %>%
    select(DIST_VERT_KM, DIST_BLEU_KM, DIST_ROUGE_KM) %>%
    pivot_longer(everything(), names_to = "Type", values_to = "Distance_KM") %>%
    mutate(Type = case_when(
      Type == "DIST_VERT_KM" ~ "Zones Vertes",
      Type == "DIST_BLEU_KM" ~ "Zones Bleues", 
      Type == "DIST_ROUGE_KM" ~ "Zones Rouges"
    ))
  
  # Créer le graphique
  p <- ggplot(distances_long, aes(x = Distance_KM, fill = Type)) +
    geom_histogram(alpha = 0.7, bins = 50) +
    facet_wrap(~Type, scales = "free") +
    labs(title = "Distribution des distances aux entités les plus proches",
         x = "Distance (km)",
         y = "Nombre de bâtiments") +
    theme_minimal() +
    scale_fill_manual(values = c("Zones Vertes" = "green", 
                                 "Zones Bleues" = "blue", 
                                 "Zones Rouges" = "red"))
  
  print(p)
}



st_write(df, "data/bdnb_bretagne_2024_CLEAN_CLC.gpkg", layer = "bdnb_bzh", driver = "GPKG")

df <- st_read("data/bdnb_bretagne_2024_CLEAN_CLC.gpkg")


# REVENU MEDIAN ----------------------------------------------------------------
revenu <- read_excel("data/revenu_median.xlsx",
                     skip=4)
revenu <- revenu %>%
  rename(code_commune = codgeo,
         commune = libgeo,
         annee = an, 
         revenu_median = med_disp)
summary(revenu)

median_n_vide <- median(revenu$revenu_median, na.rm = TRUE)

# Calcul de la médiane sans les NA
median_revenu <- median(revenu$revenu_median, na.rm = TRUE)

# Remplacement des NA par la médiane
revenu <- revenu %>%
  mutate(revenu_median = if_else(is.na(revenu_median), median_revenu, revenu_median))


summary(revenu)

# Fusion
df <- df %>%
  left_join(revenu %>% select(code_commune, revenu_median), by = "code_commune")





# NB CATNAT --------------------------------------------------------------------
catnat <- read.csv2("data/catnat_gaspar.csv")

# Calcul du nombre cumulé par commune et par aléa + cumul total par aléa

catnat_wide <- catnat %>%
  group_by(cod_commune, lib_commune, lib_risque_jo) %>%
  summarise(nb_catnat = n(), .groups = 'drop') %>%
  pivot_wider(names_from = lib_risque_jo, 
              values_from = nb_catnat, 
              values_fill = 0)%>%
  rename(
    code_commune = "cod_commune",
    commune = "lib_commune",
    inond = "Inondations et/ou Coulées de Boue",
    mvt_terrain = "Mouvement de Terrain",
    secheresse = "Sécheresse",
    grele = "Grêle",
    tempete = "Tempête",
    mvt_sech = "Mouvements de terrain différentiels consécutifs à la sécheresse et à la réhydratation des sols",
    inond_nappe = "Inondations Remontée Nappe",
    glissement = "Glissement de Terrain",
    mvt_hors_sech = "Mouvements de terrains (hors sécheresse géotechnique)",
    eboulement = "Eboulement et/ou Chute de Blocs",
    seisme = "Secousse Sismique",
    effondrement = "Effondrement et/ou Affaisement",
    lave_torr = "Lave Torrentielle",
    avalanche = "Avalanche",
    choc_vagues = "Chocs Mécaniques liés à l'action des Vagues",
    gliss_effond = "Glissement et Effondrement de Terrain",
    poids_neige = "Poids de la Neige",
    vent_cyclo = "Vents Cycloniques",
    raz_maree = "Raz de Marée",
    inond_vagues = "Inondations par choc mécanique des vagues",
    gliss_eboul = "Glissement et Eboulement Rocheux",
    coulee_boue = "Coulée de Boue",
    seismes = "Séismes",
    choc_vagues2 = "Choc M�caniques li�s � l'action des Vagues",  # Version avec caractères spéciaux
    divers = "Divers",
    volcan = "Eruption Volcanique"
  )


# Fusion
df <- df %>%
  left_join(catnat_wide %>% select(-commune), by = "code_commune")

# # Calcul du nombre total de CATNAT par commune
# catnat_total <- catnat %>%
#   group_by(cod_commune, lib_commune) %>%
#   summarise(CATNAT = n(), .groups = 'drop') %>%
#   rename(
#     code_commune = "cod_commune",
#     commune = "lib_commune"
#   )


# # Fusion
# df <- df %>%
#   left_join(catnat_total %>% select(-commune), by = "code_commune")




# PART PROPRIETAIRE -----------------------------------------------------------
proprio <- read_excel("data/part_proprio.xlsx", skip = 4)

proprio <- proprio %>%
  rename(code_commune = codgeo,
         commune = libgeo,
         annee = an)%>%
  filter(annee==2021)%>%
  select(-c("annee"))

# Calcul de la médiane sans les NA
median_proprio <- median(proprio$part_proprio, na.rm = TRUE)

# Remplacement des NA par la médiane dans la colonne part_proprio
proprio <- proprio %>%
  mutate(part_proprio = if_else(is.na(part_proprio), median_proprio, part_proprio))


# Fusion
df <- df %>%
  left_join(proprio %>% select(-commune), by = "code_commune")


st_write(df, "data/DF_FINAL_AVEC_CATNAT.gpkg", layer = "bdnb_bzh", driver = "GPKG")


# Conversion CSV
# Supprimer la colonne de géométrie
df_no_geom <- df %>% st_drop_geometry()

# Sauvegarder en CSV
write.csv(df_no_geom, "data/DF_FINAL_AVEC_CATNAT.csv", row.names = FALSE)

