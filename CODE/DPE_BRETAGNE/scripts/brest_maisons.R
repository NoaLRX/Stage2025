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

appart <- df %>%
  filter(type == "appartement", code_commune == "29019")

# st_write(appart, "data/appart_BREST.gpkg", driver = "GPKG")

appart_csv <- df_CSV %>%
  filter(type == "appartement", code_commune == "29019")

maison <- df %>%
  filter(type == "maison", code_commune == "29019")

# st_write(maison, "data/maison_BREST.gpkg", driver = "GPKG")
maison_csv <- df_CSV %>%
  filter(type == "maison", code_commune == "29019")


# IMPACT SUR LES MAISONS -------------------------------------------------------
## MATRICES --------------------------------------------------------------------
### Tour -----------------------------------------------------------------------
nb_tour <- poly2nb(maison, queen = FALSE)
summary(nb_tour)

nb_lines <- nb2lines(nb_tour, coords = st_centroid(st_geometry(maison)))
nb_sf <- st_as_sf(nb_lines)


### Reine ----------------------------------------------------------------------
nb_Reine <- poly2nb(maison, queen = TRUE)
WR <- nb2listw(nb_Reine, style = "W", zero.policy = TRUE)


### 1 voisin ---------------------------------------------------------------------
centroids <- st_centroid(st_geometry(maison))
coords <- st_coordinates(centroids)
crs <- st_crs(maison)
coords_sf <- st_as_sf(as.data.frame(coords), coords = c("X", "Y"), crs = crs)
coords_sp <- as(coords_sf, "Spatial")
k <- 1
knn_neighbours <- knearneigh(coords, k = k)
neighbors <- knn2nb(knn_neighbours)
PPV1 <- nb2listw(neighbors, style = "W", zero.policy = TRUE)


### 3 voisins --------------------------------------------------------------------
centroids <- st_centroid(st_geometry(maison))
coords <- st_coordinates(centroids)
crs <- st_crs(maison)
coords_sf <- st_as_sf(as.data.frame(coords), coords = c("X", "Y"), crs = crs)
coords_sp <- as(coords_sf, "Spatial")
k <- 3
knn_neighbours <- knearneigh(coords, k = k)
neighbors <- knn2nb(knn_neighbours)
PPV3 <- nb2listw(neighbors, style = "W", zero.policy = TRUE)


## Indice de Morans ------------------------------------------------------------
### Matrice Reine----
globalMoran <- moran.test(
  maison$prix,
  WR,
  zero.policy = TRUE,
  randomisation = TRUE
)
globalMoran

# I de Moran = 0,011 (très proche de 0)
# p-value = 0,4357 (> 0,05)
# Ce résultat n'est pas statistiquement significatif
# Il n'y a pas d'autocorrélation spatiale détectable avec cette matrice de contiguïté

### Matrice Voisins----
# 1 VOISIN
globalMoran <- moran.test(
  maison$prix,
  PPV1,
  zero.policy = TRUE,
  randomisation = TRUE
)
globalMoran

# I de Moran = 0.265 (positif et élevé)
# p-value < 2.227e-14 (très significatif)
# Indique une autocorrélation spatiale positive
# Les prix des maisons sont similaires pour les plus proches voisins

# set.seed(1234)
# Moranperm <- moran.mc(maison$prix, PPV1, nsim = 999, zero.policy = TRUE)
# Moranperm

# 3 VOISINS
globalMoran <- moran.test(
  maison$prix,
  PPV3,
  zero.policy = TRUE,
  randomisation = TRUE
)
globalMoran


# I de Moran = 0,213 (positif et élevé)
# p-value < 2.2e-16 (très significatif)
# Confirme l'autocorrélation spatiale positive

## Diagramme de MORAN -------------------------------------------------------------------
maison$prix_std <- scale(maison$prix)

pdf("pdf/moran_reine_maison.pdf", width = 8, height = 6)
mp <- moran.plot(
  as.vector(maison$prix_std),
  WR, # Matrice Reine
  xlab = "Prix des maisons",
  ylab = "Lag - Prix des maisons",
  main = "Matrice type Reine"
)
dev.off()

pdf("pdf/moran_ppv1_maison.pdf", width = 8, height = 6)
mp <- moran.plot(
  as.vector(maison$prix_std),
  PPV1, # Matrice Reine
  xlab = "Prix des maisons",
  ylab = "Lag - Prix des maisons",
  main = "Matrice type PPV1"
)
dev.off()

pdf("pdf/moran_ppv3_maison.pdf", width = 8, height = 6)
mp <- moran.plot(
  as.vector(maison$prix_std),
  PPV3, # Matrice Reine
  xlab = "Prix des maisons",
  ylab = "Lag - Prix des maisons",
  main = "Matrice type PPV3"
)
dev.off()


## Modèle MCO -------------------------------------------------------------------
colnames(maison)
fit1 <- lm(
  prix ~
    dpe +
      age_batiment +
      surface_habitable +
      surface_terrain +
      nb_etages +
      nb_pieces +
      DIST_VERT_KM +
      DIST_BLEU_KM +
      DIST_ROUGE_KM,
  data = maison
)
summary(fit1) # Adj R2 = 0.5491

vif(fit1) # Pas de multico


## Indice de Moran sur MCO ------------------------------------------------------
# Reine
moran.lm <- lm.morantest(fit1, WR, alternative = "two.sided")
print(moran.lm)

# PPV1
moran.lm <- lm.morantest(fit1, PPV1, alternative = "two.sided")
print(moran.lm)

# PPV3
moran.lm <- lm.morantest(fit1, PPV3, alternative = "two.sided")
print(moran.lm)

# Il faut tester modèles d'auto-corrélation spatiale sur PPV1 et PPV3 car p<0.05

## Tests de Lagrange ------------------------------------------------------------
options(scipen = 0)
LM1 <- lm.LMtests(fit1, PPV1, test = c("LMerr", "LMlag", "RLMerr", "RLMlag"))
print(LM1) # Modèle SAR


LM1 <- lm.LMtests(fit1, PPV3, test = c("LMerr", "LMlag", "RLMerr", "RLMlag"))
print(LM1)
options(scipen = 0) # Modèle SEM


## MODELE SAR -------------------------------------------------------------------
formule <- log(prix) ~
  dpe +
    age_batiment +
    surface_habitable +
    surface_terrain +
    nb_etages +
    nb_pieces +
    DIST_VERT_KM +
    DIST_BLEU_KM +
    DIST_ROUGE_KM

dim(maison)

# PPV1
sar_ppv1 <- lagsarlm(formule, data = maison, PPV1)
summary(sar_ppv1)

# Coefficients: (asymptotic standard errors)
# Estimate   Std. Error z value              Pr(>|z|)
# (Intercept)       10.924379781  0.221255074 49.3746 < 0.00000000000000022
# dpeA               0.187356360  0.102628933  1.8256             0.0679150
# dpeB               0.160869072  0.049053495  3.2795             0.0010401
# dpeC               0.068402376  0.020474683  3.3408             0.0008353
# dpeE              -0.079410618  0.019737724 -4.0233      0.00005739036782
# dpeF              -0.156801622  0.026762818 -5.8589      0.00000000465844
# dpeG              -0.200931759  0.036172241 -5.5549      0.00000002778333
# age_batiment      -0.001736188  0.000306804 -5.6590      0.00000001523015
# surface_habitable  0.004423132  0.000276677 15.9866 < 0.00000000000000022
# surface_terrain    0.000102925  0.000015162  6.7883      0.00000000001135
# nb_etages         -0.003949138  0.016349947 -0.2415             0.8091379
# nb_pieces          0.043590536  0.007776247  5.6056      0.00000002075336
# DIST_VERT_KM       0.047846339  0.013459326  3.5549             0.0003781
# DIST_BLEU_KM      -0.064116473  0.011116956 -5.7674      0.00000000804805
# DIST_ROUGE_KM      0.045641288  0.007906547  5.7726      0.00000000780604

## MODELE SEM -------------------------------------------------------------------
# PPV3
sem_ppv3 <- errorsarlm(formule, data = maison, PPV3)
summary(sem_ppv3)
# Coefficients: (asymptotic standard errors)
# Estimate   Std. Error  z value              Pr(>|z|)
# (Intercept)       11.732623903  0.056095458 209.1546 < 0.00000000000000022
# dpeA               0.204646330  0.101607537   2.0141             0.0440005
# dpeB               0.150120466  0.050428292   2.9769             0.0029117
# dpeC               0.068609440  0.020278151   3.3834             0.0007159
# dpeE              -0.078958415  0.019517656  -4.0455      0.00005221456650
# dpeF              -0.153014520  0.026576105  -5.7576      0.00000000853194
# dpeG              -0.195768130  0.035890655  -5.4546      0.00000004909117
# age_batiment      -0.001756774  0.000316397  -5.5524      0.00000002817058
# surface_habitable  0.004409585  0.000275784  15.9893 < 0.00000000000000022
# surface_terrain    0.000101798  0.000015256   6.6725      0.00000000002514
# nb_etages         -0.005168301  0.016197758  -0.3191             0.7496696
# nb_pieces          0.044222628  0.007735342   5.7170      0.00000001084475
# DIST_VERT_KM       0.047729143  0.015860981   3.0092             0.0026192
# DIST_BLEU_KM      -0.065101241  0.013025929  -4.9978      0.00000057982235
# DIST_ROUGE_KM      0.051118309  0.009325952   5.4813      0.00000004222199

## MODELE SDM -------------------------------------------------------------------
# PPV1
sdm_ppv1 <- lagsarlm(formule, data = maison, PPV1, type = "mixed")
summary(sdm_ppv1)

# PPV3
sdm_ppv3 <- lagsarlm(formule, data = maison, PPV3, type = "mixed")
summary(sdm_ppv3)

# LR TEST -----------------------------------------------------------------------
# PPV1
LR.Sarlm(sar_ppv1, sdm_ppv1) # SAR vs SDM : différence significative
AIC(sar_ppv1, sdm_ppv1) # AIC SDM < AIC SAR : on conserve SDM

# PPV3
LR.Sarlm(sem_ppv3, sdm_ppv3) # SEM vs SDM : différence significative
AIC(sem_ppv3, sdm_ppv3) # AIC SEM < AIC SDM : on conserve SEM

#' Conclusion, on fera deux modèles pour les données sur les maisons
#' 1 - Un modèle SDM pour la matrice PPV1
#' 2 - Un modèle SEM pour la matrice PPV3

# MODELES FINAUX ---------------------------------------------------------------
summary(sdm_ppv1)
summary(sdm_ppv3)

# Impacts
impact_sdm <- impacts(sdm_ppv1, listw = PPV1)
impact_sdm

# Impacts
impact_sdm2 <- impacts(sdm_ppv3, listw = PPV3)
impact_sdm2
