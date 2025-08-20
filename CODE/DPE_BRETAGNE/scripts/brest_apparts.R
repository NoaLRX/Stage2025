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


# IMPACT SUR LES appartS -------------------------------------------------------
## MATRICES --------------------------------------------------------------------
### Tour -----------------------------------------------------------------------
nb_tour <- poly2nb(appart, queen = FALSE)
summary(nb_tour)

nb_lines <- nb2lines(nb_tour, coords = st_centroid(st_geometry(appart)))
nb_sf <- st_as_sf(nb_lines)


### Reine ----------------------------------------------------------------------
nb_Reine <- poly2nb(appart, queen = TRUE)
WR <- nb2listw(nb_Reine, style = "W", zero.policy = TRUE)


### 1 voisin ---------------------------------------------------------------------
centroids <- st_centroid(st_geometry(appart))
coords <- st_coordinates(centroids)
crs <- st_crs(appart)
coords_sf <- st_as_sf(as.data.frame(coords), coords = c("X", "Y"), crs = crs)
coords_sp <- as(coords_sf, "Spatial")
k <- 1
knn_neighbours <- knearneigh(coords, k = k)
neighbors <- knn2nb(knn_neighbours)
PPV1 <- nb2listw(neighbors, style = "W", zero.policy = TRUE)


### 3 voisins --------------------------------------------------------------------
centroids <- st_centroid(st_geometry(appart))
coords <- st_coordinates(centroids)
crs <- st_crs(appart)
coords_sf <- st_as_sf(as.data.frame(coords), coords = c("X", "Y"), crs = crs)
coords_sp <- as(coords_sf, "Spatial")
k <- 3
knn_neighbours <- knearneigh(coords, k = k)
neighbors <- knn2nb(knn_neighbours)
PPV3 <- nb2listw(neighbors, style = "W", zero.policy = TRUE)


## Indice de Morans ------------------------------------------------------------
# 1 VOISIN
globalMoran <- moran.test(
  appart$prix,
  PPV1,
  zero.policy = TRUE,
  randomisation = TRUE
)
globalMoran

# I de Moran = 0.35 (positif et élevé)
# p-value < 2.2e-16 (très significatif)
# Indique une autocorrélation spatiale positive
# Les prix des apparts sont similaires pour les plus proches voisins

# set.seed(1234)
# Moranperm <- moran.mc(appart$prix, PPV1, nsim = 999, zero.policy = TRUE)
# Moranperm

# 3 VOISINS
globalMoran <- moran.test(
  appart$prix,
  PPV3,
  zero.policy = TRUE,
  randomisation = TRUE
)
globalMoran


# I de Moran = 0.29 (positif et élevé)
# p-value < 2.2e-16 (très significatif)
# Confirme l'autocorrélation spatiale positive

## Diagramme de MORAN -------------------------------------------------------------------
appart$prix_std <- scale(appart$prix)

pdf("pdf/moran_reine_appart.pdf", width = 8, height = 6)
mp <- moran.plot(
  as.vector(appart$prix_std),
  WR, # Matrice Reine
  xlab = "Prix des apparts",
  ylab = "Lag - Prix des apparts",
  main = "Matrice type Reine"
)
dev.off()

pdf("pdf/moran_ppv1_appart.pdf", width = 8, height = 6)
mp <- moran.plot(
  as.vector(appart$prix_std),
  PPV1, # Matrice Reine
  xlab = "Prix des apparts",
  ylab = "Lag - Prix des apparts",
  main = "Matrice type PPV1"
)
dev.off()

pdf("pdf/moran_ppv3_appart.pdf", width = 8, height = 6)
mp <- moran.plot(
  as.vector(appart$prix_std),
  PPV3, # Matrice Reine
  xlab = "Prix des apparts",
  ylab = "Lag - Prix des apparts",
  main = "Matrice type PPV3"
)
dev.off()


## Modèle MCO -------------------------------------------------------------------
colnames(appart)
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
  data = appart
)
summary(fit1) # Adj R2 = 0.5107

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

# Il faut tester modèles d'auto-corrélation spatiale sur WR, PPV1 et PPV3 car p<0.05

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

dim(appart)

# REINE
sar_wr <- lagsarlm(formule, data = appart, WR, zero.policy = TRUE) # Zero Policy TRUE car 0 voisins
summary(sar_wr)

# Coefficients: (asymptotic standard errors)
#                      Estimate  Std. Error  z value  Pr(>|z|)
# (Intercept)       10.94716402  0.07088070 154.4449 < 2.2e-16
# dpeA              -0.39326702  0.54374855  -0.7233 0.4695253
# dpeB               0.29617575  0.13937696   2.1250 0.0335868
# dpeC               0.05243983  0.03072507   1.7067 0.0878697
# dpeE              -0.03450302  0.02458112  -1.4036 0.1604264
# dpeF              -0.17099021  0.04439637  -3.8514 0.0001174
# dpeG              -0.16559671  0.06190085  -2.6752 0.0074686
# age_batiment      -0.00204779  0.00031936  -6.4122 1.434e-10
# surface_habitable  0.00025619  0.00010230   2.5044 0.0122648
# surface_terrain    0.00026078  0.00016902   1.5429 0.1228670
# nb_etages          0.01474313  0.00561958   2.6235 0.0087024
# nb_pieces          0.25260303  0.00867432  29.1208 < 2.2e-16
# DIST_VERT_KM       0.08386323  0.02081955   4.0281 5.623e-05
# DIST_BLEU_KM      -0.12695753  0.01742973  -7.2840 3.242e-13
# DIST_ROUGE_KM      0.05324884  0.01605217   3.3172 0.0009091

# PPV1
sar_ppv1 <- lagsarlm(formule, data = appart, PPV1)
summary(sar_ppv1)

# Coefficients: (asymptotic standard errors)
#                      Estimate  Std. Error z value  Pr(>|z|)
# (Intercept)       10.04869908  0.18832086 53.3595 < 2.2e-16
# dpeA              -0.37267769  0.53866176 -0.6919  0.489026
# dpeB               0.28190558  0.13807701  2.0417  0.041186
# dpeC               0.05578276  0.03044516  1.8322  0.066916
# dpeE              -0.03473614  0.02433478 -1.4274  0.153457
# dpeF              -0.17508979  0.04390307 -3.9881 6.660e-05
# dpeG              -0.17363264  0.06118707 -2.8377  0.004543
# age_batiment      -0.00186825  0.00031514 -5.9284 3.059e-09
# surface_habitable  0.00023668  0.00010138  2.3345  0.019569
# surface_terrain    0.00025183  0.00016742  1.5042  0.132534
# nb_etages          0.01274387  0.00557470  2.2860  0.022253
# nb_pieces          0.24664519  0.00864991 28.5142 < 2.2e-16
# DIST_VERT_KM       0.08358934  0.02033825  4.1100 3.957e-05
# DIST_BLEU_KM      -0.12314104  0.01718161 -7.1670 7.665e-13
# DIST_ROUGE_KM      0.04870307  0.01592351  3.0586  0.002224

## MODELE SEM -------------------------------------------------------------------
# PPV3
sem_ppv3 <- errorsarlm(formule, data = appart, PPV3)
summary(sem_ppv3)

# Coefficients: (asymptotic standard errors)
#                      Estimate  Std. Error  z value  Pr(>|z|)
# (Intercept)       10.96546673  0.07828266 140.0753 < 2.2e-16
# dpeA              -0.34155292  0.53655014  -0.6366 0.5244035
# dpeB               0.28002494  0.13733060   2.0391 0.0414443
# dpeC               0.05891302  0.03066351   1.9213 0.0546971
# dpeE              -0.03124367  0.02422848  -1.2895 0.1972095
# dpeF              -0.16814669  0.04368341  -3.8492 0.0001185
# dpeG              -0.17661404  0.06099560  -2.8955 0.0037853
# age_batiment      -0.00189406  0.00032435  -5.8395 5.234e-09
# surface_habitable  0.00023229  0.00010093   2.3015 0.0213646
# surface_terrain    0.00019769  0.00016724   1.1820 0.2371878
# nb_etages          0.01416175  0.00586483   2.4147 0.0157486
# nb_pieces          0.24949021  0.00868402  28.7298 < 2.2e-16
# DIST_VERT_KM       0.08585689  0.02373526   3.6173 0.0002977
# DIST_BLEU_KM      -0.13127479  0.02000061  -6.5635 5.255e-11
# DIST_ROUGE_KM      0.05316034  0.01871863   2.8400 0.0045118

## MODELE SDM -------------------------------------------------------------------
# REINE
sdm_wr <- lagsarlm(
  formule,
  data = appart,
  WR,
  type = "mixed",
  zero.policy = TRUE
)
summary(sdm_wr)

# PPV1
sdm_ppv1 <- lagsarlm(formule, data = appart, PPV1, type = "mixed")
summary(sdm_ppv1)

# PPV3
sdm_ppv3 <- lagsarlm(formule, data = appart, PPV3, type = "mixed")
summary(sdm_ppv3)

# Coefficients: (asymptotic standard errors)
#                          Estimate  Std. Error z value  Pr(>|z|)
# (Intercept)            9.37253375  0.32191605 29.1148 < 2.2e-16
# dpeA                  -0.58871388  0.55562878 -1.0595  0.289351
# dpeB                   0.28674396  0.13773658  2.0818  0.037358
# dpeC                   0.06391279  0.03073412  2.0795  0.037568
# dpeE                  -0.03138699  0.02423853 -1.2949  0.195348
# dpeF                  -0.17526978  0.04360804 -4.0192 5.839e-05
# dpeG                  -0.17561093  0.06083686 -2.8866  0.003894
# age_batiment          -0.00162614  0.00033547 -4.8473 1.252e-06
# surface_habitable      0.00022857  0.00010102  2.2626  0.023660
# surface_terrain        0.00024373  0.00017661  1.3801  0.167559
# nb_etages              0.00798924  0.00655280  1.2192  0.222765
# nb_pieces              0.24498734  0.00871676 28.1053 < 2.2e-16
# DIST_VERT_KM          -0.12401487  0.25416345 -0.4879  0.625597
# DIST_BLEU_KM          -0.44699776  0.25016610 -1.7868  0.073969
# DIST_ROUGE_KM          0.14219861  0.23789817  0.5977  0.550021
# lag.dpeA              -0.76851708  1.00843174 -0.7621  0.446006
# lag.dpeB               0.06730834  0.26485060  0.2541  0.799390
# lag.dpeC              -0.06735154  0.05119550 -1.3156  0.188317
# lag.dpeE              -0.05168761  0.04113625 -1.2565  0.208936
# lag.dpeF              -0.05445418  0.07124373 -0.7643  0.444667
# lag.dpeG               0.09141667  0.10824799  0.8445  0.398384
# lag.age_batiment      -0.00068647  0.00047652 -1.4406  0.149707
# lag.surface_habitable  0.00017522  0.00017455  1.0038  0.315480
# lag.surface_terrain    0.00058086  0.00029929  1.9408  0.052281
# lag.nb_etages          0.00846852  0.00878818  0.9636  0.335233
# lag.nb_pieces         -0.02156495  0.01570025 -1.3735  0.169584
# lag.DIST_VERT_KM       0.22056883  0.25618968  0.8610  0.389261
# lag.DIST_BLEU_KM       0.32636037  0.25151515  1.2976  0.194433
# lag.DIST_ROUGE_KM     -0.09852755  0.23863693 -0.4129  0.679697

sem_ppv3 <- errorsarlm(formule, data = appart, PPV3)
summary(sem_ppv3)

# LR TEST -----------------------------------------------------------------------
# PPV1
LR.Sarlm(sar_ppv1, sdm_ppv1) # SAR vs SDM : PAS de différence significative
AIC(sar_ppv1, sdm_ppv1) # AIC SAR < AIC SDM : on conserve SAR

# PPV3
LR.Sarlm(sem_ppv3, sdm_ppv3) # SEM vs SDM : différence significative
AIC(sem_ppv3, sdm_ppv3) # AIC SEM < AIC SDM : on conserve SEM ?

#' Conclusion, on fera deux modèles pour les données sur les apparts
#' 1 - Un modèle SAR pour la matrice PPV1
#' 2 - Un modèle SEM pour la matrice PPV3

# MODELES FINAUX ---------------------------------------------------------------
## PPV1
summary(sar_ppv1)

# Impacts
impact_sar <- impacts(sar_ppv1, listw = PPV1)
impact_sar

## PPV3
summary(sdm_ppv3)

# Impacts
impact_sdm <- impacts(sdm_ppv3, listw = PPV3)
impact_sdm
