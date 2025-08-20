library(GWmodel)      ### GW models
library(sp)           ## Data management
library(spdep)        ## Spatial autocorrelation
library(gstat)        ## Geostatistics
library(RColorBrewer) ## Visualization
library(classInt)     ## Class intervals
library(raster)       ## spatial data
library(gridExtra)    # Multiple plot
library(ggplot2)      # Multiple plot
library(sf)     
library(dplyr)

color_palette <- c(
  "#0033A0", # bleu foncé
  "#0066CC", # bleu
  "#0099FF", # bleu clair
  "#00CCFF", # cyan
  "#33DDDD", # cyan-vert
  "#66EEBB", # vert clair
  "#99FF99", # vert pâle
  "#CCFF66", # vert-jaune
  "#FFFF00", # jaune
  "#FFCC00", # jaune-orange
  "#FF9900", # orange
  "#FF6600", # orange foncé
  "#FF3300", # rouge-orange
  "#FF0000"  # rouge
)

# INONDATIONS----
# Chargement des données
data <- st_read("/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/resultats_agregation.gpkg")
CATNAT <- st_read("/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/Inondation/INO_2_CAT/inondations_par_cp.shp")

# 1. Refaire la fusion sans perdre la géométrie
CATNAT <- CATNAT %>%
  mutate(Code_posta = as.character(Code_posta))

# Important : utiliser st_join (et pas left_join) pour conserver la géométrie
merged_data <- st_join(CATNAT, data, join = st_equals, left = TRUE)

# 2. Gérer les NA dans nb_inondat
merged_data <- merged_data %>%
  mutate(nb_inondat = ifelse(is.na(nb_inondat), 0, nb_inondat))

# 3. Filtrer et conserver la géométrie
data_to_plot <- merged_data %>%
  dplyr::select("INO_1_SUB",
                "INO_1_RUI",
                "INO_1_DCO",
                "INO_1_PLUIEXT_RCP8_5_H1",
                "INO_1_NAP",
                "nb_inondat",
                geometry)

# 4. Créer ta carte
# ggplot() +
#   geom_sf(data = data_to_plot, aes(fill = nb_inondat)) +
#   scale_fill_gradientn(colors = color_palette, na.value = "white") +
#   theme_minimal()
#st_write(data_to_plot, "/Users/noa/Downloads/ton_fichier.gpkg")

colnames(data_to_plot)
sum(is.na(data_to_plot))
data3 <- na.omit(data_to_plot)
data2 <- data3
colnames(data2)

#-------------------------------------------------------------------------------
# # # # # GWPCA # # # # 
#-------------------------------------------------------------------------------

# Charger les packages nécessaires
library(sf)
library(sp)
library(dplyr)
library(GWmodel)
library(RColorBrewer)

# Convertir les données sf en données spatiales sp
# On extrait d'abord les coordonnées de la géométrie
coords <- st_coordinates(st_centroid(data2))

# Extraire les données numériques sans la géométrie
data_numeric <- st_drop_geometry(data2) %>%
  dplyr::select(#INO_1_SUB, 
                INO_1_RUI, 
                INO_1_DCO, 
                INO_1_PLUIEXT_RCP8_5_H1,
                INO_1_NAP,
                nb_inondat)

sum(is.na(data_numeric)) # Vérifier les valeurs manquantes
data_numeric_clean <- na.omit(data_numeric)

# Standardiser les données
data.scaled <- scale(as.matrix(data_numeric_clean))

# PCA standard pour comparaison
pca <- princomp(data.scaled, cor = FALSE)
cat("Variance expliquée par composante (%):\n")
print((pca$sdev^2 / sum(pca$sdev^2)) * 100)
cat("\nLoadings des composantes principales:\n")
print(pca$loadings)

# Garder trace des indices des lignes sans NA
clean_indices <- which(complete.cases(data_numeric))

# Filtrer les coordonnées pour qu'elles correspondent aux données nettoyées
coords_clean <- coords[clean_indices, ]

# Créer un SpatialPointsDataFrame pour GWmodel
scaled.spdf <- SpatialPointsDataFrame(coords_clean, as.data.frame(data.scaled))



# # Déterminer la bande passante optimale pour GWPCA
# bw.gw.pca <- bw.gwpca(scaled.spdf, 
#                       vars = colnames(scaled.spdf@data),
#                       bw = 100,  # Utiliser la bande passante optimale
#                       k = 3,  # Nombre max de composantes (= nombre de variables)
#                       robust = FALSE, 
#                       adaptive = TRUE)
# cat("\nBande passante optimale:", bw.gw.pca, "\n")

# Réaliser la GWPCA
gw.pca <- gwpca(scaled.spdf, 
                vars = colnames(scaled.spdf@data),
                bw = 100,  # Utiliser la bande passante optimale
                k = 3,  # Nombre max de composantes
                robust = FALSE, 
                adaptive = TRUE)

# Maintenant vous pouvez calculer la proportion de variance expliquée
prop.var <- function(gwpca.obj, n.components) {
  return((rowSums(gwpca.obj$var[, 1:n.components]) /rowSums(gwpca.obj$var)) * 100)
}
var.gwpca <- prop.var(gw.pca, 3)

# Afficher les résultats
summary(var.gwpca)

# Créer un objet sf avec les résultats de la GWPCA
data2$var_gwpca <- var.gwpca

# Déterminer la variable qui contribue le plus à la première composante
loadings.pc1 <- gw.pca$loadings[, , 1]
win.item <- max.col(abs(loadings.pc1))
data2$win_item <- win.item

# Convertir l'objet sf en objet sp pour spplot (si vous préférez utiliser spplot)
data2_sp <- as(data2, "Spatial")


# # Visualiser avec ggplot2 la proportion de variance expliquée
# library(ggplot2)
# ggplot() +
#   geom_sf(data = data2, aes(fill = var_gwpca)) +
#   scale_fill_gradientn(colors = temp_palette) +
#   labs(title = "Pourcentage de variance totale expliquée par les 3 premières composantes",
#        fill = "% variance") +
#   theme_minimal()

# Calculer la proportion de variance expliquée par la PREMIÈRE composante uniquement
prop.var.pc1 <- function(gwpca.obj) {
  return((gwpca.obj$var[, 1] / rowSums(gwpca.obj$var)) * 100)
}

# Appliquer cette fonction à votre objet GWPCA
var.gwpca.pc1 <- prop.var.pc1(gw.pca)

# Ajouter cette variable à votre objet spatial data2
data2$var_gwpca_pc1 <- var.gwpca.pc1

# Visualiser avec ggplot2
ggplot() +
  geom_sf(data = data2, aes(fill = var_gwpca_pc1)) +
  scale_fill_gradientn(colors = color_palette) +
  labs(title = "Pourcentage de variance totale expliquée par la première composante",
       fill = "% variance") +
  theme_minimal()


# Calculer la proportion de variance expliquée par les DEUX premières composantes
prop.var.pc12 <- function(gwpca.obj) {
  return((rowSums(gwpca.obj$var[, 1:2]) / rowSums(gwpca.obj$var)) * 100)
}

# Appliquer cette fonction à votre objet GWPCA
var.gwpca.pc12 <- prop.var.pc12(gw.pca)

# Ajouter cette variable à votre objet spatial data2
data2$var_gwpca_pc12 <- var.gwpca.pc12

# Visualiser avec ggplot2
ggplot() +
  geom_sf(data = data2, aes(fill = var_gwpca_pc12)) +
  scale_fill_gradientn(colors = color_palette) +
  labs(title = "Pourcentage de variance totale expliquée par les deux premières composantes",
       fill = "% variance") +
  theme_minimal()


# RISQUE RGA ----
data <- st_read("/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/resultats_agregation.gpkg")


data2 <- data%>%
  dplyr::select(c("code_postal",
                 "RGA_2_CAT",
                 "RGA_1_SSWI_RCP8_5_H1",
                 "RGA_1_RGA"))

CATNAT <- st_read("/Users/noa/Desktop/PRISM/Data/Resultats_CATNAT_MVT_Secheresse_Combined/frequence_secheresse_combined_cp.gpkg")


# 1. Refaire la fusion sans perdre la géométrie
CATNAT <- CATNAT %>%
  mutate(Code_posta = as.character(Code_posta))

# Important : utiliser st_join (et pas left_join) pour conserver la géométrie
merged_data <- st_join(CATNAT, data2, join = st_equals, left = TRUE)

# 2. Gérer les NA dans nb_inondat
merged_data <- merged_data %>%
  mutate(nb_RGA_SECH = ifelse(is.na(frequence), 0, frequence))

# 3. Filtrer et conserver la géométrie
data_to_plot <- merged_data %>%
  dplyr::select("RGA_1_SSWI_RCP8_5_H1",
                "RGA_1_RGA",
                "RGA_2_CAT",
                geom)
ggplot() +
  geom_sf(data = data_to_plot, aes(fill = RGA_2_CAT)) +
  scale_fill_gradientn(colors = color_palette, na.value = "white") +
  theme_minimal()


sum(is.na(data2))
data3 <- na.omit(data2)
data2 <- data3
sum(is.na(data2))


# Convertir les données sf en données spatiales sp
# On extrait d'abord les coordonnées de la géométrie
coords <- st_coordinates(st_centroid(data2))

# Extraire les données numériques sans la géométrie
data_numeric <- st_drop_geometry(data2) %>%
  dplyr::select(
    RGA_2_CAT,
    RGA_1_SSWI_RCP8_5_H1,
    RGA_1_RGA
  )

sum(is.na(data_numeric)) # Vérifier les valeurs manquantes
data_numeric_clean <- na.omit(data_numeric)

# Standardiser les données
data.scaled <- scale(as.matrix(data_numeric_clean))

# PCA standard pour comparaison
pca <- princomp(data.scaled, cor = FALSE)
cat("Variance expliquée par composante (%):\n")
print((pca$sdev^2 / sum(pca$sdev^2)) * 100)
cat("\nLoadings des composantes principales:\n")
print(pca$loadings)

# Garder trace des indices des lignes sans NA
clean_indices <- which(complete.cases(data_numeric))

# Filtrer les coordonnées pour qu'elles correspondent aux données nettoyées
coords_clean <- coords[clean_indices, ]

# Créer un SpatialPointsDataFrame pour GWmodel
scaled.spdf <- SpatialPointsDataFrame(coords_clean, as.data.frame(data.scaled))



# # Déterminer la bande passante optimale pour GWPCA
# bw.gw.pca <- bw.gwpca(scaled.spdf, 
#                       vars = colnames(scaled.spdf@data),
#                       bw = 100,  # Utiliser la bande passante optimale
#                       k = 3,  # Nombre max de composantes (= nombre de variables)
#                       robust = FALSE, 
#                       adaptive = TRUE)
# cat("\nBande passante optimale:", bw.gw.pca, "\n")

# Réaliser la GWPCA
gw.pca <- gwpca(scaled.spdf, 
                vars = colnames(scaled.spdf@data),
                bw = 100,  # Utiliser la bande passante optimale
                k = 2,  # Nombre max de composantes
                robust = FALSE, 
                adaptive = TRUE)


# Maintenant vous pouvez calculer la proportion de variance expliquée
prop.var <- function(gwpca.obj, n.components) {
  return((rowSums(gwpca.obj$var[, 1:n.components]) /rowSums(gwpca.obj$var)) * 100)
}
var.gwpca <- prop.var(gw.pca, 2)

# Afficher les résultats
summary(var.gwpca)

# Créer un objet sf avec les résultats de la GWPCA
data2$var_gwpca <- var.gwpca

# Déterminer la variable qui contribue le plus à la première composante
loadings.pc1 <- gw.pca$loadings[, , 1]
win.item <- max.col(abs(loadings.pc1))
data2$win_item <- win.item

# Convertir l'objet sf en objet sp pour spplot (si vous préférez utiliser spplot)
data2_sp <- as(data2, "Spatial")

# Calculer la proportion de variance expliquée par la PREMIÈRE composante uniquement
prop.var.pc1 <- function(gwpca.obj) {
  return((gwpca.obj$var[, 1] / rowSums(gwpca.obj$var)) * 100)
}

# Appliquer cette fonction à votre objet GWPCA
var.gwpca.pc1 <- prop.var.pc1(gw.pca)

# Ajouter cette variable à votre objet spatial data2
data2$var_gwpca_pc1 <- var.gwpca.pc1

# Visualiser avec ggplot2
ggplot() +
  geom_sf(data = data2, aes(fill = var_gwpca_pc1)) +
  scale_fill_gradientn(colors = color_palette) +
  labs(title = "Pourcentage de variance totale expliquée par la première composante",
       fill = "% variance") +
  theme_minimal()



# Calculer la proportion de variance expliquée par les DEUX premières composantes
prop.var.pc12 <- function(gwpca.obj) {
  return((rowSums(gwpca.obj$var[, 1:2]) / rowSums(gwpca.obj$var)) * 100)
}

# Appliquer cette fonction à votre objet GWPCA
var.gwpca.pc12 <- prop.var.pc12(gw.pca)

# Ajouter cette variable à votre objet spatial data2
data2$var_gwpca_pc12 <- var.gwpca.pc12

# Visualiser avec ggplot2
ggplot() +
  geom_sf(data = data2, aes(fill = var_gwpca_pc12)) +
  scale_fill_gradientn(colors = color_palette) +
  labs(title = "Pourcentage de variance totale expliquée par les deux premières composantes",
       fill = "% variance") +
  theme_minimal()



# Mouvement de terrain ----
data <- st_read("/Users/noa/Desktop/PRISM/Data/Indicateurs_PRISM/resultats_agregation.gpkg")
colnames(data)

data2 <- data%>%
  dplyr::select(c("code_postal",
                  "MVT_2_MASSIF",
                  "MVT_1_MVT",
                  "MVT_1_CAV"))

CATNAT <- st_read("/Users/noa/Desktop/PRISM/Data/Resultats_CATNAT_MVT_Terrain/frequence_mvt_terrain_cp.gpkg")


# 1. Refaire la fusion sans perdre la géométrie
CATNAT <- CATNAT %>%
  mutate(Code_posta = as.character(Code_posta))

# Important : utiliser st_join (et pas left_join) pour conserver la géométrie
merged_data <- st_join(CATNAT, data2, join = st_equals, left = TRUE)

# 2. Gérer les NA dans nb_inondat
merged_data <- merged_data %>%
  mutate(MVT_CATNAT = ifelse(is.na(frequence), 0, frequence))

# 3. Filtrer et conserver la géométrie
data_to_plot <- merged_data %>%
  dplyr::select("MVT_2_MASSIF",
                "MVT_1_MVT",
                "MVT_1_CAV",
                "MVT_CATNAT",
                geom)
ggplot() +
  geom_sf(data = data_to_plot, aes(fill = MVT_CATNAT)) +
  scale_fill_gradientn(colors = color_palette, na.value = "white") +
  theme_minimal()


sum(is.na(data2))
data3 <- na.omit(data2)
data2 <- data3
sum(is.na(data2))


# Convertir les données sf en données spatiales sp
# On extrait d'abord les coordonnées de la géométrie
coords <- st_coordinates(st_centroid(data2))

# Extraire les données numériques sans la géométrie
data_numeric <- st_drop_geometry(data2) %>%
  dplyr::select(
    MVT_2_MASSIF,
    MVT_1_MVT,
    MVT_1_CAV,
    MVT_CATNAT
  )

sum(is.na(data_numeric)) # Vérifier les valeurs manquantes
data_numeric_clean <- na.omit(data_numeric)

# Standardiser les données
data.scaled <- scale(as.matrix(data_numeric_clean))

# PCA standard pour comparaison
pca <- princomp(data.scaled, cor = FALSE)
cat("Variance expliquée par composante (%):\n")
print((pca$sdev^2 / sum(pca$sdev^2)) * 100)
cat("\nLoadings des composantes principales:\n")
print(pca$loadings)

# Garder trace des indices des lignes sans NA
clean_indices <- which(complete.cases(data_numeric))

# Filtrer les coordonnées pour qu'elles correspondent aux données nettoyées
coords_clean <- coords[clean_indices, ]

# Créer un SpatialPointsDataFrame pour GWmodel
scaled.spdf <- SpatialPointsDataFrame(coords_clean, as.data.frame(data.scaled))



# # Déterminer la bande passante optimale pour GWPCA
# bw.gw.pca <- bw.gwpca(scaled.spdf, 
#                       vars = colnames(scaled.spdf@data),
#                       bw = 100,  # Utiliser la bande passante optimale
#                       k = 3,  # Nombre max de composantes (= nombre de variables)
#                       robust = FALSE, 
#                       adaptive = TRUE)
# cat("\nBande passante optimale:", bw.gw.pca, "\n")

# Réaliser la GWPCA
gw.pca <- gwpca(scaled.spdf, 
                vars = colnames(scaled.spdf@data),
                bw = 100,  # Utiliser la bande passante optimale
                k = 2,  # Nombre max de composantes
                robust = FALSE, 
                adaptive = TRUE)


# Maintenant vous pouvez calculer la proportion de variance expliquée
prop.var <- function(gwpca.obj, n.components) {
  return((rowSums(gwpca.obj$var[, 1:n.components]) /rowSums(gwpca.obj$var)) * 100)
}
var.gwpca <- prop.var(gw.pca, 2)

# Afficher les résultats
summary(var.gwpca)

# Créer un objet sf avec les résultats de la GWPCA
data2$var_gwpca <- var.gwpca

# Déterminer la variable qui contribue le plus à la première composante
loadings.pc1 <- gw.pca$loadings[, , 1]
win.item <- max.col(abs(loadings.pc1))
data2$win_item <- win.item

# Convertir l'objet sf en objet sp pour spplot (si vous préférez utiliser spplot)
data2_sp <- as(data2, "Spatial")

# Calculer la proportion de variance expliquée par la PREMIÈRE composante uniquement
prop.var.pc1 <- function(gwpca.obj) {
  return((gwpca.obj$var[, 1] / rowSums(gwpca.obj$var)) * 100)
}

# Appliquer cette fonction à votre objet GWPCA
var.gwpca.pc1 <- prop.var.pc1(gw.pca)

# Ajouter cette variable à votre objet spatial data2
data2$var_gwpca_pc1 <- var.gwpca.pc1

# Visualiser avec ggplot2
ggplot() +
  geom_sf(data = data2, aes(fill = var_gwpca_pc1)) +
  scale_fill_gradientn(colors = color_palette) +
  labs(title = "Pourcentage de variance totale expliquée par la première composante",
       fill = "% variance") +
  theme_minimal()



# Calculer la proportion de variance expliquée par les DEUX premières composantes
prop.var.pc12 <- function(gwpca.obj) {
  return((rowSums(gwpca.obj$var[, 1:2]) / rowSums(gwpca.obj$var)) * 100)
}

# Appliquer cette fonction à votre objet GWPCA
var.gwpca.pc12 <- prop.var.pc12(gw.pca)

# Ajouter cette variable à votre objet spatial data2
data2$var_gwpca_pc12 <- var.gwpca.pc12

# Visualiser avec ggplot2
ggplot() +
  geom_sf(data = data2, aes(fill = var_gwpca_pc12)) +
  scale_fill_gradientn(colors = color_palette) +
  labs(title = "Pourcentage de variance totale expliquée par les deux premières composantes",
       fill = "% variance") +
  theme_minimal()
