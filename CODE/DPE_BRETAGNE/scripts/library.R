library(dplyr)
library(readxl)
library(corrplot)
library(sf)
library(spdep)
library(tidyr)
library(viridis)
library(scales)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(mapview)
library(cartography)
library(gridExtra)
library(car)
library(spatialreg)
library(extrafont) # Pour changer la police
library(randomForest)
library(xgboost)
library(caret)
library(grid)
library(patchwork)


# # On définit la police des PDF
# theme_times_new_roman <- theme_minimal() +
#   theme(
#     text = element_text(family = "Times"),
#     axis.title = element_text(family = "Times"),
#     axis.text = element_text(family = "Times"),
#     legend.text = element_text(family = "Times")
#   )

# Disables scientific notation
# options(scipen = 999)
options(scipen = 0)


dates_dpe <- data.frame(
  date = as.Date(c("2020-12-17", "2021-03-31", "2021-07-01")),
  label = c(
    "Annonce - Loi DPE 2021",
    "Arrêtés - Loi DPE 2021",
    "Entrée en vigueur - Loi DPE 2021"
  ),
  color = c("darkgrey", "darkgrey", "darkgrey")
)
