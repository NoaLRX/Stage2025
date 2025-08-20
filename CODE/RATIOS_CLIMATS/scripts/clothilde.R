source("scripts/packages.R")
source("scripts/import.R")

# Calcul de la charge moyenne & médiane globale 2000-2025
summary(temp$charge)
# Mean : 1846.86    // Median : 918.00
# Mean : 1877   // Median : 999 (BEFORE)

summary(inon$charge)
# Mean : 3608   // Median : 1566
# Mean : 4352   // Median : 1667 (BEFORE)

summary(sech$charge)
# Mean : 23072   // Median : 4800
# Mean : 22212   // Median : 6291 (BEFORE)

unique(df$prism_inon) # 0 1 2 3 4
unique(df$prism_rga) # 0 2 3 4
unique(df$prism_temp) # 0 1 2 3 4

# Calcul % de dégat du sinistre par rapport à la valeur du bien
## INONDATIONS ------------------------------------------------------------------------------------------------------------------------
### GLOBALE
lgv_inon <- inon %>%
  mutate(
    LGV = (charge / valeur_fonciere) * 100
  ) %>%
  filter(LGV < 150)

summary(lgv_inon$LGV)
dim(lgv_inon) # 944 x 12
quartiles <- quantile(lgv_inon$LGV, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

# Bornes des quartiles (issues du summary)
q1 <- quartiles[1]
q2 <- quartiles[2]
q3 <- quartiles[3]

# Q1
q1_result <- lgv_inon %>%
  filter(LGV < q1) %>%
  summarise(
    risque0 = mean(prism_inon == 0) * 100,
    risque1 = mean(prism_inon == 1) * 100,
    risque2 = mean(prism_inon == 2) * 100,
    risque3 = mean(prism_inon == 3) * 100,
    risque4 = mean(prism_inon == 4) * 100
  )


# Q2
q2_result <- lgv_inon %>%
  filter(LGV >= q1 & LGV < q2) %>%
  summarise(
    risque0 = mean(prism_inon == 0) * 100,
    risque1 = mean(prism_inon == 1) * 100,
    risque2 = mean(prism_inon == 2) * 100,
    risque3 = mean(prism_inon == 3) * 100,
    risque4 = mean(prism_inon == 4) * 100
  )

# Q3
q3_result <- lgv_inon %>%
  filter(LGV >= q2 & LGV < q3) %>%
  summarise(
    risque0 = mean(prism_inon == 0) * 100,
    risque1 = mean(prism_inon == 1) * 100,
    risque2 = mean(prism_inon == 2) * 100,
    risque3 = mean(prism_inon == 3) * 100,
    risque4 = mean(prism_inon == 4) * 100
  )

# Q4
q4_result <- lgv_inon %>%
  filter(LGV >= q3) %>%
  summarise(
    risque0 = mean(prism_inon == 0) * 100,
    risque1 = mean(prism_inon == 1) * 100,
    risque2 = mean(prism_inon == 2) * 100,
    risque3 = mean(prism_inon == 3) * 100,
    risque4 = mean(prism_inon == 4) * 100
  )

q1_result
q2_result
q3_result
q4_result


### MAISONS
lgv_inon_maisons <- inon %>%
  filter(type_logement == "maison") %>%
  mutate(
    LGV = (charge / valeur_fonciere) * 100
  )
dim(lgv_inon_maisons) # 693 x 12
summary(lgv_inon_maisons$LGV)

quartiles <- quantile(
  lgv_inon_maisons$LGV,
  probs = c(0.25, 0.50, 0.75),
  na.rm = TRUE
)

# Bornes des quartiles (issues du summary)
q1 <- quartiles[1]
q2 <- quartiles[2]
q3 <- quartiles[3]

lgv_inon <- lgv_inon_maisons
# Q1
q1_result <- lgv_inon %>%
  filter(LGV < q1) %>%
  summarise(
    risque0 = mean(prism_inon == 0) * 100,
    risque1 = mean(prism_inon == 1) * 100,
    risque2 = mean(prism_inon == 2) * 100,
    risque3 = mean(prism_inon == 3) * 100,
    risque4 = mean(prism_inon == 4) * 100
  )


# Q2
q2_result <- lgv_inon %>%
  filter(LGV >= q1 & LGV < q2) %>%
  summarise(
    risque0 = mean(prism_inon == 0) * 100,
    risque1 = mean(prism_inon == 1) * 100,
    risque2 = mean(prism_inon == 2) * 100,
    risque3 = mean(prism_inon == 3) * 100,
    risque4 = mean(prism_inon == 4) * 100
  )

# Q3
q3_result <- lgv_inon %>%
  filter(LGV >= q2 & LGV < q3) %>%
  summarise(
    risque0 = mean(prism_inon == 0) * 100,
    risque1 = mean(prism_inon == 1) * 100,
    risque2 = mean(prism_inon == 2) * 100,
    risque3 = mean(prism_inon == 3) * 100,
    risque4 = mean(prism_inon == 4) * 100
  )

# Q4
q4_result <- lgv_inon %>%
  filter(LGV >= q3) %>%
  summarise(
    risque0 = mean(prism_inon == 0) * 100,
    risque1 = mean(prism_inon == 1) * 100,
    risque2 = mean(prism_inon == 2) * 100,
    risque3 = mean(prism_inon == 3) * 100,
    risque4 = mean(prism_inon == 4) * 100
  )

q1_result
q2_result
q3_result
q4_result


### APPARTEMENTS
lgv_inon_apparts <- inon %>%
  filter(type_logement == "appartement") %>%
  mutate(
    LGV = (charge / valeur_fonciere) * 100
  )

dim(lgv_inon_apparts) # 251 x 12
summary(lgv_inon_apparts$LGV)

quartiles <- quantile(
  lgv_inon_apparts$LGV,
  probs = c(0.25, 0.50, 0.75),
  na.rm = TRUE
)

lgv_inon <- lgv_inon_apparts
# Bornes des quartiles (issues du summary)
q1 <- quartiles[1]
q2 <- quartiles[2]
q3 <- quartiles[3]

# Q1
q1_result <- lgv_inon %>%
  filter(LGV < q1) %>%
  summarise(
    risque0 = mean(prism_inon == 0) * 100,
    risque1 = mean(prism_inon == 1) * 100,
    risque2 = mean(prism_inon == 2) * 100,
    risque3 = mean(prism_inon == 3) * 100,
    risque4 = mean(prism_inon == 4) * 100
  )


# Q2
q2_result <- lgv_inon %>%
  filter(LGV >= q1 & LGV < q2) %>%
  summarise(
    risque0 = mean(prism_inon == 0) * 100,
    risque1 = mean(prism_inon == 1) * 100,
    risque2 = mean(prism_inon == 2) * 100,
    risque3 = mean(prism_inon == 3) * 100,
    risque4 = mean(prism_inon == 4) * 100
  )

# Q3
q3_result <- lgv_inon %>%
  filter(LGV >= q2 & LGV < q3) %>%
  summarise(
    risque0 = mean(prism_inon == 0) * 100,
    risque1 = mean(prism_inon == 1) * 100,
    risque2 = mean(prism_inon == 2) * 100,
    risque3 = mean(prism_inon == 3) * 100,
    risque4 = mean(prism_inon == 4) * 100
  )

# Q4
q4_result <- lgv_inon %>%
  filter(LGV >= q3) %>%
  summarise(
    risque0 = mean(prism_inon == 0) * 100,
    risque1 = mean(prism_inon == 1) * 100,
    risque2 = mean(prism_inon == 2) * 100,
    risque3 = mean(prism_inon == 3) * 100,
    risque4 = mean(prism_inon == 4) * 100
  )

q1_result
q2_result
q3_result
q4_result


## SECHERESSE ------------------------------------------------------------------------------------------------------------------------
### GLOBALE
lgv_sech <- sech %>%
  mutate(
    LGV = (charge / valeur_fonciere) * 100
  ) %>%
  filter(LGV < 200)

dim(lgv_sech) # 21 x 12
summary(lgv_sech$LGV)
quartiles <- quantile(lgv_sech$LGV, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

# Bornes des quartiles (issues du summary)
q1 <- quartiles[1]
q2 <- quartiles[2]
q3 <- quartiles[3]

# Q1
q1_result <- lgv_sech %>%
  filter(LGV < q1) %>%
  summarise(
    risque0 = mean(prism_rga == 0) * 100,
    risque1 = mean(prism_rga == 1) * 100,
    risque2 = mean(prism_rga == 2) * 100,
    risque3 = mean(prism_rga == 3) * 100,
    risque4 = mean(prism_rga == 4) * 100
  )


# Q2
q2_result <- lgv_sech %>%
  filter(LGV >= q1 & LGV < q2) %>%
  summarise(
    risque0 = mean(prism_rga == 0) * 100,
    risque1 = mean(prism_rga == 1) * 100,
    risque2 = mean(prism_rga == 2) * 100,
    risque3 = mean(prism_rga == 3) * 100,
    risque4 = mean(prism_rga == 4) * 100
  )

# Q3
q3_result <- lgv_sech %>%
  filter(LGV >= q2 & LGV < q3) %>%
  summarise(
    risque0 = mean(prism_rga == 0) * 100,
    risque1 = mean(prism_rga == 1) * 100,
    risque2 = mean(prism_rga == 2) * 100,
    risque3 = mean(prism_rga == 3) * 100,
    risque4 = mean(prism_rga == 4) * 100
  )

# Q4
q4_result <- lgv_sech %>%
  filter(LGV >= q3) %>%
  summarise(
    risque0 = mean(prism_rga == 0) * 100,
    risque1 = mean(prism_rga == 1) * 100,
    risque2 = mean(prism_rga == 2) * 100,
    risque3 = mean(prism_rga == 3) * 100,
    risque4 = mean(prism_rga == 4) * 100
  )

q1_result
q2_result
q3_result
q4_result


### MAISONS
lgv_sech_maisons <- sech %>%
  filter(type_logement == "maison") %>%
  mutate(
    LGV = (charge / valeur_fonciere) * 100
  ) %>%
  filter(LGV < 200)

dim(lgv_sech_maisons) # 16 x 12
summary(lgv_sech_maisons$LGV)

quartiles <- quantile(
  lgv_sech_maisons$LGV,
  probs = c(0.25, 0.50, 0.75),
  na.rm = TRUE
)

# Bornes des quartiles (issues du summary)
q1 <- quartiles[1]
q2 <- quartiles[2]
q3 <- quartiles[3]

lgv_sech <- lgv_sech_maisons
# Q1
q1_result <- lgv_sech %>%
  filter(LGV < q1) %>%
  summarise(
    risque0 = mean(prism_rga == 0) * 100,
    risque1 = mean(prism_rga == 1) * 100,
    risque2 = mean(prism_rga == 2) * 100,
    risque3 = mean(prism_rga == 3) * 100,
    risque4 = mean(prism_rga == 4) * 100
  )


# Q2
q2_result <- lgv_sech %>%
  filter(LGV >= q1 & LGV < q2) %>%
  summarise(
    risque0 = mean(prism_rga == 0) * 100,
    risque1 = mean(prism_rga == 1) * 100,
    risque2 = mean(prism_rga == 2) * 100,
    risque3 = mean(prism_rga == 3) * 100,
    risque4 = mean(prism_rga == 4) * 100
  )

# Q3
q3_result <- lgv_sech %>%
  filter(LGV >= q2 & LGV < q3) %>%
  summarise(
    risque0 = mean(prism_rga == 0) * 100,
    risque1 = mean(prism_rga == 1) * 100,
    risque2 = mean(prism_rga == 2) * 100,
    risque3 = mean(prism_rga == 3) * 100,
    risque4 = mean(prism_rga == 4) * 100
  )

# Q4
q4_result <- lgv_sech %>%
  filter(LGV >= q3) %>%
  summarise(
    risque0 = mean(prism_rga == 0) * 100,
    risque1 = mean(prism_rga == 1) * 100,
    risque2 = mean(prism_rga == 2) * 100,
    risque3 = mean(prism_rga == 3) * 100,
    risque4 = mean(prism_rga == 4) * 100
  )

q1_result
q2_result
q3_result
q4_result


### APPARTEMENTS
lgv_sech_apparts <- sech %>%
  filter(type_logement == "appartement") %>%
  mutate(
    LGV = (charge / valeur_fonciere) * 100
  ) %>%
  filter(LGV < 200)

dim(lgv_sech_apparts)
summary(lgv_sech_apparts$LGV)

quartiles <- quantile(
  lgv_sech_apparts$LGV,
  probs = c(0.25, 0.50, 0.75),
  na.rm = TRUE
)

lgv_sech <- lgv_sech_apparts
# Bornes des quartiles (issues du summary)
q1 <- quartiles[1]
q2 <- quartiles[2]
q3 <- quartiles[3]

# Q1
q1_result <- lgv_sech %>%
  filter(LGV < q1) %>%
  summarise(
    risque0 = mean(prism_rga == 0) * 100,
    risque1 = mean(prism_rga == 1) * 100,
    risque2 = mean(prism_rga == 2) * 100,
    risque3 = mean(prism_rga == 3) * 100,
    risque4 = mean(prism_rga == 4) * 100
  )


# Q2
q2_result <- lgv_sech %>%
  filter(LGV >= q1 & LGV < q2) %>%
  summarise(
    risque0 = mean(prism_rga == 0) * 100,
    risque1 = mean(prism_rga == 1) * 100,
    risque2 = mean(prism_rga == 2) * 100,
    risque3 = mean(prism_rga == 3) * 100,
    risque4 = mean(prism_rga == 4) * 100
  )

# Q3
q3_result <- lgv_sech %>%
  filter(LGV >= q2 & LGV < q3) %>%
  summarise(
    risque0 = mean(prism_rga == 0) * 100,
    risque1 = mean(prism_rga == 1) * 100,
    risque2 = mean(prism_rga == 2) * 100,
    risque3 = mean(prism_rga == 3) * 100,
    risque4 = mean(prism_rga == 4) * 100
  )

# Q4
q4_result <- lgv_sech %>%
  filter(LGV >= q3) %>%
  summarise(
    risque0 = mean(prism_rga == 0) * 100,
    risque1 = mean(prism_rga == 1) * 100,
    risque2 = mean(prism_rga == 2) * 100,
    risque3 = mean(prism_rga == 3) * 100,
    risque4 = mean(prism_rga == 4) * 100
  )

q1_result
q2_result
q3_result
q4_result


## TEMPETE ------------------------------------------------------------------------------------------------------------------------
### GLOBALE
lgv_temp <- temp %>%
  mutate(
    LGV = (charge / valeur_fonciere) * 100
  ) %>%
  filter(LGV < 150)

dim(lgv_temp) # 20870 x 12
summary(lgv_temp$LGV)
quartiles <- quantile(lgv_temp$LGV, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

# Bornes des quartiles (issues du summary)
q1 <- quartiles[1]
q2 <- quartiles[2]
q3 <- quartiles[3]

# Q1
q1_result <- lgv_temp %>%
  filter(LGV < q1) %>%
  summarise(
    risque0 = mean(prism_temp == 0) * 100,
    risque1 = mean(prism_temp == 1) * 100,
    risque2 = mean(prism_temp == 2) * 100,
    risque3 = mean(prism_temp == 3) * 100,
    risque4 = mean(prism_temp == 4) * 100
  )


# Q2
q2_result <- lgv_temp %>%
  filter(LGV >= q1 & LGV < q2) %>%
  summarise(
    risque0 = mean(prism_temp == 0) * 100,
    risque1 = mean(prism_temp == 1) * 100,
    risque2 = mean(prism_temp == 2) * 100,
    risque3 = mean(prism_temp == 3) * 100,
    risque4 = mean(prism_temp == 4) * 100
  )

# Q3
q3_result <- lgv_temp %>%
  filter(LGV >= q2 & LGV < q3) %>%
  summarise(
    risque0 = mean(prism_temp == 0) * 100,
    risque1 = mean(prism_temp == 1) * 100,
    risque2 = mean(prism_temp == 2) * 100,
    risque3 = mean(prism_temp == 3) * 100,
    risque4 = mean(prism_temp == 4) * 100
  )

# Q4
q4_result <- lgv_temp %>%
  filter(LGV >= q3) %>%
  summarise(
    risque0 = mean(prism_temp == 0) * 100,
    risque1 = mean(prism_temp == 1) * 100,
    risque2 = mean(prism_temp == 2) * 100,
    risque3 = mean(prism_temp == 3) * 100,
    risque4 = mean(prism_temp == 4) * 100
  )

q1_result
q2_result
q3_result
q4_result


### MAISONS
lgv_temp_maisons <- temp %>%
  filter(type_logement == "maison") %>%
  mutate(
    LGV = (charge / valeur_fonciere) * 100
  ) %>%
  filter(LGV < 150)

dim(lgv_temp_maisons) # 17575 x 12
summary(lgv_temp_maisons$LGV)
quartiles <- quantile(
  lgv_temp_maisons$LGV,
  probs = c(0.25, 0.50, 0.75),
  na.rm = TRUE
)

# Bornes des quartiles (issues du summary)
q1 <- quartiles[1]
q2 <- quartiles[2]
q3 <- quartiles[3]

lgv_temp <- lgv_temp_maisons
# Q1
q1_result <- lgv_temp %>%
  filter(LGV < q1) %>%
  summarise(
    risque0 = mean(prism_temp == 0) * 100,
    risque1 = mean(prism_temp == 1) * 100,
    risque2 = mean(prism_temp == 2) * 100,
    risque3 = mean(prism_temp == 3) * 100,
    risque4 = mean(prism_temp == 4) * 100
  )


# Q2
q2_result <- lgv_temp %>%
  filter(LGV >= q1 & LGV < q2) %>%
  summarise(
    risque0 = mean(prism_temp == 0) * 100,
    risque1 = mean(prism_temp == 1) * 100,
    risque2 = mean(prism_temp == 2) * 100,
    risque3 = mean(prism_temp == 3) * 100,
    risque4 = mean(prism_temp == 4) * 100
  )

# Q3
q3_result <- lgv_temp %>%
  filter(LGV >= q2 & LGV < q3) %>%
  summarise(
    risque0 = mean(prism_temp == 0) * 100,
    risque1 = mean(prism_temp == 1) * 100,
    risque2 = mean(prism_temp == 2) * 100,
    risque3 = mean(prism_temp == 3) * 100,
    risque4 = mean(prism_temp == 4) * 100
  )

# Q4
q4_result <- lgv_temp %>%
  filter(LGV >= q3) %>%
  summarise(
    risque0 = mean(prism_temp == 0) * 100,
    risque1 = mean(prism_temp == 1) * 100,
    risque2 = mean(prism_temp == 2) * 100,
    risque3 = mean(prism_temp == 3) * 100,
    risque4 = mean(prism_temp == 4) * 100
  )

q1_result
q2_result
q3_result
q4_result


### APPARTEMENTS
lgv_temp_apparts <- temp %>%
  filter(type_logement == "appartement") %>%
  mutate(
    LGV = (charge / valeur_fonciere) * 100
  ) %>%
  filter(LGV < 150)

dim(lgv_temp_apparts) # 3295 x 12
summary(lgv_temp_apparts$LGV)

quartiles <- quantile(
  lgv_temp_apparts$LGV,
  probs = c(0.25, 0.50, 0.75),
  na.rm = TRUE
)

lgv_temp <- lgv_temp_apparts
# Bornes des quartiles (issues du summary)
q1 <- quartiles[1]
q2 <- quartiles[2]
q3 <- quartiles[3]

# Q1
q1_result <- lgv_temp %>%
  filter(LGV < q1) %>%
  summarise(
    risque0 = mean(prism_temp == 0) * 100,
    risque1 = mean(prism_temp == 1) * 100,
    risque2 = mean(prism_temp == 2) * 100,
    risque3 = mean(prism_temp == 3) * 100,
    risque4 = mean(prism_temp == 4) * 100
  )


# Q2
q2_result <- lgv_temp %>%
  filter(LGV >= q1 & LGV < q2) %>%
  summarise(
    risque0 = mean(prism_temp == 0) * 100,
    risque1 = mean(prism_temp == 1) * 100,
    risque2 = mean(prism_temp == 2) * 100,
    risque3 = mean(prism_temp == 3) * 100,
    risque4 = mean(prism_temp == 4) * 100
  )

# Q3
q3_result <- lgv_temp %>%
  filter(LGV >= q2 & LGV < q3) %>%
  summarise(
    risque0 = mean(prism_temp == 0) * 100,
    risque1 = mean(prism_temp == 1) * 100,
    risque2 = mean(prism_temp == 2) * 100,
    risque3 = mean(prism_temp == 3) * 100,
    risque4 = mean(prism_temp == 4) * 100
  )

# Q4
q4_result <- lgv_temp %>%
  filter(LGV >= q3) %>%
  summarise(
    risque0 = mean(prism_temp == 0) * 100,
    risque1 = mean(prism_temp == 1) * 100,
    risque2 = mean(prism_temp == 2) * 100,
    risque3 = mean(prism_temp == 3) * 100,
    risque4 = mean(prism_temp == 4) * 100
  )

q1_result
q2_result
q3_result
q4_result
