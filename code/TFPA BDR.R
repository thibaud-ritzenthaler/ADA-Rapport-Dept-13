library(questionr)
library(readxl)
library(dplyr)
library(tidyverse)

#Naissances BDR
naiss<-read_excel("D:/Master 2/Analyse démo appliquée/FD_NAIS_2021BDR.xlsx")

#Table de Population de femmes 15-49 ans BDR
Femmes15_49 <- read_excel("D:/Master 2/Analyse démo appliquée/Femmes15-49.xlsx")


## Recodage de naiss$AGEMERE en naiss$AGEMERE_rec
naiss$AGEMERE_rec <- as.character(naiss$AGEMERE)
naiss$AGEMERE_rec[naiss$AGEMERE == "17"] <- "15 à 19 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "18"] <- "15 à 19 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "19"] <- "15 à 19 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "20"] <- "20 à 24 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "21"] <- "20 à 24 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "22"] <- "20 à 24 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "23"] <- "20 à 24 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "24"] <- "20 à 24 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "25"] <- "25 à 29 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "26"] <- "25 à 29 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "27"] <- "25 à 29 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "28"] <- "25 à 29 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "29"] <- "25 à 29 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "30"] <- "30 à 34 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "31"] <- "30 à 34 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "32"] <- "30 à 34 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "33"] <- "30 à 34 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "34"] <- "30 à 34 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "35"] <- "35 à 39 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "36"] <- "35 à 39 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "37"] <- "35 à 39 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "38"] <- "35 à 39 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "39"] <- "35 à 39 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "40"] <- "40 à 44 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "41"] <- "40 à 44 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "42"] <- "40 à 44 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "43"] <- "40 à 44 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "44"] <- "40 à 44 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "45"] <- "45 à 49 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "46"] <- "45 à 49 ans"

naiss |> count (AGEMERE_rec)


#Liste de valeurs d'ages de la mère de 15-49 ans par groupes d'âge de 5 ans.
data <- c(251, 2167, 6625, 9225, 5627, 1575, 114)


Naisparage <- data.frame(data)

# On conbine les tables nais par age de la mère et pop de femmes 15-49 ans
tpa<-cbind(Femmes15_49,Naisparage)

#calcul de taux par âge
tpa<-mutate(tpa, Tauxparage=data/Popfm*100)



tpa<-mutate(tpa, Tauxparage5=data/Popfm*5)

#ICF Bouche du rhone
ICFBDR<-sum(tpa$Tauxparage5)


#Graphique taux de fécondité par âge
ggplot(tpa) +
  aes(x = Age, y = Tauxparage) +
  geom_point(shape = "circle", size = 3, colour = "#112446") +
  geom_line(aes(group = 1), color = "red") +
  theme_minimal() + 
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5)) +
  labs(x = "Âge quinquennal", y = "Taux de fécondité par 100 ind")


