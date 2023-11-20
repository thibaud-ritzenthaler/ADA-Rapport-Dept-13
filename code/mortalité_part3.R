#On charge les librairies
library(tidyverse)
library(questionr)

#MORTALITÉ DERNIÈRE SÉANCE

#On importe les données d'état civil et de recensement
load("/Users/adelejnd/Desktop/ADA-Rapport-Dept-13/data/DC19.rdata")
load("/Users/adelejnd/Desktop/ADA-Rapport-Dept-13/data/RP2020.rdata")
FD_INDREGZE_2019 <- read.csv("~/Desktop/ADA-Rapport-Dept-13/data/FD_INDREGZE_2019.csv", sep=";")

#On filtre pour avoir seulement le département 13
DC19$DEPDEC <- as.numeric(as.character(DC19$DEPDEC))
DC19_13 <- filter(DC19,DEPDOM==13)

RP19_13 <- filter(FD_INDREGZE_2019,DEPT==13)
save("/Users/adelejnd/Desktop/ADA-Rapport-Dept-13/data/RP19_13.rdata")

#On veut calculer des taux de mortalité 