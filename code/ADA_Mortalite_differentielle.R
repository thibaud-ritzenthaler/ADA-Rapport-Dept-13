#chargement packages
library(foreign) ; library(tidyverse)

#importation des données
#setwd("C:/Users/Tibo/Documents/Demographie/M2S1/UE1 - Analyse Demographique Appliquee/ADA-Rapport-Dept-13/code")

load("../data/DC19_13.Rdata") ; load("../data/RP19_13.Rdata") ; load("../data/RP2020.Rdata")
source("./hly_function.R")

#Calculer les taux de mortalité
#1) Le numérateur : nombre de décès par âge et lieu de naissance
DC19_13$age <- 2019-as.numeric(as.character(DC19_13$ANAIS)) #création d'une variable âge
DC19_13$depnais2 <- ifelse(DC19_13$DEPNAIS == "13", "natif", "non-natif")

NUM <- group_by(DC19_13, age, depnais2) %>%
  summarize(nb_dec = n()) %>%
  ungroup() %>%
  select(age, depnais2, nb_dec)

#2) Le dénominateur : population moyenne par sexe et âge
RP19_13$depnais2 <- ifelse(RP19_13$INAI == "1", "natif", "non-natif")
RP2020$depnais2 <- ifelse(RP2020$INAI == "1", "natif", "non-natif")

#population par dep de naissance et âge en 2014
DENOM19 <- group_by(RP19_13, ANAI, depnais2) %>%
  summarize(pop_19 = sum(IPONDI)) %>%
  ungroup() %>%
  select(ANAI, depnais2, pop_19) 

#population par dep de naissance et âge en 2015
DENOM20 <- group_by(RP2020, ANAI, depnais2) %>%
  summarize(pop_20 = sum(IPONDI)) %>%
  ungroup () %>%
  select(ANAI, depnais2, pop_20)


DENOM <- merge(DENOM19, DENOM20, by = c("ANAI", "depnais2"), all = TRUE)
DENOM$pop_moy <- (DENOM$pop_19 + DENOM$pop_20)/2
DENOM$age <- 2019 - as.numeric(as.character(DENOM$ANAI))
DENOM <- filter(DENOM, age >=0) %>% #on supprime les personnes nées après 2019 : elles ne courraient pas le risque de décéder en 2019
  select(age, depnais2, pop_moy) 

#3) on regroupe les deux tables et on fait le calcul du taux
DATA <- merge(NUM, DENOM, by = c("age", "depnais2"), all = TRUE)
DATA[is.na(DATA)] <- 0

DATA <- filter(DATA, age <= 100 & age >= 20)
DATA$mx <- DATA$nb_dec/DATA$pop_moy

DATA_natif <- filter(DATA, depnais2 == "natif")
DATA_nonnatif <- filter(DATA, depnais2 == "non-natif")

EV_natif <- hly(DATA_natif)
EV_nonnatif <- hly(DATA_nonnatif)

save(EV_natif, file = "../data/EV_natif_13.Rdata")
save(EV_nonnatif, file =  "../data/EV_non_natif_13.Rdata")