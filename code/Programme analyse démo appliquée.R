## Analyse démographique appliquée ##
library(tidyverse)
#library(FactoMineR)
#library(factoextra)
#library(psych)
library(questionr)
#library(gtsummary)
library(readxl)
#library(labelled)
#library(GGally)
#library(survey)
#library(explor)
library(reshape2)


setwd("C:/Users/abdel/Desktop/Cours Master/Git_dossier/ADA-Rapport-Dept-13/data")

# setwd("C:/Users/Tibo/Documents/Demographie/M2S1/UE1 - Analyse Démographique Appliquée/ADA-Rapport-Dept-13/code")


##Création du nuage de points ICF ~ Âge moyen de la mère ## 

## Ajout de la base et création d'une variable permettant de différencier le département des autres ##

Base <-read_excel("p3d.xlsx",sheet="2020") %>% mutate(isBouchesDuRhone =case_when(DEP == "Bouches-du-Rhône" ~ "Bouches-du-Rhône", TRUE ~ ""))

Legend<-data.frame(
  x=c(29,29,34,34),
  y=c(1.25,2.25,2.25,1.25),
  text=c("Calendrier plus précoce,\n fécondité du moment plus faible","Calendrier plus précoce,\n fécondité du moment plus forte","Calendrier plus tardif,\n fécondité du moment plus forte","Calendrier plus précoce,\n fécondité du moment plus faible")
)

ggplot(data = Base) +
  geom_point(aes(x = AGEMOY, y = ICF, size = NAISS, alpha = isBouchesDuRhone, color = isBouchesDuRhone, )) +
  geom_text(aes(x = AGEMOY, y = ICF, label = isBouchesDuRhone), nudge_y = -0.025) +
  geom_text(data = Legend, aes(x = x, y = y, label=text), vjust= "inward", hjust="inward") +
  scale_color_manual("Departement", values = c("blue", "gold")) + 
  geom_hline(yintercept=1.78)+
  geom_vline(xintercept=30.4) +
  theme_light() +
  xlab("Age moyen de la mère") +
  ylab("Indice conjoncturel de fécondité") + 
  theme(legend.position = "none")

#######################################################################################################################################

## Pyramide des âges 2020 ##

Age <- read_excel("Age2020.xlsx")

donnees_regroup <- melt(Age, id.vars ="Ages",measure.vars=c("Hommes","Femmes"))

ggplot(donnees_regroup,aes(x=Ages, y=value, fill= variable, group = variable)) +
  geom_bar(stat="identity",position="stack")+
  coord_flip()+
  labs(title="Pyramide des âges",
       x="Âge",
       y= "Population")+
  theme_minimal()

##########################################################################################################################################
#Naissances BDR
naiss<-read_excel("FD_NAIS_2021BDR.xlsx")

#Table de Population de femmes 15-49 ans BDR
Femmes15_49 <- read_excel("Femmes15-49.xlsx")


## Recodage de naiss$AGEMERE en naiss$AGEMERE_rec
naiss$AGEMERE_rec <- as.character(naiss$AGEMERE)

naiss$AGEMERE_rec[naiss$AGEMERE == "15"] <- "15 à 19 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "16"] <- "15 à 19 ans"
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
naiss$AGEMERE_rec[naiss$AGEMERE == "47"] <- "45 à 49 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "48"] <- "45 à 49 ans"
naiss$AGEMERE_rec[naiss$AGEMERE == "49"] <- "45 à 49 ans"

naiss |> count (AGEMERE_rec)


#Liste de valeurs d'ages de la mère de 15-49 ans par groupes d'âge de 5 ans.
Nbnaiss <- c(251, 2167, 6625, 9225, 5627, 1575, 114)


Naisparage <- data.frame(Nbnaiss)

# On conbine les tables nais par age de la mère et pop de femmes 15-49 ans
tpa<-cbind(Femmes15_49,Naisparage)

#calcul de taux par âge
tpa<-mutate(tpa, Tauxparage=Nbnaiss/Popfm*100)

tpa<-mutate(tpa, Tauxparage5=Nbnaiss/Popfm*5)

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

##########################################################################################################################################

##Rapport de dépendance 

Pjeune <- sum(Age$Hommes1[1:15])+sum(Age$Femmes[1:15])
Pvieux <- sum(Age$Hommes1[65:101])+sum(Age$Femmes[65:101])
Pactif <- sum(Age$Hommes1[16:64])+sum(Age$Femmes[16:64])

sum(Pjeune+Pvieux)/Pactif

## Part de la population âgée de 64 ans et + ##

(Pvieux/(Pjeune+Pactif))*100

##########################################################################################################################################
##Préparation & Chargement de la base de ses morts ## 

#  RP<-read.csv("FD_INDREGZE_2020.csv", sep = ";")
#  RP2020<-filter(FD_INDREGZE_2020,DEPT=="13")
#  PopPonder=sum(RP2020$IPONDI)
#  save(RP2020,file="C:/Users/abdel/Desktop/Cours Master/Semestre 3/Analyse démographique appliquée/RP2020.rdata")

load("RP2020.rdata")
PopPonder=sum(RP2020$IPONDI)
