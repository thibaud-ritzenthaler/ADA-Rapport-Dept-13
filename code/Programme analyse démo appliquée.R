## Analyse démographique appliquée ##
library(tidyverse)
#library(FactoMineR)
#library(factoextra)
#library(psych)
#library(questionr)
#library(gtsummary)
library(readxl)
#library(labelled)
#library(GGally)
#library(survey)
#library(explor)
library(reshape2)

# setwd("C:/Users/abdel/Desktop/Cours Master/Semestre 3/Analyse démographique appliquée")
# setwd("C:/Users/Tibo/Documents/Demographie/M2S1/UE1 - Analyse Démographique Appliquée/ADA-Rapport-Dept-13/code")


##Nuage de points ICF / Âge moyen de la mère ## 

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


##Rapport de dépendance 

Pjeune <- sum(Age$Hommes1[1:15])+sum(Age$Femmes[1:15])
Pvieux <- sum(Age$Hommes1[65:101])+sum(Age$Femmes[65:101])
Pactif <- sum(Age$Hommes1[16:64])+sum(Age$Femmes[16:64])

sum(Pjeune+Pvieux)/Pactif

## Part de la population âgée de 64 ans et + ##

(Pvieux/(Pjeune+Pactif))*100


##Préparation & Chargement de la base de ses morts ## 

#  RP<-read.csv("FD_INDREGZE_2020.csv", sep = ";")
#  RP2020<-filter(FD_INDREGZE_2020,DEPT=="13")
#  PopPonder=sum(RP2020$IPONDI)
#  save(RP2020,file="C:/Users/abdel/Desktop/Cours Master/Semestre 3/Analyse démographique appliquée/RP2020.rdata")

load("RP2020.rdata")
PopPonder=sum(RP2020$IPONDI)
