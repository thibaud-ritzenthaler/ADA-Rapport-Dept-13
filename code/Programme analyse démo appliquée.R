## Analyse démographique appliquée ##
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(psych)
library(questionr)
library(gtsummary)
library(readxl)
library(labelled)
library(GGally)
library(survey)
library(explor)

setwd("C:/Users/abdel/Desktop/Cours Master/Semestre 3/Analyse démographique appliquée")
Base <-read_excel("irsocsd2013_p3d_f.xls", sheet="Traitement")

##Nuage de points ICF / Âge moyen de la mère ##

plot(ICF~AGEMOY,
     xlab="Age moyen de la mère à la première naissance",
     ylab="ICF",
     col= ifelse(Base$DEP=="Bouches-du-Rhône","red",
                 ifelse(Base$DEP=="France","blue","gray")),pch=3,cex=1.5,
     data=Base)

couleurs <- c("red","blue","gray")
noms <- c("Bouches-du-Rhône","France","Autre département")
legend("topright",legend=noms,col=couleurs,pch=3,title="ICF et âge moyen de la mère selon les départements")


## Pyramide des âges 2020 ##

library(ggplot2)
library(reshape2)
Age <- read_excel("Age2020.xlsx")

donnees_regroup <- melt(Age, id.vars ="Ages",measure.vars=c("Hommes","Femmes"))

ggplot(donnees_regroup,aes(x=Ages, y=value, fill= variable, group = variable)) +
  geom_bar(stat="identity",position="dodge")+
  coord_flip()+
  labs(title="Pyramide des âges",
       x="Âge",
       y= "Population")+
  theme_minimal()


