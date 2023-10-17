library(tidyverse)
library(questionr)
library(writexl)

#setwd("C:/Users/Tibo/Documents/Demographie/M2S1/UE1 - Analyse Demographique Appliquee/ADA-Rapport-Dept-13/data")


taux_detail <- read_csv("./taux_detail.csv") %>% mutate(CS = substr(type, 6,10))

ggplot(taux_detail) +
  geom_point(aes(x = age_moy, y = ICF, color= CS)) +
  geom_label(aes(x = age_moy, y = ICF, label = type))
  xlab("Age moyen") +
  ylab("Indice conjoncturel de f\u00e9condit\u00e9") +
  theme_light() +
  theme(legend.position= "none")


ggplot(taux_detail) +
  geom_point(aes(x = CS, y = ICF, color= age_moy)) +
  geom_label(aes(x = CS, y = ICF, label = type), nudge_y = 0.05) +
  xlab("Age moyen") +
  ylab("Indice conjoncturel de f\u00e9condit\u00e9") +
  theme_light() #+
  #theme(legend.position= "none")