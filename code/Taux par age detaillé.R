library(tidyverse)
library(questionr)
library(readxl)

naiss<-read_excel("FD_NAIS_2021BDR.xlsx")
load("./BDR2020.rdata")
bdr2020 <- BDR2020

#femmes 16-46 en age detaillé (il ya pas age de la mere pour <15 ni pour >46)
femmes_procreer <- filter(bdr2020, (SEXE==2) & (AGED>16 & AGED<=46))

femmes_procreerpage <- femmes_procreer %>%
  group_by(AGED) %>%
  summarize(effectif = sum(IPONDI))

sum(femmes_procreerpage$effectif)

#Naissances par age de la mère ----
naiss_par_age <- naiss %>%
  group_by(AGEMERE) %>%
  summarize(naissances = n())

colnames(naiss_par_age)[colnames(naiss_par_age) == "AGEMERE"] <- "AGED"

#taux par age table 

taux_par_age <- merge(naiss_par_age, femmes_procreerpage, by = "AGED", all = TRUE)

taux_par_age <- taux_par_age %>%
  mutate(taux = naissances / effectif)

ICF <- sum(taux_par_age$taux)

#courbe

ggplot(taux_par_age) +
  aes(x = AGED, y = taux) +
  geom_point(shape = "circle", size = 3, colour = "#112446") +
  geom_line(aes(group = 1), color = "red") +
  theme_minimal() + 
  scale_y_continuous(limits = c(0, 0.2), breaks = seq(0, 0.25, by = 0.05)) +
  labs(x = "Âge", y = "Taux de fécondité par age")
