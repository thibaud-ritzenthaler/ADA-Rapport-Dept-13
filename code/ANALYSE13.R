#Chargement des packages
library(tidyverse)
library(questionr)


#setwd("C:/Users/Tibo/Documents/Demographie/M2S1/UE1 - Analyse Demographique Appliquee/ADA-Rapport-Dept-13")

#Adele
setwd("/Users/adelejnd/Desktop/ADA-Rapport-Dept-13/data")

#On charge juste le fichier déjà filtré
load("./BDR2020.rdata")
bdr2020 <- BDR2020
rm(BDR2020)

# On fait la somme de la variable ipondi pour connaitre la population du departement
pop13 <- sum(bdr2020$IPONDI)

# Identifiant du menage
bdr2020$idmen <- paste0(bdr2020$REGION,bdr2020$NUMMR)

# Identifiant de la famille
bdr2020$idfam <- paste0(bdr2020$REGION,bdr2020$NUMMR,bdr2020$NUMF)

# On cree une table avec les enfants de 1 an et de 0 an
un_an <- filter(bdr2020, AGED == 1)
zero_an <- filter(bdr2020, AGED == 0)

un_an_p <- sum(un_an$IPONDI)
zero_an_p <- sum(zero_an$IPONDI)

# Pour avoir 0 an en ddm l'ann,ee du recensement il faut etre ne en janvier, c'est pour cela qu'ils sont peu nombreux
# Les chiffres precedents sont des stats de stock, le chiffre des enfants de 1 an va etre proche des naissances de l'annee 2019

# Pour avoir le nombre d'enfants de un ou zero an en fonction des annees de naissance
freq <- table(un_an$ANAI)
freq_zero2 <- table(zero_an$ANAI)
freq_zero <- zero_an %>% count(ANAI)

# On va dire que les enfants de 1 an du recensement 2020 sont nes en 2019, on regarde pas l'annee de naissance

# On filtre pour avoir les personnes de ref de la famille
quest4 <- filter(bdr2020, (MOCO == 21 | MOCO == 22) & LPRF == 1)
sum(quest4$IPONDI)

# Sans ponderation
quest4ter <- quest4 %>% count(SEXE)

# Avec les ponderations
quest4pond <- wtd.table(quest4$SEXE, weights = quest4$IPONDI)
# Pourcentage
prop.table(quest4pond)*100

# On cree tables enfants
enfants_potentiels <- filter(bdr2020, AGED == 1 & LPRF == 3)
sum(enfants_potentiels$IPONDI)

# On cree la table des meres potentielles
femmes_procreer <- filter(bdr2020, (SEXE==2) & (AGED>15 & AGED<=50) & (LPRF==1 | LPRF==2))
sum(femmes_procreer$IPONDI)

# On selectionnes les variables utiles
enfants_potentiels <- select(enfants_potentiels, idfam, LPRF, AGED, DEPT, IPONDI)
colnames(enfants_potentiels) <- c("idfam", "LPRF_enf", "AGED_enf", "DEPT_enf", "IPONDI_enf")
femmes_procreer <- select(femmes_procreer, idfam, LPRF, AGED, DEPT, IPONDI)
bdd <- merge(femmes_procreer, enfants_potentiels, by="idfam")

# Pour les naissances
nais <- as.data.frame(wtd.table(bdd$AGED, weights = bdd$IPONDI))

# Pour les meres
meres <- as.data.frame(wtd.table(femmes_procreer$AGED, weights = femmes_procreer$IPONDI))

# Calcul des taux par age
taux <- as.data.frame(nais$Freq/meres$Freq)
sum(taux$`nais$Freq/meres$Freq`)

#Table femmes
femmes <- filter(bdr2020, SEXE==2 & (AGED>15 & AGED<=50))
femmes <- select(femmes, idfam, LPRF, AGED, DEPT, IPONDI)
femmes_tab <- as.data.frame(wtd.table(femmes$AGED, weights = femmes$IPONDI))

tauxbis <- merge(femmes_tab, nais, by="Var1")
tauxbis$taux <- tauxbis$Freq.y/tauxbis$Freq.x
sum(tauxbis$taux)

# Age moyen des meres, à priori pas bon
meres$age <- seq(from=16, to=50)
meres$agemoy <- meres$age * meres$Freq
sum(meres$agemoy)/sum(meres$Freq)

#Age moyen des femmes, pondéré avec les taux par âge 
tauxbis$age <- seq(from=16, to=50)
result <- tauxbis %>%
  summarize(AgeM = weighted.mean(age, w = taux))







