# Chargement des packages
library(tidyverse)
library(questionr)
library(writexl)

#setwd("C:/Users/Tibo/Documents/Demographie/M2S1/UE1 - Analyse Demographique Appliquee/ADA-Rapport-Dept-13/data")
#setwd("C:/Users/abdel/Desktop/Cours Master/Git_dossier/ADA-Rapport-Dept-13/data")

# Adele
# setwd("/Users/adelejnd/Desktop/ADA-Rapport-Dept-13/data")

# On charge juste le fichier déjà filtré
load("./BDR2020.rdata")
bdr2020 <- BDR2020
rm(BDR2020)

# On enlève les Z, logements pas ordinaires
bdr2020 <- filter(bdr2020,TYPL!="Z")

# On fait la somme de la variable ipondi pour connaitre la population du departement
pop13 <- sum(bdr2020$IPONDI)

# Identifiant du menage
bdr2020$idmen <- paste0(bdr2020$REGION,bdr2020$NUMMR)

# Identifiant de la famille
bdr2020$idfam <- paste0(bdr2020$REGION,bdr2020$NUMMR,bdr2020$NUMF)

# On cree une table avec les enfants de 1 an et de 0 an
un_an <- filter(bdr2020, AGED == 1)
table(un_an$LPRF)
zero_an <- filter(bdr2020, AGED == 0)
un_an_p <- sum(un_an$IPONDI)
zero_an_p <- sum(zero_an$IPONDI)

# Pour avoir 0 an en ddm l'annee du recensement il faut etre ne en janvier, c'est pour cela qu'ils sont peu nombreux
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

#Méthode DEF

# On cree la table des enfants potentiels
enfants_potentiels <- filter(bdr2020, AGED == 1 & LPRF == 3)
sum(enfants_potentiels$IPONDI)

# Table femmes
femmes <- filter(bdr2020, SEXE==2 & (AGED>15 & AGED<=50))
femmes <- select(femmes, idfam, LPRF, AGED, CS1, DEPT, IPONDI, TACT)

# On cree des tables suivant la CSP des femmes
fcsp12 <- filter(femmes,CS1 %in% c("1","2"))
fcsp34 <- filter(femmes,CS1 %in% c("3","4"))
fcsp56 <- filter(femmes,CS1 %in% c("5","6"))
fcsp8 <- filter(femmes,CS1=="8")

# On cree une table des femmes avec la CSP croisée avec le statut d'activite
fcsp12_tact_11 <- filter(fcsp12, TACT==11)
fcsp12_tact_12 <- filter(fcsp12, TACT==12)
fcsp34_tact_11 <- filter(fcsp34, TACT==11)
fcsp34_tact_12 <- filter(fcsp34, TACT==12)
fcsp56_tact_11 <- filter(fcsp56, TACT==11)
fcsp56_tact_12 <- filter(fcsp56, TACT==12)
fcsp8_tact_12 <- filter(fcsp8, TACT==12)
fcsp8_tact_autres <- filter(fcsp8, TACT!=12)

# Table des meres potentielles
femmes_procreer <- filter(bdr2020, (SEXE==2) & (AGED>15 & AGED<=50) & (LPRF==1 | LPRF==2))
femmes_procreer <- select(femmes_procreer, idfam, LPRF, AGED, CS1, DEPT, IPONDI, TACT)
sum(femmes_procreer$IPONDI)

# On cree des tables suivant la CSP des meres
mcsp12 <- filter(femmes_procreer,CS1 %in% c("1","2"))
mcsp34 <- filter(femmes_procreer,CS1 %in% c("3","4"))
mcsp56 <- filter(femmes_procreer,CS1 %in% c("5","6"))
mcsp8 <- filter(femmes_procreer,CS1=="8")

#On cree table des meres avec csp croisee avec statut
mcsp12_tact_11 <- filter(mcsp12, TACT==11)
mcsp12_tact_12 <- filter(mcsp12, TACT==12)
mcsp34_tact_11 <- filter(mcsp34, TACT==11)
mcsp34_tact_12 <- filter(mcsp34, TACT==12)
mcsp56_tact_11 <- filter(mcsp56, TACT==11)
mcsp56_tact_12 <- filter(mcsp56, TACT==12)
mcsp8_tact_12 <- filter(mcsp8, TACT==12)
mcsp8_tact_autres <- filter(mcsp8, TACT!=12)

# On selectionne les variables utiles
enfants_potentiels <- select(enfants_potentiels, idfam, LPRF, AGED, DEPT, IPONDI)

# On renomme les variables
colnames(enfants_potentiels) <- c("idfam", "LPRF_enf", "AGED_enf", "DEPT_enf", "IPONDI_enf")

indicateurs <- function(femmes,femmes_procreer){
  
  # On fait la jointure entre les mères potentielles et les enfants potentiels
  bdd <- merge(femmes_procreer, enfants_potentiels, by="idfam")
  
  # Pour les naissances
  nais <- as.data.frame(wtd.table(bdd$AGED, weights = bdd$IPONDI))
  
  femmes_tab <- as.data.frame(wtd.table(femmes$AGED, weights = femmes$IPONDI))
  
  # On joint les femmes aux naissances
  taux <- merge(femmes_tab, nais, by="Var1")
  
  # On rajoute la variable de l'âge
  taux$age <- seq(from=min(bdd$AGED), to=max(bdd$AGED))
  
  # On enlève les variables inutiles
  taux <- select(taux,-Var1)
  
  # On met dans le bon ordre les variables
  taux <- select(taux,age,Freq.x,Freq.y)
  
  # On renomme les variables
  colnames(taux) <- c("Âge","Femmes en âge de procréer","Naissances")
  
  # On calcule les taux par âge
  taux$taux_par_âge <- taux$Naissances/taux$`Femmes en âge de procréer`
  
  # On fait la somme des taux par âge pour avoir l'ICF
  ICF <- sum(taux$taux_par_âge)
  
  # Âge moyen des femmes, pondéré avec les taux par âge 
  age_moy <- weighted.mean(taux$Âge,w=taux$taux_par_âge)
  
  # On met les résultats dans un tableau
  indicateurs <- data.frame(matrix(ncol=2,nrow=1))
  colnames(indicateurs) <- c("ICF", "age_moy")
  indicateurs[1,1] <- ICF
  indicateurs[1,2] <- age_moy
  
  #return(taux)
  return(indicateurs)
  
}

taux_def <- indicateurs(femmes,femmes_procreer)
taux_12 <- indicateurs(fcsp12,mcsp12)
taux_34 <- indicateurs(fcsp34,mcsp34)
taux_56 <- indicateurs(fcsp56,mcsp56)
taux_8 <- indicateurs(fcsp8,mcsp8)

taux_csp12_actifs <- indicateurs(fcsp12_tact_11,mcsp12_tact_11)
taux_csp34_actifs <- indicateurs(fcsp34_tact_11,mcsp34_tact_11)
taux_csp56_actifs <- indicateurs(fcsp56_tact_11,mcsp56_tact_11)
taux_csp12_chom <- indicateurs(fcsp12_tact_12,mcsp12_tact_12)
taux_csp34_chom <- indicateurs(fcsp34_tact_12,mcsp34_tact_12)
taux_csp56_chom <- indicateurs(fcsp56_tact_12,mcsp56_tact_12)
taux_csp8_chom <- indicateurs(fcsp8_tact_12,mcsp8_tact_12)
taux_csp8_autres <- indicateurs(fcsp8_tact_autres,mcsp8_tact_autres)

# On exporte en excel les taux de toutes les femmes
write_xlsx(taux_def,"./taux_def.xlsx")
write_xlsx(taux_34,"./taux34.xlsx")
write_xlsx(taux_56,"./taux56.xlsx")
write_xlsx(taux_12,"./taux12.xlsx")
write_xlsx(taux_8,"./taux8.xlsx")
