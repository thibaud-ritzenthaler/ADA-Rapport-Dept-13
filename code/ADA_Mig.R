packages <- c("tidyverse", "survey", "gtsummary", "mapsf", "sf")
lapply(packages, library, character.only=TRUE)
theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")

# setwd("C:/Users/abdel/Desktop/Cours Master/Semestre 3/Analyse demographique appliquee")
# setwd("C:/Users/Tibo/Documents/Demographie/M2S1/UE1 - Analyse Demographique Appliquee/ADA-Rapport-Dept-13/data")

# migcom <- data.table::fread("C:/Users/Progedo/Desktop/Seafile/Data/RP/RP2020_MIGCOM_csv/FD_MIGCOM_2020.csv", sep = ";", header = TRUE)
# save(migcom, file = "migcom20.Rdata")
load("migcom20.Rdata")


##1) Proportions de la population selon le lieu de naissance, le lieu de residence anterieure et l'anciennete dans le logement####

#On recode le lieu de naissance (regrouper les DROM et les COM)
migcom$INAI_r <- case_match(migcom$INAI,
                            1 ~ "Dans le departement de residence actuelle",
                            2 ~ "Dans un autre departement de la region de residence actuelle",
                            3 ~ "Hors de la region de residence actuelle : en metropole",
                            c(4, 5) ~ "Hors de la region de residence actuelle : dans un DROM-COM",
                            6 ~ "À l'etranger",
                            .ptype = factor(levels = c("Dans le departement de residence actuelle","Dans un autre departement de la region de residence actuelle",
                                                       "Hors de la region de residence actuelle : en metropole",
                                                       "Hors de la region de residence actuelle : dans un DROM-COM", "À l'etranger")))

#On recode le lieu de residence anterieur (regrouper tous les lieux "France hors region actuelle", et les lieus à l'etranger)
migcom$IRAN_r <- case_match(migcom$IRAN,
                            0 ~ "Commune ou arrondissement de rattachement",
                            1 ~ "Dans le même logement",
                            2 ~ "Dans un autre logement de la même commune",
                            3 ~ "Dans une autre commune du departement",
                            4 ~ "Dans un autre departement de la region",
                            c(5,6,7) ~ "Hors de la region de residence actuelle en France (metropole ou DROM-COM)",
                            c(8,9) ~ "À l'etranger",
                            .ptype = factor(levels = c("Commune ou arrondissement de rattachement", "Dans le même logement","Dans un autre logement de la même commune",
                                                       "Dans une autre commune du departement","Dans un autre departement de la region",
                                                       "Hors de la region de residence actuelle en France (metropole ou DROM-COM)", "À l'etranger")))


#Recodage de l'anciennete dans le logement
migcom$ANEMC_r <- recode_factor(migcom$ANEMC,
                                "0" = "Moins de 2 ans",
                                "1" = "De 2 à 4 ans",
                                "2" = "De 5 à 9 ans",
                                "3" = "De 10 à 19 ans",
                                "4" = "De 20 à 29 ans",
                                "5" = "30 ans ou plus",
                                "Z" = "Hors logement ordinaire")


#declaration des poids
migcom_w <- svydesign(ids = ~ 1, weights = ~ IPONDI, data = filter(migcom, str_sub(COMMUNE, 1,2) == "13"))


#calcul des indicateurs
migcom_w %>%
  tbl_svysummary(
    include = c("INAI_r", "IRAN_r", "ANEMC_r"),
    statistic = all_categorical() ~ "{p} %"
  ) %>%
  bold_labels()

##2) indicateurs de migration inter- et intra-departementale####

#On extrait le numero du departement depuis le code commune (2 permiers caracteres)
migcom <- migcom %>%
  mutate(DEPACT = case_when(METRODOM == "M" ~ str_sub(COMMUNE,1,2),
                            METRODOM == "D" ~ str_sub(COMMUNE,1,3)),
         #Le code des DROM est code sur 3 caracteres
         DEPANT = case_when(str_sub(DCRAN, 1, 2) == "97" ~ str_sub(DCRAN, 1, 3),
                            .default = str_sub(DCRAN, 1, 2)))

#population entrante (immigrante)
ENTR <- migcom %>%
  filter(DEPACT != DEPANT) %>% #on filtre ceux dont le dep actuel n'est pas le même que le dep anterieur
  group_by(DEPACT) %>%
  summarise(ENTR = sum(IPONDI)) %>% #nb d'individus par departement de residence en tenant compte de la ponderation
  rename(DEP = DEPACT) # On renomme la variable pour compiler ulterieurement les differentes sous-populations

#population sortante (emigrante)
SORT <- migcom %>%
  filter(DEPACT != DEPANT) %>%
  group_by(DEPANT) %>%
  summarise(SORT = sum(IPONDI)) %>% #nb d'individus par departement d'origine
  rename(DEP = DEPANT)

#Population en n-1 (sedentaires + sortants)
POPN_1 <- migcom %>%
  group_by(DEPANT) %>%
  summarise(POPN_1 = sum(IPONDI)) %>% #nb d'individus par dep anterieur, qq soit le dep actuel
  rename(DEP = DEPANT)

#population en n
POPN <- migcom %>%
  group_by(DEPACT) %>%
  summarise(POPN = sum(IPONDI)) %>% #nb d'individus par dep actuel, qq soit le dep anterieur
  rename(DEP = DEPACT)

#jointure 
df_list <- list(ENTR, SORT, POPN_1, POPN)
DATA <- df_list %>% reduce(inner_join, by = "DEP")
#calcul du taux de migration nette interne (entrants - sortants / pop moyenne)
DATA$TM <- 1000*(DATA$ENTR-DATA$SORT)/((DATA$POPN_1 + DATA$POPN)/2)

###Taux de migration nette interne (def. de l'Observatoire des Territoires :
#taux d'evolution de la population imputable aux mouvements migratoires entre cette zone et les autres parties du territoire national.
###

#cartographie
France <- st_read("Fonds de cartes-20230905/DEPARTEMENT.shp")
France <- merge(France, DATA, by = "DEP", all.x = TRUE)


#Determiner l'echelle de couleurs pour la carte. On veut pouvoir distinguer facilement les dept avec un TM < 0 et ceux avec un TM > 0.
#Les classes formees doivent être +/- homogenes.
hist(DATA$TM) #distribution des TM
sd(DATA$TM) #ecart-type de la distribution
breaks <- mf_get_breaks(DATA$TM, breaks = "fixed", nbreaks = 4, fixedBreaks = c(min(DATA$TM), 0,5,10,max(DATA$TM)))
col_pal <- mf_get_pal(n = c(1,3), pal = c("Earth", "Red-Yellow"))

mf_map(France, var = "TM", type = "choro", pal = col_pal, breaks = breaks, col_na = "grey",
       leg_no_data = "Absence de donnees",
       leg_title = "Taux de migration\nnette interne (‰)")

mf_title("Migrations interdepartementales, 2020", 
         pos = "center") 
mf_credits("Auteur : M. Crouzet\nSources: INSEE & IGN, 2020")
mf_scale(size = 100, unit = "km")
mf_arrow('topleft')

#migration infra-departementale : taux de mobilite infra
#part de la population qui a change de commune au sein du même departement
TMINF <- migcom  %>%
  filter(DEPACT == DEPANT) %>% #on garde ceux qui n'ont pas change de departement
  #recodage des codes de Paris, Marseille et Lyon pour ne pas tenir compte des arrondissements
  mutate(COMANT = case_when (DCRAN %in% c("75101":"75120") ~ "75056", 
                             DCRAN %in% c("13201":"13216") ~ "13055", 
                             DCRAN %in% c("69381":"69389") ~ "69123",
                             .default = DCRAN)) %>%
  mutate(MOB = ifelse(COMANT != COMMUNE, "1", "0")) %>% #l'individu a-t-il change de commune ? 1 si oui, 0 si non
  group_by(DEPACT, MOB) %>%
  summarise(nb = sum(IPONDI)) %>% #nb de mobiles et d'immobiles
  mutate(TMINFRA = 100* nb/sum(nb)) %>% #part de mobiles et d'immobiles dans la population totale du dept
  filter(MOB == "1") %>% #on ne garde que les mobiles
  select(DEPACT, TMINFRA)

#carto
France <- merge(France, TMINF, by.x = "DEP", by.y = "DEPACT", all.x = TRUE)

hist(TMINF$TMINFRA)
#On determine l'echelle de couleurs selon la methode "moyenne ecart-type" 
msd <- mf_get_breaks(TMINF$TMINFRA, breaks = "msd", k = 1, central = TRUE) 

mf_map(France, var = "TMINFRA", type = "choro", pal = "YlOrBr", breaks = "jenks", nbreaks = 5, col_na = "grey", leg_no_data = "Absence de donnees",
       leg_title = "Taux de de mobilite\ninfra (en %)")

mf_title("Migrations intradepartementales, 2020", pos = "center")
mf_credits("Auteur : M. Crouzet\nSources: INSEE & IGN, 2020")
mf_scale(size = 100, unit = "km")
mf_arrow('topleft')