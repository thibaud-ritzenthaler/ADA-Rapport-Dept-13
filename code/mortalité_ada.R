#On charge les librairies
library(tidyverse)
library(questionr)

#On charge les donnees
F13 <- read.csv2("~/Desktop/ADA-Rapport-Dept-13/data/13F.csv")
M13 <- read.csv2("~/Desktop/ADA-Rapport-Dept-13/data/13M.csv")
base_vqs <- read.delim("~/Desktop/ADA-Rapport-Dept-13/data/base_vqs.csv")

#On filtre pour avoir seulement le departement 13
base13 <- filter(base_vqs,dep==13)

#Recodage de la variable d'incapacité limitat
base13 <- mutate(base13, incap=ifelse(limitat==1,"non","oui"))

#Calcul taux d'incapacite par sexe et age
incap13 <- base13 %>%
  group_by(sexe,age,incap) %>%
  summarize(num=sum(poids)) %>%#nb d'individus par sexe, age ,statut d'incapacite
  group_by(sexe,age) %>%
  mutate(px=num/sum(num)) %>% #calcul des taux
  filter(incap=="oui")

#On sépare en deux pour avoir une table femmes et une table hommes
h_incap13 <- filter(incap13,sexe==1)
f_incap13 <- filter(incap13,sexe==2)

#On filtre les tables de morta pour avoir seuelement 2014
F13_2014 <- filter(F13,Year==2014)
M13_2014 <- filter(M13,Year==2014)

#On joint table de morta et données de l'enquete
data_13f <- merge(f_incap13,F13_2014,by.x="age",by.y="Age")
data_13h <- merge(h_incap13,M13_2014,by.x="age",by.y="Age")

#On charge les taux de "logements"
logements_h <- read_excel("~/Desktop/ADA-Rapport-Dept-13/data/logements.xlsx", sheet = "Hommes")
logements_f <- read_excel("~/Desktop/ADA-Rapport-Dept-13/data/logements.xlsx", sheet = "Femmes")

#On joint les données nécessaires
bdd_f13 <- merge(f_incap13, F13_2014, by.x="age",by.y="Age")
bdd_f13 <- merge(bdd_f13,logements_f,all.x = TRUE)

bdd_h13 <- merge(h_incap13, M13_2014, by.x="age",by.y="Age",all.x = TRUE)
bdd_h13 <- merge(bdd_h13,logements_h,all.x = TRUE)

#Fonction

#Input: dataframe containing : 
#age ; mx or qx ; px (% disabled at home) ; ix (% living in institution) ; pix (% disabled in institution) 
#all numeric ; px, ix and pix are optional ** if no px, returns LE only
#column names must be as specified, but the function is not case sensitive
#Output: extended dataframe


hly <- function(df) {
  
  colnames(df) <- tolower(colnames(df)) #convert all column names to lower case
  df <- df[order(df$age), ] #order df by age (ascending)
  
  #age interval width 
  df$n <- c(diff(df$age), 1)  
  #years lived between age x and x+n by people deceased within the age interval
  if (df[1, "age"] == 0) { 
    a <- c(0.2, rep(0.5, nrow(df)-1)) #accounting for non-uniform distribution of infant deaths
    df$ax <- df$n * a
  } else {
    df$ax <- df$n * 0.5
  }
  
  #computing mx and qx if one of them is missing
  if ("mx" %in% colnames(df) & !("qx" %in% colnames(df))) { 
    df$qx <- df$n*df$mx/(1+df$ax*df$mx)
  } else if ("qx" %in% colnames(df) & !("mx" %in% colnames(df))) {
    df$mx <- df$qx/(df$n-df$ax*df$qx)
  }
  
  #Survivors of age x
  df$lx <- NA #initializing lx
  df[1,"lx"] <- 100000 #for the first age, lx is fixed at 100000
  i <- 2 #indicator of position
  while (i <= nrow(df)) {
    df[i, "lx"] <- 
      df[i-1, "lx"]-(df[i-1, "lx"]*df[i-1, "qx"])
    i <- i+1
  }
  
  #Person-years lived between age x and x+n
  df$Lx <- NA
  i <- 1
  while(i<=nrow(df)){
    if (i == nrow(df)) {
      df[i,"Lx"] <- df[i,"lx"]/df[i,"mx"]
    }
    else {
      df[i,"Lx"] <- df[i+1,"lx"]*df[i,"n"] + (df[i,"lx"] - df[i+1,"lx"])*df[i,"ax"]
    }
    i <- i+1
  }
  
  #Total number of person-years lived by survivors of age x
  df$Tx <- rev(cumsum(rev(df$Lx)))
  
  #Total life expectancy at age x
  df$ex <- ifelse(df$age == max(df$age), 1/df$mx, df$Tx/df$lx)
  
  
  #----DISABILITY-FREE LIFE EXPECTANCY----
  #check if px is provided
  if ("px" %in% colnames(df)) {
    
    #Person-years lived with disability between age x and x+n
    if ("ix" %in% colnames(df)) {
      #if percentage living in institution is provided
      df$ILx <- df$Lx*df$ix #PY lived in institution
      if ("pix" %in% colnames(df)) {
        #PY lived with disability: applying specific disability rates to PY lived at home and in institution
        df$DLx <- df$ILx*df$pix + (df$Lx-df$ILx)*df$px
      } else {
        #If no disability rates are provided for people living in institution, PY lived in institution are years of disability
        df$DLx <- df$ILx + (df$Lx-df$ILx)*df$px
      }
    } else {
      #if percentage living in institution is not provided
      df$DLx <- df$Lx*df$px
    }
    
    #Person_years lived without disability between age x and x+n
    df$DFLx <- df$Lx - df$DLx 
    
    #Total person-years lived with/without disability by survivors of age x
    df$DTx <- rev(cumsum(rev(df$DLx)))
    df$DFTx <- rev(cumsum(rev(df$DFLx)))
    
    #DFLE and DLE at age x
    df$DFLEx <- df$DFTx/df$lx
    df$DLEx <- df$DTx/df$lx
    
    #percentage of DFLE in total LE
    df$DFLE_pct <- round(100*(df$DFLEx/df$ex),1)
    
  }
  
  return(df)
}

newbddf_13 <- hly(bdd_f13)
newbddh_13 <- hly(bdd_h13)
