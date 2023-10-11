#Chargement des packages
library(tidyverse)
library(questionr)

#On charge les données

FD_INDREGZE_2020=read.csv("/Users/adelejnd/Desktop/Analyse démo appliquée/FD_INDREGZE_2020.csv", sep=";")

#On veut juste le département des Bouches-du-Rhone

bdr2020=filter(FD_INDREGZE_2020,DEPT=="13")

#On fait la somme de la variable ipondi pour connaitre la population du département

pop13=sum(bdr2020$IPONDI)

#Identifiant du ménage

bdr2020$idmen=paste0(bdr2020$REGION,bdr2020$NUMMR)

#Identifiant de la famille

bdr2020$idfam=paste0(bdr2020$REGION,bdr2020$NUMMR,bdr2020$NUMF)

#On crée une table avec les enfants de 1 an et de 0 an
un_an=filter(bdr2020,AGED==1)
zero_an=filter(bdr2020,AGED==0)

un_an_p=sum(un_an$IPONDI)
zero_an_p=sum(zero_an$IPONDI)

#Pour avoir 0 an en ddm l'ann,ée du recensement il faut etre né en janvier, c'est pour cela qu'ils sont peu nombreux
#Les chiffres precedents sont des stats de stock, le chiffre des enfants de 1 an va etre proche des naissances de l'année 2019

#Pour avoir le nombre d'enfants de un ou zero an en fonction des années de naissance, 2 manières de faire
freq=table(un_an$ANAI)
freq2=un_an%>%count(ANAI)

freq_zero2=table(zero_an$ANAI)
freq_zero=zero_an%>%count(ANAI)

#On va dire que les enfants de 1 an du recensement 2020 sont nés en 2019, on regarde pas l'année de naissance

#On filtre pour avoir les personnes de ref de la famille

quest4=filter(bdr2020,(MOCO==21|MOCO==22)& LPRF==1)
sum(quest4$IPONDI)

#Sans pondération

quest4ter=quest4%>%count(SEXE)

#Faire avec les pondérations

#Technique 1
quest4pond=wtd.table(quest4$SEXE,weights = quest4$IPONDI)
#pourcentage
prop.table(quest4pond)*100

#Technique 2
quest4pond_h=filter(quest4,SEXE==1)
sum(quest4pond_h$IPONDI)

quest4pond_f=filter(quest4,SEXE==2)
sum(quest4pond_f$IPONDI)
#pourcentage d'homme

317921/(317921+128097.6)*100

#on crée des groupes d'âge

quest4$Grp_age=case_when(quest4$AGED <18 ~"0 à 17 ans",
                         quest4$AGED<25 ~"18-24 ans",
                         quest4$AGED<45 ~ "25-44 ans",
                         quest4$AGED<65~"15-64 ans",
                         quest4$AGED<80~"65-79 ans",
                         quest4$AGED<100~"80-99 ans",
                         quest4$AGED>=100~"100 et plus")

#On crée tables enfants

enfants_potentiels=filter(bdr2020,AGED==1 & LPRF==3)
sum(enfants_potentiels$IPONDI)

#On créé la table des mères potentielles

femmes_procréer=filter(bdr2020,(SEXE==2) & (AGED>15 & AGED<=50) & (LPRF==1 | LPRF==2))
sum(femmes_procréer$IPONDI)

#On sélectionnes les variables utiles
enfants_potentiels=select(enfants_potentiels,idfam,LPRF,AGED,DEPT,IPONDI)
colnames(enfants_potentiels)=c("idfam","LPRF_enf","AGED_enf","DEPT_enf","IPONDI_enf")
femmes_procréer=select(femmes_procréer,idfam,LPRF,AGED,DEPT,IPONDI)

bdd=merge(femmes_procréer,enfants_potentiels,by="idfam")

#Pour les naissances
nais=as.data.frame(wtd.table(bdd$AGED,weights = bdd$IPONDI))

#pour les mères

meres=as.data.frame(wtd.table(femmes_procréer$AGED,weights=femmes_procréer$IPONDI))

#Calcul des taux par âge

taux=as.data.frame(nais$Freq/meres$Freq)
sum(taux$`nais$Freq/meres$Freq`)

#Table femmes

femmes=filter(bdr2020,SEXE==2 & (AGED>15 & AGED<=50))
femmes=select(femmes,idfam,LPRF,AGED,DEPT,IPONDI)
femmes_tab=as.data.frame(wtd.table(femmes$AGED,weights = femmes$IPONDI))

tauxbis=merge(femmes_tab,nais,by="Var1")
tauxbis$taux=tauxbis$Freq.y/tauxbis$Freq.x
sum(tauxbis$taux)


#Age moyen

meres$age=seq(from=16,to=50)
meres$agemoy=meres$age*meres$Freq
sum(meres$agemoy)/sum(meres$Freq)


