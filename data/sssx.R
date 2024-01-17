DATF<- filter(BR, DEPANT == 13 & DEPACT == 13 & !(EPCI_ANT == 200054807 & EPCI_ACT == 200054807))
DATI<- filter(BR, DEPANT == 13 & DEPACT == 13 & !(EPCI_ANT == 200054807 & EPCI_ACT == 200054807)& !(EPCI_ANT == 241300417 & EPCI_ACT == 241300417)& !(EPCI_ANT == 200035087 & EPCI_ACT == 200035087)& !(EPCI_ANT == 241300375 & EPCI_ACT == 241300375))


DATA <- filter(DATI, IRAN != 1) %>% #on ne garde que les individus qui ont migré
  mutate(EPCI_ACT_R = case_when(EPCI_ACT == "200054807" ~ "Métropole d'Aix-Marseille-Provence",
                                EPCI_ACT == "241300417" ~ "Arles-Crau-Camargue-Montagnette",
                                EPCI_ACT == "200035087" ~ "Terre de Provence",
                                EPCI_ACT == "241300375" ~ "Vallée des Baux-Alpilles"),
         EPCI_ANT_R = case_when(EPCI_ANT == "200054807" ~ "Métropole d'Aix-Marseille-Provence",
                                EPCI_ANT == "241300417" ~ "Arles-Crau-Camargue-Montagnette",
                                EPCI_ANT == "200035087" ~ "Terre de Provence",
                                EPCI_ANT == "241300375" ~ "Vallée des Baux-Alpilles"
         )) %>%
  group_by(EPCI_ANT_R, EPCI_ACT_R) %>%
  summarise(Freq = sum(IPONDI))

sum(filter(DATI, EPCI_ACT == "200054807")$IPONDI)
sum(filter(DATI, EPCI_ACT == "241300417")$IPONDI)
