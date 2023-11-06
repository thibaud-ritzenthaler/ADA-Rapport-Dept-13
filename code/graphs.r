library(tidyverse)
library(questionr)
library(writexl)
library(ggrepel)

#setwd("C:/Users/Tibo/Documents/Demographie/M2S1/UE1 - Analyse Demographique Appliquee/ADA-Rapport-Dept-13/data")


taux_detail <- read_csv("./taux_detail.csv") %>%
  separate(type, c("Type", "CS", "Acti")) %>%
  mutate(Acti = case_when(
    Acti == "actifs" ~ "En emploi",
    Acti == "chom" ~ "Ch\u00F4meuses",
    Acti == "autres" ~ "Inactives",
  ))


taux_detail <- taux_detail %>% group_by(CS) %>%
  mutate(lblmeanicf = mean(ICF)) %>%
  mutate(lblmeanagemoy = mean(age_moy)) %>%
  mutate(lblCSP = case_when(
    Acti == "Ch\u00F4meuses" & CS == "csp12" ~ "Agri., Arti. Comm. (CSP 1 et 2)",
    Acti == "Ch\u00F4meuses" & CS == "csp34" ~ "Cadres, Prof. Inter. (CSP 3 & 4)",
    Acti == "Ch\u00F4meuses" & CS == "csp56" ~ "Ouvri., Empl. (CSP 5 & 6)",
    Acti == "Ch\u00F4meuses" & CS == "csp8" ~ "Primo-Chom., Inact. (CSP 8)",
    T ~ NA
  ))

ggplot(taux_detail) +
  geom_point(aes(x = age_moy, y = ICF, color= CS, shape = Acti, size = Effectifs),alpha = 0.5) +
  #geom_text_repel(aes(x = age_moy, y = ICF, label = Acti)) +
  geom_label_repel(aes(x = lblmeanagemoy, y = lblmeanicf, label = lblCSP),
                  na.rm = TRUE,
                   min.segment.length = 0
  ) +
  geom_line(aes(x = age_moy, y = ICF,  color= CS, group = CS)) +
  scale_color_manual("CSP", values = c("#90E0EF", "#00B4D8", "#0077B6","#03045E"), guide = "none") +
  scale_shape_manual("Activit\u00E9", values = c(16,21,13)) +
  xlab("Age moyen") +
  ylab("Indice conjoncturel de f\u00e9condit\u00e9") +
  scale_size(range = c(2,12), guide = "none") +
  theme_light() +
  theme(legend.position=c(0.9, 0.85), legend.text = element_text(size = 12))


ggplot(taux_detail) +
  geom_point(aes(x = CS, y = ICF, color= age_moy)) +
  geom_label(aes(x = CS, y = ICF, label = type), nudge_y = 0.05) +
  xlab("Age moyen") +
  ylab("Indice conjoncturel de f\u00e9condit\u00e9") +
  theme_light() #+
  #theme(legend.position= "none")