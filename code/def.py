import pandas as pd
import os

print(os.getcwd())

# survey = pd.read_csv("../data/FD_INDREGZE_2020.csv", sep=";")
survey = pd.read_feather("../data/BDR2020.feather")

# ID of the household
survey["idmen"] = survey.REGION.map(str) + survey.NUMMR

# ID of the family
survey["idfam"] = survey.idmen.map(str) + survey.NUMF

enfants_potentiels = survey.loc[(survey['AGED'] == 1) & (survey['LPRF'] == 3)]

femmes_procreer = survey[(survey['SEXE'] == 2) & ((survey['AGED'] >= 16) & (survey['AGED'] <= 50))]
femmes_procreer = femmes_procreer.loc[(femmes_procreer['LPRF'] == 1) | (femmes_procreer['LPRF'] == 2)]

enfants_potentiels = enfants_potentiels[["idfam", "LPRF", "AGED", "DEPT", "IPONDI"]]
enfants_potentiels.rename(
    columns={"LPRF": "LPRF_enf", "AGED": "AGED_enf", "DEPT": "DEPT_enf", "IPONDI": "IPONDI_enf"}, inplace=True)
femmes_procreer = femmes_procreer[["idfam", "LPRF", "AGED", "DEPT", "IPONDI"]]

bdd = pd.merge(femmes_procreer, enfants_potentiels)
