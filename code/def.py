import pandas as pd
import os

print(os.getcwd())

survey = pd.read_csv("../data/FD_INDREGZE_2020.csv", sep=";")

# On garde que les bouches du rhones
survey13 = survey.loc[survey["DEPT"] == 13]


# One year kids with age reached in the year
# Hypothesis that the census is from 2020
survey13_1year_kids = survey13.loc[(survey13['AGED'] == 1) & (survey13['LPRF'] == 3)]
# survey13_kids_1year.describe()
# survey13_1year_kids = survey13[(survey13['LPRF'] == 3)]




# survey13_univariate = survey13['IPONDI'].describe()

# print(survey13.describe())
# print(survey13_kids.describe())
# print(survey13['LPRM'].describe())
# print(survey13_kids['LPRM'].describe())

survey13_mothers = survey13[(survey13['SEXE'] == 2) & ((survey13['AGED'] >= 16) & (survey13['AGED'] <= 50))]
survey13_mothers = survey13_mothers.loc[(survey13_mothers['LPRF'] == 1) | (survey13_mothers['LPRF'] == 2)]
survey13_mothers.describe()
survey13.loc[((survey13['MOCO'] == 21) | (survey13['MOCO'] == 22)) & (survey13['LPRF'] == 1)].describe()



# %%
