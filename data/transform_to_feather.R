# install.packages("feather")
library(feather)
load("./BDR2020.rdata")
write_feather(BDR2020, "./BDR2020.feather")