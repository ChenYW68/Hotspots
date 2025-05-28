# options(rgdal_show_exportToProj4_warnings="none")
# # 1 loading packages ---------------------------------------
packages <- c("data.table",
              "ggplot2",
              "Hmisc",
              "randomForest",
              "dplyr",
              "ggmap",
              "mapproj",
              "h2o",
              "kernlab",
              "ROSE",
              "reglogit",
              "dynaTree",
              "klaR",
              "sparseSVM",
              "performanceEstimation",
              "DataExplorer"
              )
# ,'MASS'
# 2  library
for(i in 1:length(packages))
{
  if(!lapply(packages[i], require,
             character.only = TRUE)[[1]])
  {
    install.packages(packages[i])
    # library(packages[i])
    lapply(packages[i], require,
           character.only = TRUE)
  }else{lapply(packages[i], require,
               character.only = TRUE)}
}
# x=lapply(packages, require, character.only = TRUE)
# rm(list=ls())
rm(i, packages)


