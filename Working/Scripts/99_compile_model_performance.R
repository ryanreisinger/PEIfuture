#TEMP

# Build habitat models

# Ryan R Reisinger

# Last modified: 2020-06

library(foreach)
library(parallel)
library(doParallel)

## Various machine learning models fit in caret
library(gbm)
library(mgcv)
library(ranger)
library(earth)
library(caret)
library(e1071)
library(pROC)
library(caretEnsemble)
library(plyr)

# setwd("D:/PEIfuture/Working/") # Local
setwd("/mnt/home/ryan/PEIfuture/Working/") # Server

#-------------------------------------------------------------------

this.species <- "SFS"
# this.climate <- "ACCESS1-0"
this.season <- "winter"

these.climates <- c(
  "ACCESS1-0",
  "BCC-CSM1.1",
  "CanESM2",
  "CMCC-CM",
  "EC-EARTH",
  "GISS-E2-H-CC",
  "MIROC-ESM",
  "NorESM1-M")

for (i in these.climates) {
  this.climate <- i
  print(i)
  
#-------------------------------------------------------------------

  these.mods <- c("gbm",
                  "gam",
                  "rf",
                  "svm",
                  "ann")

  for (j in these.mods) {

this.mod <- readRDS(paste0("./Data/modelOutput/", this.species, "_", this.season, "_", this.climate, "_", this.mod, ".RDS"))

# Diagnostics
tmp <- this.mod$results
tmp <- tmp[order(tmp$ROC, decreasing = T),] # Arrange by ROC
tmp <- tmp[1,] # Keep only the best score
tmp <- dplyr::select(tmp, Accuracy, Kappa, ROC, Sens, Spec, AUC, Precision, Recall, F,
                     AccuracySD, KappaSD, ROCSD)
tmp$sp <- this.species
tmp$season <- this.season
tmp$climate <- this.climate
tmp$method <- this.mod

write.csv(tmp,
          paste0("./Data/modelOutputDiagnostics/", this.species, "_", this.season, "_", this.climate, "_ensemble.csv"),
          row.names = F)
rm(tmp)

}

}