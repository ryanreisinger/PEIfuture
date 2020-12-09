# Build habitat models

# Ryan R Reisinger

# Last modified: 2019-01-09

library(foreach)
library(parallel)
library(doParallel)

## Various machine learning models fit in caret
library(gbm)
library(mgcv)
library(ranger)
#library(earth)
library(caret)
library(e1071)
library(pROC)
library(caretEnsemble)
library(plyr)

setwd("~/PEIfuture/Working")

#-------------------------------------------------------------------
# Main params
this.species <- "AFS"
# this.climate <- "NorESM1-M"
this.season <- "summer"

# Common variables
source("./Scripts/00 - Project variables.R")

#-------------------------------------------------------------------
# Which models to run
run.GBM <- FALSE
run.RF <- TRUE
run.GAM <- FALSE
run.SVM <- FALSE
run.ANN <- FALSE
run.MARS <- FALSE

#-------------------------------------------------------------------

#-------------------------------------------------------------------
for (j in 1:length(all.climate.mods)) {
  this.climate <- all.climate.mods[j]
  print(this.climate)
#-------------------------------------------------------------------

# Prepare data

# Load data
DAT <- readRDS(paste0("./Data/trackDataEnvar/", this.species, "_", this.climate, ".RDS"))


# Create binonmial column
# for caret, must be factor
DAT[DAT$s == "observed track", "s"] <- "Obs"
DAT[DAT$s == "simulated track", "s"] <- "Sim"
DAT$s <- as.factor(DAT$s)

#-------------------------------------------------------------------
# Function to create folds for individual based CV

foldCaret <- function(dat, nm = 10) {
  
  #--------------------------------------------------------
  # Create a folds index to keep cells together during cross-validation
  # The resulting list "folds" is passed to "index" in the "trainControl" function in caret
  # Keeps individuals together during splitting data for cross validation
  
  use.long <- dat
  nm <- nm
  
  if (length(unique(use.long$track_id)) < nm) {
    nm <- length(unique(use.long$track_id))
  }
  
  rws <- floor(length(unique(use.long$track_id))/nm) #number of cells to include in each fold
  cells <- unique(use.long$track_id)
  folds <- list()
  for (i in 1:nm) {
    samp <- sample(x = cells, size = rws)
    l <- list()
    for (j in 1:length(samp)) {
      k <- which(use.long$track_id == samp[j])
      l[j] <- list(k)
    }
    l <- unlist(l)
    folds[i] <- list(l)
    for (r in 1:length(samp)) {
      cells <- cells[cells != samp[r]]
    }
  }
  
  names(folds) <- paste0("fold_", c(1:nm)) #train needs list to be named
  
  return(folds)
  
}

#Select season
seasons <- readRDS("~/PEIfuture/Working/Data/other/ALL_seasons.rds")
DAT <- merge(x = DAT, y = seasons[ , c("track_id", "which.season")], all.x = TRUE, all.y = F)
DAT <- DAT[DAT$which.season == this.season, ]

#Select variables
if (this.climate == "GISS-E2-H-CC") {
  DAT <- DAT[ , c("track_id", "sim_trk_id", "s", "date", "lat", "lon", "species", "month", "which.season",
                  "ICE",
                  "SST",
                  "SSTg",
                  # "WIN",
                  "WINu",
                  "WINv",
                  # "CUR",
                  "CURu",
                  "CURv",
                  "DEP")]
} else {
DAT <- DAT[ , c("track_id", "sim_trk_id", "s", "date", "lat", "lon", "species", "month", "which.season",
                "ICE",
                "SST",
                "SSTg",
                # "WIN",
                "WINu",
                "WINv",
                # "CUR",
                "CURu",
                "CURv",
                "SSH",
                "SSHg",
                "DEP")]
}
                

#Complete cases
DAT <- DAT[complete.cases(DAT[ ,10:ncol(DAT)]), ]


#-------------------------------------------------------------------
# Set up parallel processing

# clust <- makeCluster(detectCores() - 1) # leave 1 core for OS
# registerDoParallel(clust)

#-------------------------------------------------------------------

#Create folds
folds <- foldCaret(dat = DAT, nm = 10)

#MODELS:

#Train control
tc <- trainControl(method = "cv",
                   number = length(folds),
                   search = "grid",
                   classProbs = TRUE,
                   allowParallel = TRUE,
                   summaryFunction = twoClassSummary,
                   sampling = "down",
                   index = folds)

# Train control with random cross-validation
# tc <- trainControl(method = "cv",
#                    number = 10,
#                    search = "grid",
#                    classProbs = TRUE,
#                    allowParallel = TRUE,
#                    summaryFunction = twoClassSummary,
#                    sampling = "down")

# Search grids for each model type
# 1. GBM/BRT
gbmGrid <-  expand.grid(interaction.depth = c(1, 3, 5), 
                        n.trees = (1:5)*2000, 
                        shrinkage = c(0.1, 0.5, 0.01),
                        n.minobsinnode = 20)

# 2. GAM
gamGrid <- data.frame(method = "fREML", select = FALSE)

# 3. RF
rfGrid <-  data.frame(mtry = c(3, 4, 5),
                      splitrule = "gini",
                      min.node.size = c(1))

# 4. MARS
marsGrid <- data.frame(degree = c(1, 2, 3))

# 5. SVM with radial kernel
svmGrid <- data.frame(C = c(0.25, 0.5, 1, 2, 4, 8, 16, 32))

# 6. ANN
annGrid <- data.frame(size = c(2, 4, 6),
                      decay = 0)


#Fit the model
#searching over a given grid

#-------------------
#GBM
#-------------------
if (run.GBM) {
  clust <- makeCluster(detectCores() - 1) # leave 1 core for OS
  registerDoParallel(clust)
  
  system.time(
    MOD.gbm <- train(x = as.data.frame(DAT[ ,10:ncol(DAT)]),
                     y = DAT$s,
                     method = "gbm",
                     metric = "ROC",
                     trControl = tc,
                     tuneGrid = gbmGrid)
  )
  
  
  saveRDS(MOD.gbm, paste0("./Data/modelOutput/", this.species, "_", this.season, "_", this.climate, "_gbm.RDS"))
  MOD.gbm
  summary(MOD.gbm)
  
  stopCluster(clust)
  registerDoSEQ()
  
  # RESULTS
  
  # Diagnostics
  tmp <- MOD.gbm$results
  tmp$sp <- this.species
  tmp$climate <- this.climate
  tmp$season <- this.season
  tmp$method <- "gbm"
  
  write.csv(tmp,
            paste0("./Data/modelOutputDiagnostics/", this.species, "_", this.season, "_", this.climate, "_gbm.csv"),
            row.names = F)
  rm(tmp)
  
  # Variable importance
  tmp <- varImp(MOD.gbm)$importance
  tmp$variable <- row.names(tmp)
  tmp$sp <- this.species
  tmp$climate <- this.climate
  tmp$season <- this.season
  tmp$method <- "gbm"
  
  write.csv(tmp,
            paste0("./Data/modelOutputVarImp/", this.species, "_", this.season, "_", this.climate, "_gbm.csv"),
            row.names = F)
  rm(tmp)
  
  rm(MOD.gbm)
}

#-------------------
#GAM
#-------------------
if (run.GAM) {
  clust <- makeCluster(detectCores() - 1) # leave 1 core for OS
  registerDoParallel(clust)
  
  system.time(
    MOD.gam <- train(x = as.data.frame(DAT[ ,10:ncol(DAT)]),
                     y = DAT[ ,3],
                     method = "bam",
                     metric = "ROC",
                     trControl = tc,
                     tuneGrid = gamGrid)
  )
  
  saveRDS(MOD.gam, paste0("./Data/ModelOutput/", this.species, "_", this.climate, "_gam.RDS"))
  MOD.gam
  summary(MOD.gam)
  
  stopCluster(clust)
  registerDoSEQ()
  
  # RESULTS
  
  # Diagnostics
  tmp <- MOD.gam$results
  tmp$sp <- this.species
  tmp$climate <- this.climate
  tmp$method <- "gam"
  
  write.csv(tmp,
            paste0("./Data/modelOutputDiagnostics/", this.species, "_", this.climate, "_gam.csv"),
            row.names = F)
  rm(tmp)
  
  # Variable importance
  tmp <- varImp(MOD.gam)$importance
  tmp$variable <- row.names(tmp)
  tmp$sp <- this.species
  tmp$climate <- this.climate
  tmp$method <- "gam"
  
  write.csv(tmp,
            paste0("./Data/modelOutputVarImp/", this.species, "_", this.climate, "_gam.csv"),
            row.names = F)
  rm(tmp)
}

#-------------------
#RF
#-------------------
if (run.RF) {
  clust <- makeCluster(detectCores() - 1) # leave 1 core for OS
  registerDoParallel(clust)
  
  system.time(
    MOD.rf <- train(x = as.data.frame(DAT[ ,10:ncol(DAT)]),
                    y = DAT[ ,3],
                    method = "ranger",
                    importance = 'impurity',
                    metric = "ROC",
                    trControl = tc,
                    tuneGrid = rfGrid)
  )
  
  saveRDS(MOD.rf, paste0("./Data/modelOutput/", this.species, "_", this.season, "_", this.climate, "_rf.RDS"))
  MOD.rf
  summary(MOD.rf)
  
  stopCluster(clust)
  registerDoSEQ()
  
  # RESULTS
  
  # Diagnostics
  tmp <- MOD.rf$results
  tmp$sp <- this.species
  tmp$climate <- this.climate
  tmp$season <- this.season
  tmp$method <- "rf"
  
  write.csv(tmp,
            paste0("./Data/modelOutputDiagnostics/", this.species, "_", this.season, "_", this.climate, "_rf.csv"),
            row.names = F)
  rm(tmp)
  
  # Variable importance
  tmp <- varImp(MOD.rf)$importance
  tmp$variable <- row.names(tmp)
  tmp$sp <- this.species
  tmp$climate <- this.climate
  tmp$season <- this.season
  tmp$method <- "rf"
  
  write.csv(tmp,
            paste0("./Data/modelOutputVarImp/", this.species, "_", this.season, "_", this.climate, "_rf.csv"),
            row.names = F)
  rm(tmp)
}

#-------------------
#MARS
#-------------------
if (run.MARS) {
  clust <- makeCluster(detectCores() - 1) # leave 1 core for OS
  registerDoParallel(clust)
  
  system.time(
    MOD.mars <- train(x = as.data.frame(DAT[ ,10:ncol(DAT)]),
                      y = DAT[ ,3],
                      method = "bagEarthGCV",
                      metric = "ROC",
                      trControl = tc,
                      tuneGrid = marsGrid)
  )
  
  saveRDS(MOD.mars, paste0("./Data/ModelOutput/", this.species, "_", this.climate, "_mars.RDS"))
  MOD.mars
  summary(MOD.mars)
  
  stopCluster(clust)
  registerDoSEQ()
  
  # RESULTS
  
  # Diagnostics
  tmp <- MOD.mars$results
  tmp$sp <- this.species
  tmp$climate <- this.climate
  tmp$method <- "mars"
  
  write.csv(tmp,
            paste0("./Data/modelOutputDiagnostics/", this.species, "_", this.climate, "_mars.csv"),
            row.names = F)
  rm(tmp)
  
  # Variable importance
  tmp <- varImp(MOD.mars)$importance
  tmp$variable <- row.names(tmp)
  tmp$sp <- this.species
  tmp$climate <- this.climate
  tmp$method <- "mars"
  
  write.csv(tmp,
            paste0("./Data/modelOutputVarImp/", this.species, "_", this.climate, "_mars.csv"),
            row.names = F)
  rm(tmp)
}

#-------------------
#SVM
#-------------------
if (run.SVM) {
  clust <- makeCluster(detectCores() - 1) # leave 1 core for OS
  registerDoParallel(clust)
  
  system.time(
    MOD.svm <- train(x = as.data.frame(DAT[ ,10:ncol(DAT)]),
                     y = DAT[ ,3],
                     method = "svmRadialCost",
                     metric = "ROC",
                     trControl = tc,
                     tuneGrid = svmGrid)
  )
  
  saveRDS(MOD.svm, paste0("./Data/ModelOutput/", this.species, "_", this.season, "_", this.climate, "_svm.RDS"))
  MOD.svm
  summary(MOD.svm)
  
  stopCluster(clust)
  registerDoSEQ()
  
  # RESULTS
  
  # Diagnostics
  tmp <- MOD.svm$results
  tmp$sp <- this.species
  tmp$climate <- this.climate
  tmp$method <- "svm"
  
  write.csv(tmp,
            paste0("./Data/modelOutputDiagnostics/", this.species, "_", this.climate, "_svm.csv"),
            row.names = F)
  rm(tmp)
  
  # Variable importance
  tmp <- varImp(MOD.svm)$importance
  tmp$variable <- row.names(tmp)
  tmp$sp <- this.species
  tmp$climate <- this.climate
  tmp$method <- "svm"
  
  write.csv(tmp,
            paste0("./Data/modelOutputVarImp/", this.species, "_", this.climate, "_svm.csv"),
            row.names = F)
  rm(tmp)
}

#-------------------
#ANN
#-------------------
if (run.ANN) {
  clust <- makeCluster(detectCores() - 1) # leave 1 core for OS
  registerDoParallel(clust)
  
  system.time(
    MOD.ann <- train(x = as.data.frame(DAT[ ,10:ncol(DAT)]),
                     y = DAT[ ,3],
                     method = "nnet",
                     metric = "ROC",
                     trControl = tc,
                     tuneGrid = annGrid)
  )
  
  saveRDS(MOD.ann, paste0("./Data/ModelOutput/", this.species, "_", this.climate, "_ann.RDS"))
  MOD.ann
  summary(MOD.ann)
  
  stopCluster(clust)
  registerDoSEQ()
  
  # RESULTS
  
  # Diagnostics
  tmp <- MOD.ann$results
  tmp$sp <- this.species
  tmp$climate <- this.climate
  tmp$method <- "ann"
  
  write.csv(tmp,
            paste0("./Data/modelOutputDiagnostics/", this.species, "_", this.climate, "_ann.csv"),
            row.names = F)
  rm(tmp)
  
  # Variable importance
  tmp <- varImp(MOD.ann)$importance
  tmp$variable <- row.names(tmp)
  tmp$sp <- this.species
  tmp$climate <- this.climate
  tmp$method <- "ann"
  
  write.csv(tmp,
            paste0("./Data/modelOutputVarImp/", this.species, "_", this.climate, "_ann.csv"),
            row.names = F)
  rm(tmp)
}

#--------------------------------------------------------
# Ensembles using caretEnsemble

if (FALSE) {
  # If neccessary, load models to get best tuning params.
  MOD.gbm <- readRDS(paste0("./Data/ModelOutput/", this.species, "_", this.climate, "_gbm.RDS"))
  MOD.gam <- readRDS(paste0("./Data/ModelOutput/", this.species, "_", this.climate, "_gam.RDS"))
  MOD.rf <- readRDS(paste0("./Data/ModelOutput/", this.species, "_", this.climate, "_rf.RDS"))
  #MOD.mars <- readRDS(paste0("./Data/ModelOutput/", this.species, "_", this.climate, "_mars.RDS"))
  MOD.svm <- readRDS(paste0("./Data/ModelOutput/", this.species, "_", this.climate, "_svm.RDS"))
  MOD.ann <- readRDS(paste0("./Data/ModelOutput/", this.species, "_", this.climate, "_ann.RDS"))
  
  
  #Train control
  tc <- trainControl(method = "cv",
                     number = length(folds),
                     search = "grid",
                     classProbs = TRUE,
                     allowParallel = TRUE,
                     summaryFunction = twoClassSummary,
                     index = folds,
                     savePredictions = TRUE)
  
  
  #With tuning params spcecified - a model must be built for each param combination to be tried
  #Use tuning params from already-run models
  
  clust <- makeCluster(detectCores() - 1) # leave 1 core for OS
  registerDoParallel(clust)
  
  model_list <- caretList(
    x = as.data.frame(DAT[ ,10:ncol(DAT)]),
    y = DAT[ ,3],
    trControl=tc,
    metric="ROC",
    continue_on_fail = FALSE,
    tuneList=list(
      gbm=caretModelSpec(method="gbm",
                         tuneGrid=data.frame(interaction.depth=MOD.gbm$bestTune$interaction.depth,
                                             n.trees=MOD.gbm$bestTune$n.trees,
                                             shrinkage=MOD.gbm$bestTune$shrinkage,
                                             n.minobsinnode=MOD.gbm$bestTune$n.minobsinnode)),
      gam = caretModelSpec(method = "bam",
                           tuneGrid = data.frame(method = "fREML", select=FALSE)),
      rf = caretModelSpec(method = "ranger",
                          tuneGrid = data.frame(mtry =MOD.rf$bestTune$mtry,
                                                splitrule = "gini",
                                                min.node.size = 1)),
      # mars = caretModelSpec(method = "earth",
      #                     tunegrid = data.frame(degree = MOD.mars$bestTune$degree)),
      svm = caretModelSpec(method = "svmRadialCost",
                           tuneGrid = data.frame(C = MOD.svm$bestTune$C)),
      ann = caretModelSpec(method = "nnet",
                           tuneGrid = data.frame(size = MOD.ann$bestTune$size,
                                                 decay = MOD.ann$bestTune$decay))
    )
  )
  
  #Ensemble
  MOD.ensemble <- caretEnsemble(
    model_list,
    metric="ROC",
    trControl=trainControl(method = "cv",
                           number=10,
                           summaryFunction=twoClassSummary,
                           allowParallel = TRUE,
                           savePredictions = TRUE,
                           classProbs=TRUE
    ))
  
  #Summary
  summary(MOD.ensemble)
  
  # RESULTS
  
  # Diagnostics
  tmp <- MOD.ensemble$results
  tmp$sp <- this.species
  tmp$climate <- this.climate
  tmp$method <- "ensemble"
  
  write.csv(tmp,
            paste0("./Data/modelOutputDiagnostics/", this.species, "_", this.climate, "_ensemble.csv"),
            row.names = F)
  rm(tmp)
  
  # Variable importance
  tmp <- varImp(MOD.ensemble)$importance
  tmp$variable <- row.names(tmp)
  tmp$sp <- this.species
  tmp$climate <- this.climate
  tmp$method <- "ensemble"
  
  write.csv(tmp,
            paste0("./Data/modelOutputVarImp/", this.species, "_", this.climate, "_ensemble.csv"),
            row.names = F)
  rm(tmp)
  
  #Check the correlations
  cors <- as.data.frame(modelCor(resamples(model_list)))
  row.names(cors) <- NULL
  cors$method <- colnames(cors)
  cors$sp <- this.species
  cors$climate <- this.climate
  write.csv(cors,
            paste0("./Data/modelOutputEnsembleCor/correlations_", this.species, "_", this.climate, ".csv"),
            row.names = F)
  rm(cors)
  
  #Save
  saveRDS(MOD.ensemble, paste0("./Data/ModelOutput/", this.species, "_", this.climate, "_ensemble.RDS"))
  saveRDS(model_list, paste0("./Data/ModelOutput/", this.species, "_", this.climate, "_ensembleList.RDS"))
  
  
  stopCluster(clust)
  registerDoSEQ()
}

rm(DAT)

}
