library(ranger)
library(caret)
library(edarf)


# Parallel
library(parallel)
library(foreach)
library(doParallel)

# Plotting
require(gridExtra)
library(pdp)
library(plotmo)

setwd("~/PEIfuture/Working")

# Make a partial dependence curve?
make_pdp <- TRUE

# Plot a plotmo plot?
plot_plotmo <- FALSE

#----------------------------------------
# Common variables
source("./Scripts/00 - Project variables.R")
model <- "rf"

#----------------------------------------
for (j in c("summer", "winter")) {
  season <- j
  print(season)
  
  if (j == "summer") {
    these_species <- spNamesSummer
  } else {
    these_species <- spNamesWinter
  }
  
  for (k in these_species) {
    species <- k
    print(species)
    
    for (h in all.climate.mods) {
      climate <- h
      print(climate)
      
      #----------------------------------------
      # Load model
      this_mod <- readRDS(paste0("./Data/modelOutput/", species, "_", season, "_", climate, "_", model, ".RDS"))
      
      # Parallel processing
      # cluster <- parallel::makeCluster(detectCores() - 1) # convention to leave 1 core for OS
      # registerDoParallel(cluster)
      
      # Get variables
      vars <- caret::varImp(this_mod)$importance
      vars$varname <- row.names(vars)
      
      #----------------------------------------
      # One by one, get the dataframe
      # for partial dependendence plots from pdp package
      if (make_pdp) {
        cat("Constsructing partial dependence \n")
        
        hold <- data.frame()
        
        for (i in 1:length(vars$varname)) {
          this_var <- vars$varname[i]
          print(this_var)
          
          # Construct the partial dependence curve
          foo <- partial(this_mod$finalModel,
                         pred.var = paste(this_var),
                         # plot = TRUE,
                         plot = FALSE,
                         prob = TRUE,
                         parallel = FALSE,
                         rug = FALSE,
                         smooth = FALSE,
                         train = this_mod$trainingData,
                         paropts = list(.packages = c("ranger", "caret")))
          
          # Organise output
          names(foo) <- c("covariate_value", "yhat")
          foo$covariate <- this_var
          foo$species <- species
          foo$season <- season
          foo$climate <- climate
          
          hold <- rbind(hold, foo)
          
        }
        
        # Save
        write.csv(hold, paste0("./Data/modelOutputPartialCurve/", species, "_", climate, "_", season, ".csv"),
                  row.names = F)
        
      }
      
      #---------------------------------------- 
      # Plot plotmos?
      if (plot_plotmo) {
        
        # Filename
        this_plotmo_filename <- paste0("./Data/modelOutputPartialPlot/", species, "_", climate, "_", season, ".pdf")
        
        # Load the right data
        DAT <- readRDS(paste0("./Data/trackDataEnvar/", species, "_", climate, ".RDS"))
        DAT[DAT$s == "observed track", "s"] <- "Obs"
        DAT[DAT$s == "simulated track", "s"] <- "Sim"
        DAT$s <- as.factor(DAT$s)
        ssn <- readRDS("~/PEIfuture/Working/Data/other/ALL_seasons.rds")
        ssn <- ssn[ssn$which.season == season, ]
        DAT <- DAT[DAT$track_id %in% ssn$track_id, ]
        DAT <- DAT[complete.cases(DAT[ ,c(9:ncol(DAT))]), ]
        
        
        # Plot
        cat("Plotting plotmo \n")
        pdf(file = paste0(this_plotmo_filename), paper = "a4r")
        plotmo(this_mod,
               type = "prob",
               all1 = TRUE,
               degree1 = vars$varname,
               ylim=c(0,1),
               caption = paste(species, season, climate, sep = " - "),
               nrug = 10, rug.ticksize = 0.1, rug.lwd = 1, rug.col = "gray50")
        # while (!is.null(dev.list()))
        dev.off()
      }
      
      #----------------------------------------
    }
  }
}

#----------------------------------------
# Make plots