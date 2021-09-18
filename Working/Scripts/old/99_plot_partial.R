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

# Common variables
source("./Scripts/00 - Project variables.R")

species <- "AFS"
season <- "summer"
climate <- "CanESM2"
model <- "rf"







# Load model
this_mod <- readRDS(paste0("./Data/modelOutput/", species, "_", season, "_", climate, "_", model, ".RDS"))

# Give a file name
this_pdp_filename <- "test.pdf"
this_plotmo_filename <- "test.pdf"

plot_plotmo <- TRUE
plot_pdp <- FALSE

# Parallel processing
cluster <- parallel::makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

# Get variables
vars <- caret::varImp(this_mod)$importance
vars$varname <- row.names(vars)

if (plot_pdp) {
  cat("Plotting partial dependence \n")
  plts <- list()
  for (i in 1:length(vars$varname[1])) {
    print(vars$varname[i])
    plts[i] <- list(partial(this_mod$finalModel,
                            pred.var = paste(vars$varname[i]),
                            # plot = TRUE,
                            plot = FALSE,
                            prob = TRUE,
                            parallel = FALSE,
                            rug = T,
                            smooth = FALSE,
                            train = this_mod$trainingData,
                            paropts = list(.packages = "ranger")))
  }
  #dev.off()
  while (!is.null(dev.list()))  dev.off()
  ggsave(paste(this_pdp_filename),
         marrangeGrob(plts,
                      ncol=4,
                      nrow=3,
                      as.table = TRUE,
                      top = paste(species, " | ", season, " | ", climate)),
         device = "pdf",
         width = 297, height = 210, units = "mm")
}

if (plot_plotmo) {
  
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
           caption = paste(species, season, climate),
           nrug = 10, rug.ticksize = 0.1, rug.lwd = 1, rug.col = "gray50")
    # while (!is.null(dev.list()))
    dev.off()
}
