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
library(ggplot2)

setwd("~/PEIfuture/Working")


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
    
    
    
    #---------------------------
    # Run the pdps in parrallel
    
    # Libraries for parallel processing
    require(iterators)
    require(foreach)
    require(parallel)
    require(doParallel)
    
    # Set up
    ncr <- detectCores() - 1
    cluster <- makeCluster(ncr, type = "FORK", outfile = "")
    registerDoParallel(cluster)
    
    
    # Fit and predict
    out <- foreach(h = all.climate.mods,
                    .combine = 'c',
                    .inorder =FALSE,
                    .packages = c("pdp", "ranger", "caret")) %dopar% {
                      
                      climate <- h
                      #----------------------------------------
                      # Load model
                      this_mod <- readRDS(paste0("./Data/modelOutput/", species, "_", season, "_", climate, "_", model, ".RDS"))
                      
                      # Get variables
                      vars <- caret::varImp(this_mod)$importance
                      vars$varname <- row.names(vars)
                      
                      #----------------------------------------
                      # One by one, get the dataframe
                      # for partial dependendence plots from pdp package
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
                      
                      return("done")
                    }
    
    
    # Stop the cluster
    stopCluster(cluster)
    #---------------------------
    
  }
}

#----------------------------------------
# Make plots

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
    
    # Get the output
    these_files <- list.files("./Data/modelOutputPartialCurve/", pattern = "*.csv", full.names = T)
    these_files <- these_files[grepl(species, these_files)]
    these_files <- these_files[grepl(season, these_files)]
    
    if (length(these_files) > 0) {
    
    # Read in
    this_dat <- do.call(rbind, lapply(these_files, read.csv, stringsAsFactors = F))
    
    # Plot
    png(paste0("./Data/modelOutputPartialPlot/pdp_", species, "_", season, ".png"),
        width = 9,
        height = 12,
        units = "in",
        res = 300)
    p <- ggplot(data = this_dat, aes(x = covariate_value, y = yhat, group = climate, colour = climate)) +
      geom_line() +
      facet_wrap(~covariate, ncol = 4, scales = "free_x") +
      scale_color_brewer(type = "qual", palette = "Set1", name = "Model") +
      labs(title = paste(species, season, sep = " - "), x = "Covariate value", y = "p(observed track location)") +
      theme_bw() +
      theme(axis.text = element_text(colour = "black"),
            plot.background = element_rect(fill = "white", colour = "white"))
    plot(p)
    dev.off()
    }
  }
}
