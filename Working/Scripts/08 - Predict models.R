# Predict fitted models

# Ryan R Reisinger

# Last modified: 2019-10-04

# library(foreach)
# library(parallel)
# library(doParallel)

## Various machine learning models fit in caret
library(gbm)
library(mgcv)
library(ranger)
#library(earth)
library(caret)
library(e1071)
library(pROC)
# library(caretEnsemble)
library(plyr)

library(raster)
library(janitor)

library(pals)

library(SOmap)

library(sf)

setwd("~/PEIfuture/Working")

#-------------------------------------------------------------------
# Main params
this.species <- "WCP"
# this.climate <- "ACCESS1-0"
this.season <- "summer"

# Common variables
source("./Scripts/00 - Project variables.R")

#-------------------------------------------------------------------
# Function for prediction
predictR <- function (data = NULL,
                      species = this.species,
                      model = "rf",
                      season = this.season,
                      climate = this.climate){
  # Load model
  mod <- readRDS(paste0("./Data/modelOutput/", species, "_", season, "_", climate, "_", model, ".RDS"))
  
  # Dump empty columns
  data <- remove_empty(dat = data, which = "cols")
  
  # Predict model
  if (model == "gbm") {
    pred <- predict.train(mod, newdata = data, type = "prob", na.action = "na.keep")
    # Get predicted obs
    data$p <- pred$Obs
  }
  
  if (model == "rf") {
    dx <- which(complete.cases(data))
    pred <- predict.train(mod, newdata = data[dx, ], type = "prob", na.action = "na.keep")
    data$p <- NA
    data$p[dx] <- pred$Obs
    # Currently can't pass qunatile prediction to caret
    # pred.q <- predict.train(mod, newdata = data[dx, ], type = "quantiles",
    #                         quantiles = c(0.25, 0.75), na.action = "na.keep")
    # data$q25 <- NA
    # data$q75 <- NA
    # data$q25[dx] <- pred.q$25
    # data$q75[dx] <- pred.q$75
    
  }
  
  return(data)
}

# -----------------------------------
# Get grids to add each prediction to
his.grid <- readRDS(paste0("./Data/predictionGrids/predGrid_", "ACCESS1-0", "_", this.season, "_historical.RDS"))
fut.grid <- readRDS(paste0("./Data/predictionGrids/predGrid_", "ACCESS1-0", "_", this.season, "_rcp85.RDS"))

his.grid <- his.grid[, c("x", "y")]
fut.grid <- fut.grid[, c("x", "y")]

# Raster template
rst <- raster(res = 0.1, xmn = -180, xmx = 180, ymn = -80, ymx = -30, crs = "+proj=longlat +datum=WGS84")

# Mapping stuff
rst.crop <- raster(res = 0.1, xmn = -10, xmx = 70, ymn = -75, ymx = -30, crs = "+proj=longlat +datum=WGS84")
data(countriesLow, package = "rworldmap")
wrld <- crop(countriesLow, rst.crop)

# Get EEZs, MPAs and CCAMLR
eez <- readRDS("./Data/eez/eez_sp.RDS")
mpa <- readRDS("./Data/eez/mpa_sp.RDS")
ccamlr <- readRDS("./Data/eez/ccamlr_sp.RDS")

# ----------------------------------------------------------------------
# Loop through the models, predicing each
for (i in 1:length(all.climate.mods)) {
  # i = 1
  this.climate <- all.climate.mods[i]
  print(this.climate)
  
  # -----------------------------------
  # Load the data for prediction
  this.his.grid <- readRDS(paste0("./Data/predictionGrids/predGrid_", this.climate, "_", this.season, "_historical.RDS"))
  this.fut.grid <- readRDS(paste0("./Data/predictionGrids/predGrid_", this.climate, "_", this.season, "_rcp85.RDS"))
  
  # -----------------------------------
  # Predict
  # Historical
  this.his.pred <- predictR(data = this.his.grid,
                            species = this.species,
                            model = "rf",
                            climate = this.climate)
  
  # Future
  this.fut.pred <- predictR(data = this.fut.grid,
                            species = this.species,
                            model = "rf",
                            climate = this.climate)
  
  # Create rasters
  his.rst <- rasterFromXYZ(this.his.pred[, c("x", "y", "p")])
  fut.rst <- rasterFromXYZ(this.fut.pred[, c("x", "y", "p")])
  
  # Projections
  crs(his.rst) <- "+proj=longlat +datum=WGS84"
  crs(fut.rst) <- "+proj=longlat +datum=WGS84"
  
  diff.rst <- fut.rst - his.rst
  
  # Save rasters
  writeRaster(his.rst,
              paste0("./Data/predictionOutput/rasters/pred_", this.species, "_", this.climate, "_", this.season, "_historical.grd"),
              overwrite = T)
  
  writeRaster(fut.rst,
              paste0("./Data/predictionOutput/rasters/pred_", this.species, "_", this.climate, "_", this.season, "_future.grd"),
              overwrite = T)
  
  # Plots
  # Crop rasters
  his.rst <- crop(his.rst, rst.crop)
  fut.rst <- crop(fut.rst, rst.crop)
  diff.rst <- crop(diff.rst, rst.crop)
  
  # Islands
  islands <- data.frame(x = c(37.743611, 37.943333),
                        y = c(-46.9125, -46.644167))
  coordinates(islands) <- ~x+y
  crs(islands) <- "+proj=longlat +datum=WGS84 +no_defs"
  
  
  tiff(
    file = paste0("./Data/predictionOutput/plots/pred_", this.species, "_", this.climate, "_", this.season, "_difference.tiff"),
    width = 8.75,
    height = 7.5,
    units = "in",
    res = 300,
    bg = "white"
  )
  
  # Set up the plot
  p <- SOmap_auto(diff.rst)
  plot(p)
  
  # Plot the raster
  SOplot(diff.rst, col = rev(ocean.balance(201)),
         alpha = 1,
         legend = F,
         breaks = seq(-1, 1, by = 0.01))
  
  # EEZs, etc.
  ccamlr_p <- st_transform(ccamlr, SOcrs())
  plot(SOauto_crop(ccamlr, p), add = T, col = NA, border = "#ee337")
  
  mpa_p <- st_transform(mpa, SOcrs())
  plot(SOauto_crop(mpa_p, p), add = T, col = NA, border = "#33BBEE")
  
  eez_p <- st_transform(eez, SOcrs())
  plot(SOauto_crop(eez_p, p), add = T, col = NA)
  
  # Reproject the islands to the correct CRS, and add
  islands <- spTransform(islands, SOcrs())
  points(islands,
         pch = 19,
         cex = 1.2,
         col = "black")
  points(islands,
         pch = 19,
         cex = 0.9,
         col = "#EE7733")
  
  # Plot the raster legend
  SOplot(diff.rst, col = rev(ocean.balance(201)),
         alpha = 1,
         legend.only = T,
         horizontal = TRUE,
         breaks = seq(-1, 1, by = 0.01),
         axis.args = list(at = c(-1, -0.75, -0.5, -0.25, 0, +0.25, +0.5, +0.75, +1),
                          cex = 0.8,
                          cex.axis = 0.8),
         legend.args = list(text = 'Change in habitat selection',
                            cex = 0.8))
  
  # Title
  title(main = paste0(this.species, " - ", this.climate, " - ", this.season),
        cex.main = 0.8,
        font.main = 1)
  
  dev.off()
  
}

