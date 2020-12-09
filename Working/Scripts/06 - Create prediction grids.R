# Create grids of environmental covariates for prediction

setwd("D:/PEIfuture/Working/")

library(raster)

source("./Scripts/00 - Project variables.R")

# -----------------------------
# Define summer and winter months
summer.months <- c(1, 2, 10, 11, 12)
winter.months <- c(3, 4, 5, 6, 7, 8, 9)

# -----------------------------
# Define function load and interpolate rasters
xtractGrid <- function(this.climate.mod = "ACCESS1-0",
                       which.season = "summer",
                       which.time = "historical") {
  
  print(this.climate.mod)
  
  # Define the common raster
  R <- raster(ext = extent(c(-180, 180, -80, -30)), res = 0.5)
  
  # Get depth
  dep <- raster("D:/RAATDfuture/Working/Data/StaticCovariates/depth.grd")
  
  # List all the files
  list.rasters.dyn <- c(
    list.files("D:/RAATDfuture/Working/Data/ClimateProcessed/",
               full.names = T, recursive = T),
    list.files("D:/RAATDfuture/Working/Data/ClimateDerived/",
               full.names = T, recursive = T)
  )
  
  # List only files for this climate model
  list.rasters.dyn <- list.rasters.dyn[grep(this.climate.mod, list.rasters.dyn)]
  
  # List only the rasters
  list.rasters.dyn <- list.rasters.dyn[grep(".grd", list.rasters.dyn)]
  
  # And only the historical OR RCP8.5
  list.rasters.dyn <- list.rasters.dyn[grep(which.time, list.rasters.dyn)]
  
  
  hlpr <- function (varname = "sic") {
    print(varname)
    this.raster.name <- list.rasters.dyn[grep(varname, list.rasters.dyn)]
    if (length(this.raster.name) != 0) {
      this.raster <- stack(this.raster.name)
      
      if (which.season == "summer") {
        this.raster <- mean(subset(this.raster, subset = summer.months))
      }
      if (which.season == "winter") {
        this.raster <- mean(subset(this.raster, subset = winter.months))
      }
      this.raster <- resample(this.raster, R, method = "bilinear")
      return(this.raster)
      
    } else {
      print("This variable is not in the model's output -- returning an empty raster")
      R <- setValues(R, NA)
      return(R)
    }
  }
  
  # 1. Sea ice concentration
  ICE <- hlpr(varname = "sic")
  
  # 2. Sea surface temperature
  SST <- hlpr(varname = "tos")
  
  # 3. U wind
  WINu <- hlpr(varname = "uas")
  
  # 4. V wind
  WINv <- hlpr(varname = "vas")
  
  # 5. U current
  CURu <- hlpr(varname = "uo")
  
  # 6. V current
  CURv <- hlpr(varname = "vo")
  
  # 7. Sea surface height
  SSH <- hlpr(varname = "zos")
  
  # 7. Chlorophyll concentration
  CHL <- hlpr(varname = "chl")
  
  # 9. Sea surface height gradient
  SSHg <- hlpr(varname = "zosgrad")
  
  # 10. Sea surface temperature gradient
  SSTg <- hlpr(varname = "tosgrad")
  
  # 11. Eddy kinetic energy
  EKE <- hlpr(varname = "eke")
  
  # 12. Depth
  DEP <- resample(dep, R, method = "bilinear")
  
  stk <- stack(ICE, SST, WINu, WINv, CURu, CURv, SSH, CHL, SSHg, SSTg, EKE, DEP)
  names(stk) <- c("ICE", "SST", "WINu", "WINv", "CURu", "CURv", "SSH", "CHL", "SSHg", "SSTg", "EKE", "DEP")
  #return(stk)
  grd <- as.data.frame(rasterToPoints(stk))
  saveRDS(grd, paste0("./Data/predictionGrids/predGrid_",
                      this.climate.mod,
                      "_",
                      which.season,
                      "_",
                      which.time,
                      ".RDS"))
}

# -----------------------------

lapply(all.climate.mods, xtractGrid,
           which.season = "summer",
           which.time = "historical")

lapply(all.climate.mods, xtractGrid,
           which.season = "summer",
           which.time = "rcp85")

lapply(all.climate.mods, xtractGrid,
           which.season = "winter",
           which.time = "historical")

lapply(all.climate.mods, xtractGrid,
           which.season = "winter",
           which.time = "rcp85")
