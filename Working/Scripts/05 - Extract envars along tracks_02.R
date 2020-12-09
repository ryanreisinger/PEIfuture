# Extract covariates along tracks

setwd("D:/PEIfuture/Working/")

library(raster)

################################

this.species <- "AFS"

################################

source("./Scripts/00 - Project variables.R")

## Get the tracking data
tracks <- read.csv(paste0("./Data/trackDataFormatted/tracksSim&Real_",
                          this.species, ".csv"),
                   stringsAsFactors = F)

## Define the workhorse function

xtractR <- function(this.climate.mod = "ACCESS1-0", these.tracks = tracks) {
  
  print(this.climate.mod)
  
  # Format the dates
  these.tracks$date <- strptime(these.tracks$date, format = "%Y-%m-%d %H:%M:%S",
                                tz = "GMT")
  
  # Create a month indicator
  these.tracks$month <- format(these.tracks$date, format = "%m")
  
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
  
  # And only the historical ones
  list.rasters.dyn <- list.rasters.dyn[grep("historical", list.rasters.dyn)]
  
  # Don't use mean summer and mean winter layers
  
  hlpr <- function (varname = "zosgrad") {
    print(varname)
    this.raster.name <- list.rasters.dyn[grep(varname, list.rasters.dyn)]

    # Drop Ben's derived layers
    if (length(this.raster.name) > 1) {
    this.raster.name <- this.raster.name[-(grep("_sd_", this.raster.name))]
    this.raster.name <- this.raster.name[-(grep("_mean_", this.raster.name))]
    }
    
    if (length(this.raster.name) != 0) {
      this.raster <- stack(this.raster.name)
      # Extract all values
      foo <- raster::extract(this.raster, y = these.tracks[ , c("lon", "lat")])
      # Get the value from the layer corresponding to the month of the location
      foo <- foo[cbind(1:nrow(these.tracks),as.integer(these.tracks$month))]
      return(foo)
    } else {
      print("Hold on, champ -- this variable not in the model's output")
      return(NULL)
    }
  }
      
  # 1. Sea ice concentration
  these.tracks$ICE <- hlpr(varname = "sic")
  
  # 2. Sea surface temperature
  these.tracks$SST <- hlpr(varname = "tos")
  
  # 3. U wind
  these.tracks$WINu <- hlpr(varname = "uas")
  
  # 4. V wind
  these.tracks$WINv <- hlpr(varname = "vas")
  
  # # Wind
  # these.tracks$WIN <- hlpr(varname = "wind")
  
  # 5. U current
  these.tracks$CURu <- hlpr(varname = "uo")
  
  # 6. V current
  these.tracks$CURv <- hlpr(varname = "vo")
  
  # # Current
  # these.tracks$CUR <- hlpr(varname = "curr")
  
  # 7. Sea surface height
  these.tracks$SSH <- hlpr(varname = "zos")
  
  # 7. Chlorophyll concentration
  # these.tracks$CHL <- hlpr(varname = "chl")
  
  # 9. Sea surface height gradient
  these.tracks$SSHg <- hlpr(varname = "zosgrad")
  
  # 10. Sea surface temperature gradient
  these.tracks$SSTg <- hlpr(varname = "tosgrad")
  
  # 11. Eddy kinetic energy
  # these.tracks$EKE <- hlpr(varname = "eke")
  
  # Surface downward heat flux
  # these.tracks$SHF <- hlpr(varname = "hfds")
  
  # 12. Depth
  these.tracks$DEP <- raster::extract(dep, these.tracks[ , c("lon", "lat")])
  
  saveRDS(these.tracks, paste0("./Data/trackDataEnvar/", this.species, "_", this.climate.mod, ".RDS"))
  
}

## Apply for each model
lapply(all.climate.mods, xtractR, these.tracks = tracks)
