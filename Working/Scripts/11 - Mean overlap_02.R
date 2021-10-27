## Calculate mean habitat importance

setwd("~/PEIfuture/Working")
setwd("D:/PEIfuture/Working/")

library(raster)
library(sf)
library(pals)

#-------------------------------------------------------------------
# Common variables
source("./Scripts/00 - Project variables.R")

# Raster for cropping
rst.crop <- raster(res = 0.1, xmn = -10, xmx = 70, ymn = -75, ymx = -30, crs = "+proj=longlat +datum=WGS84")

# Function for habitat importance
suitability_to_percentiles <- function(x, N=21) {
  ## expect that x is a raster containing predicted usage probability (suitability multiplied by availability)
  cell_areas <- raster::values(area(x))
  vals <- raster::values(x)
  total_area <- sum(cell_areas, na.rm = T)
  tst <- seq(min(vals,na.rm=TRUE),max(vals,na.rm=TRUE),length.out=N)
  ## calculate the percentage of area corresponding to each of these tst values
  s2p <- function(z) sum(cell_areas[which(vals<=z)])/total_area*100
  arp <- vapply(tst,s2p,FUN.VALUE=1)
  values(x) <- approx(tst,arp,vals)$y
  x
}

#-------------------------------------------------------------------
for(k in c("rcp45", "rcp85")) {
  this_scenario <- k

for (i in c("summer", "winter")) {

this.season <- i

if(this.season == "summer") {
  these_species <- spNamesSummer
} else {
  these_species <- spNamesWinter
}

change_stack <- stack()

all_his_aes <- vector("list", length(all.climate.mods))
all_fut_aes <- vector("list", length(all.climate.mods))

for (j in all.climate.mods) {
  
  this.climate <- j
  
  his_stk <- stack()
  fut_stk <- stack()
  
  for (k in 1:length(these_species)) {
    
    this.species <- these_species[k]
    
    if (this.species != "GHA") {
      
      # Get files
      his_rst <- raster(
        paste0("./Data/predictionOutput/rasters/pred_", this.species, "_", this.climate, "_", this.season, "_historical.grd"))
      
      fut_rst <- raster(
        paste0("./Data/predictionOutput/rasters/pred_", this.species, "_", this.climate, "_", this.season, "_future_", this_scenario, ".grd"))
      
      # Crop
      his_rst <- crop(his_rst, rst.crop)
      fut_rst <- crop(fut_rst, rst.crop)
      
      # Importance
      his_rst <- suitability_to_percentiles(his_rst)
      fut_rst <- suitability_to_percentiles(fut_rst)
      
      # Add to stack
      his_stk <- stack(his_stk, his_rst)
      fut_stk <- stack(fut_stk, fut_rst)
      
    }
    
  }
  
  his_mean <- mean(his_stk)
  fut_mean <- mean(fut_stk)
  
  change <- fut_mean - his_mean
  
  change_stack <- stack(change_stack, change)
  
  # Calculate AES
  thresh <- quantile(as.data.frame(his_mean), probs = c(0.9), na.rm = T) # 90th percentile thrshold
  his_aes <- rasterToContour(his_mean, levels = c(thresh))
  fut_aes <- rasterToContour(fut_mean, levels = c(thresh))
  
  all_his_aes[[j]] <- his_aes
  all_fut_aes[[j]] <- fut_aes
  
}

if (this.season == "summer") {
summer_mean <- mean(change_stack)
all_his_aes_summer <- all_his_aes
all_fut_aes_summer <- all_fut_aes
} else {
  winter_mean <- mean(change_stack)
  all_his_aes_winter <- all_his_aes
  all_fut_aes_winter <- all_fut_aes
}

}

#-------------------------------------------------------------------
## Plot

# Get land
data(countriesLow, package = "rworldmap")
wrld <- crop(countriesLow, rst.crop)

# Get EEZs
# eez <- readRDS("./Data/eez/eez.RDS")
# eez_crop <- st_crop(x = eez, y = rst.crop)
eez_crop <- readRDS("./Data/eez/eez_sp.RDS")

# Get CCAMLR boundary
# ccamlr <- readRDS("./Data/eez/ccamlr.RDS")
# ccamlr_crop <- st_crop(x = ccamlr, y = rst.crop)
ccamlr_crop <- readRDS("./Data/eez/ccamlr_sp.RDS")

# Get MPAs
mpa_crop <- readRDS("./Data/eez/mpa_sp.RDS")

# Islands
islands <- data.frame(x = c(37.743611, 37.943333),
                      y = c(-46.9125, -46.644167))

if (this_scenario == "rcp45") {
  scenario_label <- "RCP 4.5"
} else {
  scenario_label <- "RCP 8.5"
}

# Plot

# Summer
tiff(
  file = paste0("./Data/predictionOutput/habitat_importance_change_summer_", this_scenario, ".tiff"),
  width = 7.85*0.75,
  height = (6.4*0.75)-0.2,
  units = "in",
  res = 600,
  bg = "white"
)

par(mar = c(2, 2, 1.6, 3))

plot(summer_mean,
     main = paste0(scenario_label, " - Summer"),
     font.main = 1,
     col = rev(ocean.balance(121)),
     legend = F,
     breaks = seq(-15, +15, by = 0.25))

# Add legend
plot(summer_mean,
     col = rev(ocean.balance(121)),
     legend.only = T,
     horizontal = F,
     breaks = seq(-15, +15, by = 0.25),
     axis.args = list(at = c(-15, -7.5, 0, +7.5, +15)),
     legend.args = list(text = 'Change in\nmean\nhabitat\nimportance\n'))

# CCAMLR
plot(ccamlr_crop, col = NA, border = "#EE3377", add = TRUE, lwd = 2)

# Add land
plot(wrld, col = "darkgrey", border = TRUE, add = TRUE)

# MPA
plot(mpa_crop, col = NA, add = TRUE, border = "#33BBEE", lwd = 2)

# EEZ
plot(eez_crop, col = NA, add = TRUE)

# Add the islands
points(islands$x, islands$y,
       pch = 16,
       cex = 1.8,
       col = "black")
points(islands$x, islands$y,
       pch = 16,
       cex = 1.4,
       col = "#EE7733")

# for (i in 1:length(all_his_aes_summer)) {
#   this_aes <- all_his_aes_summer[[i]]
#   lines(this_aes, col = "black")
# }
# 
# for (i in 1:length(all_fut_aes_summer)) {
#   this_aes <- all_fut_aes_summer[[i]]
#   lines(this_aes, col = "orange")
# }

dev.off()

# Winter
tiff(
  file = paste0("./Data/predictionOutput/habitat_importance_change_winter_", this_scenario, ".tiff"),
  width = 7.85*0.75,
  height = (6.4*0.75)-0.2,
  units = "in",
  res = 600,
  bg = "white"
)

par(mar = c(2, 2, 1.6, 3))

plot(winter_mean,
     main = paste0(scenario_label, " - Winter"),
     font.main = 1,
     col = rev(ocean.balance(121)),
     legend = F,
     breaks = seq(-15, +15, by = 0.25))

# Add legend
plot(winter_mean,
     col = rev(ocean.balance(121)),
     legend.only = T,
     horizontal = F,
     breaks = seq(-15, +15, by = 0.25),
     axis.args = list(at = c(-15, -7.5, 0, +7.5, +15)),
     legend.args = list(text = 'Change in\nmean\nhabitat\nimportance\n'))

# CCAMLR
plot(ccamlr_crop, col = NA, border = "#EE3377", add = TRUE, lwd = 2)

# Add land
plot(wrld, col = "darkgrey", border = TRUE, add = TRUE)

# MPA
plot(mpa_crop, col = NA, add = TRUE, border = "#33BBEE", lwd = 2)

# EEZ
plot(eez_crop, col = NA, add = TRUE)

# Add the islands
points(islands$x, islands$y,
       pch = 16,
       cex = 1.8,
       col = "black")
points(islands$x, islands$y,
       pch = 16,
       cex = 1.4,
       col = "#EE7733")

# for (i in 1:length(all_his_aes_winter)) {
#   this_aes <- all_hist_aes_winter[[i]]
#   lines(this_aes, col = "black")
# }
# 
# for (i in 1:length(all_fut_aes_winter)) {
#   this_aes <- all_fut_aes_winter[[i]]
#   lines(this_aes, col = "orange")
# }

dev.off()


#----------------------------------------------
# Plot AES

if (FALSE) {
# Summer
tiff(
  file = paste0("./Data/predictionOutput/aes_change_summer_", this_senario, ".tiff"),
  width = 7.85,
  height = 6.4,
  units = "in",
  res = 300,
  bg = "white"
)

par(mar = c(2, 2, 1.6, 3))

plot(summer_mean,
     main = paste0("Summer - ", this_scenario),
     font.main = 1,
     col = "white",
     legend = F)

# Add land
plot(wrld, col = "darkgrey", border = TRUE, add = TRUE)

# Plot the AES
for (i in 1:length(all_his_aes_summer)) {
  this_aes <- all_his_aes_summer[[i]]
  lines(this_aes, col = "black")
}

for (i in 1:length(all_fut_aes_summer)) {
  this_aes <- all_fut_aes_summer[[i]]
  lines(this_aes, col = "#EE7733")
}

# Add the islands
points(islands$x, islands$y,
       pch = 16,
       cex = 1.8,
       col = "black")
points(islands$x, islands$y,
       pch = 16,
       cex = 1.4,
       col = "#EE7733")

dev.off()

# Winter
tiff(
  file = paste0("./Data/predictionOutput/aes_change_winter.tiff"),
  width = 7.85,
  height = 6.4,
  units = "in",
  res = 300,
  bg = "white"
)

par(mar = c(2, 2, 1.6, 3))

plot(winter_mean,
     main = "Winter",
     font.main = 1,
     col = "white",
     legend = F)

# Add land
plot(wrld, col = "darkgrey", border = TRUE, add = TRUE)

# Plot the AES
for (i in 1:length(all_his_aes_winter)) {
  this_aes <- all_his_aes_winter[[i]]
  lines(this_aes, col = "black")
}

for (i in 1:length(all_fut_aes_winter)) {
  this_aes <- all_fut_aes_winter[[i]]
  lines(this_aes, col = "#EE7733")
}

# Add the islands
points(islands$x, islands$y,
       pch = 16,
       cex = 1.8,
       col = "black")
points(islands$x, islands$y,
       pch = 16,
       cex = 1.4,
       col = "#EE7733")

dev.off()

}

}