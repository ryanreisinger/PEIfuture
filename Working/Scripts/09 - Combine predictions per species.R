# Compile predictions from different climate models

# Ryan R Reisinger

# Last modified: 2019-10-25

library(raster)
library(SOmap)
library(pals)
library(geosphere)
library(ggplot2)
library(rworldmap)
library(sf)


setwd("~/PEIfuture/Working")

#-------------------------------------------------------------------
# Main params
# this.species <- "AFS"
this.season <- "winter"

# Common variables
source("./Scripts/00 - Project variables.R")
spNamesSummer <- spNamesSummer[spNamesSummer != "GHA"]

for (z in spNamesWinter) {
  this.species <- z
  print(z)

# Raster for cropping
rst.crop <- raster(res = 0.1, xmn = -10, xmx = 70, ymn = -75, ymx = -30, crs = "+proj=longlat +datum=WGS84")

# Function for Range shift
# modified from:
# https://livefreeordichotomize.com/2018/06/27/bringing-the-family-together-finding-the-center-of-geographic-points-in-r/
geographic_average <- function(r) {
  r.df <- as.data.frame(rasterToPoints(r))
  lon <- weighted.mean(r.df$x, w = r.df$p)
  lat <- weighted.mean(r.df$y, w = r.df$p)
  data.frame(lon = lon, lat = lat)
}

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

# Step through each of the climate models

# Hold the geographic mean results
all.geomeans <- data.frame()

# Hold the distances
dists <- data.frame()

# Hold the rasters
all.rasters.his <- stack()
all.rasters.fut <- stack()
all.rasters.diff <- stack()

for (i in 1:length(all.climate.mods)) {
  # i = 1
  this.climate <- all.climate.mods[i]
  print(this.climate)
  
  # Load the predictions
  his.rst <- raster(
              paste0("./Data/predictionOutput/rasters/pred_", this.species, "_", this.climate, "_", this.season, "_historical.grd"))
  
  fut.rst <- raster(
              paste0("./Data/predictionOutput/rasters/pred_", this.species, "_", this.climate, "_", this.season, "_future.grd"))
  
  # Crop
  his.rst <- crop(his.rst, rst.crop)
  fut.rst <- crop(fut.rst, rst.crop)
  
  # Importance transformation
  # his.rst <- suitability_to_percentiles(his.rst)
  # fut.rst <- suitability_to_percentiles(fut.rst)
  
  # Difference
  diff.rst <- fut.rst - his.rst
  
  diff.rst[diff.rst < -0.6] <- -0.6 # For plotting
  
  # -----------------------------------
  # Calculate the weighted mean distribution
  his_mean <- geographic_average(his.rst)
  fut_mean <- geographic_average(fut.rst)
  
  # -----------------------------------
  # Plot to check
  plot(diff.rst)
  # lines(rbind(his_mean, fut_mean))
  points(his_mean$lon, his_mean$lat, col = "black", pch = 16)
  arrows(his_mean$lon, his_mean$lat, fut_mean$lon, fut_mean$lat, length = 0.05)
  # points(fut_mean$lon, fut_mean$lat, col = "red", pch = 16)
  
  # -----------------------------------
  # Keep the geographic means
  his_mean$when <- "present"
  fut_mean$when <- "future"
  geomeans <- rbind(his_mean, fut_mean)
  geomeans$species <- this.species
  geomeans$season <- this.season
  geomeans$model <- this.climate
  
  all.geomeans <- rbind(all.geomeans, geomeans)
  
  # -----------------------------------
  # Distance to important habitats
  his.df <- as.data.frame(rasterToPoints(his.rst))
  names(his.df) <- c("lon", "lat", "score")
  his.df$when <- "present"
  
  fut.df <- as.data.frame(rasterToPoints(fut.rst))
  names(fut.df) <- c("lon", "lat", "score")
  fut.df$when <- "future"
  
  home <- c(37.717325, -46.903322)
  
  his.df$distance <- distGeo(p1 = his.df[ , c("lon", "lat")], p2 = home)/1000
  fut.df$distance <- distGeo(p1 = fut.df[ , c("lon", "lat")], p2 = home)/1000
  
  d <- rbind(his.df, fut.df)
  d$species <- this.species
  d$season <- this.season
  d$model <- this.climate
  
  dists <- rbind(dists, d)
  
  # -----------------------------------
  # Keep the rasters in a stack
  all.rasters.his <- stack(all.rasters.his, his.rst)
  all.rasters.fut <- stack(all.rasters.fut, fut.rst)
  all.rasters.diff <- stack(all.rasters.diff, diff.rst)
  
}

# Give names to the raster layers
names(all.rasters.diff) <- all.climate.mods
names(all.rasters.his) <- all.climate.mods
names(all.rasters.fut) <- all.climate.mods

# Calculate means and uncertainty
mean.diff <- mean(all.rasters.diff, na.rm = T)
sd.rast <- calc(x = all.rasters.diff, fun = sd, na.rm = T)

# -----------------------------------
# Save the outputs

# The rasters
writeRaster(mean.diff, paste0("./Data/predictionOutput/perSpecies/rasters/meandiff_",
                      this.species, "_", this.season, ".grd"), format = "raster",
            overwrite = T)
writeRaster(sd.rast, paste0("./Data/predictionOutput/perSpecies/rasters/uncertainty_",
                              this.species, "_", this.season, ".grd"), format = "raster",
            overwrite = T)


# The geographically weighted range means
saveRDS(all.geomeans, paste0("./Data/predictionOutput/perSpecies/dataframes/rangeCenters_",
                             this.species, "_", this.season, ".RDS"))
# The distances
saveRDS(dists, paste0("./Data/predictionOutput/perSpecies/dataframes/distances_",
                             this.species, "_", this.season, ".RDS"))

# -----------------------------------
# Plot the distances
pdf(paste0("./Data/predictionOutput/distanceChange/",
                      this.species, "_", this.season, ".pdf"), paper = "a4")
this_plot <- ggplot(dat = dists[dists$score > 0.3, ], aes(x = distance, fill = when, color = when, group = when)) +
  geom_density(alpha = 0.3) +
  scale_color_manual(values = c("#BF583B", "#3787BA"), guide = FALSE) +
  scale_fill_manual(values = c("#BF583B", "#3787BA"), name = "Time period", labels = c("Future", "Current")) +
  coord_flip() +
  facet_wrap(~model, ncol = 2, drop = T, scales = "free_x") +
  theme_bw() +
  labs(title = this.species, subtitle = this.season, y = "Density", x = "Distance to habitat (km)")
print(this_plot)
dev.off()

# -----------------------------------
# Plots in base

# Get land
data(countriesLow, package = "rworldmap")
wrld <- crop(countriesLow, rst.crop)

# Get EEZs
eez <- readRDS("./Data/eez/eez_sp.RDS")

# Get MPAs
mpa <- readRDS("./Data/eez/mpa_sp.RDS")

# Get CCAMLR boundary
ccamlr <- readRDS("./Data/eez/ccamlr_sp.RDS")

# -------------------
# Plot mean
tiff(
  file = paste0("./Data/predictionOutput/perSpecies/plots/", this.species, "_", this.season, "_mean_difference_BASE.tiff"),
  width = 8.085*0.8,
  height = 6.4*0.8,
  units = "in",
  res = 300,
  bg = "white"
)

par(mar = c(2, 2, 1.6, 3))

plot(mean.diff,
     col = rev(ocean.balance(141)),
     main = paste(this.species, "-", this.season, sep = " "),
     font.main = 1,
     legend = F,
     breaks = seq(-0.7, 0.7, by = 0.01))

# Add legend
plot(mean.diff,
       col = rev(ocean.balance(141)),
       legend.only = T,
       horizontal = F,
       breaks = seq(-0.7, 0.7, by = 0.01),
       axis.args = list(at = c(-1, -0.5, 0, +0.5, +1)),
       legend.args = list(text = 'Change in\nhabitat\nselection'))

# CCAMLR
plot(ccamlr, col = NA, border = "#EE3377", add = TRUE, lwd = 2)

# Add land
plot(wrld,
     col = "darkgrey",
     border = TRUE,
     add = TRUE
)

# MPA
plot(mpa, col = NA, border = "#33BBEE", add = TRUE, lwd = 2)

# EEZ
plot(eez, col = NA, add = TRUE)


# Add the islands
# Islands
islands <- data.frame(x = c(37.743611, 37.943333),
                      y = c(-46.9125, -46.644167))

points(islands$x, islands$y,
       pch = 16,
       cex = 1.8,
       col = "black")
points(islands$x, islands$y,
       pch = 16,
       cex = 1.4,
       col = "#EE7733")

# Add the range shifts
for (k in all.climate.mods) {
  foo <- all.geomeans[all.geomeans$model == k, ]
  points(foo[foo$when == "present",]$lon, foo[foo$when == "present",]$lat, col = "black", pch = 16, cex = 1.4)
arrows(foo[foo$when == "present",]$lon,
       foo[foo$when == "present",]$lat,
       foo[foo$when == "future",]$lon,
       foo[foo$when == "future",]$lat,
       length = 0.05)
}

dev.off()

# -------------------
# Plot uncertainty
tiff(
  file = paste0("./Data/predictionOutput/perSpecies/plots/", this.species, "_", this.season, "_sd_difference_BASE.tiff"),
  width = 7.5*0.8,
  height = 7.475*0.8,
  units = "in",
  res = 300,
  bg = "white"
)

par(mar = c(7, 2, 1.6, 1))

plot(sd.rast,
     col = parula(101),
     main = paste(this.species, "-", this.season, sep = " "),
     font.main = 1,
     legend = F,
     breaks = seq(0, 0.5, by = 0.005))

# Add legend
plot(sd.rast,
       col = parula(101),
       legend.only = T,
       horizontal = TRUE,
       breaks = seq(0, 0.5, by = 0.005),
       axis.args = list(at = c(0, 0.25, 0.5)),
       legend.args = list(text = 'SD of change in habitat selection'))

# CCAMLR
plot(ccamlr, col = NA, border = "#EE3377", add = TRUE, lwd = 2)

# Add land
plot(wrld,
     col = "darkgrey",
     border = TRUE,
     add = TRUE
)

# MPA
plot(mpa, col = NA, border = "#33BBEE", add = TRUE, lwd = 2)

# EEZ
plot(eez, col = NA, add = TRUE)

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

# ---------------------------
# Plots with SOplot

# Predefine the projection
prj <- "+proj=stere +lat_0=-59.6069608076895 +lon_0=30 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

mean.diff.p <- SOproj(mean.diff, target = prj)

tiff(
  file = paste0("./Data/predictionOutput/perSpecies/plots/", this.species, "_", this.season, "_mean_difference.tiff"),
  width = 8.75*0.8,
  height = 7.2*0.9,
  units = "in",
  res = 300,
  bg = "white"
)

# Set up the plot
p <- SOmap_auto(mean.diff.p, target = prj)
plot(p)

# Plot the raster
SOplot(mean.diff.p, col = rev(ocean.balance(141)),
       target = prj,
       legend = F,
       breaks = seq(-0.7, 0.7, by = 0.01))

# CCAMLR
SOplot(SOauto_crop(ccamlr, p), col = NA, border = "#EE3377")

# MPA
SOplot(SOauto_crop(mpa, p), col = NA, border = "#33BBEE")

# EEZ
SOplot(SOauto_crop(eez, p), col = NA)

# Add islands
SOplot(islands$x, islands$y,
       target = prj,
       pch = 19,
       cex = 1.2,
       col = "black")
SOplot(islands$x, islands$y,
       target = prj,
       pch = 19,
       cex = 0.9,
       col = "#EE7733")

# Add the range shifts
for (k in all.climate.mods) {
  foo <- all.geomeans[all.geomeans$model == k, ]
  foo_sp <- foo
  coordinates(foo_sp) <- foo_sp[ , c("lon", "lat")]
  crs(foo_sp) <- "+proj=longlat +datum=WGS84 +no_defs"
  foo_sp <- spTransform(foo_sp, SOcrs())
  foo$x <- coordinates(foo_sp)[,1]
  foo$y <- coordinates(foo_sp)[,2]
  
  SOplot(foo[foo$when == "present",]$lon,
         foo[foo$when == "present",]$lat,
         target = prj,
         col = "black", pch = 16)
  
  # SOplot(foo[foo$when == "future",]$lon,
  #        foo[foo$when == "future",]$lat,
  #        target = prj,
  #        col = "red", pch = 16)
  
  arrows(foo[foo$when == "present",]$x,
         foo[foo$when == "present",]$y,
         foo[foo$when == "future",]$x,
         foo[foo$when == "future",]$y,
         length = 0.05)
  
}

# Plot the legend
SOplot(mean.diff.p, col = rev(ocean.balance(141)),
       target = prj,
       legend.only = T,
       horizontal = TRUE,
       breaks = seq(-0.7, 0.7, by = 0.01),
       axis.args = list(at = c(-0.6, -0.3, 0, +0.3, +0.6),
                        cex = 0.8,
                        cex.axis = 0.8),
       legend.args = list(text = 'Change in habitat selection',
                          cex = 0.8))

# Title
title(main = paste0(this.species, " - ", this.season),
      cex.main = 0.8,
      font.main = 1)

dev.off()

}
