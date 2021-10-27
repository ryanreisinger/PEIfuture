## Calculate change in habitat area

setwd("~/PEIfuture/Working")

library(raster)
library(ggplot2)
library(sf)
library(dplyr)

#------------------------------------------
# Common variables
source("./Scripts/00 - Project variables.R")

rst.crop <- raster(res = 0.1, xmn = -10, xmx = 70, ymn = -75, ymx = -30, crs = "+proj=longlat +datum=WGS84")

outer_hold <- data.frame()

scenarios <- c("rcp45", "rcp85")

for (z in scenarios) {
  
  this_scenario <- z

big_hold <- data.frame()

for (k in c("summer", "winter")) {
  
# this.season <- "winter"
this.season <- k

if (this.season == "winter") {
  spNames <- spNamesWinter
} else {
  spNames <- spNamesSummer
}

spNames <- spNames[spNames != "GHA"]

species_hold <- data.frame()

for (i in spNames) {
  
  this.species <- i
  print(i)
  
  climate_hold <- data.frame()
  
  for (j in all.climate.mods) {
    
    this.climate <- j
    
    # Historical
    his_rst <- raster(
      paste0("./Data/predictionOutput/rasters/pred_", this.species, "_", this.climate, "_", this.season, "_historical.grd"))
    
    # Future
    fut_rst <- raster(
      paste0("./Data/predictionOutput/rasters/pred_", this.species, "_", this.climate, "_", this.season, "_future_", this_scenario, ".grd"))
    
    # Crop
    his_rst <- crop(his_rst, rst.crop)
    fut_rst <- crop(fut_rst, rst.crop)
    
    # Calculate area
    rst_area_his <- area(his_rst)
    rst_area_fut <- area(fut_rst)
    
# Historical
    his <- as.data.frame(his_rst)     # To dataframe
    his$area <- as.data.frame(rst_area_his)[,1]     # Add area to dataframe
    his <- his[complete.cases(his), ]     # Remove NAs
    thresh <- quantile(his$p, probs = c(0.9)) # Get thrshold for 90th percentile
    his <- his[his$p > thresh,]
    his_area <- sum(his$area)
    
    # Future
    fut <- as.data.frame(fut_rst)     # To dataframe
    fut$area <- as.data.frame(rst_area_fut)[,1]     # Add area to dataframe
    fut <- fut[complete.cases(fut), ]     # Remove NAs
    fut <- fut[fut$p > thresh,] # Use historical threshold
    fut_area <- sum(fut$area)
    
    area_change <- fut_area - his_area
    
    foo_one <- data.frame("species" = this.species,
                          "season" = this.season,
                          "climate" = this.climate,
                          "scenario" = this_scenario,
                          "current_area" = his_area,
                          "future_area" = fut_area,
                          "change" = area_change)
    
    climate_hold <- rbind(climate_hold, foo_one)
    
  }
  
  species_hold <- rbind(species_hold, climate_hold)
  
}

big_hold <- rbind(big_hold, species_hold)

}

# Go to million km2
big_hold$change <- big_hold$change/1000000

outer_hold <- rbind(outer_hold, big_hold)

}



# Plot
pdf("./Data/other/habitat_area_change.pdf", width = 7.5, height = 8.5, useDingbats = F)
ggplot(data = outer_hold, aes(x = change, y = species, color = climate)) +
  geom_jitter(height = 0.3) +
  scale_y_discrete(labels = c("Antarctic fur seal (AFS)",
                              "Sooty albatross (DMS)",
                              "Indian yellow-nosed albatross (IYA)",
                              "King penguin (KIN)",
                              "Light-mantled albatross (LMS)",
                              "Macaroni penguin (MAC)",
                              "Northern giant petrel (NGP)",
                              "Killer whale (ORC)",
                              "Southern elephant seal (SES)",
                              "Subantarctic fur seal (SFS)",
                              "Southern rockhopper penguin (SRP)",
                              "Wandering albatross (WAL)",
                              "White-chinned petrel (WCP)"
  )) +
  geom_vline(xintercept = 0) +
  scale_color_brewer(type = "qual", palette = "Set1", name = "Climate\nmodel") +
  facet_grid(scenario~season) +
  labs(x = "Change in important habitat area (million km2)", y = "Species") +
  theme_bw() +
  theme(axis.text = element_text(colour = "black"),
        plot.background = element_rect(fill = "white", colour = "white"))
dev.off()

# Summary

outer_hold %>% group_by(., season, scenario) %>% 
  summarize(., "average" = mean(change), "st_dev" = sd(change), "minimum" = min(change), "maximum" = max(change))

outer_hold

## Summaries
nrow(filter(outer_hold, scenario == "rcp85"))
