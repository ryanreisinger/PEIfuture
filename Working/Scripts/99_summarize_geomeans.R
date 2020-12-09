# Summarize geomeans

setwd("~/PEIfuture/Working")

library(ggplot2)
library(tidyr)
library(geosphere)
library(pals)
library(rnaturalearth)
library(rnaturalearthdata)

#-------------------------------------------------------------------
# Common variables
source("./Scripts/00 - Project variables.R")


hold_summer <- data.frame()
for (i in spNamesSummer) {
  this.season <- "summer"
  this.species <- i
  if (this.species != "GHA") {
  # Read the geographically weighted range means
  geomeans <- readRDS(paste0("./Data/predictionOutput/perSpecies/dataframes/rangeCenters_",
                             this.species, "_", this.season, ".RDS"))
  hold_summer <- rbind(hold_summer, geomeans)
  }
}

hold_winter <- data.frame()
for (i in spNamesWinter) {
  this.season <- "winter"
  this.species <- i
    # Read the geographically weighted range means
    geomeans <- readRDS(paste0("./Data/predictionOutput/perSpecies/dataframes/rangeCenters_",
                               this.species, "_", this.season, ".RDS"))
    hold_winter <- rbind(hold_winter, geomeans)
}

geomeans <- rbind(hold_summer, hold_winter)

# Piwot to wide
dat <- pivot_wider(geomeans, names_from = when, values_from = c("lon", "lat"))

# Calculate distance and bearing
dat$dist <- distGeo(p1 = dat[,c("lon_present", "lat_present")], p2 = dat[,c("lon_future", "lat_future")])/1000
dat$bearing <- bearing(p1 = dat[,c("lon_present", "lat_present")], p2 = dat[,c("lon_future", "lat_future")])


# Look at distance shifts, on average
group_by(dat, species, season) %>% 
  summarise(., ave_dist = mean(dist), sd_dist = sd(dist)) %>% 
  arrange(., season, ave_dist) %>% 
  print(., n = nrow(.))

# Individually
group_by(dat, species, season) %>% 
  arrange(., season, dist) %>% 
  print(., n = nrow(.))


#------------------------------------------
# Map

# Marion coordinates
islands <- data.frame(x = c(37.743611, 37.943333),
                      y = c(-46.9125, -46.644167))

# Country data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Get EEZs
eez <- readRDS("./Data/eez/eez.RDS")

# Get CCAMLR boundary
ccamlr <- readRDS("./Data/eez/ccamlr.RDS")

# Get MPAs
mpa <- readRDS("./Data/eez/mpa_cropped.RDS")

pdf("./Data/predictionOutput/geomean_shift.pdf", width = 7/0.6666666, height = 6.5/0.6666666, useDingbats = F)
ggplot(data = world) +
  geom_sf(colour = "black", fill = "darkgrey") +
  geom_sf(data = ccamlr, colour = "#ee3377", fill = NA) +
  geom_sf(data = mpa, colour = "#33bbee", fill = NA) +
  geom_sf(data = eez, colour = "black", fill = NA) +
  coord_sf(xlim = c(10, 50), ylim = c(-72, -32), expand = FALSE) +
  geom_point(data = dat,
             aes(x = lon_present, y = lat_present, colour = sqrt(bearing^2)),
             pch = 19) +
  geom_segment(data = dat,
               aes(x = lon_present, y = lat_present, xend = lon_future, yend = lat_future, colour = sqrt(bearing^2)),
               arrow = arrow(length = unit(0.03, "npc"))) +
  geom_point(data = islands, aes(x = x, y = y), colour = "black", size = 2.3) +
  geom_point(data = islands, aes(x = x, y = y), colour = "#ee7733", size = 1.5) +
  scale_colour_gradientn(colours = warmcool(125), limits = c(0, 180),
                         breaks = c(0, 90, 180),
                         labels = c("North", "East/West", "South"),
                         name = "Bearing") +
  facet_grid(.~season) +
  theme_bw()
dev.off()
