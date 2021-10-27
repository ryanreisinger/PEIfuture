# Summarize geomeans

setwd("~/PEIfuture/Working")

library(ggplot2)
library(tidyr)
library(geosphere)
library(pals)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(sf)
library(ncdf4)

#-------------------------------------------------------------------
# Common variables
source("./Scripts/00 - Project variables.R")


hold_summer <- data.frame()
for (i in spNamesSummer) {
  this.season <- "summer"
  this.species <- i
  if (this.species != "GHA") {
  # Read the geographically weighted range means
  geomeans_45 <- readRDS(paste0("./Data/predictionOutput/perSpecies/dataframes/rangeCenters_",
                             this.species, "_", this.season, "_rcp45.RDS"))
  geomeans_85 <- readRDS(paste0("./Data/predictionOutput/perSpecies/dataframes/rangeCenters_",
                                this.species, "_", this.season, "_rcp85.RDS"))
  geomeans_45$scenario <- "rcp45"
  geomeans_85$scenario <- "rcp85"
  geomeans <- rbind(geomeans_45, geomeans_85)
  hold_summer <- rbind(hold_summer, geomeans)
  }
}

hold_winter <- data.frame()
for (i in spNamesWinter) {
  this.season <- "winter"
  this.species <- i
    # Read the geographically weighted range means
    geomeans_45 <- readRDS(paste0("./Data/predictionOutput/perSpecies/dataframes/rangeCenters_",
                               this.species, "_", this.season, "_rcp45.RDS"))
    geomeans_85 <- readRDS(paste0("./Data/predictionOutput/perSpecies/dataframes/rangeCenters_",
                                  this.species, "_", this.season, "_rcp85.RDS"))
    geomeans_45$scenario <- "rcp45"
    geomeans_85$scenario <- "rcp85"
    geomeans <- rbind(geomeans_45, geomeans_85)
    hold_winter <- rbind(hold_winter, geomeans)
}

geomeans <- rbind(hold_summer, hold_winter)

# Piwot to wide
dat <- pivot_wider(geomeans, names_from = when, values_from = c("lon", "lat"))

# Calculate distance and bearing
dat$dist <- distGeo(p1 = dat[,c("lon_present", "lat_present")], p2 = dat[,c("lon_future", "lat_future")])/1000
dat$bearing <- bearing(p1 = dat[,c("lon_present", "lat_present")], p2 = dat[,c("lon_future", "lat_future")])


# Look at distance shifts, on average
group_by(dat, species, season, scenario) %>% 
  summarise(., ave_dist = mean(dist), sd_dist = sd(dist)) %>% 
  arrange(., scenario, season, ave_dist) %>% 
  print(., n = nrow(.))

# Individually
group_by(dat, species, season, scenario) %>% 
  arrange(., season, dist) %>% 
  print(., n = nrow(.))

# Which shifts are northwards
filter(dat, bearing > -90 & bearing < 90 & scenario == "rcp85") %>% 
  arrange(., bearing)

unique(filter(dat, bearing > -90 & bearing < 90 & scenario == "rcp45")$species)

filter(dat, bearing > -90 & bearing < 90 & scenario == "rcp85")
unique(filter(dat, bearing > -90 & bearing < 90 & scenario == "rcp45")$species)

# Relative shifts
ggplot(dat, aes(y = dist, x = bearing, colour = species)) + geom_point() +
  coord_polar(start = 3.14159) +
  scale_x_continuous(breaks = seq(-180, +180, 30), expand=c(0,0), lim=c(-180, +180)) +
  scale_y_continuous(expand=c(0,0), lim=c(0, 800)) +
  labs(x = "Relative bearing\nof shift (degrees)", y = "Distance of\nshift (km)") +
  facet_grid(scenario ~ season) + theme_bw()

#------------------------------------------
# Map

# Marion coordinates
islands <- data.frame(x = c(37.743611, 37.943333),
                      y = c(-46.9125, -46.644167))

# Country data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Get EEZs
eez <- readRDS("./Data/eez/eez.RDS")
st_crs(eez) <- 4326
eez <- st_transform(eez, 4326)

# Get CCAMLR boundary
ccamlr <- readRDS("./Data/eez/ccamlr.RDS")
st_crs(ccamlr) <- 4326
ccamlr <- st_transform(ccamlr, 4326)

# Get MPAs
mpa_designated <- readRDS("./Data/eez/mpa_designated.RDS")
st_crs(mpa_designated) <- 4326
mpa_designated <- st_transform(mpa_designated, 4326)

mpa_proposed <- readRDS("./Data/eez/mpa_proposed.RDS")
st_crs(mpa_proposed ) <- 4326
mpa_proposed  <- st_transform(mpa_proposed, 4326)

# Get fronts
# Get Southern Ocean fronts from Park & Durand 2019
# https://doi.org/10.17882/59800
frnts <- nc_open("./Data/privateData/62985.nc")
NB <- data.frame(
  "lat" = ncvar_get(frnts, "LatNB"),
  "lon" = ncvar_get(frnts, "LonNB"),
  "name" = "NB"
)
SAF <- data.frame(
  "lat" = ncvar_get(frnts, "LatSAF"),
  "lon" = ncvar_get(frnts, "LonSAF"),
  "name" = "SAF"
)
PF <- data.frame(
  "lat" = ncvar_get(frnts, "LatPF"),
  "lon" = ncvar_get(frnts, "LonPF"),
  "name" = "PF"
)
SACCF <- data.frame(
  "lat" = ncvar_get(frnts, "LatSACCF"),
  "lon" = ncvar_get(frnts, "LonSACCF"),
  "name" = "SACCF"
)
SB <- data.frame(
  "lat" = ncvar_get(frnts, "LatSB"),
  "lon" = ncvar_get(frnts, "LonSB"),
  "name" = "SB"
)
nc_close(frnts)

frnts <- rbind(NB, SAF, PF, SACCF, SB)

# Crop
frnts <- dplyr::filter(frnts, lat > -72 & lat < -32)
frnts <- dplyr::filter(frnts, lon > 10 & lon < 50)

# Better facet labels
# New facet label names for dose variable
scenario.labs <- c("RCP 4.5", "RCP 8.5")
names(scenario.labs) <- c("rcp45", "rcp85")

# New facet label names for supp variable
season.labs <- c("Summer", "Winter")
names(season.labs) <- c("summer", "winter")


# Plot
p1 <- ggplot(data = world) +
  geom_sf(colour = "black", fill = "darkgrey") +
  geom_sf(data = eez, colour = "black", fill = NA) +
  geom_sf(data = ccamlr, colour = "#ee3377", fill = NA) +
  geom_sf(data = mpa_designated, colour = "#33bbee", fill = NA) +
  geom_sf(data = mpa_proposed, colour = "#33bbee", fill = NA) +
  coord_sf(xlim = c(10, 50), ylim = c(-72, -32), expand = FALSE) +
  geom_path(data = frnts,
            aes(x = lon, y = lat, group = name),
            linetype = 1,
            colour = "grey60") +
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
  facet_grid(scenario~season,
             labeller = labeller(scenario = scenario.labs, season = season.labs)) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = "black"))

pdf("./Data/predictionOutput/geomean_shift.pdf", width = 6.61417/0.6666666, height = 8/0.6666666, useDingbats = F)
plot(p1)
dev.off()

tiff("./Data/predictionOutput/geomean_shift.tiff", width = 6.61417/0.888888, height = 8.38583/0.888888,
     units = "in",
     res = 800,
     compression = "lzw")
plot(p1)
dev.off()
