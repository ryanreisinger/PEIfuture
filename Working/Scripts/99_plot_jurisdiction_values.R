## Plot distributions of habitat values in different jurisdictions for present and future predictions

setwd("~/PEIfuture/Working")

library(raster)
library(sf)
library(ggplot2)
library(ggridges)
library(patchwork)

library(coin)

#-----------------------------------------
# Common variables
source("./Scripts/00 - Project variables.R")

#-----------------------------------------
# Get distances
datadir <- "./Data/predictionOutput/perSpecies/dataframes/"

s_hold <- data.frame()
for (i in spNamesSummer) {
  if (i != "GHA") {
    s <- readRDS(paste0(datadir, "distances_", i, "_summer.RDS"))
    s_hold <- rbind(s_hold, s)
  }
}

w_hold <- data.frame()
for (i in spNamesWinter) {
  w <- readRDS(paste0(datadir, "distances_", i, "_winter.RDS"))
  w_hold <- rbind(w_hold, w)
}

dists <- rbind(s_hold, w_hold)

#-----------------------------------------
# Raster for cropping
rst.crop <- raster(res = 0.1, xmn = -10, xmx = 70, ymn = -75, ymx = -30, crs = "+proj=longlat +datum=WGS84")

# Get EEZs
eez <- readRDS("./Data/eez/eez.RDS")
eez_crop <- st_crop(eez, rst.crop)

# Get CCAMLR boundary
ccamlr <- readRDS("./Data/eez/ccamlr.RDS")
ccamlr_crop <- st_crop(ccamlr, rst.crop)

# Go from sfc to sf
eez_sf = eez_crop %>%
  st_sf %>%
  st_cast

ccamlr_sf = ccamlr_crop %>%
  st_sf %>%
  st_cast

# Create an sf object
pnts <- st_as_sf(x = dists, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84")

dists$is_ccamlr <- "N"
dists$is_eez <- "N"

# Look up
dists[which(st_intersects(pnts, ccamlr_sf, sparse = FALSE)), ]$is_ccamlr <- "Y"
dists[which(st_intersects(pnts, eez_sf, sparse = FALSE)), ]$is_eez <- "Y"

# All points south of -60 should be ccamlr
dists[dists$lat < -60, "is_ccamlr"] <- "Y"

# Check
ggplot(data = dists[dists$species == "AFS", ], aes(x = lon, y = lat, colour = is_eez)) + geom_point()

# Ridgeplots
# EEZ
ridge_eez <-
  ggplot(data = dists[dists$is_eez == "Y", ], aes(x = score, y = when, fill = when, colour = when)) +
  geom_density_ridges(alpha = 0.5, quantile_lines = TRUE, quantiles = 2) +
  scale_fill_manual(values = c("#ee7733", "grey20"), guide = "none") +
  scale_color_manual(values = c("#ee7733", "grey20"), guide = "none") +
  theme_ridges(center = TRUE) +
  scale_y_discrete(position = "right") +
  scale_x_continuous(limits = c(0, 1)) +
  facet_grid(species ~ season, switch = "y") +
  labs(title = "a) In EEZ", y = "", x = "Habitat importance in EEZ")

# CCAMLR
ridge_ccamlr <-
  ggplot(data = dists[dists$is_ccamlr == "Y", ], aes(x = score, y = when, fill = when, colour = when)) +
  geom_density_ridges(alpha = 0.5, quantile_lines = TRUE, quantiles = 2) +
  scale_fill_manual(values = c("#ee7733", "grey20"), guide = "none") +
  scale_color_manual(values = c("#ee7733", "grey20"), guide = "none") +
  theme_ridges(center = TRUE) +
  scale_y_discrete(position = "right") +
  scale_x_continuous(limits = c(0, 1)) +
  facet_grid(species ~ season, switch = "y") +
  labs(title = "b) In CCAMLR area", y = "", x = "Habitat importance in CCAMLR area")

pdf("./Data/predictionOutput/importance_in_eez&ccamlr_ridge.pdf", width = 7/0.6666666, height = 6.5/0.6666666, useDingbats = F)
ridge_eez + ridge_ccamlr
dev.off()

# Boxplot
# EEZ
eez_box <-
  ggplot(data = dists[dists$is_eez == "Y", ], aes(y = score, x = when, fill = when, colour = when)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  scale_fill_manual(values = c("#ee7733", "grey20"), guide = "none") +
  scale_color_manual(values = c("#ee7733", "grey20"), guide = "none") +
  scale_x_discrete(position = "top") +
  scale_y_continuous(limits = c(0, 1)) +
  facet_grid(species ~ season, switch = "y") +
  labs(title = "a) In EEZ", x = "", y = "Habitat importance in EEZ") +
  coord_flip() +
  theme_bw()


# CCAMLR
ccamlr_box <-
  ggplot(data = dists[dists$is_ccamlr == "Y", ], aes(y = score, x = when, fill = when, colour = when)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  scale_fill_manual(values = c("#ee7733", "grey20"), guide = "none") +
  scale_color_manual(values = c("#ee7733", "grey20"), guide = "none") +
  scale_x_discrete(position = "top") +
  scale_y_continuous(limits = c(0, 1)) +
  facet_grid(species ~ season, switch = "y") +
  labs(title = "b) In CCAMLR area", x = "", y = "Habitat importance in CCAMLR area") +
  coord_flip() +
  theme_bw()

pdf("./Data/predictionOutput/importance_in_eez&ccamlr_box.pdf", width = 7/0.6666666, height = 6.5/0.6666666, useDingbats = F)
eez_box + ccamlr_box
dev.off()

# Permutation independence test
dists$model <- as.factor(dists$model)
dists$when <- as.factor(dists$when)

# CCAMLR
hold <- data.frame()
seasons <- c("summer", "winter")

for (j in seasons) {
  which_season <- j
  frame <- data.frame("species" = spNamesSummer,
                      "season" = which_season,
                      "which" = "ccamlr",
                      "z" = NA,
                      "p" = NA)
  
  if (j == "summer") {
    these_sp_names <- spNamesSummer
  } else {
    these_sp_names <- spNamesWinter
  }
  
  for (i in 1:length(these_sp_names)) {
    this_sp <- these_sp_names[i]
    if (this_sp != "GHA") {
      print(this_sp)
      foo <- independence_test(score ~ when|model,
                               data = dists[dists$is_ccamlr == "Y" & dists$species == this_sp & dists$season == which_season, ])
      
      z <- statistic(foo)
      p <- pvalue(foo)
      
      frame[i, "z"] <- z
      frame[i, "p"] <- p
    }
  }
  hold <- rbind(hold, frame)
}

hold_ccamlr <- hold

# EEZ
hold <- data.frame()
seasons <- c("summer", "winter")

for (j in seasons) {
  which_season <- j
  frame <- data.frame("species" = spNamesSummer,
                      "season" = which_season,
                      "which" = "eez",
                      "z" = NA,
                      "p" = NA)
  
  if (j == "summer") {
    these_sp_names <- spNamesSummer
  } else {
    these_sp_names <- spNamesWinter
  }
  
  for (i in 1:length(these_sp_names)) {
    this_sp <- these_sp_names[i]
    if (this_sp != "GHA") {
      print(this_sp)
      foo <- independence_test(score ~ when|model,
                               data = dists[dists$is_eez == "Y" & dists$species == this_sp & dists$season == which_season, ])
      
      z <- statistic(foo)
      p <- pvalue(foo)
      
      frame[i, "z"] <- z
      frame[i, "p"] <- p
    }
  }
  hold <- rbind(hold, frame)
}

hold_eez <- hold

write.csv(rbind(hold_ccamlr, hold_eez), "./Data/predictionOutput/independence_tests.csv", row.names = F)
