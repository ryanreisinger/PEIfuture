## Plot distributions of habitat values in different jurisdictions for present and future predictions

setwd("~/PEIfuture/Working")

library(raster)
library(sf)
library(ggplot2)
library(ggridges)
library(ggforce)
library(patchwork)

library(dplyr)
library(tidyr)

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
    s45 <- readRDS(paste0(datadir, "distances_", i, "_summer_rcp45.RDS"))
    s85 <- readRDS(paste0(datadir, "distances_", i, "_summer_rcp85.RDS"))
    s45$scenario <- "rcp45"
    s85$scenario <- "rcp85"
    s <- rbind(s45, s85)
    s_hold <- rbind(s_hold, s)
  }
}

w_hold <- data.frame()
for (i in spNamesWinter) {
  w45 <- readRDS(paste0(datadir, "distances_", i, "_winter_rcp45.RDS"))
  w85 <- readRDS(paste0(datadir, "distances_", i, "_winter_rcp85.RDS"))
  w45$scenario <- "rcp45"
  w85$scenario <- "rcp85"
  w <- rbind(w45, w85)
  w_hold <- rbind(w_hold, w)
}

dists <- rbind(s_hold, w_hold)

# Extract area
rst.crop <- raster(res = 0.1, xmn = -10, xmx = 70, ymn = -75, ymx = -30, crs = "+proj=longlat +datum=WGS84")
rst.area <- raster::area(rst.crop)
dists$area <- raster::extract(rst.area, dists[,c("lon", "lat")])

## Calculate habitat preference thresholds corresponding to some percentile, for each species, season, model
## the same threshold is applied to future, to take into account changing values (e.g., lower habitat preference)
tst <- filter(dists, when == "present") %>%
  group_by(., species, season, model, scenario) %>%
  summarise(., threshold = quantile(score, probs = c(0.9)))

dists <- left_join(x = dists, y = tst, by = c("species", "season", "model", "scenario"), keep = FALSE) %>%
  mutate(., above_threshold = case_when(score > threshold ~ "Y", score <= threshold ~ "N"))

#-----------------------------------------
# Raster for cropping
rst.crop <- raster(res = 0.1, xmn = -10, xmx = 70, ymn = -75, ymx = -30, crs = "+proj=longlat +datum=WGS84")

# Get EEZs
eez <- readRDS("./Data/eez/eez_sp.RDS")

# Get CCAMLR boundary
ccamlr <- readRDS("./Data/eez/ccamlr_sp.RDS")

# Get MPAs
mpa <- raster("./Data/eez/mpa_raster.grd")

# # Go from sfc to sf
# eez_sf = eez %>%
#   st_sf %>%
#   st_cast
# 
# ccamlr_sf = ccamlr %>%
#   st_sf %>%
#   st_cast

# Got to sf
eez_sf <- st_as_sf(eez)
ccamlr_sf <- st_as_sf(ccamlr)

# Create an sf object
pnts <- st_as_sf(x = dists, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84")

dists$is_ccamlr <- "N"
dists$is_eez <- "N"

# Look up
dists[which(st_intersects(pnts, ccamlr_sf, sparse = FALSE)), ]$is_ccamlr <- "Y"
dists[which(st_intersects(pnts, eez_sf, sparse = FALSE)), ]$is_eez <- "Y"

# All points south of -60 should be ccamlr
dists[dists$lat < -60, "is_ccamlr"] <- "Y"

# Extract MPAs
dists$is_mpa <- raster::extract(mpa, dists[,c("lon", "lat")])
dists <- mutate(dists, is_mpa = case_when(is_mpa == 1 ~ "Y",
                                          is.na(is_mpa) ~ "N"))

# Filter, but save a copy for later
dists_copy <- dists
dists <- filter(dists, above_threshold == "Y")

# Check
# ggplot(data = dists[dists$species == "SES", ], aes(x = lon, y = lat, colour = is_ccamlr)) + geom_point()

dists <-
  dists %>%
  mutate(when2 = case_when(when == "present" ~ "current",
                           when == "future" & scenario == "rcp45" ~ "rcp45",
                           when == "future" & scenario == "rcp85" ~ "rcp85"
  ))

# Colours
cols <- c("grey20", "#009988", "#ee7733")

# Ridgeplots
# EEZ
ridge_eez <-
  ggplot(data = dists[dists$is_eez == "Y", ], aes(x = score, y = when2, fill = when2, colour = when2)) +
  geom_density_ridges(alpha = 0.5, quantile_lines = TRUE, quantiles = 2) +
  scale_fill_manual(values = cols, guide = "none") +
  scale_color_manual(values = cols, guide = "none") +
  theme_ridges(center = TRUE) +
  scale_y_discrete(position = "right") +
  scale_x_continuous(limits = c(0, 1)) +
  facet_grid(species ~ season, switch = "y") +
  labs(title = "a) In EEZ", y = "", x = "Habitat importance in EEZ")

# CCAMLR
ridge_ccamlr <-
  ggplot(data = dists[dists$is_ccamlr == "Y", ], aes(x = score, y = when2, fill = when2, colour = when2)) +
  geom_density_ridges(alpha = 0.5, quantile_lines = TRUE, quantiles = 2) +
  scale_fill_manual(values = cols, guide = "none") +
  scale_color_manual(values = cols, guide = "none") +
  theme_ridges(center = TRUE) +
  scale_y_discrete(position = "right") +
  scale_x_continuous(limits = c(0, 1)) +
  facet_grid(species ~ season, switch = "y") +
  labs(title = "b) In CCAMLR area", y = "", x = "Habitat importance in CCAMLR area")

ridge_mpa <-
  ggplot(data = dists[dists$is_mpa == "Y", ], aes(x = score, y = when2, fill = when2, colour = when2)) +
  geom_density_ridges(alpha = 0.5, quantile_lines = TRUE, quantiles = 2) +
  scale_fill_manual(values = cols, guide = "none") +
  scale_color_manual(values = cols, guide = "none") +
  theme_ridges(center = TRUE) +
  scale_y_discrete(position = "right") +
  scale_x_continuous(limits = c(0, 1)) +
  facet_grid(species ~ season, switch = "y") +
  labs(title = "c) In MPAs", y = "", x = "Habitat importance in MPAs")

pdf("./Data/predictionOutput/high_importance_in_eez&ccamlr_ridge.pdf", width = 7/0.6666666, height = 8/0.6666666, useDingbats = F)
ridge_eez + ridge_ccamlr
dev.off()

# Boxplot
# EEZ
eez_box <-
  ggplot(data = dists[dists$is_eez == "Y", ], aes(y = score, x = when2, fill = when2, colour = when2)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  scale_fill_manual(values = cols, guide = "none") +
  scale_color_manual(values = cols, guide = "none") +
  scale_x_discrete(position = "top") +
  scale_y_continuous(limits = c(0, 1)) +
  facet_grid(species ~ season, switch = "y") +
  labs(title = "a) In EEZ", x = "", y = "Habitat importance in EEZ") +
  coord_flip() +
  theme_bw()


# CCAMLR
ccamlr_box <-
  ggplot(data = dists[dists$is_ccamlr == "Y", ], aes(y = score, x = when2, fill = when2, colour = when2)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  scale_fill_manual(values = cols, guide = "none") +
  scale_color_manual(values = cols, guide = "none") +
  scale_x_discrete(position = "top") +
  scale_y_continuous(limits = c(0, 1)) +
  facet_grid(species ~ season, switch = "y") +
  labs(title = "b) In CCAMLR area", x = "", y = "Habitat importance in CCAMLR area") +
  coord_flip() +
  theme_bw()

# CCAMLR
mpa_box <-
  ggplot(data = dists[dists$is_mpa == "Y", ], aes(y = score, x = when2, fill = when2, colour = when2)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  scale_fill_manual(values = cols, guide = "none") +
  scale_color_manual(values = cols, guide = "none") +
  scale_x_discrete(position = "top") +
  scale_y_continuous(limits = c(0, 1)) +
  facet_grid(species ~ season, switch = "y") +
  labs(title = "c) In MPAs", x = "", y = "Habitat importance in MPAs") +
  coord_flip() +
  theme_bw()

pdf("./Data/predictionOutput/high_importance_in_eez&ccamlr_box.pdf", width = 7/0.6666666, height = 8/0.6666666, useDingbats = F)
eez_box + ccamlr_box
dev.off()

#-----------------------------------------
## Change in number of cells
# tst_ccamlr <- group_by(dists, species, season, model, when, is_ccamlr) %>%
#   tally(.) %>%
#   pivot_wider(., names_from = when, values_from = n) %>%
#   mutate(., change = future - present) %>%
#   filter(., is_ccamlr == "Y")

tst_ccamlr <- group_by(dists, species, season, model, when, scenario, is_ccamlr) %>%
  summarise(., n = sum(area)) %>%
  pivot_wider(., names_from = when, values_from = n) %>%
  mutate(., change = future - present) %>%
  filter(., is_ccamlr == "Y")

tst_eez <- group_by(dists, species, season, model, when, scenario, is_eez) %>%
  summarise(., n = sum(area)) %>%
  pivot_wider(., names_from = when, values_from = n) %>%
  mutate(., change = future - present) %>%
  filter(., is_eez == "Y")

tst_mpa <- group_by(dists, species, season, model, when, scenario, is_mpa) %>%
  summarise(., n = sum(area)) %>%
  pivot_wider(., names_from = when, values_from = n) %>%
  mutate(., change = future - present) %>%
  filter(., is_mpa == "Y")

tst_ccamlr$where <- "CCAMLR"
tst_eez$where <- "EEZ"
tst_mpa$where <- "MPA"
tst <- bind_rows(tst_ccamlr, tst_eez, tst_mpa)

pdf("./Data/predictionOutput/high_importance_coverage_change.pdf", width = 6/0.6666666, height = 7/0.6666666, useDingbats = F)
p1 <- ggplot(data = dplyr::filter(tst, scenario == "rcp45"), aes(y = change/1000000, x = where, colour = where, fill = where)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  geom_sina(size = 0.6, alpha = 0.6) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(position = "top") +
  scale_fill_manual(values = c("#ee3377", "black", "#0077bb"), guide = "none") +
  scale_colour_manual(values = c("#ee3377", "black", "#0077bb"), guide = "none") +
  facet_grid(species ~ season, switch = "y") +
  coord_flip() +
  labs(y = "Change in important habitat area (million km2)",
       x = "",
       subtitle = "RCP 4.5") +
  theme_bw()
p2 <- ggplot(data = dplyr::filter(tst, scenario == "rcp85"), aes(y = change/1000000, x = where, colour = where, fill = where)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  geom_sina(size = 0.6, alpha = 0.6) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(position = "top") +
  scale_fill_manual(values = c("#ee3377", "black", "#0077bb"), guide = "none") +
  scale_colour_manual(values = c("#ee3377", "black", "#0077bb"), guide = "none") +
  facet_grid(species ~ season, switch = "y") +
  coord_flip() +
  labs(y = "Change in important habitat area (million km2)",
       x = "",
       subtitle = "RCP 8.5") +
  theme_bw()

plot(p1 + p2)
dev.off()


#-----------------------------------------
# Permutation independence test
dists_copy$model <- as.factor(dists_copy$model)
dists_copy$when <- as.factor(dists_copy$when)
dists_copy$scenario <- as.factor(dists_copy$scenario)

master_hold <- data.frame()
scenarios <- c("rcp45", "rcp85")

for (k in scenarios) {
  
  this_scenario <- k
  
  this_data <- dplyr::filter(dists_copy, scenario == this_scenario) # TODO, sub this_data into below... and add scenario col to output
  
# CCAMLR
hold <- data.frame()
seasons <- c("summer", "winter")

for (j in seasons) {
  which_season <- j
  frame <- data.frame("species" = spNamesSummer,
                      "season" = which_season,
                      "scenario" = this_scenario,
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
                               data = this_data[this_data$is_ccamlr == "Y" & this_data$species == this_sp & this_data$season == which_season, ])
      
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
                      "scenario" = this_scenario,
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
                               data = this_data[this_data$is_eez == "Y" & this_data$species == this_sp & this_data$season == which_season, ])
      
      z <- statistic(foo)
      p <- pvalue(foo)
      
      frame[i, "z"] <- z
      frame[i, "p"] <- p
    }
  }
  hold <- rbind(hold, frame)
}

hold_eez <- hold

# MPA
hold <- data.frame()
seasons <- c("summer", "winter")

for (j in seasons) {
  which_season <- j
  frame <- data.frame("species" = spNamesSummer,
                      "season" = which_season,
                      "scenario" = this_scenario,
                      "which" = "mpa",
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
                               data = this_data[this_data$is_mpa == "Y" & this_data$species == this_sp & this_data$season == which_season, ])
      
      z <- statistic(foo)
      p <- pvalue(foo)
      
      frame[i, "z"] <- z
      frame[i, "p"] <- p
    }
  }
  hold <- rbind(hold, frame)
}

hold_mpa <- hold

hold <- rbind(hold_ccamlr, hold_eez, hold_mpa)
hold$z <- round(hold$z, 3)
hold$p <- round(hold$p, 3)
hold <- mutate(hold, significant = case_when(p < 0.05 ~ "Y",
                                             p >= 0.05 ~ "N"))

master_hold <- rbind(master_hold, hold)

}

master_hold <- dplyr::filter(master_hold, !is.na(z))

write.csv(master_hold, "./Data/predictionOutput/high_importance_jurisdiction_independence_tests.csv", row.names = F)


## Summaries
nrow(filter(tst_eez, change > 0 & season == "winter" & scenario == "rcp85"))
