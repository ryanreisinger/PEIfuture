## Plot density of distance to habitat for present and future predictions

setwd("~/PEIfuture/Working")

library(ggplot2)
library(ggridges)
library(plyr)
library(readr)
library(geosphere)
library(coin)

#---------------------------------------------
# Common variables
source("./Scripts/00 - Project variables.R")

#---------------------------------------------
# Get distances
datadir <- "./Data/predictionOutput/perSpecies/dataframes/"

s_hold <- data.frame()
for (i in spNamesSummer) {
  if (i != "GHA") {
s45 <- readRDS(paste0(datadir, "distances_", i, "_summer_rcp45.RDS"))
s45 <- filter(s45, when == "future") # Keep only one set of present predictions
s85 <- readRDS(paste0(datadir, "distances_", i, "_summer_rcp85.RDS"))

s45[s45$when == "future", "when"] <- "rcp45"
s85[s85$when == "future", "when"] <- "rcp85"

s <- rbind(s45, s85)

s_hold <- rbind(s_hold, s)
}
}

w_hold <- data.frame()
for (i in spNamesWinter) {
    w45 <- readRDS(paste0(datadir, "distances_", i, "_winter_rcp45.RDS"))
    w45 <- filter(w45, when == "future") # Keep only one set of present predictions
    w85 <- readRDS(paste0(datadir, "distances_", i, "_winter_rcp85.RDS"))
    
    w45[w45$when == "future", "when"] <- "rcp45"
    w85[w85$when == "future", "when"] <- "rcp85"
    
    w <- rbind(w45, w85)
    w_hold <- rbind(w_hold, w)
}

dists <- rbind(s_hold, w_hold)

# Get distances for real tracks
track_files <- list.files(path="./Data/trackDataFormatted/", pattern="*.csv", full.names=TRUE)
tracks <- ldply(track_files, read_csv)

# Approximate position of Marion
mar_lat <- -46.9096324
mar_lon <- 37.747117
tracks$distance <- distGeo(p1 = tracks[ ,c("lon", "lat")], p2 = c(mar_lon, mar_lat))/1000

# Split into seasons
seasons <- readRDS("./Data/other/ALL_seasons.rds")
seasons <- seasons[ , c("track_id", "which.season")]
tracks <- merge(x = tracks, y = seasons, by = "track_id", all = F)

tracks <- filter(tracks, species != "GHA")

# check distances
ggplot(data = tracks, aes(y = species, x = distance, color = which.season)) +
  geom_boxplot() +
  facet_grid(which.season ~ species)

# Hacks for a combined plot
tracks$season <- tracks$which.season
tracks$when <- "present"

## Summary stats
library(dplyr)
tracks_group <- group_by(tracks, species, which.season)
tracks_group <- summarize(tracks_group, distance = max(distance))
tracks_group$when = "present"
tracks_group$season = tracks_group$which.season

## Calculate habitat preference thresholds corresponding to some percentile, for each species, season, model
## the same threshold is applied to future, to take into account changing values (e.g., lower habitat preference)
tst <- filter(dists, when == "present") %>%
  group_by(., species, season, model) %>%
  summarise(., threshold = quantile(score, probs = c(0.9)))

dists_thr <- left_join(x = dists, y = tst, by = c("species", "season", "model"), keep = TRUE) %>%
  mutate(., above_threshold = case_when(score > threshold ~ "Y", score <= threshold ~ "N")) %>%
  filter(., above_threshold == "Y")

#---------------------------------------------
## Plot
# Separate by model
pdf("./Data/predictionOutput/distance_all.pdf", height = 3.5/0.6666666, width = 6.5/0.6666666, useDingbats = F)
ggplot(data = dists_thr, aes(x = distance, y = model, fill = when, colour = when)) +
  geom_density_ridges(alpha = 0.5) +
  scale_x_continuous(limits = c(0, 5000), breaks = c(0, 2500, 5000), expand = c(0, 0)) +
  # scale_fill_manual(values = c("#ee7733", "grey20"), name = "Time period", labels = c("Future", "Current")) +
  # scale_color_manual(values = c("#ee7733", "grey20"), guide = "none") +
  theme_ridges(center = TRUE) +
  facet_grid(season ~ species) +
  labs(y = "Model", x = "Distance to habitat (km)") +
  theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.5))
dev.off()

# Not separated by model
pdf("./Data/predictionOutput/distance_all_combined.pdf", width = 3.5/0.6666666, height = 6.5/0.6666666, useDingbats = F)
ggplot(data = dists_thr, aes(x = distance, y = when, fill = when, colour = when)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 5000)) +
  scale_y_discrete(position = "right") +
  # scale_fill_manual(values = c("#ee7733", "grey20"), name = "Time period", labels = c("Future", "Current")) +
  # scale_color_manual(values = c("#ee7733", "grey20"), guide = "none") +
  theme_ridges(center = TRUE) +
  facet_grid(species ~ season, switch = "y") +
  theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.5)) +
  labs(y = "", x = "Distance to habitat (km)") +
  geom_point(data = tracks_group, aes(y = when, x = distance), shape = "|", size = 6, colour = "#0077bb")
dev.off()


#---------------------------------------------
# Permutation independence test
dists_thr$model <- as.factor(dists_thr$model)
dists_thr$when <- as.factor(dists_thr$when)

# CCAMLR
hold <- data.frame()
seasons <- c("summer", "winter")

for (j in seasons) {
  which_season <- j
  frame <- data.frame("species" = spNamesSummer,
                      "season" = which_season,
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
                               data = dists_thr[dists_thr$species == this_sp & dists_thr$season == which_season, ])
      
      z <- statistic(foo)
      p <- pvalue(foo)
      
      frame[i, "z"] <- z
      frame[i, "p"] <- p
    }
  }
  hold <- rbind(hold, frame)
}

hold <- mutate(hold, significant = case_when(p < 0.05 ~ "Y",
                                             p >= 0.05 ~ "N"))

hold$p <- round(hold$p, 3)

write.csv(hold, "./Data/predictionOutput/independence_tests_distance.csv", row.names = F)

#---------------------------------------------
