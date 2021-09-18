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
s45[s45$when == "future", ]$when <- "rcp45"
s85 <- readRDS(paste0(datadir, "distances_", i, "_summer_rcp85.RDS"))
s85 <- filter(s85, when == "future")
s85$when <- "rcp85"
s <- rbind(s45, s85)
s_hold <- rbind(s_hold, s)
}
}

w_hold <- data.frame()
for (i in spNamesWinter) {
  w45 <- readRDS(paste0(datadir, "distances_", i, "_winter_rcp45.RDS"))
  w45[w45$when == "future", ]$when <- "rcp45"
  w85 <- readRDS(paste0(datadir, "distances_", i, "_winter_rcp85.RDS"))
  w85 <- filter(w85, when == "future")
  w85$when <- "rcp85"
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

dists_thr <- left_join(x = dists, y = tst, by = c("species", "season", "model"), keep = FALSE) %>%
  mutate(., above_threshold = case_when(score > threshold ~ "Y", score <= threshold ~ "N")) %>%
  filter(., above_threshold == "Y")

#---------------------------------------------
## Plot
pdf("./Data/predictionOutput/distance_all_combined.pdf", width = 3.5/0.6666666, height = 6.5/0.6666666, useDingbats = F)
ggplot(data = dists_thr, aes(x = distance, y = when, fill = when, colour = when)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 5000)) +
  scale_y_discrete(labels = c("Current", "RCP 4.5", "RCP 8.5"), position = "right") +
  scale_fill_manual(values = c("grey", "#009988", "#ee7733"), name = "Scenario", labels = c("Current", "RCP 4.5", "RCP 8.5")) +
  scale_color_manual(values = c("grey", "#009988", "#ee7733"), guide = "none") +
  theme_ridges(center = TRUE) +
  facet_grid(species ~ season, switch = "y") +
  theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.5)) +
  labs(y = "", x = "Distance to habitat (km)") +
  geom_point(data = tracks_group, aes(y = when, x = distance), shape = "|", size = 6, colour = "black")
dev.off()


#---------------------------------------------
# Permutation independence test
for (k in c("rcp45", "rcp85")) {
  
  this_scenario <- k
  print(this_scenario)
  
  these_dists <- dplyr::filter(dists_thr, when == "present" | when == this_scenario)
  
  these_dists$model <- as.factor(these_dists$model)
  these_dists$when <- as.factor(these_dists$when)
  
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
                               data = these_dists[these_dists$species == this_sp & these_dists$season == which_season, ])
      
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

hold$scenario <- this_scenario

write.csv(hold, paste0("./Data/predictionOutput/independence_tests_distance_", this_scenario, ".csv"), row.names = F)

}

# Combine
results_rcp45 <- read.csv("./Data/predictionOutput/independence_tests_distance_rcp45.csv", stringsAsFactors = F)
results_rcp85 <- read.csv("./Data/predictionOutput/independence_tests_distance_rcp85.csv", stringsAsFactors = F)

results_both <- dplyr::left_join(x = results_rcp45, y = results_rcp85,
                                 by = c("species", "season"),
                                 keep = FALSE)

# Write to file
write.csv(results_both, "./Data/predictionOutput/independence_tests_distance.csv", row.names = F)

# Any non-signficant differences?
results_rcp45[results_rcp45$significant == "N", ]
results_rcp85[results_rcp85$significant == "N", ]

#---------------------------------------------