# Format tracking data for further use in PEI future
# Read in tracking data and clean up for further analyses

setwd("D:/PEIfuture/Working/")

source("./Scripts/00 - Project variables.R")

trackDir <- "D:/PEIfuture/Working/Data/trackDataREADONLY/"

# Read in all the data
files <- list.files(trackDir, full.names = T)

data <- do.call(rbind, lapply(files, read.csv, stringsAsFactors = F))

# Reorganise
data <- data[ , c("track_id", "sim_trk_id", "s", "date", "lat", "lon")]

# Add species names
data$species <- substr(data$track_id, start = 1, stop = 3)

# Split into seasons
seasons <- readRDS("./Data/other/ALL_seasons.rds")
seasons <- seasons[ , c("track_id", "which.season")]
data <- merge(x = data, y = seasons, by = "track_id", all = F)

# Write to individual species files
speciesList <- unique(data$species)

for (i in speciesList) {
  print(i)
  dat <- data[data$species == i, ]
  nm <- paste0("./Data/trackDataFormatted/", "tracksSim&Real_", i, ".csv")
  write.csv(dat, file = nm, row.names = F)
}
