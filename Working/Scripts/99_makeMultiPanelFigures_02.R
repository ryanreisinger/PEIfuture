# Make multipanel figures
library(magick)

# setwd("~/PEIfuture/Working")
setwd("D:/PEIfuture/Working/")

#------------------------------------
# Mean and SD of habitat shift per species
#------------------------------------

if (TRUE) {
  
all_files <- list.files("./Data/predictionOutput/perSpecies/plots/", pattern="*_BASE.tif", full.names = T)

which_season <- "winter"
which_scenario <- "rcp45"

if (which_scenario == "rcp45") {
  scenario_label <- "RCP 4.5"
}

if (which_scenario == "rcp85") {
  scenario_label <- "RCP 8.5"
}

these_files <- all_files
these_files <- these_files[grep(which_season, these_files)]
these_files <- these_files[grep(which_scenario, these_files)]

#------------------------------------
# Mean
these_these_files <- these_files[grep("mean", these_files)]

if (which_season == "summer") {
  this_title <- paste0("Summer - ", scenario_label)
}
if (which_season == "winter") {
  this_title <- paste0("Winter - ", scenario_label)
}

tiff(paste0("./Data/compiledPlots/mean_difference_", which_season, "_", which_scenario, ".tiff"), units="in", width=8.3*0.75, height=10*0.75, res=800, compression = 'lzw')
par(mfrow = c(4, 3), mai = c(0,0,0,0), oma = c(0, 0.5, 3, 0))
for (i in these_these_files) {
  this <- image_read(i)
  plot(this)
}
mtext(this_title, side = 3, line = 1, outer = TRUE, adj = 0, cex = 0.8)
dev.off()

#------------------------------------
# SD
these_these_files <- these_files[grep("sd", these_files)]

if (which_season == "summer") {
  this_title <- paste0("Summer - ", scenario_label)
}
if (which_season == "winter") {
  this_title <- paste0("Winter - ", scenario_label)
}

tiff(paste0("./Data/compiledPlots/sd_difference_", which_season, "_", which_scenario, ".tiff"), units="in", width=8.3, height=10, res=800, compression = 'lzw')
par(mfrow = c(4, 3), mai = c(0,0,0,0), oma = c(0, 0.5, 3, 0))
for (i in these_these_files) {
  this <- image_read(i)
  plot(this)
}
mtext(this_title, side = 3, line = 1, outer = TRUE, adj = 0, cex = 0.8)
dev.off()

}

#------------------------------------
# Change in mean habitat importance
#------------------------------------
if (TRUE) {
  
these_files <- list(
"./Data/predictionOutput/habitat_importance_change_summer_rcp45.tiff",
"./Data/predictionOutput/habitat_importance_change_winter_rcp45.tiff",
"./Data/predictionOutput/habitat_importance_change_summer_rcp85.tiff",
"./Data/predictionOutput/habitat_importance_change_winter_rcp85.tiff"
)

tiff(paste0("./Data/compiledPlots/mean_habitat_importance_change.tiff"), units="in", width=8, height=7, res=800, compression = 'lzw')
par(mfrow = c(2, 2), mai = c(0,0,0,0), oma = c(0, 0, 0, 0))

for (i in these_files) {
  this <- image_read(i)
  plot(this)
}

dev.off()

}

#------------------------------------
# Climate analogues
#------------------------------------

these_titles <- list("RCP 4.5 - Summer",
                     "RCP 4.5 - Winter",
                     "RCP 8.5 - Summer",
                     "RCP 8.5 - Winter")

#------------
# Mean
these_files <- list(paste0("./Data/climateOutputPlots/", "meanDistance_", "summer", "_", "rcp45", ".tiff"),
            paste0("./Data/climateOutputPlots/", "meanDistance_", "winter", "_", "rcp45", ".tiff"),
            paste0("./Data/climateOutputPlots/", "meanDistance_", "summer", "_", "rcp85", ".tiff"),
            paste0("./Data/climateOutputPlots/", "meanDistance_", "winter", "_", "rcp85", ".tiff")
)

tiff(paste0("./Data/compiledPlots/climate_analogue_mean.tiff"), units="in", width=8, height=7, res=800, compression = 'lzw')
par(mfrow = c(2, 2), mai = c(0.1,0,0,0), oma = c(0, 0, 1.5, 0))

for (i in 1:length(these_files)) {
  this_file <- these_files[i]
  this <- image_read(this_file[[1]])
  plot(this)
  mtext(these_titles[i], side = 3, line = 0, outer = FALSE, adj = 0.2, cex = 0.6)
}

dev.off()

#------------
# SD
these_files <- list(paste0("./Data/climateOutputPlots/", "sdDistance_", "summer", "_", "rcp45", ".tiff"),
                    paste0("./Data/climateOutputPlots/", "sdDistance_", "winter", "_", "rcp45", ".tiff"),
                    paste0("./Data/climateOutputPlots/", "sdDistance_", "summer", "_", "rcp85", ".tiff"),
                    paste0("./Data/climateOutputPlots/", "sdDistance_", "winter", "_", "rcp85", ".tiff")
)

tiff(paste0("./Data/compiledPlots/climate_analogue_sd.tiff"), units="in", width=8, height=7, res=800, compression = 'lzw')
par(mfrow = c(2, 2), mai = c(0.1,0,0,0), oma = c(0, 0, 1.5, 0))

for (i in 1:length(these_files)) {
  this_file <- these_files[i]
  this <- image_read(this_file[[1]])
  plot(this)
  mtext(these_titles[i], side = 3, line = 0, outer = FALSE, adj = 0.2, cex = 0.6)
}

dev.off()