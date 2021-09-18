# Make variable importance plots

setwd("~/PEIfuture/Working")

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggradar)
library(pals)
library(patchwork)

#----------------------------------------
# Common variables
source("./Scripts/00 - Project variables.R")

# List files
these_files <- list.files("./Data/modelOutputVarImp/", pattern = "*.csv", full.names = T)

# Random forest only
these_files <- these_files[grepl("_rf", these_files)]

# Read in
dat <- do.call(rbind, lapply(these_files, read.csv, stringsAsFactors = F))

# Reformat for radar plot
plot_data <- dplyr::select(dat, Overall, variable, sp, season, climate)
plot_data <- pivot_wider(data = plot_data,
                         names_from = "variable",
                         values_from = "Overall")

# Fill in NAs
plot_data[is.na(plot_data)] <- 0

# Hold a copy
plot_data_all <- plot_data

# Colours
these_cols <- brewer.set1(8)


for (j in c("summer", "winter")) {
  this_season <- j
  print(this_season)
  
  if (this_season == "summer") {
    spNames <- spNamesSummer
  } else {
    spNames <- spNamesWinter
  }
  
  # Select the season
  plot_data <- filter(plot_data_all, season == this_season)
  
  # Create a list of plots
  plts <- list()
  
  for (i in spNames) {
    this_species <- i
    print(this_species)
    
    this_plot_data <- dplyr::filter(plot_data, sp == this_species)
    
    # Plot
    plts[[i]] <-
      ggradar(select(this_plot_data, climate:DEP),
              values.radar = c("0", "50", "100"),
              grid.min = 0,
              grid.mid = 50,
              grid.max = 100,
              gridline.mid.colour = "grey",
              group.colours = these_cols,
              base.size = 5,
              axis.label.size = 5,
              group.line.width = 1,
              group.point.size = 2,
              background.circle.colour = "white",
              gridline.min.linetype = 1,
              gridline.mid.linetype = 1,
              gridline.max.linetype = 1,
              legend.title = "Climate\nmodel",
              plot.title = this_species
      )
    
  }
  
  tiff(paste0("./Data/compiledPlots/variableImportance_", this_season, ".tiff"),
       units="in", width=20, height=22, res=72, compression = 'lzw')
  plot(wrap_plots(plts, ncol = 3,
             guides = "collect") +
         plot_annotation(title = this_season,
                         theme = theme(plot.title = element_text(size = 22))))
  dev.off()
  
}


# Calculate some overall summaries
op <- dat %>%
  group_by(sp, season) %>%
  mutate(rank = rank(Overall, ties.method = "first")) %>%
  arrange(rank) %>%
  ungroup() %>%
  group_by(variable) %>% 
  summarise(mean_rank = mean(rank)) %>% 
  arrange(mean_rank)
