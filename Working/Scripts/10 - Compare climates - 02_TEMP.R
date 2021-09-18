## Compare climates

setwd("~/PEIfuture/Working")

library(raster)
library(janitor)
library(gower)
library(ggplot2)
library(pals)
library(mapdata)
library(RColorBrewer)
library(sf)
library(analogue)

this.season <- "winter"
this.scenario <- "rcp85"

#------------------------------------------
# Common variables
source("./Scripts/00 - Project variables.R")

#------------------------------------------
# Raster for cropping
rst.crop <- raster(res = 0.1, xmn = -10, xmx = 70, ymn = -75, ymx = -30, crs = "+proj=longlat +datum=WGS84")

#------------------------------------------
# Marion coordinates
islands <- data.frame(x = c(37.743611, 37.943333),
                      y = c(-46.9125, -46.644167))

#------------------------------------------
# World map

make180 <- function(lon) {
  ind <- which(lon > 180)
  lon[ind] <- lon[ind] - 360
  return(lon)
}

world <- map_data("world2Hires")
world$long <- make180(world$long)

#------------------------------------------
# Land for plotting
# data(countriesLow, package = "rworldmap")
# wrld <- crop(countriesLow, rst.crop)
# 
# # Get EEZs
# # eez <- readRDS("./Data/eez/eez.RDS")
# # eez_crop <- st_crop(eez, rst.crop)
# eez_crop <- readRDS("./Data/eez/eez_sp.RDS")
# 
# # Get CCAMLR boundary
# # ccamlr <- readRDS("./Data/eez/ccamlr.RDS")
# # ccamlr_crop <- st_crop(ccamlr, rst.crop)
# ccamlr_crop <- readRDS("./Data/eez/ccamlr_sp.RDS")
# 
# # Get MPAs
# # mpa <- readRDS("./Data/eez/mpa.RDS")
# # mpa_crop <- st_crop(mpa, rst.crop)
# mpa_crop <- readRDS("./Data/eez/mpa_sp.RDS")

#------------------------------------------
# # Islands
# islands <- data.frame(x = c(37.743611, 37.943333),
#                       y = c(-46.9125, -46.644167))

#------------------------------------------
# Climate change velocity using multivariate analogs,
# method described in:
# Ordonez & Williams (2013) DOI: 10.1007/s10584-013-0752-1

distMap <- function(historical,
                    future,
                    thr = 0.5,
                    which.distance = "SQeuclidean") {
  
  require(analogue)
  require(geosphere)
  
  pb <- txtProgressBar(min = 0, max = nrow(historical), style = 3)
  
  frm <- data.frame()
  
  # Add an ID column for matching
  historical$ID <- 1:nrow(historical)
  future$ID <- 1:nrow(future)
  
  # Get the future conditions
  fut <- future[, 3:ncol(future)]
  fut <- fut[complete.cases(fut), ]
  futIDs <- fut$ID
  fut$ID <- NULL
  
  for (i in 1:nrow(historical)) {
    
    setTxtProgressBar(pb, i)
    
    his <- historical[i, 3:ncol(historical)]
    his <- his[complete.cases(his), ]
    hisIDs <- his$ID
    his$ID <- NULL
    
    # Run only if the cell in question has no missing values
    if(nrow(his) > 0) {
      
      d <- analogue::distance(his, fut, method = which.distance)
      
      nnval <- min(d)
      if(nnval < thr) {
        nndx <- which(d == min(d))
        nnid <- futIDs[nndx]
        
        # Get the data for the nearest neighbour
        this.future.cell <- future[future$ID == nnid, ]
        
        # Calculate distance and bearing to the nearest neighbour
        this.historical.cell <- historical[i, c("x", "y")]
        
        d <- distGeo(p1 = this.historical.cell[ , c("x", "y")], p2 = this.future.cell[ , c("x", "y")])/1000
        b <- bearing(p1 = this.historical.cell[ , c("x", "y")], p2 = this.future.cell[ , c("x", "y")])
        target.lon <- this.future.cell$x
        target.lat <- this.future.cell$y
      } else {
        d <- NA
        b <- NA
        target.lon <- NA
        target.lat <- NA
      }
      
    } else {
      d <- NA
      b <- NA
      target.lon <- NA
      target.lat <- NA
    }
    giveback <- historical[i, ]
    giveback$ID <- NULL
    giveback$distance <- d
    giveback$bearing <- b
    giveback$target.lon <- target.lon
    giveback$target.lat <- target.lat
    
    frm <- rbind(frm, giveback)
    rm(giveback)
    rm(his)
  }
  
  return(frm)
  
  close(pb)
}

#------------------------------------------
# Calculate for each climate model

# for (j in all.climate.mods) {
#   
#   this.climate <- j
#   
#   print(j)
# 
# # Get the prediction grids
# his.grid <- readRDS(paste0("./Data/predictionGrids/predGrid_", this.climate, "_", this.season, "_historical.RDS"))
# fut.grid <- readRDS(paste0("./Data/predictionGrids/predGrid_", this.climate, "_", this.season, "_", this.scenario, ".RDS"))
# 
# # Dump empty columns
# his.grid <- remove_empty(dat = his.grid, which = "cols")
# fut.grid <- remove_empty(dat = fut.grid, which = "cols")
# 
# # Deal with any mismatched variables
#   common_cols <- intersect(colnames(his.grid), colnames(fut.grid))
#   his.grid <- subset(his.grid, select = common_cols)
#   fut.grid <- subset(fut.grid, select = common_cols)
# 
# #------------------------------------------
# # Pixel by pixel, Gower's distance
# 
# this.gower <- gower_dist(his.grid[ , 3:ncol(his.grid)], fut.grid[ , 3:ncol(fut.grid)])
# 
# this.gower <- data.frame("gower" = this.gower,
#                            "lon" = his.grid$x,
#                            "lat" = his.grid$y)
# 
# # ggplot(data = dist.gower, aes(x = lon, y = lat, fill = gower)) + geom_tile() +
# #   coord_quickmap(xlim = c(-10, 70), ylim = c(-75, -30), expand = FALSE) +
# #   geom_polygon(data = world, aes(x=long, y = lat, group = group), color = "gray40", fill = "gray") +
# #   annotate("point", x = islands$x, y = islands$y, colour = "black", size = 4) +
# #   annotate("point", x = islands$x, y = islands$y, colour = "#EE7733", size = 3) +
# #   scale_fill_gradientn(colours=ocean.deep(100), guide = "colourbar", name = "Gower's distance")
# 
# this.gower$climate <- this.climate
# this.gower$seaon <- this.season
# 
# saveRDS(this.gower, paste0("./Data/climateOutput/gower_", this.season, "_", this.climate, "_", this.scenario, ".RDS"))
# 
# rm(this.gower)
# 
# #------------------------------------------
# # Climate change velocity using multivariate analogs
# foo <- distMap(historical = his.grid,
#                 future = fut.grid,
#                 thr = 1,
#                 which.distance = "gower")
# 
# foo$season <- this.season
# foo$climate <- this.climate
# 
# saveRDS(foo, paste0("./Data/climateOutput/gowerDistanceBearing_", this.season, "_", this.climate, "_", this.scenario, ".RDS"))
# 
# rm(foo)
# 
# }

#------------------------------------------
# Plot distance and bearing for each model,
# and keep the distance and bearing for mean plots

d.all <- data.frame()
r.all <- stack()

for (j in all.climate.mods) {
  
  this.climate <- j
  print(j)
  
  d <- readRDS(paste0("./Data/climateOutput/gowerDistanceBearing_", this.season, "_", this.climate, "_", this.scenario, ".RDS"))
  
  # Create raster
  r <- rasterFromXYZ(d[ , c("x", "y", "distance")])
  r <- crop(r, rst.crop)
  

  
  # Save to stack
  r.all <- stack(r.all, r)
  
  # Plot bearing to nearest analogue
  
  # Correct the bearings
  f <- function(x) {
    if (!is.na(x)) {
      if (x < 0) {
        x <- 360 + x
      } else {
        x <- x
      }
    } else {
      x <- NA
    }
    return(x)
  }
  
  d$bearing2 <- sapply(d$bearing, FUN = f)
  

  
  # Bind
  d <- d[ , c("distance", "bearing", "bearing2", "season", "climate")]
  d.all <- rbind(d.all, d)
  rm(d)
  
}


#------------------------------------------
# Plot bearings

pdf(paste0("./Data/climateOutputPlots/", "analogueBearing_", this.season, "_", this.scenario, ".pdf"),
    width = (100*0.04)/0.8888889, height = (100*0.04)/0.8888889, useDingbats = F)

p <- ggplot(data = d.all, aes(x = bearing2, fill = climate)) +
  geom_histogram(bins = 50) +
  coord_polar(start = 0) +
  scale_x_continuous(breaks = seq(0, 360, 30), expand=c(0,0), lim=c(0, 360)) +
  scale_y_continuous(lim=c(0, 20000)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  theme_bw() +
  labs(x = "Bearing (\u00B0)", y = "Count") +
  theme(axis.text = element_text(colour = "black"),
        plot.background = element_rect(fill = "white", colour = "white"))
print(p)
dev.off()
