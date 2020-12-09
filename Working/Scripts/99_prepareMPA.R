## Prepare World MPA Database

setwd("D:\\PEIfuture\\Working")

library(rgdal)
library(sf)
library(fasterize)
library(raster)
library(SOmap)

## Received by email from Beth Pike,
## 15 April 2020
## beth.pike@marine-conservation.org
dat <- st_read(dsn = "D:/RAATD/hotspot/dat_in/mpa_data/2020_04_07_FullCalcs/mpatlas_20200407_FullCalcs.shp", layer = "mpatlas_20200407_FullCalcs")

# Reference area
study_area <- st_bbox(c(xmin = -10, xmax = +70, ymax = -30, ymin = -75), crs = st_crs(4326))

# Raster template
raster_temp <- raster(res = 0.1, xmn = -10, xmx = 70, ymn = -75, ymx = -30, crs = "+proj=longlat +datum=WGS84")

# Crop
dat <- st_crop(st_buffer(dat, dist = 0), study_area) # 0 buffer to resolve geometry problem

# Plot the valid MPAs
plot(dat["is_mpa"])

# Plot status
plot(dat["status"])

# Plot fishing
plot(dat["no_take"])

# Subsets
# MPAs
mpa <- dat["is_mpa"]
# Designated and proposed MPAs
mpa_designated <- dat[dat$status == "Designated","status"]
mpa_proposed <- dat[dat$status == "Proposed","status"]
# No-take MPAs
no_take_mpa <- dat[dat$"no_take" == "All","no_take"]

# Write
saveRDS(mpa, "./Data/eez/mpa.RDS")
writeRaster(fasterize(st_cast(mpa), raster_temp), "./Data/eez/mpa_raster.grd", format = "raster", overwrite = T)

saveRDS(mpa_designated, "./Data/eez/mpa_designated.RDS")
writeRaster(fasterize(st_cast(mpa_designated), raster_temp), "./Data/eez/mpa_designated_raster.grd", format = "raster", overwrite = T)

saveRDS(mpa_proposed, "./Data/eez/mpa_proposed.RDS")
writeRaster(fasterize(st_cast(mpa_proposed), raster_temp), "./Data/eez/mpa_proposed_raster.grd", format = "raster", overwrite = T)

saveRDS(no_take_mpa, "./dat_out/no_take_mpa.RDS")
writeRaster(fasterize(st_cast(no_take_mpa), raster_temp), "./Data/eez/no_take_mpa_raster.grd", format = "raster", overwrite = T)

saveRDS(as_Spatial(mpa), "./Data/eez/mpa_sp.RDS")

# # Raster for cropping
# rst.crop <- raster(res = 0.1, xmn = -10, xmx = 70, ymn = -75, ymx = -30, crs = "+proj=longlat +datum=WGS84")
# 
# mpa <- readRDS("./Data/eez/mpa.RDS") 
# mpa_cropped <- st_crop(x = mpa, y = rst.crop)
# 
# # Write
# saveRDS(mpa_cropped, "./Data/eez/mpa_cropped.RDS")