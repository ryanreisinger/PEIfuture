## EEZ information for plots

library(mregions)
library(sf)
library(rgdal)

setwd("D:\\PEIfuture\\Working")

#-------------------------------
# From marineregions
# Look up names in the EEZ data
rnames <- mr_names("MarineRegions:eez")

# Verify that the entries are here

# Prince Edward Islands
# http://marineregions.org/gazetteer.php?p=details&id=8384
rnames[rnames$mrgid == "8382", ]
rnames[rnames$mrgid == "8384", ]

# Get EEZs of interest (as sf objects)
sa_eez <- st_as_sf(mr_shp(key = "MarineRegions:eez",
              filter = "South African Exclusive Economic Zone"))

pei_eez <- st_as_sf(mr_shp(key = "MarineRegions:eez",
                  filter = "South African Exclusive Economic Zone (Prince Edward Islands)",
                  maxFeatures = 500))

crozet_eez <- st_as_sf(mr_shp(key = "MarineRegions:eez",
                     filter = "Crozet Islands Exclusive Economic Zone"))

kerguelen_eez <- st_as_sf(mr_shp(key = "MarineRegions:eez",
                     filter = "Kerguelen Exclusive Economic Zone"))

heard_eez <- st_as_sf(mr_shp(key = "MarineRegions:eez",
                                 filter = "Heard and McDonald Islands Exclusive Economic Zone"))

tristan_eez <- st_as_sf(mr_shp(key = "MarineRegions:eez",
                             filter = "Tristan Da Cunha Exclusive Economic Zone"))

#-------------------------------
# Union
eez <- st_union(sa_eez, pei_eez)
eez <- st_union(eez, crozet_eez)
eez <- st_union(eez, kerguelen_eez)
eez <- st_union(eez, heard_eez)
eez <- st_union(eez, tristan_eez)

# Crop

# Reference area
study_area <- st_bbox(c(xmin = -10, xmax = +70, ymax = -30, ymin = -75), crs = st_crs(4326))

# Crop
eez <- st_crop(st_buffer(eez, dist = 0), study_area) # 0 buffer to resolve geometry problem

# Write
saveRDS(eez, "./Data/eez/eez.RDS")
saveRDS(as_Spatial(eez), "./Data/eez/eez_sp.RDS")

#-------------------------------
# CCAMLR
#-------------------------------
ccamlr <- st_read(dsn = "./Data/eez", layer = "ccamlr")
ccamlr <- st_crop(st_buffer(ccamlr, dist = 0), study_area) # 0 buffer to resolve geometry problem
ccamlr <- st_union(st_buffer(ccamlr, dist = 0.0000001))

plot(ccamlr)
saveRDS(ccamlr, "./Data/eez/ccamlr.RDS")
saveRDS(as_Spatial(ccamlr), "./Data/eez/ccamlr_sp.RDS")
