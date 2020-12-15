# Project-wide variables for "PEI future"

# This file is sourced in each script to enable easy use of and changes to global variables
# needed in the analyses.

# Extent of the study
wholeExtent <- c(-180, 180, -80, -30) # Whole Southern Hemisphere at the moment

# Names of the environmental variables
enVarNames <- c("DEP",
                "SST",
                "SSTg",
                "SSH",
                "SSHg",
                "CURu",
                "CURv",
                "EKE",
                "CHL",
                "ICE",
                "WINu",
                "WINv")

# Species abbreviations
spNames <- c("AFS",
             "DMS",
             "GHA",
             "IYA",
             "KIN",
             "LMS",
             "MAC",
             "NGP",
             "ORC",
             "SES",
             "SFS",
             "SRP",
             "WAB",
             "WCP")

# Species in each season
spNamesSummer <- spNames[-c(5,8)]
spNamesWinter <- spNames[-c(3,14)]

# species full names
sp_full_names <- data.frame("abbreviated_name" = spNames,
                            "full_name" = c("Antarctic fur sea",
                                            "Sooty albatross",
                                            "Grey-headed albatross",
                                            "Indian yellow-nosed albatross",
                                            "King penguin",
                                            "Light-mantled albatross",
                                            "Macaroni penguin",
                                            "Northern giant petrel",
                                            "Orca",
                                            "Southern elephant seal",
                                            "Subantarctic fur seal",
                                            "Eastern rockhopper penguin",
                                            "Wandering albatross",
                                            "White-chinned petrel"))

# Climate model names
all.climate.mods <- c(
  "ACCESS1-0",
  "BCC-CSM1.1",
  "CanESM2",
  "CMCC-CM",
  "EC-EARTH",
  "GISS-E2-H-CC",
  "MIROC-ESM",
  "NorESM1-M")