

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/swfsc_obs/raw"
outdir <- "data/swfsc_obs/processed"
plotdir <- "data/swfsc_obs/figures"

# Read data
spp_key_orig <- read.csv(file.path(indir, "SWFSC_observer_program_species_codes.csv"), as.is=T, na.strings="---")

# Setup
################################################################################

# Format data
spp_key <- spp_key_orig %>%
  # Format category
  mutate(category=stringr::str_to_sentence(category)) %>%
  # Fix a few names
  mutate(comm_name_orig=recode(comm_name_orig,
                               "Fulmar,Northern"="Fulmar, Northern",
                               "Pipefish Bay"="Pipefish, Bay",
                               "Turbot Hornyhead"="Turbot, Hornyhead",
                               "Halibut, CaliforniaThe main"="Halibut, California",
                               "Rockfish Yellowtail"="",
                               "Sturgeon Unidentified"="",
                               "Hagfish Pacific"="",
                               "Surfperch Barred"="",
                               "Surfperch Black"="",
                               "Surfperch Pile"="",
                               "Stargazer Smooth"="",
                               "Halibut Pacific"="",
                               "Dolphinfish, Mahi-Mahi, Dorado"="",
                               "Shrimp, Spot, Spot Prawn"="",
                               "Squid, Eggs"="",
                               "Porpoise Dall's"="")) %>%
  # Make name regular
  mutate(comm_name=wcfish::convert_names(comm_name_orig, to="regular")) %>%
  # Arrange
  select(category, code, comm_name_orig, comm_name, sci_name, everything())

# Inspect
freeR::complete(spp_key)

# Export
saveRDS(spp_key, file=file.path(outdir, "SWFSC_observer_program_spp_key.Rds"))
