

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/wcgop/raw"
outdir <- "data/wcgop/processed"
plotdir <- "data/wcgop/figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "WCGOP_2002_2020_observer_data.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>%
  # Reduce to sector of interest
  filter(sector=="OA CA Halibut" & catch) %>%
  # Add set id
  mutate(set_id=paste(trip_id, haul_id, sep="-")) %>%
  # Reduce to sets with CA halibut catch
  group_by(set_id) %>%
  mutate(halibut_yn="California Halibut" %in% comm_name & landings_kg[]) %>%
  ungroup() %>%
  filter(halibut_yn==T) %>%
  # Summarize by set id
  group_by(set_id, comm_name) %>%
  summarize(halibut_kg_retained=landings_kg[comm_name=="California Halibut"],
            bycatch_ratio_retained=landings_kg/halibut_kg_retained,
            bycatch_ratio_discarded=discards_kg/halibut_kg_retained) %>%
  ungroup()
