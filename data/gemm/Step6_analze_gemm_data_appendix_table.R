

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/gemm/raw"
outdir <- "data/gemm/processed"
plotdir <- "data/gemm/figures"

# Data sources
# Groundfish Expanded Mortality Multi-year (GEMM) data
# GEMM homepage: https://www.fisheries.noaa.gov/west-coast/fisheries-observers/west-coast-fishery-observer-bycatch-and-mortality-reports
# Data Warehouse Landing Page: https://www.fisheries.noaa.gov/resource/data/fishery-resource-analysis-and-monitoring-division-fram-data-warehouse

# Read data
data_orig <- readRDS(file=file.path(outdir, "GEMM_2002_2020_data.Rds"))


# Setup
################################################################################

# Build data
data <- data_orig %>%
  # Reduce to )A CA Halibut sector
  filter(sector=="OA CA Halibut") %>%
  # Mark halibut or non-halibut
  mutate(type=ifelse(species=="California Halibut", "Halibut", "Non-halibut")) %>%
  # Compute totals by species and year (summing across mgmt grouops) %>%
  group_by(year, type) %>%
  summarize(landings_mt=sum(landings_mt),
            discards_mt=sum(discards_mt_tot)) %>%
  ungroup() %>%
  # Gather and spread
  gather(key="catch_type", value="catch_lb", 3:ncol(.)) %>%
  mutate(catg=paste(catch_type, type, sep="-")) %>%
  select(-c(type, catch_type)) %>%
  spread(key="catg", value="catch_lb")

# Export
write.csv(data, file=file.path(plotdir, "TableX_gemm_time_series_trawl_fishery.csv"), row.names=F)






