

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
data_orig <- readxl::read_excel(file.path(indir, "selection.xlsx"), sheet="data", na="N/A")

# Read sector key (only the type column is robust)
sector_key <- readxl::read_excel(file.path(indir, "GEMM_sector_key.xlsx")) %>%
  select(sector, type) %>%
  rename(sector_type=type)


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  rename(mgmt_group=grouping,
         catch_mt=total_discard_and_landings_mt,
         landings_mt=total_landings_mt,
         discards_mt=total_discard_mt,
         discards_mt_adj=total_discard_with_mort_rates_applied_mt,
         catch_mt_adj=total_discard_with_mort_rates_applied_and_landings_mt,
         discards_cv=cv,
         groundfish_fmp_yn=type) %>%
  # Convert "cv"
  mutate(discards_cv=as.numeric(discards_cv)) %>%
  # Convert year
  mutate(year=as.numeric(year)) %>%
  # Add sector type
  left_join(sector_key, by="sector") %>%
  # Arrange
  select(sector_type, sector, mgmt_group, groundfish_fmp_yn, species, year, discards_cv,
         landings_mt, discards_mt, catch_mt, discards_mt_adj, catch_mt_adj, everything())


# Confirm that:
# catch = landings + discards
sum(data$catch_mt != (data$landings_mt + data$discards_mt))

# Confirm that:
# catch = landings + discards
sum(abs(data$catch_mt_adj -  (data$landings_mt + data$discards_mt_adj)) > 1)

# Inspect species
spp_key <- data %>%
  group_by(species) %>%
  summarize(ntypes=n_distinct(groundfish_fmp_yn),
            types=paste(sort(unique(groundfish_fmp_yn)), collapse=", "))

# Sector group
sector_key <- data %>%
  group_by(sector) %>%
  summarize(n=n())

# Inspect data
str(data)
freeR::complete(data)
range(data$year)
table(data$sector) # LE = limited entry, OA = open access, CS = catch shares, DTL = daily trip limit, EM = electronic monitoring
table(data$mgmt_group)
table(data$groundfish_fmp_yn)
table(data$species)


# Build species key
################################################################################

# Export species key
write.csv(spp_key, file=file.path(outdir, "species_key.csv"), row.names = F)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "GEMM_2002_2020_data.Rds"))

