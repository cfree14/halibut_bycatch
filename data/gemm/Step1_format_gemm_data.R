

# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

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

# Build your own version
data1 <- data %>%
  # Rename discards (total) and discards (dead)
  rename(discards_mt_tot=discards_mt,
         discards_mt_dead=discards_mt_adj,
         mortality_mt=catch_mt_adj) %>%
  # If total=0 but dead>0, set total to dead
  mutate(discards_mt_tot=ifelse(discards_mt_tot==0, discards_mt_dead, discards_mt_tot)) %>%
  # Calculate live discards
  mutate(discards_mt_live=discards_mt_tot-discards_mt_dead) %>%
  # Simplify
  select(sector_type:discards_cv, landings_mt, discards_mt_tot, discards_mt_dead, discards_mt_live, catch_mt, mortality_mt)

# Inspect species
spp_key <- data1 %>%
  group_by(species) %>%
  summarize(ntypes=n_distinct(groundfish_fmp_yn),
            types=paste(sort(unique(groundfish_fmp_yn)), collapse=", "))

# Sector group
sector_key <- data1 %>%
  group_by(sector) %>%
  summarize(n=n())

# Inspect data
str(data1)
freeR::complete(data1)
range(data1$year)
table(data1$sector) # LE = limited entry, OA = open access, CS = catch shares, DTL = daily trip limit, EM = electronic monitoring
table(data1$mgmt_group)
table(data1$groundfish_fmp_yn)
table(data1$species)


# Build species key
################################################################################

# Export species key
write.csv(spp_key, file=file.path(outdir, "species_key.csv"), row.names = F)


# Export data
################################################################################

# Export
saveRDS(data1, file=file.path(outdir, "GEMM_2002_2020_data.Rds"))

