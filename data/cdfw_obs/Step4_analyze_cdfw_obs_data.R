

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/cdfw_obs/raw"
outdir <- "data/cdfw_obs/processed"
plotdir <- "data/cdfw_obs/figures"


# Read data
list.files(outdir)
metadata <- readRDS(file.path(outdir, "CDFW_1983_1989_gillnet_observer_set_info.Rds"))
data_orig <- readRDS(file.path(outdir, "CDFW_1983_1989_gillnet_observer_data.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>%
  # Add set meta data
  left_join(metadata %>% select(set_id, lat_dd, long_dd, bottom_depth_fa, net_orientation, net_type, target_spp), by="set_id") %>%
  # Reduce to sets targeting CA halibut
  filter(target_spp=="California halibut") %>%
  # Add year
  mutate(year=lubridate::year(date),
         month=lubridate::month(date),
         yday=lubridate::yday(date)) %>%
  # Simplify to columns of interest
  select(-c(target_spp, s_file_link, m_file_link)) %>%
  select(year, month, yday, date,
         vessel_id, set_num, set_id,
         net_type, net_orientation, bottom_depth_fa, lat_dd, long_dd,
         spp_code_chr, comm_name, everything()) %>%
  # Halibut bycatch
  group_by(set_id) %>%
  mutate(halibut_yn=)

# Inspect
str(data)
freeR::complete(data)


# Plot data
################################################################################








