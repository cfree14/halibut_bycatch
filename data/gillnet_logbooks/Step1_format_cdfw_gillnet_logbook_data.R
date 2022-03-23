

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/gillnet_logbooks/raw"
outdir <- "data/gillnet_logbooks/processed"
plotdir <- "data/gillnet_logbooks/figures"

# Read data
list.files(indir)
data_orig <- readxl::read_excel(file.path(indir, "GillnetLogs_2000-2020.xlsx"))


# Format data
################################################################################

# Needs a ton of work


# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(vessel=vessel_name,
         target_spp_code=tarspc)

# Inspect
str(data)
