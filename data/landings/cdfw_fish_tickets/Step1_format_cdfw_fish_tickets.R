

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/wc_landings_data/data/cdfw/confidential/processed/"
outdir <- "data/landings/barsky/processed"
plotdir <- "data/landings/barsky/figures"


# Read data
data_orig <- readRDS(file.path(datadir, "CDFW_2000_2020_monthly_landings_by_port_block_species.Rds"))

# Species key
spp_key <- data_orig %>%
  select(comm_name) %>%
  unique() %>%
  arrange(comm_name)

# Build data
################################################################################

# Summarize by port
data <- data_orig %>%
  # Reduce to CA halibut
  filter(comm_name=="California halibut") %>%
  # Summarize
  group_by(year, port_name) %>%
  summarize(landings_lb=sum(landings_lb))
