

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/landings_receipts/raw"
outdir <- "data/landings_receipts/processed"
plotdir <- "data/landings_receipts/figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CDFW_2000_2020_landings_receipts.Rds"))
permits_orig <- readRDS("data/comm_permits/processed/CDFW_2000_2021_permit_data.Rds")



# Format data
################################################################################


# Format data
data <- data_orig %>%
  filter(species=="Halibut, California")
