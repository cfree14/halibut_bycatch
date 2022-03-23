

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/trawl_logbooks/raw"
outdir <- "data/trawl_logbooks/processed"
plotdir <- "data/trawl_logbooks/figures"

# Read data
list.files(indir)
data_orig <- read.csv(file.path(indir, "MLS_TrawlExtract_03182022.csv"))


# Format data
################################################################################

# Needs a ton of work


# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake")

# Inspect
str(data)
