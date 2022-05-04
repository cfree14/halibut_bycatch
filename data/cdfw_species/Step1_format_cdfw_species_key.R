

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/cdfw_species/raw"
outdir <- "data/cdfw_species/processed"
plotdir <- "data/cdfw_species/figures"

# Read species key
spp_key_orig <- readxl::read_excel(file.path(indir, "species codes 2009.xlsx"))


# Format keys
################################################################################

# Format species key
spp_key <- spp_key_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(spp_code_num=exsp,
         spp_code_chr=exsp_text,
         comm_name_orig=common_name,
         sci_name=scientific_name) %>%
  # Regularize common name
  mutate(comm_name=wcfish::convert_names(comm_name_orig, to="regular")) %>%
  # Arrange
  select(spp_code_num, spp_code_chr, pacfin_code,
         comm_name_orig, comm_name, sci_name, everything())

# Inspect
str(spp_key)
freeR::complete(spp_key)

# Export
saveRDS(spp_key, file=file.path(outdir, "CDFW_species_key.Rds"))






