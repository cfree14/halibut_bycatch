

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/cdfw_keys/raw"
outdir <- "data/cdfw_keys/processed"
plotdir <- "data/cdfw_keys/figures"



# Format species key
################################################################################

# Read species key
spp_key_orig <- readxl::read_excel(file.path(indir, "species codes 2009.xlsx"))

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


# Format port key
################################################################################

# Read port key
port_key_orig <- read.csv(file.path(indir, "PortCodesExtract.csv"), as.is=T)


# Format port key
port_key <- port_key_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(port=port_name,
         port_complex=major_port_name,
         port_complex_code=major_port_code) %>%
  # Format port
  mutate(port=stringr::str_to_title(port),
         port_complex=stringr::str_to_title(port_complex)) %>%
  # Format date
  mutate(discontinued_date=lubridate::mdy(discontinued_date)) %>%
  # Arrange
  select(port_complex, port_complex_code, port, port_code, everything()) %>%
  arrange(port_complex, port)

# Inspect
str(port_key)

# Export
saveRDS(port_key, file=file.path(outdir, "CDFW_port_key.Rds"))


# Format gear key
################################################################################

# Read port key
gear_key_orig <- read.csv(file.path(indir, "GearCodesExtract.csv"), as.is=T)


# Format port key
gear_key <- gear_key_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(gear=gear_description,
         discontinued_date=discontinue_date) %>%
  # Format gear
  mutate(gear=stringr::str_to_title(gear)) %>%
  # Format date
  mutate(discontinued_date=lubridate::mdy(discontinued_date))

# Inspect
str(gear_key)

# Export
saveRDS(gear_key, file=file.path(outdir, "CDFW_gear_key.Rds"))



