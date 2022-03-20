

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
list.files(indir)
data_orig1 <- read.csv(file.path(indir, "GNC8389.csv"), as.is=T)
data_orig2 <- read.csv(file.path(indir, "GNM8389.csv"), as.is=T)
data_orig3 <- read.csv(file.path(indir, "GNS8389.csv"), as.is=T)

# Read species key
spp_key_orig <- readxl::read_excel(file.path(indir, "species codes 2009.xlsx"))


# Format species key
################################################################################

# Format species key
spp_key <- spp_key_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(spp_code_num=exsp,
         spp_code_chr=exsp_text,
         comm_name_orig=common_name,
         sci_name=scientific_name) %>%
  # Arrange
  select(spp_code_num, spp_code_chr, pacfin_code,
         comm_name_orig, sci_name, everything())

# Inspect
str(spp_key)
freeR::complete(spp_key)

# Export
saveRDS(spp_key, file=file.path(outdir, "species_key.Rds"))


# Format data 1
################################################################################

# To do list
# 1) Understand columns
# 2) Convert numerics properly
# 3) Not all species have matches

# Format data
data1 <- data_orig1 %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(set_id=setno,
         spp_code_chr=species) %>%
  # Format date
  mutate(date=lubridate::ymd(date)) %>%
  # Add species name
  left_join(spp_key %>% select(spp_code_chr, comm_name_orig)) %>%
  # Convert to numeric
  mutate(dlive1=as.numeric(dlive),
         nodam1=as.numeric(nodam),
         nokept1=as.numeric(nokept))

# Inspect
str(data1)
freeR::complete(data1)

# Data
range(data1$date)

# File links
sort(unique(data1$m_file_link))
sort(unique(data1$s_file_link))


# Format data 2
################################################################################

# Format data
data2 <- data_orig2 %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(set_id=setno,
         spp_code_chr=species) %>%
  # Format date
  mutate(date=lubridate::ymd(date)) %>%
  # Add species name
  left_join(spp_key %>% select(spp_code_chr, comm_name_orig))


# Inspect
freeR::complete(data2)
str(data2)
range(data2$length)
range(data2$altlen)
sort(unique(data2$sex))
sort(unique(data2$mat))
sort(unique(data2$disp))


# Format data 3
################################################################################

# Format data
data3 <- data_orig3 %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(set_id=setno) %>%
  # Format date
  mutate(date=lubridate::mdy(date))

# Inspect
str(data3)
freeR::complete(data3)



