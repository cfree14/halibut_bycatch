

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
list.files(indir)

# Read port key
port_key_orig <- readRDS("data/cdfw_keys/processed/CDFW_port_key.Rds")

# Read gear key
gear_key_orig <- readRDS("data/cdfw_keys/processed/CDFW_gear_key.Rds")

# Block id
blocks <- wcfish::blocks
blocks_df <- blocks %>%
  sf::st_drop_geometry()


# Merge data
################################################################################

# Files
files <- list.files(indir, pattern=".csv")
files_do <- files[files!="VesselPermitExtract.CSV"]

# Merge
data1 <- purrr::map_df(files_do, function(x){

  # Read data
  fdata <- read.csv(file.path(indir, x), as.is=T, na.strings = "")

  # Format data
  fdata1 <- fdata %>%
    # Coerce to character for successful merge
    mutate(StatePermitNumber=as.character(StatePermitNumber),
           GFPermitNum=as.character(GFPermitNum))


})


# Format data
################################################################################

# Format data
data2 <- data1 %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(receipt_id=landing_receipt_num,
         date=landing_date,
         permit_state=state_permit_number,
         permit_gf=gf_permit_num,
         port=port_name,
         block_id=cdfw_block_id,
         primary_gear=primary_gear_name,
         species=species_name,
         landings_lb=pounds,
         price_usd_lb=unit_price,
         value_usd=total_price,
         gear=gear_name,
         condition_id=fish_condition_id,
         condition=fish_condition_name,
         use=use_name) %>%
  # Format date
  mutate(date=lubridate::mdy(date),
         year=lubridate::year(date),
         month=lubridate::month(date)) %>%
  # Format species
  mutate(species=species %>% stringr::str_trim(.) %>% gsub("  ", ", ", .)) %>%
  # Format gear
  mutate(gear_id=ifelse(is.na(gear_id), 0, gear_id),
         gear=ifelse(gear_id==0, "Unknown", gear),
         gear=stringr::str_to_sentence(gear)) %>%
  # Add gear category
  left_join(gear_key_orig %>% select(gear_code, gear_type), by=c("gear_id"="gear_code")) %>%
  mutate(gear_type=ifelse(gear=="Invalid", "Unknown", gear_type)) %>%
  # Format primary gear
  # Assume that 75=Invalid and that NA=Unknown
  mutate(primary_gear_id=ifelse(is.na(primary_gear_id), 0, primary_gear_id),
         primary_gear_id=ifelse(primary_gear_id==75, -1, primary_gear_id),
         primary_gear=ifelse(primary_gear_id==0, "Unknown", primary_gear),
         primary_gear=ifelse(primary_gear_id==-1, "Invalid", primary_gear),
         primary_gear=stringr::str_to_sentence(primary_gear)) %>%
  # Format port
  mutate(port=stringr::str_to_title(port),
         port=recode(port,
                     "Princeton-Half Moon"="Princeton-Half Moon Bay"),
         port=case_when(port_id==0 ~ "Unknown",
                        port_id==-1 ~ "Invalid",
                        TRUE ~ port)) %>%
  # Add port complex
  left_join(port_key_orig %>% select(port_code, port_complex), by=c("port_id"="port_code")) %>%
  # Format port complex
  mutate(port_complex=ifelse(is.na(port_complex), "Unknown Major Port", port_complex),
         port_complex=recode(port_complex, "Unknown Major Port"="Unknown")) %>%
  # Format condition
  mutate(condition=stringr::str_to_sentence(condition),
         condition=ifelse(condition_id==0, "Dead", condition)) %>%
  # Format use
  # Assume that NA=Unknown
  mutate(use_id=ifelse(is.na(use_id), 0, use_id),
         use=ifelse(use_id==0, "Unknown", use),
         use=stringr::str_to_sentence(use)) %>%
  # Add block info
  left_join(blocks_df %>% select(block_id, block_state, block_type), by="block_id") %>%
  mutate(block_type=ifelse(is.na(block_type), "Invalid", block_type),
         block_state=ifelse(is.na(block_state), "Invalid", block_state)) %>%
  # Arrange
  select(receipt_id, year, month, date,
         business_id, fisher_id, vessel_id, permit_state, permit_gf,
         port_complex, port_id, port, block_id, block_type, block_state,
         primary_gear_id, primary_gear, gear_id, gear, gear_type,
         species_id, species,
         condition_id, condition, use_id, use,
         landings_lb, value_usd, price_usd_lb,
         everything())

# Inspect
str(data2)
freeR::complete(data2)

# Inspect
range(data2$date)

# Port key
port_key <- data2 %>%
  select(port_id, port, port_complex) %>%
  unique() %>%
  arrange(port_id)
anyDuplicated(port_key$port)
anyDuplicated(port_key$port_id)
freeR::which_duplicated(port_key$port_id)

# Use key
use_key <- data2 %>%
  select(use_id, use) %>%
  unique() %>%
  arrange(use_id)
anyDuplicated(use_key$use)
anyDuplicated(use_key$use_id)
freeR::which_duplicated(use_key$use_id)

# Condition key
condition_key <- data2 %>%
  select(condition_id, condition) %>%
  unique() %>%
  arrange(condition_id)
anyDuplicated(condition_key$condition)
anyDuplicated(condition_key$condition_id)
freeR::which_duplicated(condition_key$condition_id)

# Species key
species_key <- data2 %>%
  select(species_id, species) %>%
  unique() %>%
  arrange(species_id)
anyDuplicated(species_key$species)
anyDuplicated(species_key$species_id)
freeR::which_duplicated(species_key$species_id)

# Gear key
gear_key <- data2 %>%
  select(gear_id, gear, gear_type) %>%
  unique() %>%
  arrange(gear_id)
anyDuplicated(gear_key$gear)
anyDuplicated(gear_key$gear_id)
freeR::which_duplicated(gear_key$gear_id)

# Primary gear key
primary_gear_key <- data2 %>%
  select(primary_gear_id, primary_gear) %>%
  unique() %>%
  arrange(primary_gear)
anyDuplicated(primary_gear_key$primary_gear)
anyDuplicated(primary_gear_key$primary_gear_id)
freeR::which_duplicated(primary_gear_key$primary_gear)
freeR::which_duplicated(primary_gear_key$primary_gear_id)

# Block key
block_key <- data2 %>%
  select(block_id, block_state, block_type) %>%
  unique()


# Export data
################################################################################

# Export data
saveRDS(data2, file=file.path(outdir, "CDFW_2000_2020_landings_receipts.Rds"))


