

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
data_orig <- readxl::read_excel(file.path(indir, "GillnetLogs_2000-2020.xlsx"), col_types="text", na=c("N/A"))

# Get blocks
blocks <- wcfish::blocks
blocks_df <- blocks %>% sf::st_drop_geometry()

# Read species key
spp_key <- readRDS("data/cdfw_keys/processed/CDFW_species_key.Rds")

# Read vessel key
vessel_key <- readRDS("data/comm_permits/processed/CDFW_vessel_key.Rds")

# Meta-data
# https://www.fisheries.noaa.gov/inport/item/20833

table(data_orig$MESH_SIZE)

# Format data
################################################################################

# How to define a unique set?
# Could theoreticially fill in vessel meta-data gaps

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(logbook_id=sn,
         receipt_id=fish_game_receipt_no,
         boat_id=boatno,
         vessel=vessel_name,
         date=fishing_date,
         target_spp=tarspc,
         set_type=drift_set,
         soak_duration_hr=hours_net_soaked,
         block_id=fg_blocks,
         depth_fa=depths,
         net_length_fa=net_length,
         mesh_size_in=mesh_size,
         line_depth_ft=bouy_line_depth,
         comm_name_orig=common_name,
         spp_code=mlds_species_code,
         catch_n=num_catch,
         catch_lb=weights) %>%
  # Format year/date
  rename(date_orig=date) %>%
  mutate(year=as.numeric(year),
         date1=ifelse(grepl("/", date_orig), date_orig, NA) %>% lubridate::mdy(.) %>% as.character(),
         date2=ifelse(grepl("/", date_orig), NA, date_orig) %>% openxlsx::convertToDate() %>% as.character(),
         date=ifelse(!is.na(date1), date1, date2) %>% lubridate::ymd()) %>%
  select(-c(date1, date2)) %>%
  # Add day of year and date dummy
  mutate(yday=lubridate::yday(date),
         date_dummy=paste("2020", lubridate::month(date), lubridate::day(date), sep="-") %>% lubridate::ymd()) %>%
  # Format set type
  mutate(set_type=toupper(set_type),
         set_type=recode(set_type,
                         "D"="Drift gillnet",
                         "DRIFT"="Drift gillnet",
                         "S"="Set net",
                         "SET"="Set net",
                         "H"="Unknown",
                         "X"="Unknown"),
         set_type=ifelse(is.na(set_type), "Unknown", set_type)) %>%
  # Format status
  mutate(status=tolower(status)) %>%
  # Format target species
  # S - swordfish, H - halibut, R - rockfish, W - white sea bass, B - barracuda
  mutate(target_spp=recode(target_spp,
                           "Angel_Shark"="Angelshark",
                           "B"="Barracuda",
                           "Croaker_Kingfish"="White croaker (kingfish)",
                           "h"="Halibut",
                           "H"="Halibut",
                           "H, Angel, Sole, X"="Halibut, angelshark, sole, X",
                           "H, Angel, X, Sole"="Halibut, angelshark, sole, X",
                           "H, Sole"="Halibut, sole",
                           "H, Sole, Angel, X"="Halibut, angelshark, sole, X",
                           "H, Sole, S"="Halibut, sole, swordfish",
                           "H, W"="Halibut, white sea bass",
                           "H, W, Angel"="Halibut, white sea bass, angelshark",
                           "H, W, Angel, X"="Halibut, white sea bass, angelshark, X",
                           "H, W, X, Angel"="Halibut, white sea bass, angelshark, X",
                           "H, X, Angel"="Halibut, angelshark, X",
                           "H, X, Angel, Smoothhound"="Halibut, angelshark, smoothhound, X",
                           "H, X, Angel, Sole"="Halibut, angelshark, sole, X",
                           "H, X, Sole"="Halibut, sole, X",
                           "H, X, Sole, Angel"="Halibut, angelshark, sole, X",
                           "H, X, Sole, Bonito"="Halibut, sole, bonito, X",
                           "H, X, W, Angel"="Halibut, white sea bass, angelshark, X",
                           "H, X, W, Sole"="Halibut, white sea bass, sole, X",
                           # "Halibut"
                           # "J"
                           # "M"
                           # "Mackerel"
                           # "Midshipman"
                           # "N"
                           # "O"
                           # "R"
                           "S"="Swordfish",
                           "S, Bluefin, Opah"="Swordfish, bluefin, opah",
                           "S, Opah"="Swordfish, opah",
                           "Shovelnose_Shark"="Shovelnose shark",
                           # "Skate"
                           "Soupfin_Shark"="Soupfin shark",
                           # "SW"
                           "Swordfish/Shark"="Swordfish, shark",
                           # "T"
                           # "Thresher shark"
                           "w"="White sea bass",
                           "W"="White sea bass",
                           "W, H"="Halibut, white sea bass",
                           "W, X"="White sea bass, X",
                           "W, X, Angel"="White sea bass, angelshark, X",
                           "W, X, Leopard"="White sea bass, leopard shark, X",
                           "W, X, Sole"="White sea bass, sole, X",
                           "W, X, Yellowtail"="White sea bass, yellowtail, X",
                           "W, Y"="White sea bass, Y",
                           "W, Yellowtail"="White sea bass, yellowtail",
                           "Wite_Seabass"="White sea bass",
                           # "X"
                           "X, Soupfin"="Soupfin shark, X",
                           "X, W"="White sea bass, X",
                           "X, W, Leopard"="White sea bass, leopard shark, X",
                           "X, W, Sole, Angel"="White sea bass, sole, angelshark, X",
                           # "Yellowtail"
                           # "Z"
                           )) %>%
  # Correct a few species codes
  mutate(spp_code=as.numeric(spp_code),
         spp_code=case_when(spp_code==90 ~ 91, # swordfish
                            spp_code==242 ~ 236, # diamond turbot
                            spp_code==945 ~ 732, # sea snails
                            comm_name_orig=="AGARS" ~ 951,
                            comm_name_orig=="BLT-BULLET TUNA" ~ 19,
                            comm_name_orig=="AMPHIOXUS LANCELETS" ~ 915,
                            comm_name_orig=="EASTERN PACIFIC BONITO" ~ 3,
                            comm_name_orig=="PACIFIC SIERRA" ~ 52,
                            comm_name_orig=="ROCK CRABS" ~ 801,
                            TRUE ~ spp_code)) %>%
  # Format species
  left_join(spp_key %>% select(spp_code_num, comm_name), by=c("spp_code"="spp_code_num")) %>%
  # Fill in some common names
  mutate(comm_name=case_when(comm_name_orig=="Crab" & is.na(comm_name) ~ "Crab spp.",
                             comm_name_orig=="DLP-DOLPHINS NEI" & is.na(comm_name) ~ "Dolphin spp.",
                             comm_name_orig=="HYDROZOANS" & is.na(comm_name) ~ "Hydrozoan spp.",
                             comm_name_orig=="MAR-MARLINS (BILLFISHES) NEI" & is.na(comm_name) ~ "Marlin spp.",
                             comm_name_orig=="ROUGHEYE ROCKFISH" & is.na(comm_name) ~ "Rougheye rockfish",
                             TRUE ~ comm_name)) %>%
  mutate(comm_name=ifelse(is.na(comm_name), "Unknown", comm_name)) %>%
  # Format predator
  mutate(predator=predator %>% gsub("_", " ", .) %>% stringr::str_to_sentence(.),
         predator=recode(predator,
                         "150"="Shark",
                         "Harbor"="Harbor seal",
                         "Mako"="Mako shark",
                         "Sea lion - hagfish"="Sea lion, hagfish",
                         "Seals"="Seal")) %>%
  # Convert net length
  mutate(net_length_fa=recode(net_length_fa, "1 Mile"="880"), # measurements::conv_unit(1, "mi", "fathom")
         net_length_fa=as.numeric(net_length_fa)) %>%
  # Build a unique set id
  mutate(vessel_id_use=ifelse(!is.na(vessel_id), vessel_id, boat_id)) %>%
  mutate(set_id=paste(logbook_id, vessel_id_use, date, block_id, set_type, target_spp,
                      depth_fa, net_length_fa, mesh_size_in, line_depth_ft, soak_duration_hr, sep="-")) %>%
  # Add block info
  mutate(block_id_use=recode(block_id,
                             "685/708"="685",
                             "708/664"="708",
                             "708/685"="708",
                             "711, 710"="711") %>% as.numeric(.)) %>%
  left_join(blocks_df %>% select(block_id, block_lat_dd), by=c("block_id_use"="block_id")) %>%
  # Convert a bunch to numeric
  mutate(#depth_fa1=as.numeric(depth_fa),
         net_length_fa=as.numeric(net_length_fa),
         mesh_size_in=as.numeric(mesh_size_in),
         line_depth_ft=as.numeric(line_depth_ft),
         catch_n=as.numeric(catch_n),
         catch_lb=as.numeric(catch_lb)) %>%
  # Convert depth to numeric
  mutate(depth_fa=ifelse(depth_fa %in% c("200, 350", "200, 500", "300, 500"), NA, depth_fa),
         depth_fa=as.numeric(depth_fa)) %>%
  # Convert soak duration to numeric
  mutate(soak_duration_hr=recode(soak_duration_hr, "24-48"="36") %>% as.numeric()) %>%
  # Arrange
  select(logbook_id, receipt_id, vessel, boat_id, vessel_id, vessel_id_use, permit,
         year, date_orig, date, date_dummy, yday, set_id,
         set_type, block_id, block_id_use, block_lat_dd,
         depth_fa, net_length_fa:soak_duration_hr,
         target_spp, comm_name_orig, comm_name, spp_code,
         status, predator, catch_n, catch_lb, everything()) %>%
  # Remove useless columns (after checking
  select(-c(date_orig))

# Inspect
str(data)
freeR::complete(data)

# Inspect
range(data$year)
range(data$date, na.rm=T)
range(data$date_dummy, na.rm=T)
range(data$yday)
sort(unique(data$target_spp))
sort(unique(data$vessel_id))
table(data$set_type)
table(data$status)
table(data$predator)
sort(unique(data$line_depth_ft))
sort(unique(data$soak_duration_hr))
sort(unique(data$depth_fa))
sort(unique(data$block_id))

# Inspect species
spp_key_check <- data %>%
  count(spp_code, comm_name_orig, comm_name) %>%
  arrange(spp_code)
spp_key_check %>% filter(is.na(comm_name)) %>% pull(comm_name_orig) %>% sort()

# Inspect species more
spp_key_check2 <- data %>%
  count(spp_code, comm_name) %>%
  arrange(spp_code)
freeR::which_duplicated(spp_key_check2$spp_code)
freeR::which_duplicated(spp_key_check2$comm_name)

# Inspect set id
set_key <- data %>%
  select(logbook_id, receipt_id, vessel, boat_id, vessel_id, vessel_id_use, permit,
         year, date, set_id,
         set_type, block_id, block_id_use, block_lat_dd,
         depth_fa, net_length_fa:soak_duration_hr,
         target_spp) %>%
  unique()
freeR::which_duplicated(set_key$set_id)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "2000_2020_gillnet_logbook_data.Rds"))

