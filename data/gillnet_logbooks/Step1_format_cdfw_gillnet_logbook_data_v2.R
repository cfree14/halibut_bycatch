

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

# Read species key
spp_key <- readRDS("data/cdfw_keys/processed/CDFW_species_key.Rds")

# Read data
data1_orig <- readxl::read_excel(file.path(indir, "2000-18_GillnetLogbookDataset_220502.xlsx"), col_types = "text", na=c("N/A"))
data2_orig <- readxl::read_excel(file.path(indir, "GillnetLogs_2017-2020.xlsx"), col_types = "text", na=c("N/A"))

# Blocks
blocks <- wcfish::blocks
blocks_df <- blocks %>% sf::st_drop_geometry()

# Format dataset #1
################################################################################

# Format data
data1 <- data1_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(date=fishing_date,
         vessel_name=current_name,
         vessel_id=cdfw_vessel_id,
         license_num=cfl,
         target_spp=tarspc,
         net_type=drift_set,
         net_length_ft=net_length,
         mesh_size_in=mesh_size,
         buoy_line_depth_fa=bouy_line_depth,
         block_id=fg_blocks,
         comm_name=common_name,
         catch_n=nocatch,
         catch_lb=weights,
         depth_fa=depths,
         soak_hr=hours_net_soaked) %>%
  # Remove last two rows which are crazy
  slice(1:(nrow(.)-2)) %>%
  # Format date
  mutate(date=openxlsx::convertToDate(date) %>% as.character() %>% lubridate::ymd(),
         year=lubridate::year(date)) %>%
  # Format status
  mutate(status=stringr::str_to_sentence(status)) %>%
  # Format net type
  mutate(net_type=stringr::str_to_upper(net_type),
         net_type=recode(net_type, "D"="Drift", "S"="Set")) %>% # What are X and H?
  # Format target species
  mutate(target_spp=target_spp %>% gsub("_", " ", .) %>% stringr::str_to_sentence(.),
         target_spp=recode(target_spp, "Wite seabass"="White seabass")) %>%
  # Format predator
  mutate(predator=predator %>% gsub("_", " ", .) %>% stringr::str_to_sentence(),
         predator=recode(predator,
                         "150"="Shark",
                         "Harbor"="Harbor seal",
                         "Seal bird"="Seal, bird")) %>%
  # Convert to numeric
  mutate(set_id=as.numeric(set_id),
         vessel_id=as.numeric(vessel_id),
         license_num=as.numeric(license_num),
         block_id=as.numeric(block_id),
         depth_fa=as.numeric(depth_fa),
         soak_hr=as.numeric(soak_hr),
         net_length_ft=as.numeric(net_length_ft),
         mesh_size_in=as.numeric(mesh_size_in),
         buoy_line_depth_fa=as.numeric(buoy_line_depth_fa),
         catch_n=as.numeric(catch_n),
         catch_lb=as.numeric(catch_lb)) %>%
  # Add block type
  left_join(blocks_df %>% select(block_id, block_type), by="block_id") %>%
  mutate(block_type=ifelse(is.na(block_type), "Invalid", block_type)) %>%
  # Arrange
  select(set_id, vessel_id, vessel_num, vessel_name, license_num, skipper_name,
         year, date, block_type, block_id, depth_fa,
         target_spp, net_type, net_length_ft, mesh_size_in, buoy_line_depth_fa,
         soak_hr,
         comm_name,
         status, predator, catch_n, catch_lb)


# Inspect
str(data1)
freeR::complete(data1)

# Inspect
sort(unique(data1$set_id))
table(data1$vessel_name)
range(data1$date, na.rm=T)
table(data1$status)
table(data1$net_type) # What are H and X?
table(data1$target_spp) # What are H, J, M, N, O, R, T, W, and Z?
range(data1$depth_fa, na.rm=T) # Are the depths fathoms?
table(data1$comm_name)
table(data1$predator)

# Vessel key
vessel_key <- data1 %>%
  select(vessel_id, vessel_num, vessel_name) %>%
  unique() %>%
  arrange(vessel_id)

# Skipper key
skipper_key <- data1 %>%
  select(license_num, skipper_name) %>%
  unique() %>%
  arrange(license_num)

# Set key
set_key <- data1 %>%
  group_by(vessel_id, vessel_num, date, block_id, target_spp, depth_fa, net_length_ft, mesh_size_in, soak_hr) %>%
  summarize(nsetids=n_distinct(set_id)) %>%
  arrange(desc(nsetids))

# Format dataset #2
################################################################################

# Format data
data2 <- data2_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(logbook_id=sn,
         receipt_id=fish_game_receipt_no,
         boat_num=boatno,
         date_orig=fishing_date,
         target_spp=tarspc,
         target_spp_final=final_target_species,
         net_type_orig=drift_set,
         net_type_final=final_net_type_set_drift,
         block_id=fg_blocks,
         depth_fa=depths,
         net_length_ft=net_length,
         mesh_size_in=mesh_size,
         buoy_line_depth_fa=bouy_line_depth,
         soak_hr=hours_net_soaked,
         comm_name1=common_name,
         comm_name2=final_mlds_common_name,
         spp_code=mlds_species_code,
         catch_n=num_catch,
         catch_lb=weights) %>%
  # Convert date
  mutate(date1=ifelse(grepl("/", date_orig), date_orig, NA) %>% lubridate::mdy(.) %>% as.character(),
         date2=ifelse(grepl("/", date_orig), NA, date_orig) %>% openxlsx::convertToDate() %>% as.character(),
         date=ifelse(!is.na(date1), date1, date2) %>% lubridate::ymd()) %>%
  select(-c(date1, date2, date_orig)) %>%
  # Convert to numeric
  mutate(across(.cols=c(year, vessel_id, boat_num), .fns=as.numeric)) %>%
  # Convert numeric catch
  mutate(catch_n=catch_n %>% stringr::str_trim(.) %>% as.numeric(.)) %>%
  mutate(predator=ifelse(catch_lb=="Sea Lion", "Sea lion", predator),
         catch_lb=catch_lb %>% gsub("Sea Lion", "", .) %>% stringr::str_trim(.) %>% as.numeric(.)) %>%
  # Format net type
  mutate(net_type=recode(net_type_orig, "D"="Drift", "S"="Set"),
         net_type=ifelse(net_type %in% c("1", "2", "3", "67") | is.na(net_type), net_type_final, net_type)) %>%
  # Format status
  mutate(status=stringr::str_to_sentence(status)) %>%
  # Format predator
  mutate(predator=stringr::str_to_sentence(predator),
         predator=recode(predator,
                         "?"="Unknown",
                         "Birds"="Seabird",
                         "Blue sharks"="Blue shark",
                         "Harbor seals"="Harbor seal",
                         "Harbor seals and sea lions"="Harbor seal, sea lion",
                         "Mako"="Mako shark",
                         "Sea lions"="Sea lion",
                         "Sea lion - hagfish"="Sea lion, hagfish",
                         "Sea lions and harbor seals"="Sea lion, harbor seal",
                         "Sea lions and slime eels"="Sea lion, slime eel",
                         "Seal?"="Seal",
                         "Seals"="Seal",
                         "Seals, sea lion"="Seal, sea lion",
                         "Slime eels"="Slime eel",
                         "Unknown, maybe seal"="Unknown",
                         "National marine fisheries"="NMFS",
                         "Sea lions"="Sea lion")) %>%
  # Format common name
  mutate(spp_code=recode(spp_code,
                         "1520"="152", "154/158"="154") %>% as.numeric(.)) %>%
  left_join(spp_key %>% select(spp_code_num, comm_name), by=c("spp_code"="spp_code_num")) %>%
  mutate(comm_name=case_when(spp_code==90 ~ "Swordfish",
                             spp_code==241 ~ "Curlfin turbot",
                             spp_code==243 ~ "C-O turbot",
                             spp_code==677 ~ "Shortraker rockfish",
                             spp_code==980 ~ "Sea lion",
                             T ~ comm_name)) %>%
  # Fill in missing common names
  rowwise() %>%
  mutate(comm_name=ifelse(is.na(comm_name), paste(comm_name1, comm_name2, collapse=", "), comm_name)) %>%
  ungroup() %>%
  mutate(comm_name=recode(comm_name,
                          "NA NA"="",
                          "Blue Mackerel NA"="Blue mackerel",
                          "SB NA"="",
                          "X Unknown"="",
                          "Crab Crab,RockUnspecified"="Unspecified rock crab",
                          "Harbor Seal NA"="Harbor seal",
                          "S NA"="",
                          "Grass Back NA"="",
                          "Verde NA"="",
                          "NA Crab,RockUnspecified"="Unspecified rock crab",
                          "NA SeaUrchin,unspecified"="Unspecified sea urchin"),
         comm_name=ifelse(comm_name=="", NA, comm_name)) %>%
  # Remove empty columns
  select(-c(target_spp_final, boat_num)) %>%
  # Remove columns that I'm not interested in
  select(-c(crew_member_1:x40)) %>%
  # Arrange
  select(logbook_id, receipt_id, vessel_id, vessel_name, skipper_name, permit,
         year, date, block_type, block_id, depth_fa,
         target_spp, net_type, net_length_ft, mesh_size_in, buoy_line_depth_fa,
         soak_hr,
         spp_code, comm_name, comm_name1, comm_name2,
         status, predator, catch_n, catch_lb, everything()) %>%
  # Remove a few after checking
  select(-c(net_type_orig, net_type_final, comm_name1, comm_name2))

# Inspect
str(data2)
freeR::complete(data2)

# Inspect
range(data2$year)
range(data2$date)
table(data2$target_spp) # format this
table(data2$net_type)
table(data2$status)
table(data2$predator)


# Inspect net type
if(F){
  net_key <- data2 %>%
    select(net_type, net_type_orig, net_type_final) %>%
    unique()

  spp_key_check <- data2 %>%
    select(spp_code, comm_name, comm_name1, comm_name2) %>%
    unique()
}






