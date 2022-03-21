

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/swfsc_obs/raw"
outdir <- "data/swfsc_obs/processed"
plotdir <- "data/swfsc_obs/figures"

# Read data
list.files(indir)
data_orig1 <- read.csv(file.path(indir, "obs_setnet_catch.csv"), as.is=T)
data_orig2 <- read.csv(file.path(indir, "obs_setnet_measurement.csv"), as.is=T)
data_orig3 <- read.csv(file.path(indir, "SN_trip_SET_1990_2017.csv"), as.is=T)


# Format data 1
################################################################################

# Format data 1
data1 <- data_orig1 %>%
  # Rename
  janitor::clean_names() %>%
  rename(trip_id=observer_trip_number,
         set_id=set_number,
         spp_code=catch_species_code,
         comm_name=species_common_name,
         n_caught=total_catch_count,
         n_kept=total_kept_count,
         n_returned_alive=returned_alive_count,
         n_returned_dead=returned_dead_count,
         n_returned_unknown=returned_unknown_count,
         n_damaged_mammals=damage_by_marine_mammals_count,
         n_damaged=damage_total_count,
         tag_yn=was_tag_present,
         mammal_damage_yn=was_damaged_by_marine_mammals,
         condition=condition_description)

# Inspect
str(data1)

# Inspect
sort(unique(data1$tag_yn))
sort(unique(data1$mammal_damage_yn))
sort(unique(data1$condition_code))
sort(unique(data1$condition))
sort(unique(data1$sex))

# Export data
saveRDS(data1, file=file.path(outdir, "SWFSC_set_net_observer_data.Rds"))


# Format data 2
################################################################################

# Format data 2
data2 <- data_orig2 %>%
  # Rename
  janitor::clean_names() %>%
  rename(trip_id=observer_trip_number,
         set_id=set_number,
         spp_code=catch_species_code,
         comm_name=species_common_name,
         length_cm=measurement) %>%
  # Remove useless columns
  select(-c(measurement_units, condition))

# Inspect
str(data2)
freeR::complete(data2)

# Inspect columns
# sort(unique(data2$condition)) ## ALWAYS EMPTY
# sort(unique(data2$measurement_units)) ## ALWAYS EMPTY
sort(unique(data2$disposition))

# Inspect species key
spp_key <- data2 %>%
  select(spp_code, comm_name) %>%
  unique()

# Plot lengths
# g <- ggplot(data2, aes(x=length_cm)) +
#   facet_wrap(~comm_name, ncol=8, scales = "free") +
#   geom_density() +
#   # Labels
#   labs(x="Length (cm)", y="Density") +
#   # Theme
#   theme_bw()
# g

# Export data
saveRDS(data2, file=file.path(outdir, "SWFSC_set_net_observer_length_comps.Rds"))


# Format data 3
################################################################################

# Format data 3
data3 <- data_orig3 %>%
  # Rename
  janitor::clean_names() %>%
  rename(trip_id=observer_trip_number,
         vessel=vessel_names,
         vessel_plate=vessel_plates,
         vessel_permit=vessel_permits,
         port_depart=departure_port_name,
         port_return=return_port_name,
         date_haul1=haul_date,
         set_id=set_number,
         soak_hr=soak_time_hrs,
         soak_hr_est=est_soak_time,
         obs_perc=percent_observed,
         target1_spp_code=primary_target_species_code,
         target1_spp=primary_target_species_name,
         target2_spp_code=secondary_target_species_code,
         target2_spp=secondary_target_species_name,
         date_haul2=begin_haul_date_time,
         haul_temp_device=begin_haul_temp_device,
         haul_pos_code=begin_haul_position_code,
         haul_lat=begin_haul_latitude,
         haul_long=begin_haul_longitude,
         haul_sst_f=begin_haul_surface_temp,
         haul_beauf=begin_haul_beaufort_number) %>%
  # Format ports
  mutate(port_depart=stringr::str_to_title(port_depart),
         port_return=stringr::str_to_title(port_return)) %>%
  # Format dates
  mutate(date_haul1=lubridate::dmy(date_haul1),
         date_haul2=lubridate::dmy(date_haul2))

# Inspect
str(data3)
freeR::complete(data3)

# Inspect
table(data3$port_depart)
table(data3$port_return)

# Export data
saveRDS(data3, file=file.path(outdir, "SWFSC_1990_2017_set_net_observer_trips.Rds"))



