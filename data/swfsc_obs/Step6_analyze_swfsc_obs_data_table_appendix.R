

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
data_obs <- readRDS(file=file.path(outdir, "SWFSC_set_net_observer_data.Rds"))
data_lengths <- readRDS(file=file.path(outdir, "SWFSC_set_net_observer_length_comps.Rds"))
data_meta <- readRDS(file=file.path(outdir, "SWFSC_1990_2017_set_net_observer_trips.Rds"))

# Source
source("code/helper_functions.R")


# Build data
################################################################################

# Port key
# Determine whether ports are N/S of Pt Arguello
port_key_big <- wcfish::ports
port_key <- tibble(port=sort(unique(c(data_meta$port_depart, data_meta$port_return)))) %>%
  mutate(port_match=recode(port,
                           "Newport"="Newport Beach",
                           "Port San Luis"="Avila/Port San Luis")) %>%
  left_join(port_key_big %>% select(port, lat_dd, long_dd), by=c("port_match"="port")) %>%
  mutate(region=ifelse(lat_dd>=34.6, "Northern", "Southern"))
southern_ports <- port_key %>%
  filter(region=="Southern") %>%
  pull(port)


# Build data
data_ts <- data_obs %>%
  # Add set meta data
  left_join(data_meta %>% select(set_id, target1_spp, season, date_haul1, port_depart, port_return,  haul_depth_fa), by="set_id") %>%
  rename(depth_fa=haul_depth_fa, date=date_haul1) %>%
  # Reduce to post-1994 data
  filter(season>=1994) %>%
  # Reduce to SOUTHERN fishery
  filter(port_depart %in% southern_ports & port_return %in% southern_ports) %>%
  # Reduce to sets targeting CA halibut
  filter(target1_spp=="Halibut, California") %>%
  # Add year
  mutate(year=lubridate::year(date),
         month=lubridate::month(date),
         yday=lubridate::yday(date)) %>%
  # Record halibut catch and reduce to sets with halibut
  group_by(set_id) %>%
  mutate(halibut_yn="California halibut" %in% comm_name) %>%
  filter(halibut_yn==T) %>%
  ungroup() %>%
  select(-halibut_yn) %>%
  # Summarize
  mutate(type=ifelse(comm_name=="California halibut", "Halibut", "Non-Halibut")) %>%
  group_by(year, type) %>%
  summarize(n_kept=sum(n_kept, na.rm=T),
            n_returned_alive=sum(n_returned_alive, na.rm=T),
            n_returned_dead=sum(n_returned_dead, na.rm=T),
            n_returned_unknown=sum(n_returned_unknown, na.rm=T)) %>%
  ungroup() %>%
  mutate(n_returned=n_returned_alive+n_returned_dead+n_returned_unknown) %>%
  # Gather then spread
  gather(key="catch_type", value="catch_n", 3:ncol(.)) %>%
  mutate(catg=paste(type, catch_type, sep="-"),
         catg=recode_factor(catg,
                            "Halibut-n_kept"="halibut_landings_n",
                            "Non-Halibut-n_kept"="nonhalibut_landings_n",
                            "Halibut-n_returned"="halibut_discards_n",
                            "Non-Halibut-n_returned"="nonhalibut_discards_n",
                            "Halibut-n_returned_alive"="halibut_discards_alive_n",
                            "Non-Halibut-n_returned_alive"="nonhalibut_discards_alive_n",
                            "Halibut-n_returned_dead"="halibut_discards_dead_n",
                            "Non-Halibut-n_returned_dead"="nonhalibut_discards_dead_n",
                            "Halibut-n_returned_unknown"="halibut_discards_unknown_n",
                            "Non-Halibut-n_returned_unknown"="nonhalibut_discards_unknown_n")) %>%
  select(-c(type, catch_type)) %>%
  spread(key=catg, value=catch_n)

# Export
write.csv(data_ts, file=file.path(plotdir, "TableX_gillnet_observer_time_series_southern_large_mesh.csv"), row.names=F)




