

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/wcgop/raw"
outdir <- "data/wcgop/processed"
plotdir <- "data/wcgop/figures"

# Read data
data_orig <- read.csv(file=file.path(indir, "wcgop_220224_nonconf.csv"), as.is=T)


# Format data - pass 1
################################################################################

# Format data
data1 <- data_orig %>%
  # Names
  janitor::clean_names("snake") %>%
  rename(departure_datetime=d_date,
         departure_state=d_state,
         departure_port=d_port,
         return_datetime=r_date,
         return_state=r_state,
         return_port=r_port,
         set_datetime=set_date,
         set_depth_units=set_depth_um,
         set_lat_dd=set_lat,
         set_long_dd=set_long,
         haul_datetime=up_date,
         haul_time=up_time,
         haul_depth=up_depth,
         haul_depth_units=up_depth_um,
         haul_lat_dd=up_lat,
         haul_long_dd=up_long,
         soak_time_calc=haul_duration,
         soak_time_calc_units=haul_dur_um,
         soak_time_obs=avg_soak_time,
         comm_name=species,
         sci_name=scientific_name,
         pacfin_code=spid_eqv,
         catch_n=exp_sp_ct,
         catch_weight=exp_sp_wt,
         catch_weight_units=exp_spwt_um,
         landings_mt=ret_mt,
         discards_mt=dis_mt) %>%
  # Format ports
  mutate(departure_port=departure_port %>% stringr::str_to_title(),
         return_port=return_port %>% stringr::str_to_title()) %>%
  # Format catch disposition
  mutate(catch_disposition=recode(catch_disposition, "D"="discarded", "R"="retained")) %>%
  # Format datetimes
  mutate(departure_datetime=lubridate::ymd_hms(departure_datetime),
         return_datetime=lubridate::ymd_hms(return_datetime),
         set_datetime=lubridate::ymd_hms(set_datetime),
         haul_datetime=lubridate::ymd_hms(haul_datetime)) %>%
  # Extract dates
  mutate(departure_date=lubridate::date(departure_datetime),
         return_date=lubridate::date(return_datetime),
         set_date=lubridate::date(set_datetime),
         haul_date=lubridate::date(haul_datetime)) %>%
  # Extract times
  mutate(departure_time=strftime(departure_datetime, format="%r", tz="UTC"),
         return_time=strftime(return_datetime, format="%r", tz="UTC"),
         set_time=strftime(set_datetime, format="%r", tz="UTC"),
         haul_time=strftime(haul_datetime, format="%r", tz="UTC")) %>%
  # Expand catch units
  mutate(discards_kg=discards_mt*1000,
         discards_lb=measurements::conv_unit(discards_kg, "kg", "lbs"),
         landings_kg=landings_mt*1000,
         landings_lb=measurements::conv_unit(landings_kg, "kg", "lbs")) %>%
  # Add trip duration
  mutate(trip_duration_hr=difftime(return_date, departure_date, units="hours") %>% as.numeric()) %>%
  # Format observer soak time
  mutate(soak_time_obs=ifelse(soak_time_obs=="", NA, soak_time_obs),
         soak_time_obs_hr=recode(soak_time_obs,
                                 "1 to 5 mins"=mean(c(1, 5))/60,
                                 "1-2 hrs"=1.5,
                                 "15 to 30 mins"=mean(c(15, 30))/60,
                                 "2-6 hrs"=mean(c(2, 6)),
                                 "30 to 45 mins"=mean(c(30, 45))/60,
                                 "45 to 60 mins"=mean(c(45, 60))/60) %>% as.numeric()) %>%
  # Arrange
  select(year, sector,
         trip_id, trip_duration_hr,
         departure_datetime, departure_date, departure_time,
         departure_state, departure_port,
         return_datetime, return_date, return_time,
         return_state, return_port,
         haul_id, gear,
         set_datetime, set_date, set_time,
         set_depth, set_depth_units, set_lat_dd, set_long_dd,
         haul_datetime, haul_date, haul_time,
         haul_depth, haul_depth_units, haul_lat_dd, haul_long_dd,
         soak_time_obs, soak_time_obs_hr, soak_time_calc, soak_time_calc_units,
         comm_name, sci_name, pacfin_code, catch_disposition,
         catch_n, catch_weight, catch_weight_units,
         landings_lb, landings_kg, landings_mt,
         discards_lb, discards_kg, discards_mt,
         everything())

# Inspect
str(data1)

# The only columns missing data are:
# soak_time_obs, soak_time_obs_hr, catch_n, catch_weight, catch_weight_units
freeR::complete(data1)

# Date
range(data1$year)
sort(unique(data1$year))

# Sector
sort(unique(data1$sector))

# States/ports
table(data1$departure_state)
table(data1$return_state)
table(data1$return_port)
table(data1$departure_port)

# Units
table(data1$set_depth_units)
table(data1$haul_depth_units)
table(data1$soak_time_units)
table(data1$catch_weight_units)

# Soak times
table(data1$soak_time_obs)


# Format data - pass 2
################################################################################

# Format data
data2 <- data1 %>%
  # Rename
  rename(set_depth_fm=set_depth,
         haul_depth_fm=haul_depth,
         catch_lb=catch_weight,
         soak_time_calc_hr=soak_time_calc) %>%
  # Mark which effort to use
  mutate(soak_time_type=ifelse(!is.na(soak_time_obs), "Observer", "Computed"),
         soak_time_use_hr=ifelse(soak_time_type=="Observer", soak_time_obs_hr, soak_time_calc_hr)) %>%
  # Remove units columns
  select(-c(set_depth_units, haul_depth_units, soak_time_calc_units, catch_weight_units)) %>%
  # Arrange
  select(year:soak_time_calc_hr, soak_time_type, soak_time_use_hr, everything())



# Build keys
################################################################################

# Trip key
##########################

# Build trip key
trip_key <- data2 %>%
  group_by(year, trip_id, trip_duration_hr) %>%
  summarize(nsectors=n_distinct(sector),
            sectors=paste(sort(unique(sector)), collapse=", "),
            nhauls=n_distinct(haul_id)) %>%
  ungroup()

# Confirm that attributes are unique to trip id
anyDuplicated(trip_key$trip_id)

# Plot trip durations
g <- ggplot(trip_key, aes(x=trip_duration_hr, fill=sectors)) +
  geom_density(alpha=0.5) +
  # Label
  labs(x="Trip duration (hr)", y="Density") +
  # Scale
  # scale_x_continuous(trans="log10", breaks=c(1, 2.5, 5, 10, 25, 50, 100)) +
  # Theme
  theme_bw()
g

# Haul key
##########################

# Build key
haul_key <- data2 %>%
  group_by(haul_id, gear, set_datetime, haul_datetime) %>%
  summarize(n=n())

# The HAUL ID looks like its a flawed unique identifier

haul_key_me <- data2 %>%
  group_by(sector, gear, set_datetime, haul_datetime) %>%
  summarize(n=n())


# Species key
##########################

# Species key
spp_key <- data2 %>%
  group_by(comm_name, sci_name) %>%
  summarize(codes=paste(sort(unique(pacfin_code)), collapse=", ")) %>%
  ungroup()

freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$sci_name)


# Export data
################################################################################

# Export data
saveRDS(data2, file=file.path(outdir, "WCGOP_2002_2020_observer_data.Rds"))


