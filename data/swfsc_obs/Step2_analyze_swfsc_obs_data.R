

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

# Build data
data <- data_obs %>%
  # Add set meta data
  left_join(data_meta %>% select(set_id, target1_spp, date_haul1, haul_lat_dd, haul_long_dd, haul_depth_fa), by="set_id") %>%
  rename(lat_dd=haul_lat_dd, long_dd=haul_long_dd, depth_fa=haul_depth_fa, date=date_haul1) %>%
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
  # Calculate bycatch ratios
  group_by(set_id) %>%
  mutate(halibut_n=n_caught[comm_name=="California halibut"],
         ratio=n_caught/halibut_n) %>%
  ungroup() %>%
  # Remove halibut
  filter(comm_name!="California halibut") %>%
  # Add date dummy
  mutate(date_dummy=paste("2020", lubridate::month(date), lubridate::day(date), sep="-") %>% lubridate::ymd(.))


# Inspect
str(data)
freeR::complete(data)


# Build statistics
################################################################################

# Number of sets
nsets_tot <- n_distinct(data$set_id)

# Build stats
stats <- data %>%
  group_by(comm_name) %>%
  summarize(nsets=n_distinct(set_id),
            psets=nsets/nsets_tot) %>%
  ungroup() %>%
  arrange(desc(psets))

# Top 20 species
top20spp <- stats$comm_name[1:20]


# Plot data
################################################################################

# By species
g <- plot_bycatch_spp(stats=stats, plot_title="SWFSC gillnet observer data")
ggsave(g, filename=file.path(plotdir, "FigX_swfc_gillnet_obs_bycatch_ratio_by_species.png"),
       width=6.5, height=10.5, units="in", dpi=600)

# By species over time
g <- plot_bycatch_spp_over_time(stats=stats, top20spp=top20spp, years=seq(1990,2020, 5),
                                plot_title="SWFSC gillnet observer data")
g






