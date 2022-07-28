

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
data_orig <- readRDS(file=file.path(outdir, "CDFW_2000_2020_landings_receipts.Rds"))

# Blocks
blocks <- wcfish::blocks
blocks_df <- blocks %>%
  sf::st_drop_geometry()

# Source
source("code/helper_functions.R")


# Build data
################################################################################

# Build data
data1 <- data_orig %>%
  # Reduce to gears of interest
  filter(gear_type %in% c("Trawl") | gear %in% c("Small mesh set gn", "Large mesh set gn", "Small mesh drift gn", "Large mesh drift gn")) %>%
  # Reclassify gears
  mutate(gear_use=case_when(gear_type=="Trawl" ~ "Trawl",
                            gear %in% c("Small mesh set gn", "Small mesh set gn") ~ "Small mesh gillnet",
                            gear %in% c("Large mesh set gn", "Large mesh set gn") ~ "Large mesh gillnet",
                            T ~ "Other")) %>%
  # Add block info
  left_join(blocks_df %>% select(block_id, block_lat_dd), by="block_id") %>%
  # Separate into N/S Point Arguello (34.6°N)
  mutate(region=ifelse(is.na(block_lat_dd), "Unknown",
                         ifelse(block_lat_dd>=34.6, "North of Pt. Arguello", "South of Pt. Arguello"))) %>%
  # Remove trips in unknown region
  filter(region!="Unknown") %>%
  # Add trip id
  mutate(trip_id=paste(receipt_id, date, gear, block_id, sep="-")) %>%
  # Reduce to trips with CA halibut
  group_by(trip_id) %>%
  mutate(halibut_yn="Halibut, California" %in% species) %>%
  ungroup() %>%
  filter(halibut_yn==T) %>%
  select(-halibut_yn) %>%
  # Summarize catch trip id (this is summing across uses/conditions)
  group_by(trip_id, year, date, gear_use, region, block_id, species) %>%
  summarise(landings_lb=sum(landings_lb)) %>%
  ungroup() %>%
  # Calculate bycatch ratios
  group_by(trip_id) %>%
  mutate(halibut_lb=landings_lb[species=="Halibut, California"]) %>%
  ungroup() %>%
  mutate(ratio=landings_lb/halibut_lb) %>%
  # Format species
  mutate(species=wcfish::convert_names(species, to="regular")) %>%
  # Rename
  rename(set_id=trip_id, comm_name=species, gear=gear_use) %>%
  # Remove CA halibut
  filter(comm_name!="California halibut") %>%
  # Add date dummy
  mutate(date_dummy=paste(2016, lubridate::month(date), lubridate::day(date)) %>% lubridate::ymd(.))

# Step 3. Break into various fishery types
trawl_n <- data1 %>%
  filter(gear=="Trawl" & region=="North of Pt. Arguello")
trawl_s <- data1 %>%
  filter(gear=="Trawl" & region=="South of Pt. Arguello")





# Plot bycatch species
################################################################################

# Northern trawl
g <- plot_bycatch_spp(data=trawl_n, plot_title = "CDFW landings receipts:\nnorthern trawl fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_trawl_north.png"),
       width=6.5, height=7, units="in", dpi=600)

# Southern trawl
g <- plot_bycatch_spp(data=trawl_s, plot_title = "CDFW landings receipts:\nsouthern trawl fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_trawl_south.png"),
       width=6.5, height=7, units="in", dpi=600)



# Plot bycatch species over time
################################################################################

# Northern trawl
g <- plot_bycatch_spp_over_time(data=trawl_n,
                                plot_title = "CDFW landings receipts northern trawl fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_trawl_north_over_time.png"),
       width=6.5, height=6.5, units="in", dpi=600)

# Souther trawl
g <- plot_bycatch_spp_over_time(data=trawl_s,
                                plot_title = "CDFW landings receipts southern trawl fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_trawl_south_over_time.png"),
       width=6.5, height=6.5, units="in", dpi=600)


# Plot bycatch species by block
################################################################################


# Northern trawl
g <- plot_bycatch_spp_by_block(data=trawl_n,
                                plot_title = "CDFW landings receipts northern trawl fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_trawl_north_by_block.png"),
       width=5.5, height=9.5, units="in", dpi=600)

# Southern trawl
g <- plot_bycatch_spp_by_block(data=trawl_s,
                                plot_title = "CDFW landings receipts southern trawl fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_trawl_south_by_block.png"),
       width=6.5, height=5, units="in", dpi=600)


# Plot bycatch species by day of year
################################################################################

# Northern trawl
g <- plot_bycatch_spp_by_yday(data=trawl_n,
                               plot_title = "CDFW landings receipts northern trawl fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_trawl_north_by_yday.png"),
       width=6.5, height=6.5, units="in", dpi=600)

# Southern trawl
g <- plot_bycatch_spp_by_yday(data=trawl_s,
                               plot_title = "CDFW landings receipts southern trawl fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_trawl_south_by_yday.png"),
       width=6.5, height=5, units="in", dpi=600)

