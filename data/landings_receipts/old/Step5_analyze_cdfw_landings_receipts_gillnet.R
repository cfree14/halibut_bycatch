

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
gillnet_n <- data1 %>%
  filter(grepl("gillnet", gear) & region=="North of Pt. Arguello")
gillnet_s <- data1 %>%
  filter(grepl("gillnet", gear) & region=="South of Pt. Arguello")
gillnet_lm <- data1 %>%
  filter(gear=="Large mesh gillnet")
gillnet_sm <- data1 %>%
  filter(gear=="Small mesh gillnet")

# Plot bycatch species
################################################################################

# Northern gillnet
g <- plot_bycatch_spp(data=gillnet_n, plot_title = "CDFW landings receipts:\nnorthern gillnet fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_gillnet_north.png"),
       width=6.5, height=2.5, units="in", dpi=600)

# Southern gillnet
g <- plot_bycatch_spp(data=gillnet_s, plot_title = "CDFW landings receipts:\nsouthern gillnet fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_gillnet_south.png"),
       width=6.5, height=6.5, units="in", dpi=600)

# Small mesh gillnet
g <- plot_bycatch_spp(data=gillnet_sm, plot_title = "CDFW landings receipts:\nsmall-mesh gillnet fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_gillnet_small_mesh.png"),
       width=6.5, height=5.5, units="in", dpi=600)

# Large mesh gillnet
g <- plot_bycatch_spp(data=gillnet_lm, plot_title = "CDFW landings receipts:\nlarge-mesh gillnet fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_gillnet_large_mesh.png"),
       width=6.5, height=6.5, units="in", dpi=600)



# Plot bycatch species over time
################################################################################

# Northern gillnet
g <- plot_bycatch_spp_over_time(data=gillnet_n,
                                plot_title = "CDFW landings receipts northern gillnet fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_gillnet_north_over_time.png"),
       width=6.5, height=2.5, units="in", dpi=600)

# Souther gillnet
g <- plot_bycatch_spp_over_time(data=gillnet_s,
                                plot_title = "CDFW landings receipts southern gillnet fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_gillnet_south_over_time.png"),
       width=6.5, height=6.5, units="in", dpi=600)

# Small mesh
g <- plot_bycatch_spp_over_time(data=gillnet_sm,
                                plot_title = "CDFW landings receipts small-mesh gillnet fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_gillnet_small_over_time.png"),
       width=6.5, height=6.5, units="in", dpi=600)

# Large mesh
g <- plot_bycatch_spp_over_time(data=gillnet_lm,
                                plot_title = "CDFW landings receipts large-mesh gillnet fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_gillnet_large_over_time.png"),
       width=6.5, height=6.5, units="in", dpi=600)


# Plot bycatch species by block
################################################################################

# Northern gillnet
g <- plot_bycatch_spp_by_block(data=gillnet_n,
                                plot_title = "CDFW landings receipts northern gillnet fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_gillnet_north_by_block.png"),
       width=5.5, height=1.5, units="in", dpi=600)

# Southern gillnet
g <- plot_bycatch_spp_by_block(data=gillnet_s,
                                plot_title = "CDFW landings receipts southern gillnet fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_gillnet_south_by_block.png"),
       width=6.5, height=5, units="in", dpi=600)

# Large gillnet
g <- plot_bycatch_spp_by_block(data=gillnet_lm,
                               plot_title = "CDFW landings receipts large-mesh gillnet fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_gillnet_large_by_block.png"),
       width=6.5, height=5.5, units="in", dpi=600)

# Small gillnet
g <- plot_bycatch_spp_by_block(data=gillnet_sm,
                               plot_title = "CDFW landings receipts small-mesh gillnet fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_gillnet_small_by_block.png"),
       width=6.5, height=5.5, units="in", dpi=600)


# Plot bycatch species by day of year
################################################################################

# Northern gillnet
g <- plot_bycatch_spp_by_yday(data=gillnet_n,
                               plot_title = "CDFW landings receipts northern gillnet fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_gillnet_north_by_yday.png"),
       width=6.5, height=6.5, units="in", dpi=600)

# Southern gillnet
g <- plot_bycatch_spp_by_yday(data=gillnet_s,
                               plot_title = "CDFW landings receipts southern gillnet fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_gillnet_south_by_yday.png"),
       width=6.5, height=5, units="in", dpi=600)

# Large gillnet
g <- plot_bycatch_spp_by_yday(data=gillnet_lm,
                              plot_title = "CDFW landings receipts large-mesh gillnet fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_gillnet_large_by_yday.png"),
       width=6.5, height=6.5, units="in", dpi=600)

# Small gillnet
g <- plot_bycatch_spp_by_yday(data=gillnet_sm,
                              plot_title = "CDFW landings receipts small-mesh gillnet fishery")
g
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_gillnet_small_by_yday.png"),
       width=6.5, height=5, units="in", dpi=600)

