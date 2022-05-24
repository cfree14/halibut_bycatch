

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

# Build data
################################################################################

# Build data
data <- data_orig %>%
  # Reduce to trawl/gillnet gear
  filter(gear_type %in% c("Trawl", "Gillnet")) %>%
  # Add trip id
  mutate(trip_id=paste(receipt_id, date, gear, block_id, sep="-")) %>%
  # Add block info
  left_join(blocks_df %>% select(block_id, block_lat_dd), by="block_id") %>%
  # Separate into N/S Point Arguello (34.6°N)
  mutate(region=ifelse(is.na(block_lat_dd), "Unknown",
                         ifelse(block_lat_dd>=34.6, "North of Pt. Arguello", "South of Pt. Arguello"))) %>%
  # Reduce to trips with CA halibut
  group_by(trip_id) %>%
  mutate(halibut_yn="Halibut, California" %in% species) %>%
  ungroup() %>%
  filter(halibut_yn==T) %>%
  select(-halibut_yn) %>%
  # Remove trips in unknown region
  filter(region!="Unknown") %>%
  # Summarize catch trip id (this is summing across uses/conditions)
  group_by(trip_id, receipt_id, date, gear_type, gear, vessel_id, region, block_id, species) %>%
  summarise(landings_lb=sum(landings_lb)) %>%
  ungroup()


# Calculate by catch ratios
################################################################################

# Calculate bycatch ratios
data1 <- data %>%
  # Calculate bycatch ratios
  group_by(trip_id) %>%
  mutate(halibut_lb=landings_lb[species=="Halibut, California"]) %>%
  ungroup() %>%
  mutate(ratio=landings_lb/halibut_lb) %>%
  # Format species
  mutate(species=wcfish::convert_names(species, to="regular"))



# Plot data
################################################################################

# Plot ratios
g <- ggplot(data1 %>% filter(gear_type=="Trawl"), aes(x=ratio, y=species, fill=region)) +
  geom_boxplot() +
  # Reference line
  geom_vline(xintercept=1) +
  # Labels
  labs(x="Bycatch ratio", y="") +
  # Axis
  scale_x_continuous(trans="log10",
                     breaks=c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
                     labels=c("0.001", "0.01", "0.1", "1", "10", "100", "1000")) +
  # Legend
  scale_fill_discrete(name="Region") +
  # Theme
  theme_bw()
g









