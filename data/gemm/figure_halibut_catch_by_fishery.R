

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
# library(tidyverse)

# Directories
indir <- "data/gemm/raw"
outdir <- "data/gemm/processed"
plotdir <- "data/gemm/figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "GEMM_2002_2020_data.Rds"))


# Format data
################################################################################


pacfin <- wcfish::pacfin_all6 %>%
  # Reduce
  filter(state=="California" & comm_name %in% c("California halibut", "Nom. Calif halibut")) %>%
  # Sum by year
  group_by(port_name, year) %>%
  summarize(landings_mt=sum(landings_mt)) %>%
  # Convert units
  mutate(landings_kg=landings_mt*1000,
         landings_lb=measurements::conv_unit(landings_kg, "kg", "lbs")) %>%
  # Format ports
  mutate(port_name=gsub(" Area ports", "", port_name),
         port_name=recode(port_name,
                          "Other/Unknown California ports 2"="Unknown"),
         port_name=factor(port_name,
                          levels=c("San Diego", "Los Angeles", "Santa Barbara",
                                   "Morro Bay", "Monterey", "San Francisco",
                                   "Bodega Bay", "Fort Bragg", "Crescent City", "Eureka", "Other") %>% rev()))

# Plot
g1 <- ggplot(pacfin, aes(x=year, y=landings_mt, fill=port_name)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="Catch (mt)") +
  # Legend
  scale_fill_discrete(name="") +
  # Theme
  theme_bw()
g1


# Build data
################################################################################

# Halibut data
data <- data_orig %>%
  # CA halibut
  filter(species=="California Halibut")

# Annual landings/discards
stats1 <- data %>%
  # Summarize
  group_by(year) %>%
  summarize(landings_mt=sum(landings_mt),
            discards_mt=sum(discards_mt)) %>%
  ungroup() %>%
  # Gather
  gather(key="type", value="catch_mt", 2:ncol(.)) %>%
  mutate(type=type %>% gsub("_mt", "", .) %>% stringr::str_to_sentence())

# Plot
g1 <- ggplot(stats1, aes(x=year, y=catch_mt, fill=type)) +
  geom_bar(stat="identity") +
  # Plot PACFIN
  geom_line(data=pacfin, aes(x=year, y=landings_mt), inherit.aes = F) +
  # Labels
  labs(x="", y="Catch (mt)") +
  # Legend
  scale_fill_discrete(name="") +
  # Theme
  theme_bw()
g1








# Identify major sectors
data <- data_orig %>%
  # CA halibut
  filter(species=="California Halibut") %>%
  # Total by sector
  group_by(sector) %>%
  summarize(landings_mt=sum(landings_mt)) %>%
  ungroup()

# Major sectors
major_sectors <- c("LE CA Halibut", "OA CA Halibut", "Combined LE & OA CA Halibut",
                   "Research", "Incidental")


# Build data
data <- data_orig %>%
  # CA halibut
  filter(species=="California Halibut") %>%
  # Group sectors
  mutate(sector_simple=ifelse(!sector %in% major_sectors, "Other", sector)) %>%
  # Annual catch by sector
  group_by(sector_simple, year) %>%
  summarize(landings_mt=sum(landings_mt)) %>%
  ungroup() %>%
  # Arrange
  arrange(year, desc(landings_mt))

# Plot
g <- ggplot(data, aes(x=year, y=landings_mt, fill=sector_simple)) +
  geom_bar(stat="identity") +
  # Landings
  # Theme
  theme_bw()
g










