

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
spp_key <- readRDS(file=file.path(outdir, "SWFSC_observer_program_spp_key.Rds"))


# Sensitive species
################################################################################

spp_key1 <- spp_key %>%
  filter(category %in% c("Seabirds", "Marine mammals", "Sea tutles") | comm_name %in% c("Silver salmon", "Green sturgeon", "Yelloweye rockfish"))


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
data <- data_obs %>%
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
  # Reduce to sensitive species
  filter(comm_name %in% spp_key1$comm_name) %>%
  # Summarize number
  group_by(comm_name, sci_name) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  # Add type
  left_join(spp_key1 %>% select(comm_name, category)) %>%
  # Arrange
  arrange(desc(n)) %>%
  mutate(comm_name=factor(comm_name, levels=comm_name))

# Build data
################################################################################

theme2 <-  theme(axis.text=element_text(size=6),
                 axis.title=element_text(size=7),
                 axis.title.y=element_blank(),
                 plot.title=element_text(size=8),
                 plot.tag=element_text(size=8),
                 # Gridlines
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 axis.line = element_line(colour = "black"),
                 # Legend
                 legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g <- ggplot(data, aes(x=n, y=comm_name, fill=category)) +
  geom_bar(stat="identity") +
  # Add count
  geom_text(mapping=aes(x=n, y=comm_name, label=n, hjust=-0.4), size=2) +
  # Labels
  labs(x="Number of sensitive species caught\nfrom 1994-2017", y="") +
  # Legend
  scale_fill_ordinal(name="") +
  # Theme
  theme_bw() +
  theme(legend.position=c(0.8, 0.8)) + theme2
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_swfsc_observer_sensitive_species_southern_large.png"),
       width=6.5, height=2.5, units="in", dpi=600)

