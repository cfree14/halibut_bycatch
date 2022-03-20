

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "data/landings"

# Get data
noaa_orig <- readRDS("data/landings/noaa/processed/NOAA_1950_2019_CA_halibut_landings_by_sector.Rds")
barsky_orig <- readRDS("data/landings/barsky/processed/1916_1988_CA_halibut_landings_by_country.Rds")
pacfin_orig <- readRDS("data/landings/pacfin/processed/PACFIN_1981_2020_CA_halibut_landings_by_port_complex.Rds")
gemm_orig <- readRDS("data/gemm/processed/GEMM_2002_2020_data.Rds")
swfsc_orig <- wcfish::swfsc
cdfw_orig1 <- wcfish::cdfw_waters
cdfw_orig2 <- wcfish::cdfw_ports

# Build data
################################################################################

# Format NOAA
noaa <- noaa_orig %>%
  filter(fishery=="Commercial") %>%
  mutate(dataset="NOAA FOSS") %>%
  select(dataset, year, landings_lb, value_usd)

# Format Barksy
barsky <- barsky_orig %>%
  mutate(dataset="Barsky (1990)") %>%
  select(dataset, year, total) %>%
  rename(landings_lb=total)

# Format PACFIN
pacfin <- pacfin_orig %>%
  group_by(year) %>%
  summarize(landings_lb=sum(landings_lb),
            value_usd=sum(revenues_usd)) %>%
  mutate(dataset="PACFIN")

# Format GEMM
gemm <- gemm_orig %>%
  filter(species=="California Halibut" & sector_type %in% c("commercial", "tribal")) %>%
  group_by(year) %>%
  summarise(landings_mt=sum(landings_mt)) %>%
  ungroup() %>%
  mutate(dataset="GEMM",
         landings_lb=measurements::conv_unit(landings_mt*1000, "kg", "lbs")) %>%
  select(dataset, year, landings_lb)

# Format SWFSC
# Not included - we know this dataset excludes landings captured in Mexico while the others don't
swfsc <- swfsc_orig %>%
  filter(comm_name_orig %in% c("Halibut, California", "Halibut, Unspecified")) %>%
  group_by(year) %>%
  summarize(landings_lb=sum(landings_lb)) %>%
  ungroup() %>%
  mutate(dataset="SWFSC")

# Format CDFW
cdfw1 <- cdfw_orig1 %>%
  filter(comm_name %in% c("California halibut")) %>%
  group_by(year) %>%
  summarize(landings_lb=sum(landings_lb)) %>%
  ungroup() %>%
  mutate(dataset="CDFW (waters)")

# Format CDFW
cdfw2 <- cdfw_orig2 %>%
  filter(comm_name %in% c("California halibut")) %>%
  group_by(year) %>%
  summarize(landings_lb=sum(landings_lb)) %>%
  ungroup() %>%
  mutate(dataset="CDFW (ports)")

# Merge datasets
data <- bind_rows(noaa, barsky, pacfin, gemm, cdfw1, cdfw2) %>%
  complete(dataset, year) # swfsc


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=9),
                   axis.title.x=element_blank(),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=9),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=year, y=landings_lb/1e6, color=dataset)) +
  geom_line(alpha=0.5, size=1) +
  # Labels
  labs(x="", y="Commercial landings\n(millions of pounds)") +
  # Axis
  scale_y_continuous(lim=c(0, NA)) +
  scale_x_continuous(breaks=seq(1910, 2020, 10)) +
  # Legend
  scale_color_discrete(name="Dataset") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.8))
g

# Export
ggsave(g, filename=file.path(plotdir, "halibut_landings_multi_datasets.png"),
       width=6.5, height=3, units="in", dpi=600)






