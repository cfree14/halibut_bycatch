

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outdir <- "data/landings/noaa/processed"
plotdir <- "data/landings/noaa/figures"

# Get data
data_orig <- wcfish::noaa

# Species key
spp_key <- data_orig %>%
  select(comm_name_orig) %>%
  unique()

# Build data
################################################################################

# Summarize by port
data <- data_orig %>%
  # Reduce to CA halibut
  filter(state=="California" & comm_name_orig=="HALIBUT, CALIFORNIA") %>%
  # Simplify
  select(year, fishery, landings_lb, value_usd) %>%
  # Remove missing landings
  filter(!is.na(landings_lb)) %>%
  # Code sectors
  mutate(fishery=factor(fishery, levels=c("Recreational", "Commercial"))) %>%
  # Add proportion by sector
  group_by(year) %>%
  mutate(landings_prop=landings_lb/sum(landings_lb)) %>%
  ungroup()

# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=9),
                   axis.title.x=element_blank(),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=9),
                   plot.title=element_blank(),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g1 <- ggplot(data, aes(x=year, y=value_usd/1e6, fill=fishery)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="Value\n(USD millions)") +
  # Axis
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  # Legend
  scale_fill_discrete(name="Sector", drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.1, 0.8))
g1

# Plot data
g2 <- ggplot(data, aes(x=year, y=landings_lb/1e6, fill=fishery)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="Landings\n(millions of lbs)") +
  # Axis
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  # Legend
  scale_fill_discrete(name="Sector") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Plot data
g3 <- ggplot(data, aes(x=year, y=landings_prop, fill=fishery)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="Landings\n(millions of lbs)") +
  # Axis
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  # Legend
  scale_fill_discrete(name="Sector") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, heights=c(0.4, 0.4, 0.2))
g

# Export
ggsave(g, filename=file.path(plotdir, "halibut_landings_noaa.png"),
       width=6.5, height=5.5, units="in", dpi=600)

# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "NOAA_1950_2019_CA_halibut_landings_by_sector.Rds"))





