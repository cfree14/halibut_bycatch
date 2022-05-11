

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outdir <- "data/landings/pacfin/processed"
plotdir <- "data/landings/pacfin/figures"

# Read data
data_orig <- wcfish::pacfin_all5


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Filter to CA halibut
  filter(state=="California" & comm_name %in% c("California halibut", "Nom. Calif halibut")) %>%
  # Reduce to columns of interest
  select(year, port_name, landings_mt, price_usd_lb, revenues_usd) %>%
  rename(port_complex=port_name) %>%
  # Format ports
  mutate(port_complex=gsub(" Area ports", "", port_complex),
         port_complex=recode(port_complex, "Other/Unknown California ports 2"="Other/Unknown"),
         port_complex=factor(port_complex,
                             levels=c("San Diego", "Los Angeles", "Santa Barbara", "Morro Bay", "Monterey",
                                      "San Francisco", "Bodega Bay", "Fort Bragg", "Crescent City", "Eureka", "Other/Unknown") %>% rev())) %>%
  # Convert units
  mutate(landings_kg=landings_mt*1000,
         landings_lb=measurements::conv_unit(landings_kg, "kg", "lbs")) %>%
  # Calculate proportion
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
g1 <- ggplot(data, aes(x=year, y=landings_lb/1e6, fill=port_complex)) +
  geom_bar(stat="identity", lwd=0.4) +
  # Labels
  labs(x="", y="Landings\n(millions of lbs)") +
  # Axis
  scale_x_continuous(breaks=seq(1980, 2020, 5)) +
  # Legend
  scale_fill_manual(name="Port complex", values=c("grey80", RColorBrewer::brewer.pal(10, "Set3"))) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.65))
g1

# Plot data
g2 <- ggplot(data, aes(x=year, y=landings_prop, fill=port_complex)) +
  geom_bar(stat="identity", lwd=0.4) +
  # Labels
  labs(x="", y="Proportion\nof landings") +
  # Axis
  scale_x_continuous(breaks=seq(1980, 2020, 5)) +
  # Legend
  scale_fill_manual(name="Port complex", values=c("grey80", RColorBrewer::brewer.pal(10, "Set3"))) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Arrange
g <- gridExtra::grid.arrange(g1, g2, heights=c(0.7, 0.3))

# Export
ggsave(g, filename=file.path(plotdir, "halibut_landings_pacfin.png"),
       width=6.5, height=4.5, units="in", dpi=600)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "PACFIN_1981_2020_CA_halibut_landings_by_port_complex.Rds"))
