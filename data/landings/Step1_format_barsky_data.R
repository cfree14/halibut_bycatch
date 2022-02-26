

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/landings/barsky"

# Read data
data1_orig <- readxl::read_excel(file.path(datadir, "Barsky_1990.xlsx"), sheet=1)
data2_orig <- readxl::read_excel(file.path(datadir, "Barsky_1990.xlsx"), sheet=2)

# PACFIN data
pacfin_orig <- wcfish::pacfin_all6
pacfin_hal <- pacfin_orig %>%
  # Reduce
  filter(state=="California" & comm_name %in% c("California halibut", "Nom. Calif halibut")) %>%
  # Sum by year
  group_by(year) %>%
  summarize(landings_mt=sum(landings_mt)) %>%
  # Convert units
  mutate(landings_kg=landings_mt*1000,
         landings_lb=measurements::conv_unit(landings_kg, "kg", "lbs"))


# Format data
################################################################################

# Format data
data1 <- data1_orig %>%
  # Rename
  rename(year=Year) %>%
  # Gather
  gather(key="port_complex", value="landings_lb", 2:ncol(.)) %>%
  # Order
  mutate(port_complex=factor(port_comples,
                             levels=c("San Diego", "Los Angeles", "Santa Barbara", "Monterey", "San Francisco")))
range(data1$year)

# Format data
data2 <- data2_orig %>%
  mutate(notes=ifelse(total_check!=0 & is.na(notes), "Total != CA + Mexico", notes)) %>%
  select(-total_check)
range(data2$year)

# Export data
saveRDS(data1, file=file.path(datadir, "1930_1988_CA_halibut_landings_by_port_complex.Rds"))
saveRDS(data2, file=file.path(datadir, "1916_1988_CA_halibut_landings_by_country.Rds"))


# Plot data
################################################################################

# Build data
data_tots <- data2 %>%
  select(year, california, total) %>%
  gather(key="dataset", value="landings_lb", 2:ncol(.)) %>%
  mutate(dataset=recode(dataset,
                        "california"="Barsky-California only",
                        "total"="Barsky-Total (includes Mexico)")) %>%
  rbind(pacfin_hal %>% select(year, landings_lb) %>% mutate(dataset="PACFIN")) %>%
  filter(dataset %in% c("Barsky-California only", "PACFIN"))

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data1, aes(x=year, y=landings_lb/1e6, fill=port_complex)) +
  geom_bar(stat="identity", lwd=0.4) +
  geom_line(data=data_tots, mapping=aes(x=year, y=landings_lb/1e6, linetype=dataset), inherit.aes = F) +
  # Labels
  labs(x="", y="Landings (millions of lbs)") +
  # Legend
  scale_fill_discrete(name="Port complex") +
  # Axis
  scale_x_continuous(breaks=seq(1900, 2020, 10)) +
  scale_linetype_manual(name="Dataset", values=c("dashed", "solid")) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(datadir, "halibut_landings_barsky_pacfin.png"),
       width=6.5, height=3, units="in", dpi=600)


