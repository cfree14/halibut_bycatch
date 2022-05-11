

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# Get data
data_orig <- wcfish::pacfin_all2


# Build data
################################################################################

# Format data
data <- data_orig %>%
  # Reduce to CA halibut
  filter(state=="California" & comm_name %in% c("Nom. Calif halibut", "California halibut") & year <= 2021) %>%
  # Recode gears
  mutate(gear=recode(gear,
                     "Net"="Gillnet"),
         gear=ifelse(gear %in% c("Trawl", "Gillnet", "Hook and line"), gear, "Other gear"),
         gear=factor(gear, levels=c("Gillnet", "Trawl", "Hook and line", "Other gear") %>% rev())) %>%
  # Summarize by gear
  group_by(gear, year) %>%
  summarize(landings_mt=sum(catch_mt)) %>%
  ungroup() %>%
  # Convert landings
  mutate(landings_kg=landings_mt*1000,
         landings_lb=measurements::conv_unit(landings_kg, "kg", "lbs")) %>%
  # Add prop
  group_by(year) %>%
  mutate(landings_prop=landings_lb/sum(landings_lb)) %>%
  ungroup()


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=10),
                   axis.title.x=element_blank(),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(data, aes(x=year, y=landings_lb/1e6, fill=gear)) +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="", y="Commercial landings\n(millions of pounds)") +
  # Axis
  # Legend
  scale_fill_discrete(name="Gear", na.value="grey80") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.8))
g1

# Plot data
g2 <- ggplot(data, aes(x=year, y=landings_prop, fill=gear)) +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="", y=" \nProportion of landinds") +
  # Axis
  # Legend
  scale_fill_discrete(name="Gear", na.value="grey80") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, heights=c(0.7, 0.3))

# Export
ggsave(g, filename=file.path(plotdir, "Fig3_comm_landings_by_gear.png"),
       width=6.5, height=4.5, units="in", dpi=600)






