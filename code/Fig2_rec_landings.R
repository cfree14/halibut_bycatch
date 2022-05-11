

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# Read data
recfin_orig <- wcfish::recfin_cte2
noaa_orig <- wcfish::noaa


# Build data
################################################################################

# Format NOAA data
noaa <- noaa_orig %>%
  filter(state=="California" & fishery=="Recreational" & comm_name=="California halibut") %>%
  mutate(mode=NA) %>%
  select(year, mode, landings_lb) %>%
  filter(year < 2005)


# Format RECFIN data (2005-2021)
recfin <- recfin_orig %>%
  filter(state=="California" & comm_name=="California halibut" & status=="Retained") %>%
  group_by(year, mode) %>%
  summarize(landings_mt=sum(catch_mt)) %>%
  ungroup() %>%
  mutate(landings_kg=landings_mt*1000,
         landings_lb=measurements::conv_unit(landings_kg, "kg", "lbs")) %>%
  select(year, mode, landings_lb)

# Merge
data <- bind_rows(noaa, recfin)



# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=10),
                   axis.title.x=element_blank(),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=10),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=year, y=landings_lb/1e6, fill=mode)) +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="", y="Recreational landings\n(millions of pounds)") +
  # Axis
  # Legend
  scale_fill_discrete(name="Mode", na.value="grey80") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig1_comm_landings.png"),
       width=6.5, height=3, units="in", dpi=600)






