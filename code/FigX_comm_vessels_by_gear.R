

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "data/enhanced_status_report"

# Get data
data_orig <- readxl::read_excel(file.path(datadir, "CDFW_ca_halibut_participation.xlsx"))


# Build data
################################################################################

# Format data
data <- data_orig %>%
  # Gather
  gather(key="fishery", value="nvessels", 2:ncol(.)) %>%
  mutate(fishery=recode(fishery,
                        "gillnet"="Gill net",
                        "trawl"="Trawl",
                        "hook_and_line"="Hook and line"))


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
g1 <- ggplot(data, aes(x=year, y=nvessels, color=fishery)) +
  geom_line() +
  # Labels
  labs(x="Year", y="Number of\ncommercial fishing vessels") +
  # Legend
  scale_color_discrete(name="Gear") +
  # Theme
  theme_bw() + my_theme
g1

# Export
ggsave(g1, filename=file.path(plotdir, "FigX_comm_vessels_by_gear.png"),
       width=6.5, height=2.5, units="in", dpi=600)






