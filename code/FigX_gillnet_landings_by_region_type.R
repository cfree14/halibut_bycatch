

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
outdir <- "data/landings_receipts/processed"

# Get data
blocks <- wcfish::blocks

# Read data
data_orig <- readRDS(file=file.path(outdir, "CDFW_2000_2020_landings_receipts.Rds"))



# Build data
################################################################################

# Build data
data <- data_orig %>%
  # Reduce to halibut
  filter(species=="Halibut, California") %>%
  # Reduce to gillnet and classify type
  filter(gear %in% c("Large mesh drift gn", "Large mesh set gn", "Small mesh drift gn", "Small mesh set gn")) %>%
  mutate(gear_type=ifelse(grepl("Large", gear), "large-mesh", "small-mesh")) %>%
  # Classify region
  left_join(blocks %>% select(block_id, block_lat_dd), by="block_id") %>%
  mutate(region=ifelse(is.na(block_lat_dd), "Unknown",
                       ifelse(block_lat_dd>=34.6, "Northern", "Southern"))) %>%
  filter(region!="Unknown") %>%
  # Classify fishery
  mutate(fishery=paste(region, gear_type)) %>%
  # Summarize halibut landings by fishery and year
  group_by(fishery, year) %>%
  summarize(landings_lb=sum(landings_lb, na.rm=T)) %>%
  ungroup() %>%
  # Calculate proportion by year
  group_by(year) %>%
  mutate(landings_prop=landings_lb/sum(landings_lb)) %>%
  ungroup() %>%
  # Order fisheries
  mutate(fishery=factor(fishery,
                        levels=c("Southern large-mesh", "Southern small-mesh",
                                 "Northern large-mesh", "Northern small-mesh") %>% rev()))


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
                   legend.key.size = unit(0.4, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=year, y=landings_prop, fill=fishery)) +
  geom_bar(stat="identity", color="black", lwd=0.2) +
  # Labels
  labs(x="Year", y="Percent of halibut landings") +
  scale_x_continuous(breaks=2015:2020) +
  scale_y_continuous(labels=scales::percent) +
  # Legend
  scale_fill_ordinal(name="Gill net fishery") +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_gillnet_landings_by_fishery.png"),
       width=5, height=3, units="in", dpi=600)







