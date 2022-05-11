

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

# Summarize catch by block (all gears)
data_tot <- data_orig %>%
  # Reduce to CA halibut
  filter(species=="Halibut, California") %>%
  # Reduce to INSHORE blocks (b/c others are mistakes)
  filter(block_type=="Inshore") %>%
  # Calculate totals
  group_by(block_id) %>%
  summarize(landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>%
  ungroup() %>%
  # Calculate proportions
  mutate(landings_prop=landings_lb/sum(landings_lb),
         value_prop=value_usd/sum(value_usd)) %>%
  # Add gear
  mutate(gear_type="All gears")

# Summarize catch by block and gears
data_gear <- data_orig %>%
  # Reduce to CA halibut
  filter(species=="Halibut, California") %>%
  # Reduce to INSHORE blocks (b/c others are mistakes)
  filter(block_type=="Inshore") %>%
  # Reduce to gear types of interest
  filter(gear_type %in% c("Gillnet", "Hook & Line", "Trawl")) %>%
  # Calculate totals
  group_by(gear_type, block_id) %>%
  summarize(landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>%
  ungroup() %>%
  # Calculate proportions
  mutate(landings_prop=landings_lb/sum(landings_lb),
         value_prop=value_usd/sum(value_usd))

# Merge data
data <- bind_rows(data_tot, data_gear) %>%
  select(gear_type, block_id, everything()) %>%
  arrange(gear_type, block_id) %>%
  mutate(gear_type=recode(gear_type, "Hook & Line"="Hook and line"),
         gear_type=factor(gear_type, levels=c("All gears", "Trawl", "Gillnet", "Hook and line")))

table(data$gear_type)



# By block spatial
data_sf <- blocks %>%
  filter(block_state=="California" & block_type=="Inshore") %>%
  left_join(data, by="block_id") %>%
  filter(!is.na(landings_prop))



# Build data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_blank(),
                   legend.text=element_text(size=5),
                   legend.title=element_text(size=6),
                   strip.text=element_text(size=7),
                   plot.title=element_blank(),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# World
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf", scale="small")

# Plot data
g <- ggplot() +
  geom_sf(data=data_sf, mapping=aes(fill=landings_prop*100), lwd=0.1, color="grey30") +
  facet_wrap(~gear_type, nrow=1) +
  # Plot Point Arguello
  geom_hline(yintercept = 34.577201, lwd=0.2) +
  # Plot land
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn(name="Percent\nof landings", trans="log10",
                       breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10),
                       labels=c("0.0001%", "0.001%", "0.01%", "0.1%", "1%", "10%"),
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"), na.value=NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim=c(-125.5, -117), ylim=c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.05, 0.18),
        legend.key.size=unit(0.2, "cm"))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig4_halibut_landings_by_port.png"),
       width=6.5, height=2.75, units="in", dpi=600)

