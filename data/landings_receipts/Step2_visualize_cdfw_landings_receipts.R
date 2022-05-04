

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/landings_receipts/raw"
outdir <- "data/landings_receipts/processed"
plotdir <- "data/landings_receipts/figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CDFW_2000_2020_landings_receipts.Rds"))

# Blocks
blocks <- wcfish::blocks


# Build data
################################################################################

# Format data
data <- data_orig %>%
  # Reduce to CA halibut
  filter(species=="Halibut, California") %>%
  # Order ports
  mutate(port_complex=factor(port_complex, levels=c("Unknown", "Eureka", "Fort Bragg",  "Bodega Bay", "San Francisco", "Monterey",
                                                    "Morro Bay", "Santa Barbara", "Los Angeles", "San Diego"))) %>%
  # Format gear type
  mutate(gear_type_use=ifelse(gear_type %in% c("Gillnet", "Hook & Line", "Trawl"), gear_type, "Other"),
         gear_type_use=recode(gear_type_use,
                              "Hook & Line"="Hook and line",
                              "Gillnet"="Gill net"),
         gear_type_use=factor(gear_type_use, levels=c("Trawl", "Gill net", "Hook and line", "Other") %>% rev()))

# By port complex
data_port <- data %>%
  # Calculate totals
  group_by(port_complex, year) %>%
  summarize(landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>%
  ungroup() %>%
  # Calculate proportions
  group_by(year) %>%
  mutate(landings_prop=landings_lb/sum(landings_lb),
            value_prop=value_usd/sum(value_usd)) %>%
  ungroup()

# By gear
data_gear <- data %>%
  # Calculate totals
  group_by(gear_type_use, year) %>%
  summarize(landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>%
  ungroup() %>%
  # Calculate proportions
  group_by(year) %>%
  mutate(landings_prop=landings_lb/sum(landings_lb),
         value_prop=value_usd/sum(value_usd)) %>%
  ungroup()

# By use
# VIRTUALLY ALL FOR HUMAN FOOD (NOT CANNED)
data_use <- data %>%
  # Calculate totals
  group_by(use, year) %>%
  summarize(landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>%
  ungroup() %>%
  # Calculate proportions
  group_by(year) %>%
  mutate(landings_prop=landings_lb/sum(landings_lb),
         value_prop=value_usd/sum(value_usd)) %>%
  ungroup()

# By month
data_month <- data %>%
  # Calculate totals
  group_by(year, month) %>%
  summarize(landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>%
  ungroup() %>%
  # Calculate proportions
  group_by(year) %>%
  mutate(landings_prop=landings_lb/sum(landings_lb),
         value_prop=value_usd/sum(value_usd)) %>%
  ungroup()

# By block
data_block <- data %>%
  # Reduce to INSHORE only
  # Because MIDSHORE AND OFFSHORE appear to be grab bags for inshore blocks
  filter(block_type=="Inshore") %>%
  # Calculate totals
  group_by(block_id) %>%
  summarize(landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>%
  ungroup() %>%
  # Calculate proportions
  mutate(landings_prop=landings_lb/sum(landings_lb),
         value_prop=value_usd/sum(value_usd))

# By block spatial
data_block_sf <- blocks %>%
  filter(block_state=="California" & block_type=="Inshore") %>%
  left_join(data_block, by="block_id")



# Landings by port complex
################################################################################

# Theme
theme1 <- theme(axis.text=element_text(size=6),
                axis.text.y=element_text(angle = 90, hjust = 0.5),
                axis.title=element_text(size=7),
                axis.title.x=element_blank(),
                legend.text=element_text(size=6),
                legend.title=element_text(size=7),
                plot.title=element_blank(),
                plot.tag=element_text(size=7),
                # Gridlines
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                # Legend
                legend.key.size = unit(0.3, "cm"),
                legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g1 <- ggplot(data_port, aes(x=year, y=landings_lb/1e3, fill=port_complex)) +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="", y="Landings\n(1000s of pounds)", tag="A") +
  # Legend
  scale_fill_discrete(name="Port complex\n(north to south)") +
  # Theme
  theme_bw() + theme1
g1

# Plot
g2 <- ggplot(data_port, aes(x=year, y=landings_prop, fill=port_complex)) +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="", y=" \nPropotion of landings", tag="B") +
  # Legend
  scale_fill_discrete(name="Port complex\n(north to south)") +
  # Theme
  theme_bw() + theme1
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, ncol=1, heights=c(0.65, 0.35))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_halibut_landings_by_port.png"),
       width=6.5, height=4.5, units="in", dpi=600)


# Landings by gear type
################################################################################

# Plot
g1 <- ggplot(data_gear, aes(x=year, y=landings_lb/1e3, fill=gear_type_use)) +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="", y="Landings\n(1000s of pounds)", tag="A") +
  # Legend
  scale_fill_discrete(name="Gear type") +
  # Theme
  theme_bw() + theme1
g1

# Plot
g2 <- ggplot(data_gear, aes(x=year, y=landings_prop, fill=gear_type_use)) +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="", y=" \nPropotion of landings", tag="B") +
  # Legend
  scale_fill_discrete(name="Gear") +
  # Theme
  theme_bw() + theme1
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, ncol=1, heights=c(0.65, 0.35))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_halibut_landings_by_gear.png"),
       width=6.5, height=4.5, units="in", dpi=600)



# Landings by month
################################################################################

# Plot data
g <- ggplot(data_month, aes(x=month, y=landings_prop, color=year, group=year)) +
  geom_line() +
  # Labels
  labs(x="Month", y="Proportion of annual landings") +
  # Axis
  scale_x_continuous(breaks=1:12) +
  # Theme
  theme_bw()
g


# Landings by block
################################################################################

# World
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf", scale="small")

# Plot data
g <- ggplot() +
  geom_sf(data=data_block_sf, mapping=aes(fill=landings_prop*100), lwd=0.2, color="grey30") +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn(name="Percent\nof landings", trans="log2",
                       breaks=c(0.0001, 0.001, 0.01, 0.1, 1, 10),
                       labels=c("0.0001%", "0.001%", "0.01%", "0.1%", "1%", "10%"),
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"), na.value=NA) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim=c(-125.5, -117), ylim=c(32.5, 42)) +
  # Theme
  theme_bw()
g




