

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/landings_receipts/raw"
outdir <- "data/landings_receipts/processed"
plotdir <- "data/landings_receipts/figures/trawl"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CDFW_2000_2020_landings_receipts.Rds"))
table(data_orig$gear_type)

# Read trawl vessel key
permit_key <-readRDS("data/comm_permits/processed/trawl_permit_key.Rds") %>%
  mutate(permit_yn=T)

# Blocks
blocks <- wcfish::blocks
blocks_df <- blocks %>%
  sf::st_drop_geometry()



# Build data
################################################################################

# Build data
data <- data_orig %>%
  # Reduce to gear of interest
  filter(gear_type=="Trawl") %>%
  # Reduce to region of interest
  left_join(blocks_df %>% select(block_id, block_lat_dd), by="block_id") %>%
  filter(!is.na(block_lat_dd) & block_lat_dd < 34.6) %>%
  # Reduce to vessels with CA halibut permit
  left_join(permit_key, by=c("vessel_id", "date")) %>%
  filter(permit_yn==T) %>%
  # Add trip id
  mutate(trip_id=paste(receipt_id, date, gear, block_id, sep="-")) %>%
  # Reduce to trips with CA halibut
  group_by(trip_id) %>%
  mutate(halibut_yn="Halibut, California" %in% species) %>%
  ungroup() %>%
  filter(halibut_yn==T) %>%
  select(-halibut_yn) %>%
  # Summarize catch trip id (this is summing across uses/conditions)
  group_by(trip_id, year, date, gear, block_id, species) %>%
  summarise(landings_lb=sum(landings_lb)) %>%
  ungroup() %>%
  # Calculate bycatch ratios
  group_by(trip_id) %>%
  mutate(halibut_lb=landings_lb[species=="Halibut, California"]) %>%
  ungroup() %>%
  mutate(ratio=landings_lb/halibut_lb) %>%
  # Format species
  mutate(species=wcfish::convert_names(species, to="regular")) %>%
  # Rename
  rename(set_id=trip_id, comm_name=species) %>%
  # Remove CA halibut
  filter(comm_name!="California halibut") %>%
  # Add date dummy
  mutate(date_dummy=paste(2016, lubridate::month(date), lubridate::day(date)) %>% lubridate::ymd(.))

# Number of sets in dataset
nsets_tot <- n_distinct(data$set_id)

# Determine species order
stats <- data %>%
  group_by(comm_name) %>%
  summarize(nsets=n_distinct(set_id),
            psets=nsets/nsets_tot,
            ratio_med=median(ratio)) %>%
  arrange(desc(psets))

# Top 20 species
top20spp <- stats$comm_name[1:20]
top40spp <- stats$comm_name[1:40]




# Block-level data
##########################################

# Blocks
blocks <- wcfish::blocks

# Data by block
data_block <- data %>%
  group_by(comm_name, block_id) %>%
  summarize(ratio_med=median(ratio)) %>%
  ungroup()

# Merge
data_block_sf <- blocks %>%
  left_join(data_block, by="block_id") %>%
  filter(block_type=="Inshore")

# USA
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf", scale="small")


# Plot data
################################################################################

# Bycatch species
#######################################

# Theme
theme2 <-  theme(axis.text=element_text(size=6),
                 axis.title=element_text(size=7),
                 axis.title.y=element_blank(),
                 plot.title=element_text(size=8),
                 plot.tag=element_text(size=8),
                 # Gridlines
                 panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank(),
                 panel.background = element_blank(),
                 axis.line = element_line(colour = "black"),
                 # Legend
                 legend.background = element_rect(fill=alpha('blue', 0)))


# Plot bycatch ratio boxplots
g1 <- ggplot(stats %>% filter(comm_name %in% top40spp), aes(x=psets, y=factor(comm_name, levels=stats$comm_name))) +
  geom_bar(stat="identity") +
  # Reference line
  geom_hline(yintercept=20.5, linetype="dashed") +
  # Labels
  labs(x="Catch occurrence\n(percent of trawl tows)", y="", tag="A", title="CDFW landing receipts - southern trawl fishery") +
  # Axis
  scale_x_continuous(labels=scales::percent) +
  # Theme
  theme_bw() + theme2
g1

# Plot bycatch ratio boxplots
g2 <- ggplot(data %>% filter(comm_name %in% top40spp), aes(x=ratio, y=factor(comm_name, levels=top40spp))) +
  geom_boxplot(lwd=0.2, outlier.size = 0.5, color="grey30", fill="grey90") +
  # Reference line
  geom_hline(yintercept=20.5, linetype="dashed") +
  # Vertical line
  geom_vline(xintercept = 1) +
  # Labels
  labs(x="Catch ratio\n(non-halibut landings / halibut landings)", y="", tag="B", title=" ") +
  # Axis
  scale_x_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + theme2 +
  theme(axis.text.y = element_blank())
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.55, 0.45))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_bycatch_ratio_by_species_trawl_southern.png"),
       width=6.5, height=6.5, units="in", dpi=600)


# Bycatch over time
#######################################

# Theme
theme1 <- theme(axis.text=element_text(size=7),
                axis.title=element_text(size=8),
                strip.text=element_text(size=7),
                plot.title=element_text(size=8),
                # Gridlines
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                # Legend
                legend.background = element_rect(fill=alpha('blue', 0)))

# Plot ratio over time
g <- ggplot(data %>% filter(comm_name%in%top20spp), aes(x=year, y=ratio, group=year)) +
  facet_wrap(~factor(comm_name, levels=top20spp), ncol=5) +
  geom_boxplot(lwd=0.3, outlier.size = 0.5, color="grey40", fill="grey90") +
  # Reference line
  geom_hline(yintercept=1) +
  # Labels
  labs(y="Catch ratio\n(non-halibut landings / halibut landings)", x="Year", title="CDFW landings receipts - southern trawl fishery") +
  # Axis
  scale_y_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + theme1 +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_bycatch_ratio_over_time_trawl_southern.png"),
       width=6.5, height=5, units="in", dpi=600)

# Bycatch over space
#######################################

# Theme
theme_map <- theme(axis.text=element_text(size=6),
                   axis.title=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   plot.title = element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "bottom",
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g <- ggplot(data_block_sf %>% filter(comm_name%in%top20spp), aes(fill=ratio_med)) +
  facet_wrap(~factor(comm_name, levels=top20spp), ncol=5) +
  geom_sf(lwd=0.1) +
  # Reference line
  geom_hline(yintercept = 34.6, lwd=0.5) +
  # USA
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  # Labels
  labs(x="", y="", title="CDFW landings receipts - southern trawl fishery") +
  # Legend
  scale_fill_gradientn(name="Median\ncatch ratio",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Axis
  scale_x_continuous(breaks=seq(-124, -116, 4)) +
  scale_y_continuous(breaks=seq(32, 42, 2)) +
  # Crop
  coord_sf(xlim=c(-125, -116), ylim=c(32, 42)) +
  # Theme
  theme_bw() + theme_map
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_bycatch_ratio_over_space_trawl_southern.png"),
       width=5.5, height=7.5, units="in", dpi=600)


# Bycatch by driver
#######################################

# Theme
theme3 <- theme(axis.text=element_text(size=7),
                axis.title=element_text(size=8),
                strip.text=element_text(size=7),
                plot.title = element_text(size=8),
                # Gridlines
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                # Legend
                legend.background = element_rect(fill=alpha('blue', 0)))


# Bycatch ratio by day of year
g <- ggplot(data %>% filter(comm_name%in%top20spp), aes(x=date_dummy, y=ratio)) +
  facet_wrap(~factor(comm_name, levels=top20spp), ncol=5) +
  geom_point(pch=21, color="grey60", alpha=0.5, size=0.7) +
  geom_smooth(fill="grey30", color="black", alpha=0.7, lwd=0.5) +
  # Horizontal line
  geom_hline(yintercept=1, linetype="dotted") +
  # Labels
  labs(x="Day of year", y="Catch ratio\n(non-halibut landings / halibut landings)", title="CDFW landings receipts - southern trawl fishery") +
  # Axis
  scale_x_date(date_breaks = "2 months", date_labels =  "%b") +
  scale_y_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + theme3 +
  theme(axis.text.x=element_text(size=6))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_landings_receipts_bycatch_ratio_by_date_trawl_southern.png"),
       width=6.5, height=5.5, units="in", dpi=600)






# Sensitive species - none
################################################################################

data_sens <- data %>%
  filter(comm_name %in% c("Silver salmon", "Yelloweye rockfish", "Green sturgeon", "Giant sea bass"))




# Table
################################################################################


# Calculate time series
data_ts <- data_orig %>%
  # Reduce to gear of interest
  filter(gear_type=="Trawl") %>%
  # Reduce to region of interest
  left_join(blocks_df %>% select(block_id, block_lat_dd), by="block_id") %>%
  filter(!is.na(block_lat_dd) & block_lat_dd < 34.6) %>%
  # Reduce to vessels with CA halibut permit
  left_join(permit_key, by=c("vessel_id", "date")) %>%
  filter(permit_yn==T) %>%
  # Add trip id
  mutate(trip_id=paste(receipt_id, date, gear, block_id, sep="-")) %>%
  # Reduce to trips with CA halibut
  group_by(trip_id) %>%
  mutate(halibut_yn="Halibut, California" %in% species) %>%
  ungroup() %>%
  filter(halibut_yn==T) %>%
  select(-halibut_yn) %>%
  # Summarize by year
  group_by(year) %>%
  summarise(halibut_lb=sum(landings_lb[species=="Halibut, California"], na.rm=T),
            other_lb=sum(landings_lb[species!="Halibut, California"], na.rm=T)) %>%
  ungroup()

# Export
write.csv(data_ts, file=file.path(plotdir, "TableX_landing_receipt_time_series_trawl_southern.csv"), row.names=F)


