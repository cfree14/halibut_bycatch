

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/gillnet_logbooks/raw"
outdir <- "data/gillnet_logbooks/processed"
plotdir <- "data/gillnet_logbooks/figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CDFW_2000_2020_gillnet_logbook_data.Rds"))

# Get blocks
blocks <- wcfish::blocks

# Build data
################################################################################

# Summarize catch by set
data <- data_orig %>%
  # Convert block and mesh to numeric (multis won't work but that's okay because we're excluding)
  mutate(block_id_num=as.numeric(block_id),
         mesh_size_in_num=as.numeric(mesh_size_in)) %>%
  # Add block info
  left_join(blocks %>% select(block_id, block_lat_dd), by=c("block_id_num"="block_id")) %>%
  # Filter to REGION and MESH SIZE of interest
  filter(!is.na(block_lat_dd) & !is.na(mesh_size_in) & block_lat_dd>=34.6 & mesh_size_in<8.5) %>%
  # Filter to trips of interest
  filter(grepl("Halibut", target_spp) & catch_n>0) %>%
  # Focus on landed catch
  filter(!is.na(status) & status=="Kept") %>%
  # Remove unknown catch
  filter(!is.na(comm_name)) %>%
  # Add variables of interest
  mutate(yday=lubridate::yday(date),
         date_dummy=paste(2020, lubridate::month(date), lubridate::day(date), sep="-") %>% lubridate::ymd()) %>%
  # Summarize catch by set
  group_by(set_id, net_type, year, date, date_dummy, yday, block_id, depth_fa, comm_name) %>%
  summarize(catch_n=sum(catch_n, na.rm=T)) %>%
  ungroup() %>%
  # Remove sets without any landed CA halibut
  group_by(set_id) %>%
  mutate(halibut_yn="California halibut" %in% comm_name) %>%
  ungroup() %>%
  filter(halibut_yn==T) %>%
  # Compute bycatch ratio
  group_by(set_id) %>%
  mutate(halibut_n=catch_n[comm_name=="California halibut"],
         ratio=catch_n/halibut_n) %>%
  ungroup() %>%
  # Remove halibut catch
  filter(comm_name!="California halibut") %>%
  # Convert values to numeric
  mutate(across(depth_fa, as.numeric))

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

# Blocks
blocks <- wcfish::blocks %>%
  mutate(block_id=as.character(block_id))

# Data by block
data_block <- data %>%
  group_by(comm_name, block_id) %>%
  summarize(ratio_med=median(ratio)) %>%
  ungroup()

# Merge
data_block_sf <- blocks %>%
  left_join(data_block, by="block_id")

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
  labs(x="Catch occurrence\n(percent of gill net sets)", y="", tag="A", title="CDFW gill net logbooks\nnorthern small-mesh fishery") +
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
  labs(x="Catch ratio\n(non-halibut landings / halibut landings)", y="", tag="B", title=" \n ") +
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
ggsave(g, filename=file.path(plotdir, "FigX_gillnet_logbook_bycatch_ratio_by_species_northern_small.png"),
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
  labs(y="Catch ratio\n(non-halibut landings / halibut landings)", x="Year", title="CDFW gill net logbooks - northern small-mesh fishery") +
  # Axis
  scale_y_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + theme1 +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_gillnet_logbook_bycatch_ratio_over_time_northern_small.png"),
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
  labs(x="", y="", title="CDFW gill net logbooks - northern small-mesh fishery") +
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
ggsave(g, filename=file.path(plotdir, "FigX_gillnet_logbook_bycatch_ratio_over_space_northern_small.png"),
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

# Bycatch ratio by depth
g <- ggplot(data %>% filter(comm_name%in%top20spp & depth_fa<100), aes(x=depth_fa, y=ratio)) +
  facet_wrap(~factor(comm_name, levels=top20spp), ncol=5, scales="free_x") +
  geom_point(pch=21, color="grey60", alpha=0.5, size=0.7) +
  geom_smooth(fill="grey30", color="black", alpha=0.7, lwd=0.5) +
  # Horizontal line
  geom_hline(yintercept=1, linetype="dotted") +
  # Labels
  labs(x="Depth (fathoms)", y="Catch ratio\n(non-halibut landings / halibut landings)", title="CDFW gill net logbooks - northern small-mesh fishery") +
  # Axis
  scale_y_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + theme3
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_gillnet_logbook_bycatch_ratio_by_depth_northern_small.png"),
       width=6.5, height=5.5, units="in", dpi=600)


# Bycatch ratio by day of year
g <- ggplot(data %>% filter(comm_name%in%top20spp), aes(x=date_dummy, y=ratio)) +
  facet_wrap(~factor(comm_name, levels=top20spp), ncol=5) +
  geom_point(pch=21, color="grey60", alpha=0.5, size=0.7) +
  geom_smooth(fill="grey30", color="black", alpha=0.7, lwd=0.5) +
  # Horizontal line
  geom_hline(yintercept=1, linetype="dotted") +
  # Labels
  labs(x="Day of year", y="Catch ratio\n(non-halibut landings / halibut landings)", title="CDFW gill net logbooks - northern small-mesh fishery") +
  # Axis
  scale_x_date(date_breaks = "2 months", date_labels =  "%b") +
  scale_y_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + theme3 +
  theme(axis.text.x=element_text(size=6))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_gillnet_logbook_bycatch_ratio_by_date_northern_small.png"),
       width=6.5, height=5.5, units="in", dpi=600)






