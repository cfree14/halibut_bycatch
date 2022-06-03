

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
data_orig <- readRDS(file=file.path(outdir, "2000_2020_gillnet_logbook_data.Rds"))


# Build data
################################################################################

# Summarize catch by set
data <- data_orig %>%
  # Filter to trips of interest
  filter(target_spp=="Halibut" & catch_lb>0) %>%
  # Summarize catch by set
  group_by(set_id, set_type, year, date, date_dummy, yday, block_id, block_lat_dd, depth_fa, comm_name) %>%
  summarize(catch_lb=sum(catch_lb, na.rm=T)) %>%
  ungroup() %>%
  # Remove sets without any CA halibut
  group_by(set_id) %>%
  mutate(halibut_yn="California halibut" %in% comm_name) %>%
  ungroup() %>%
  filter(halibut_yn==T) %>%
  # Compute bycatch ratio
  group_by(set_id) %>%
  mutate(halibut_lb=catch_lb[comm_name=="California halibut"],
         ratio=catch_lb/halibut_lb) %>%
  ungroup() %>%
  # Remove halibut catch
  filter(comm_name!="California halibut")

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
g1 <- ggplot(stats, aes(x=psets, y=factor(comm_name, levels=stats$comm_name))) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Bycatch occurence\n(percent of gillnet sets)", y="", tag="A", title="CDFW gillnet logbooks") +
  # Axis
  scale_x_continuous(labels=scales::percent) +
  # Theme
  theme_bw() + theme2
g1

# Plot bycatch ratio boxplots
g2 <- ggplot(data, aes(x=ratio, y=factor(comm_name, levels=stats$comm_name))) +
  geom_boxplot(lwd=0.2, outlier.size = 0.5, color="grey30", fill="grey90") +
  # Vertical line
  geom_vline(xintercept = 1) +
  # Labels
  labs(x="Bycatch ratio\n(bycatch / halibut catch)", y="", tag="B", title=" ") +
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
ggsave(g, filename=file.path(plotdir, "FigX_gillnet_logbook_bycatch_ratio_by_species.png"),
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
  labs(x="Bycatch ratio", y="Year", title="CDFW gillnet logbooks") +
  # Axis
  scale_y_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + theme1 +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_gillnet_logbook_bycatch_ratio_over_time.png"),
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
  # USA
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  # Labels
  labs(x="", y="", title="CDFW gillnet logbooks") +
  # Legend
  scale_fill_gradientn(name="Median\nbycatch ratio",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Axis
  scale_x_continuous(breaks=seq(-120, -116, 2)) +
  scale_y_continuous(breaks=seq(32, 35, 1)) +
  # Crop
  coord_sf(xlim=c(-121, -117), ylim=c(32, 34.5)) +
  # Theme
  theme_bw() + theme_map
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_gillnet_logbook_bycatch_ratio_over_space.png"),
       width=6.5, height=5.5, units="in", dpi=600)


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
g <- ggplot(data %>% filter(comm_name%in%top20spp), aes(x=depth_fa, y=ratio)) +
  facet_wrap(~factor(comm_name, levels=top20spp), ncol=5, scales="free_x") +
  geom_smooth(fill="grey80", color="black") +
  geom_point(pch=21, color="grey30", alpha=0.5) +
  # Horizontal line
  geom_hline(yintercept=1, linetype="dotted") +
  # Labels
  labs(x="Depth (fathoms)", y="Bycatch ratio", title="CDFW gillnet logbooks") +
  # Axis
  scale_y_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + theme3
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_gillnet_logbook_bycatch_ratio_by_depth.png"),
       width=6.5, height=5.5, units="in", dpi=600)


# Bycatch ratio by day of year
g <- ggplot(data %>% filter(comm_name%in%top20spp), aes(x=date_dummy, y=ratio)) +
  facet_wrap(~factor(comm_name, levels=top20spp), ncol=5) +
  geom_smooth(fill="grey80", color="black") +
  geom_point(pch=21, color="grey30", alpha=0.5) +
  # Horizontal line
  geom_hline(yintercept=1, linetype="dotted") +
  # Labels
  labs(x="Day of year", y="Bycatch ratio") +
  # Axis
  scale_x_date(date_breaks = "2 months", date_labels =  "%b") +
  scale_y_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + theme3 +
  theme(axis.text.x=element_text(size=6))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_gillnet_logbook_bycatch_ratio_by_date.png"),
       width=6.5, height=5.5, units="in", dpi=600)





