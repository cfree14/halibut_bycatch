

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/trawl_logbooks/raw"
outdir <- "data/trawl_logbooks/processed"
plotdir <- "data/trawl_logbooks/figures"

# Read data
data_orig <- readRDS(file.path(outdir, "CDFW_2000_2020_trawl_logbook_data.Rds"))
table(data_orig$target_spp)


# Build data
################################################################################

# Build data
data <- data_orig  %>%
  # Reduce to halibut trips
  filter(target_spp=="California halibut" & !is.na(catch_lb)) %>%
  # Summarize by tow
  group_by(tow_id, tow_year, tow_date, set_lat_dd, set_block_id, block_id_orig, depth_avg_fathoms, species) %>%
  summarize(catch_lb=sum(catch_lb)) %>%
  ungroup() %>%
  # Reduce to tows with CA halibut
  group_by(tow_id) %>%
  mutate(halibut_yn="California halibut" %in% species) %>%
  ungroup() %>%
  filter(halibut_yn==T) %>%
  # Calculate bycatch ratio
  group_by(tow_id) %>%
  mutate(halibut_lb=catch_lb[species=="California halibut"],
         ratio=catch_lb/halibut_lb) %>%
  ungroup() %>%
  # Rename
  rename(comm_name=species,
         date=tow_date,
         year=tow_year) %>%
  # Add date dummy
  mutate(date_dummy=paste("2020", lubridate::month(date), lubridate::day(date), sep="-") %>% lubridate::ymd(.)) %>%
  # Remove CA halibut
  filter(comm_name!="California halibut") %>%
  # Remove outliers
  filter(depth_avg_fathoms<150)

# Number of tows in dataset
ntows_tot <- n_distinct(data$tow_id)

# Determine species order
stats <- data %>%
  group_by(comm_name) %>%
  summarize(ntows=n_distinct(tow_id),
            ptows=ntows/ntows_tot,
            ratio_med=median(ratio)) %>%
  arrange(desc(ptows))

# Top 20 species
top20spp <- stats$comm_name[1:20]


# Block-level data
##########################################

# Blocks
blocks <- wcfish::blocks

# Data by block
data_block <- data %>%
  group_by(comm_name, set_block_id) %>%
  summarize(ratio_med=median(ratio)) %>%
  ungroup()

# Merge
data_block_sf <- blocks %>%
  left_join(data_block, by=c("block_id"="set_block_id"))

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
g1 <- ggplot(stats, aes(x=ptows, y=factor(comm_name, levels=stats$comm_name))) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Bycatch occurence\n(percent of trawl tows)", y="", tag="A", title="CDFW trawl logbooks") +
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
ggsave(g, filename=file.path(plotdir, "FigX_trawl_logbook_bycatch_ratio_by_species.png"),
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
ggsave(g, filename=file.path(plotdir, "FigX_trawl_logbook_bycatch_ratio_over_time.png"),
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
  labs(x="", y="", title="CDFW trawl logbooks") +
  # Legend
  scale_fill_gradientn(name="Median\nbycatch ratio",
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
ggsave(g, filename=file.path(plotdir, "FigX_trawl_logbook_bycatch_ratio_over_space.png"),
       width=6.5, height=7.5, units="in", dpi=600)


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
g <- ggplot(data %>% filter(comm_name%in%top20spp), aes(x=depth_avg_fathoms, y=ratio)) +
  facet_wrap(~factor(comm_name, levels=top20spp), ncol=5, scales="free_x") +
  geom_smooth(fill="grey80", color="black") +
  geom_point(pch=21, color="grey30", alpha=0.5) +
  # Horizontal line
  geom_hline(yintercept=1, linetype="dotted") +
  # Labels
  labs(x="Depth (fathoms)", y="Bycatch ratio", title="CDFW trawl logbooks") +
  # Axis
  scale_y_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + theme3
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_trawl_logbook_bycatch_ratio_by_depth.png"),
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
ggsave(g, filename=file.path(plotdir, "FigX_trawl_logbook_bycatch_ratio_by_date.png"),
       width=6.5, height=5.5, units="in", dpi=600)






