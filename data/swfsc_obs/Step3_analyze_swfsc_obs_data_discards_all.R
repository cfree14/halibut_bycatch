

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/swfsc_obs/raw"
outdir <- "data/swfsc_obs/processed"
plotdir <- "data/swfsc_obs/figures"

# Read data
data_obs <- readRDS(file=file.path(outdir, "SWFSC_set_net_observer_data.Rds"))
data_lengths <- readRDS(file=file.path(outdir, "SWFSC_set_net_observer_length_comps.Rds"))
data_meta <- readRDS(file=file.path(outdir, "SWFSC_1990_2017_set_net_observer_trips.Rds"))

# Source
source("code/helper_functions.R")


# Build data
################################################################################

# Port key
# Determine whether ports are N/S of Pt Arguello
port_key_big <- wcfish::ports
port_key <- tibble(port=sort(unique(c(data_meta$port_depart, data_meta$port_return)))) %>%
  mutate(port_match=recode(port,
                           "Newport"="Newport Beach",
                           "Port San Luis"="Avila/Port San Luis")) %>%
  left_join(port_key_big %>% select(port, lat_dd, long_dd), by=c("port_match"="port")) %>%
  mutate(region=ifelse(lat_dd>=34.6, "Northern", "Southern"))
southern_ports <- port_key %>%
  filter(region=="Southern") %>%
  pull(port)


# Build data
data <- data_obs %>%
  # Add set meta data
  left_join(data_meta %>% select(set_id, target1_spp, season, date_haul1, port_depart, port_return,  haul_depth_fa), by="set_id") %>%
  rename(depth_fa=haul_depth_fa, date=date_haul1) %>%
  # Reduce to post-1994 data
  filter(season>=1994) %>%
  # Reduce to SOUTHERN fishery
  filter(port_depart %in% southern_ports & port_return %in% southern_ports) %>%
  # Reduce to sets targeting CA halibut
  filter(target1_spp=="Halibut, California") %>%
  # Add year
  mutate(year=lubridate::year(date),
         month=lubridate::month(date),
         yday=lubridate::yday(date)) %>%
  # Record halibut catch and reduce to sets with halibut
  group_by(set_id) %>%
  mutate(halibut_yn="California halibut" %in% comm_name) %>%
  filter(halibut_yn==T) %>%
  ungroup() %>%
  select(-halibut_yn) %>%
  # Calculate bycatch ratios
  mutate(discards_n=n_returned_alive+n_returned_dead+n_returned_unknown) %>%
  group_by(set_id) %>%
  mutate(halibut_n=n_kept[comm_name=="California halibut"],
         ratio=discards_n/halibut_n) %>%
  ungroup() %>%
  # Filter to useable surveys
  filter(halibut_n>0 & ratio>0) %>%
  # Add date dummy
  mutate(date_dummy=paste("2020", lubridate::month(date), lubridate::day(date), sep="-") %>% lubridate::ymd(.))


# Inspect
str(data)
freeR::complete(data)


# Build statistics
################################################################################

# Number of sets
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
  labs(x="Catch occurrence\n(percent of gill net sets)", y="", tag="A", title="SWFSC gill net observer data\nsouthern large-mesh fishery") +
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
  labs(x="Catch ratio\n(discards / halibut landings)", y="", tag="B", title=" \n ") +
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
ggsave(g, filename=file.path(plotdir, "FigX_swfsc_observer_discards_ratio_by_species_southern_large.png"),
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
  labs(y="Catch ratio\n(discards / halibut landings)", x="Year", title="SWFSC gill net observer data - southern large-mesh fishery") +
  # Axis
  scale_y_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  scale_x_continuous(breaks = seq(1995,2020,5), lim=c(1993,2020)) +
  # Theme
  theme_bw() + theme1 +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_swfsc_observer_discards_ratio_over_time_southern_large.png"),
       width=6.5, height=5, units="in", dpi=600)



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
  labs(x="Depth (fathoms)", y="Catch ratio\n(discards / halibut landings)", title="SWFSC gill net observer data - southern large-mesh fishery") +
  # Axis
  scale_y_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + theme3
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_swfsc_observer_discards_ratio_by_depth_southern_large.png"),
       width=6.5, height=5.5, units="in", dpi=600)


# Bycatch ratio by day of year
g <- ggplot(data %>% filter(comm_name%in%top20spp), aes(x=date_dummy, y=ratio)) +
  facet_wrap(~factor(comm_name, levels=top20spp), ncol=5) +
  geom_point(pch=21, color="grey60", alpha=0.5, size=0.7) +
  geom_smooth(fill="grey30", color="black", alpha=0.7, lwd=0.5) +
  # Horizontal line
  geom_hline(yintercept=1, linetype="dotted") +
  # Labels
  labs(x="Day of year", y="Catch ratio\n(discards / halibut landings)", title="SWFSC gill net observer data - southern large-mesh fishery") +
  # Axis
  scale_x_date(date_breaks = "2 months", date_labels =  "%b") +
  scale_y_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + theme3 +
  theme(axis.text.x=element_text(size=6))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_swfsc_observer_discards_ratio_by_date_southern_large.png"),
       width=6.5, height=5.5, units="in", dpi=600)


