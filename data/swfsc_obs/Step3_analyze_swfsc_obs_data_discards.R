

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

# Build data
data <- data_obs %>%
  # Add set meta data
  left_join(data_meta %>% select(set_id, target1_spp, date_haul1, haul_lat_dd, haul_long_dd, haul_depth_fa), by="set_id") %>%
  rename(lat_dd=haul_lat_dd, long_dd=haul_long_dd, depth_fa=haul_depth_fa, date=date_haul1) %>%
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

# Build stats
stats <- data %>%
  group_by(comm_name) %>%
  summarize(nsets=n_distinct(set_id),
            psets=nsets/nsets_tot) %>%
  ungroup() %>%
  arrange(desc(psets))

# Top 20 species
top20spp <- stats$comm_name[1:20]



# Plot data - bycatch species
################################################################################

# Theme
theme1 <-  theme(axis.text=element_text(size=5),
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

# Plot data
g1 <- ggplot(stats, aes(y=factor(comm_name, levels=comm_name), x=psets)) +
  # Boxplots
  geom_bar(stat="identity") +
  # Labels
  labs(x="Bycatch occurence\n(percent of gillnet sets)", y="", tag="A", title="SWFSC gillnet observer data") +
  # Axis
  scale_x_continuous(labels=scales::percent) +
  # Theme
  theme_bw() + theme1
g1

# Plot data
g2 <- ggplot(data, aes(y=factor(comm_name, levels=stats$comm_name), x=ratio)) +
  # Boxplots
  geom_boxplot(fill="grey90", lwd=0.2, outlier.size = 0.3) +
  # Reference line
  geom_vline(xintercept = 1) +
  # Labels
  labs(x="Bycatch ratio\n(bycatch / halibut catch)", y="", tag="B", title="  ") +
  # Axis
  scale_x_continuous(trans="log10",
                     breaks=c(0.1, 1, 10, 100),
                     labels=c("0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + theme1 +
  theme(axis.text.y=element_blank())
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.55, 0.45))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_swfsc_gillnet_obs_bycatch_ratio_by_species_discards.png"),
       width=6.5, height=8.5, units="in", dpi=600)



# Plot data - bycatch over time
################################################################################

# Theme
theme2 <- theme(axis.text=element_text(size=7),
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
  labs(y="Bycatch ratio\n(bycatch / halibut catch)", x="Year", title="SWFSC gillnet observer data") +
  # Axis
  scale_y_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + theme2 +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_swfsc_gillnet_obs_bycatch_ratio_over_time_discards.png"),
       width=6.5, height=5, units="in", dpi=600)

# Plot data - bycatch by driver
################################################################################

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
  geom_point(pch=21, color="grey60", alpha=0.5, size=0.7) +
  geom_smooth(fill="grey30", color="black", alpha=0.7, lwd=0.5) +
  # Horizontal line
  geom_hline(yintercept=1, linetype="dotted") +
  # Labels
  labs(x="Depth (fathoms)", y="Bycatch ratio\n(bycatch / halibut catch)", title="SWFSC gillnet observer data") +
  # Axis
  scale_y_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + theme3
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_swfsc_gillnet_obs_bycatch_ratio_by_depth_discards.png"),
       width=6.5, height=5.5, units="in", dpi=600)


# Bycatch ratio by day of year
g <- ggplot(data %>% filter(comm_name%in%top20spp), aes(x=date_dummy, y=ratio)) +
  facet_wrap(~factor(comm_name, levels=top20spp), ncol=5) +
  geom_point(pch=21, color="grey60", alpha=0.5, size=0.7) +
  geom_smooth(fill="grey30", color="black", alpha=0.7, lwd=0.5) +
  # Horizontal line
  geom_hline(yintercept=1, linetype="dotted") +
  # Labels
  labs(x="Day of year", y="Bycatch ratio\n(bycatch / halibut catch)", title="SWFSC gillnet observer data") +
  # Axis
  scale_x_date(date_breaks = "2 months", date_labels =  "%b") +
  scale_y_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + theme3 +
  theme(axis.text.x=element_text(size=6))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_swfsc_gillnet_obs_bycatch_ratio_by_date_discards.png"),
       width=6.5, height=5.5, units="in", dpi=600)






