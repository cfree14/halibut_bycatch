

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/cdfw_obs/raw"
outdir <- "data/cdfw_obs/processed"
plotdir <- "data/cdfw_obs/figures"


# Read data
list.files(outdir)
metadata <- readRDS(file.path(outdir, "CDFW_1983_1989_gillnet_observer_set_info.Rds"))
data_orig <- readRDS(file.path(outdir, "CDFW_1983_1989_gillnet_observer_data.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>%
  # Add set meta data
  left_join(metadata %>% select(set_id, lat_dd, long_dd, bottom_depth_fa, net_orientation, net_type, target_spp), by="set_id") %>%
  # Reduce to sets targeting CA halibut
  filter(target_spp=="California halibut") %>%
  # Add year
  mutate(year=lubridate::year(date),
         month=lubridate::month(date),
         yday=lubridate::yday(date)) %>%
  # Summarize by set and species
  # This is b/c some sets will have multiple rows for a species...
  group_by(year, month, yday, date,
         vessel_id, set_num, set_id,
         net_type, net_orientation, bottom_depth_fa, lat_dd, long_dd,
         spp_code_chr, comm_name) %>%
  summarize(catch_n=sum(n_caught, na.rm=T)) %>%
  ungroup() %>%
  # Halibut bycatch
  group_by(set_id) %>%
  mutate(halibut_yn="California halibut" %in% comm_name) %>%
  filter(halibut_yn==T) %>%
  ungroup() %>%
  select(-halibut_yn) %>%
  # Calculate bycatch ratios
  group_by(set_id) %>%
  mutate(halibut_n=catch_n[comm_name=="California halibut"],
         ratio=catch_n/halibut_n) %>%
  ungroup() %>%
  # Remove halibut
  filter(comm_name!="California halibut") %>%
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
  labs(x="Bycatch occurence\n(percent of gillnet sets)", y="", tag="A", title="CDFW gillnet observer data") +
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
ggsave(g, filename=file.path(plotdir, "FigX_cdfw_gillnet_obs_bycatch_ratio_by_species.png"),
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
  labs(y="Bycatch ratio\n(bycatch / halibut catch)", x="Year", title="CDFW gillnet observer data") +
  # Axis
  scale_x_continuous(breaks=1983:1989) +
  scale_y_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + theme2 +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_cdfw_gillnet_obs_bycatch_ratio_over_time.png"),
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
g <- ggplot(data %>% filter(comm_name%in%top20spp), aes(x=bottom_depth_fa, y=ratio)) +
  facet_wrap(~factor(comm_name, levels=top20spp), ncol=5, scales="free_x") +
  geom_smooth(fill="grey80", color="black") +
  geom_point(pch=21, color="grey30", alpha=0.5) +
  # Horizontal line
  geom_hline(yintercept=1, linetype="dotted") +
  # Labels
  labs(x="Depth (fathoms)", y="Bycatch ratio\n(bycatch / halibut catch)", title="CDFW gillnet observer data") +
  # Axis
  scale_y_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + theme3
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_cdfw_gillnet_obs_bycatch_ratio_by_depth.png"),
       width=6.5, height=5.5, units="in", dpi=600)


# Bycatch ratio by day of year
g <- ggplot(data %>% filter(comm_name%in%top20spp), aes(x=date_dummy, y=ratio)) +
  facet_wrap(~factor(comm_name, levels=top20spp), ncol=5) +
  geom_smooth(fill="grey80", color="black") +
  geom_point(pch=21, color="grey30", alpha=0.5) +
  # Horizontal line
  geom_hline(yintercept=1, linetype="dotted") +
  # Labels
  labs(x="Day of year", y="Bycatch ratio\n(bycatch / halibut catch)") +
  # Axis
  scale_x_date(date_breaks = "2 months", date_labels =  "%b") +
  scale_y_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + theme3 +
  theme(axis.text.x=element_text(size=6))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_cdfw_gillnet_obs_bycatch_ratio_by_date.png"),
       width=6.5, height=5.5, units="in", dpi=600)

