

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/wcgop/raw"
outdir <- "data/wcgop/processed"
plotdir <- "data/wcgop/figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "WCGOP_2002_2020_observer_data.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>%
  # Reduce to sector of interest
  filter(sector=="OA CA Halibut") %>%
  # Reduce to sets SOUTH and exclude offshore federal trawlers
  filter(set_lat_dd <= 34.6 & haul_depth_fm <= 40) %>%
  # Add set id
  mutate(set_id=paste(trip_id, haul_id, sep="-")) %>%
  # Reduce to sets with CA halibut catch
  group_by(set_id) %>%
  mutate(halibut_yn="California Halibut" %in% comm_name) %>%
  ungroup() %>%
  filter(halibut_yn==T) %>%
  # Sum catches within set (retain attributes of interest)
  # You shouldn't have to do this but it solves problems where a species has 1 row for discarded catch and 1 row for retained catch
  group_by(set_id, set_date, set_depth_fm, haul_depth_fm, comm_name) %>%
  summarize(landings_kg=sum(landings_kg),
            discards_kg=sum(discards_kg)) %>%
  ungroup() %>%
  # Calculate bycatch ratios
  group_by(set_id) %>%
  mutate(halibut_kg_retained=landings_kg[comm_name=="California Halibut"],
         bycatch_ratio_retained=landings_kg/halibut_kg_retained,
         bycatch_ratio_discarded=discards_kg/halibut_kg_retained) %>%
  ungroup() %>%
  # Remove sets without retained halibut catch
  filter(halibut_kg_retained>0) %>%
  # Compute variables of interest
  mutate(year=lubridate::year(set_date),
         depth_fa_avg=(set_depth_fm+haul_depth_fm)/2,
         day_of_year=lubridate::yday(set_date),
         date_dummy=paste("2020", lubridate::month(set_date), lubridate::day(set_date), sep="-") %>% lubridate::ymd()) %>%
  # Arrange
  select(-c(set_depth_fm, haul_depth_fm)) %>%
  select(set_id, year, set_date, date_dummy, day_of_year, depth_fa_avg, comm_name, landings_kg:bycatch_ratio_discarded, everything()) %>%
  # Reduce to data on discards
  filter(bycatch_ratio_discarded>0)


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
top40spp <- stats$comm_name[1:40]


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
g1 <- ggplot(stats %>% filter(comm_name %in% top40spp), aes(y=factor(comm_name, levels=stats$comm_name), x=psets)) +
  # Boxplots
  geom_bar(stat="identity") +
  # Labels
  labs(x="Bycatch occurrence\n(percent of trawl tows)", y="", tag="A", title="WCGOP trawl observer data\nnsouthern trawl fishery") +
  # Axis
  scale_x_continuous(labels=scales::percent) +
  # Theme
  theme_bw() + theme1
g1

# Plot data
g2 <- ggplot(data %>% filter(comm_name %in% top40spp), aes(y=factor(comm_name, levels=stats$comm_name), x=bycatch_ratio_discarded)) +
  # Boxplots
  geom_boxplot(fill="grey90", lwd=0.2, outlier.size = 0.3) +
  # Reference line
  geom_vline(xintercept = 1) +
  # Labels
  labs(x="Bycatch ratio\n(discards / halibut landings)", y="", tag="B", title=" \n ") +
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
ggsave(g, filename=file.path(plotdir, "FigX_wcgop_trawl_obs_bycatch_ratio_by_species_discards_south.png"),
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
g <- ggplot(data %>% filter(comm_name%in%top20spp), aes(x=year, y=bycatch_ratio_discarded, group=year)) +
  facet_wrap(~factor(comm_name, levels=top20spp), ncol=5) +
  geom_boxplot(lwd=0.3, outlier.size = 0.5, color="grey40", fill="grey90") +
  # Reference line
  geom_hline(yintercept=1) +
  # Labels
  labs(y="Bycatch ratio\n(discards bycatch / halibut landings)", x="Year", title="WCGOP trawl observer data - southern trawl fishery") +
  # Axis
  scale_y_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + theme2 +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_wcgop_trawl_obs_bycatch_ratio_over_time_discards_south.png"),
       width=6.5, height=5, units="in", dpi=600)


# Plot data by depth and day of year
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

# Plot data
g <- ggplot(data %>% filter(comm_name%in%top20spp), aes(x=depth_fa_avg, y=bycatch_ratio_discarded)) +
  facet_wrap(~factor(comm_name, levels=top20spp), ncol=5) +
  geom_point(pch=21, color="grey60", alpha=0.5, size=0.7) +
  # geom_smooth(fill="grey30", color="black", alpha=0.7, lwd=0.5) +
  # Labels
  labs(x="Depth (fathoms)", y="Bycatch ratio\n(discards bycatch / halibut landings)",
       title="WCGOP trawl observer data - southern trawl fishery") +
  # Theme
  theme_bw() + theme3
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_wcgop_trawl_obs_bycatch_ratio_by_depth_discards_south.png"),
       width=6.5, height=5.5, units="in", dpi=600)

# Plot data
g <- ggplot(data %>% filter(comm_name%in%top20spp), aes(x=date_dummy, y=bycatch_ratio_discarded)) +
  facet_wrap(~factor(comm_name, levels=top20spp), ncol=5) +
  geom_point(pch=21, color="grey60", alpha=0.5, size=0.7) +
  # geom_smooth(fill="grey30", color="black", alpha=0.7, lwd=0.5) +
  # Horizontal line
  geom_hline(yintercept=1, linetype="dotted") +
  # Labels
  labs(x="Day of year", y="Bycatch ratio\n(discards / halibut landings)",
       title="WCGOP trawl observer data - southern trawl fishery") +
  # Axis
  scale_x_date(date_breaks = "2 months", date_labels =  "%b") +
  # Theme
  theme_bw() + theme3
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_wcgop_trawl_obs_bycatch_ratio_by_yday_discards_south.png"),
       width=6.5, height=5.5, units="in", dpi=600)






