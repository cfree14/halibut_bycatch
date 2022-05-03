

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
  group_by(set_id, set_type, date, block_id, block_lat_dd, depth_fa, comm_name) %>%
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


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   axis.title.y=element_blank(),
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
  labs(x="Bycatch occurence\n(percent of gillnet sets)", y="", tag="A") +
  # Axis
  scale_x_continuous(labels=scales::percent) +
  # Theme
  theme_bw() + my_theme
g1

# Plot bycatch ratio boxplots
g2 <- ggplot(data, aes(x=ratio, y=factor(comm_name, levels=stats$comm_name))) +
  geom_boxplot() +
  # Vertical line
  geom_vline(xintercept = 1) +
  # Labels
  labs(x="Bycatch ratio\n(bycatch / halibut catch)", y="", tag="B") +
  # Axis
  scale_x_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_blank())
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, nrow=1)
g



# Plot data
################################################################################

# Theme
theme2 <- theme(axis.text=element_text(size=7),
                axis.title=element_text(size=8),
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
  geom_point(pch=21, color="grey30") +
  # Horizontal line
  geom_hline(yintercept=1, linetype="dotted") +
  # Labels
  labs(x="Depth (fathoms)", y="Bycatch ratio") +
  # Axis
  scale_y_continuous(trans="log10", breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  # Theme
  theme_bw() + theme2
g





