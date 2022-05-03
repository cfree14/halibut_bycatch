

# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/gemm/raw"
outdir <- "data/gemm/processed"
plotdir <- "data/gemm/figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "GEMM_2002_2020_data.Rds"))


# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=5),
                   legend.title=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Sea bass bycatch
################################################################################

# Format data
bass <- data_orig %>%
  # Reduce to sharks
  filter(grepl("sea bass", tolower(species))) %>%
  # Simple sectors
  mutate(halibut_yn=ifelse(grepl("CA Halibut", sector), "CA Halibut", "Other fishery")) %>%
  # Summarize
  group_by(halibut_yn, year) %>%
  summarize(discards_mt=sum(discards_mt)) %>%
  ungroup()

# Plot data
g1 <- ggplot(bass, aes(x=year, y=discards_mt, fill=halibut_yn)) +
  geom_bar(stat="identity", color="grey30", lwd=0.3) +
  # Labels
  labs(x="", y="Discards (mt)") +
  scale_fill_discrete(name="Fishery") +
  # Theme
  theme_bw() + my_theme
g1


# Export figure
ggsave(g1, filename=file.path(plotdir, "bass_discard.png"),
       width=4.5, height=2.5, units="in", dpi=600)

# Format data
bass1 <- data_orig %>%
  # Reduce to bass
  filter(grepl("sea bass", tolower(species))) %>%
  # Simple sectors
  mutate(halibut_yn=ifelse(grepl("CA Halibut", sector), "CA Halibut", "Other fishery")) %>%
  filter(halibut_yn=="CA Halibut") %>%
  # Summarize
  group_by(species, year) %>%
  summarize(discards_mt=sum(discards_mt)) %>%
  ungroup()

# Plot data
g1 <- ggplot(bass1, aes(x=year, y=discards_mt, fill=species)) +
  geom_bar(stat="identity", color="grey30", lwd=0.3) +
  # Labels
  labs(x="", y="Discards (mt)") +
  scale_fill_discrete(name="Species") +
  # Theme
  theme_bw() + my_theme
g1

# Export figure
ggsave(g1, filename=file.path(plotdir, "bass_discards2.png"),
       width=4.5, height=2.5, units="in", dpi=600)


# Ray bycatch
################################################################################

# Ray species
spp <- sort(unique(data_orig$species))
spp_ray <- spp[grepl("ray", tolower(spp))]
spp_ray_use <- spp_ray[!spp_ray%in%c("California Moray", "Gray Smoothhound Shark", "Moray Unid",
                                     "Silvergray Rockfish", "Praya Unid")]

# Format data
rays <- data_orig %>%
  # Reduce to sharks
  filter(species %in% spp_ray_use) %>%
  # Simple sectors
  mutate(halibut_yn=ifelse(grepl("CA Halibut", sector), "CA Halibut", "Other fishery")) %>%
  # Summarize
  group_by(halibut_yn, year) %>%
  summarize(discards_mt=sum(discards_mt)) %>%
  ungroup()

# Plot data
g1 <- ggplot(rays, aes(x=year, y=discards_mt, fill=halibut_yn)) +
  geom_bar(stat="identity", color="grey30", lwd=0.3) +
  # Labels
  labs(x="", y="Discards (mt)") +
  scale_fill_discrete(name="Fishery") +
  # Theme
  theme_bw() + my_theme
g1

# Export figure
ggsave(g1, filename=file.path(plotdir, "ray_discard.png"),
       width=4.5, height=2.5, units="in", dpi=600)


# Format data
rays1 <- data_orig %>%
  # Reduce to sharks
  filter(species %in% spp_ray_use) %>%
  # Simple sectors
  mutate(halibut_yn=ifelse(grepl("CA Halibut", sector), "CA Halibut", "Other fishery")) %>%
  filter(halibut_yn=="CA Halibut") %>%
  # Summarize
  group_by(species, year) %>%
  summarize(discards_mt=sum(discards_mt)) %>%
  ungroup()

# Plot data
g1 <- ggplot(rays1, aes(x=year, y=discards_mt, fill=species)) +
  geom_bar(stat="identity", color="grey30", lwd=0.3) +
  # Labels
  labs(x="", y="Discards (mt)") +
  scale_fill_discrete(name="Species") +
  # Theme
  theme_bw() + my_theme
g1

# Export figure
ggsave(g1, filename=file.path(plotdir, "ray_discards2.png"),
       width=4.5, height=2.5, units="in", dpi=600)



# Ray bycatch
################################################################################

# Format data
data1 <- data_orig %>%
  # Filter
  filter(species=="California Halibut" & sector_type %in% c("commercial", "tribal")) %>%
  # Annual by sector
  group_by(sector, year) %>%
  summarise(landings_mt=sum(landings_mt)) %>%
  ungroup() %>%
  # Convert
  mutate(landings_lb=measurements::conv_unit(landings_mt*1000, "kg", "lbs"))

