

# Clear workspace
rm(list = ls())

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


# Build data
################################################################################

# Sectors
# OA CA Halibut, LE CA Halibut, Combined LE & OA CA Halibut
sort(unique(data_orig$sector))
sectors_use <- c("OA CA Halibut", "LE CA Halibut", "Combined LE & OA CA Halibut")

# Build data
data <- data_orig %>%
  # CA halibut fishery
  filter(sector %in% sectors_use) %>%
  # Recode sector
  mutate(sector=recode_factor(sector,
                              "OA CA Halibut"="Open-access",
                              "LE CA Halibut"="Limited entry",
                              "Combined LE & OA CA Halibut"="Combined")) %>%
  # Proportion of catch by year
  group_by(year) %>%
  mutate(pcatch=catch_mt/sum(catch_mt)) %>%
  ungroup()

# Inspect data
str(data)
table(data$mgmt_group)
table(data$groundfish_fmp_yn)
table(data$groundfish_fmp_yn)

# Species key
spp_key <- data %>%
  select(mgmt_group, species) %>%
  unique() %>%
  arrange(mgmt_group, species)

# Build data
################################################################################

# Stats
stats1 <- data %>%
  # Median prop by species
  group_by(species) %>%
  summarize(pcatch_med=median(pcatch)) %>%
  ungroup() %>%
  # Arrange
  arrange(desc(pcatch_med))

# Plot
g <- ggplot(data, aes(x=pcatch, y=factor(species, levels=stats1$species))) +
  geom_boxplot() +
  # Labels
  labs(x="Proportion of annual catch", y="") +
  # Theme
  theme_bw()
g


