

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


# Format data
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

# Identify major sectora
stats1 <- data1 %>%
  # Total by sector
  group_by(sector) %>%
  summarize(landings_lb=sum(landings_lb)) %>%
  ungroup() %>%
  # Arrange
  arrange(desc(landings_lb)) %>%
  # Calc proportion
  mutate(landings_prop=landings_lb/sum(landings_lb))

# Summarize by generalized sectors
major_sectors <- stats1$sector[stats1$landings_prop>=0.02]
data2 <- data1 %>%
  # Recode sectors
  mutate(sector=ifelse(sector %in% major_sectors, sector, "Other")) %>%
  # Annual by sector
  group_by(sector, year) %>%
  summarise(landings_lb=sum(landings_lb)) %>%
  ungroup() %>%
  # Recode sector
  mutate(sector=recode(sector,
                       "Combined LE & OA CA Halibut"="LE/OA CA Halibut"),
         sector=factor(sector, levels=c("OA CA Halibut", "LE CA Halibut",
                                        "LE/OA CA Halibut", "Incidental", "Other") %>% rev()))


# Plot data
################################################################################



# Plot
g <- ggplot(data2, aes(x=year, y=landings_lb/1e6, fill=sector)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="Landings\n(millions of lbs)") +
  # Legend
  scale_fill_discrete(name="Fishery") +
  # Theme
  theme_bw()
g



