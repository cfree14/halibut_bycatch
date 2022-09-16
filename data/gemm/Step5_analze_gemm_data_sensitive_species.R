

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

# Data sources
# Groundfish Expanded Mortality Multi-year (GEMM) data
# GEMM homepage: https://www.fisheries.noaa.gov/west-coast/fisheries-observers/west-coast-fishery-observer-bycatch-and-mortality-reports
# Data Warehouse Landing Page: https://www.fisheries.noaa.gov/resource/data/fishery-resource-analysis-and-monitoring-division-fram-data-warehouse

# Read data
data_orig <- readRDS(file=file.path(outdir, "GEMM_2002_2020_data.Rds"))


# Setup
################################################################################

# Build data
data <- data_orig %>%
  # Reduce to )A CA Halibut sector
  filter(sector=="OA CA Halibut") %>%
  # Compute totals by species and year (summing across mgmt grouops) %>%
  group_by(species, year) %>%
  summarize(landings_mt=sum(landings_mt),
            discards_mt=sum(discards_mt_tot),
            catch_mt=sum(catch_mt)) %>%
  ungroup() %>%
  # Record rertained halibut bycatch
  group_by(year) %>%
  mutate(halibut_retained_mt=landings_mt[species=="California Halibut"]) %>%
  ungroup() %>%
  # Compute bycatch ratio
  mutate(ratio_all=catch_mt / halibut_retained_mt,
         ratio_ret=landings_mt / halibut_retained_mt,
         ratio_dis=discards_mt / halibut_retained_mt) %>%
  # Simplify and gather
  select(species, year, ratio_all:ratio_dis) %>%
  gather(key="ratio_type", value="ratio", 3:ncol(.)) %>%
  mutate(ratio_type=recode_factor(ratio_type,
                                  "ratio_all"="All catch",
                                  "ratio_ret"="Retained catch",
                                  "ratio_dis"="Discarded catch")) %>%
  # Remove zeroes
  filter(ratio!=0) %>%
  # Remove halibut (and kelp)
  filter(species!="California Halibut" & species!="Kelp") %>%
  # Format species
  mutate(species=stringr::str_to_sentence(species),
         species=gsub(" unid", "", species)) %>%
  # Reduce to sensitive species
  filter(species %in% c("Yelloweye rockfish", "Green sturgeon", "Silver salmon"))



# Plot data
g <- ggplot(data, aes(x=year, y=ratio)) +
  facet_wrap(~species, scale="free_y") +
  # Reference line
  geom_hline(yintercept = 1, color="grey50", linetype="dotted") +
  # Line
  geom_line() +
  # Labels
  labs(x="", y="Bycatch ratio\n(all bycatch / retained halibut catch)", title="GEMM OA CA Halibut bycatch ratios") +
  # Axis
  scale_y_continuous(lim=c(0,NA)) +
  scale_x_continuous(breaks=seq(2000, 2020, 5), lim=c(2000, 2020)) +
  # Theme
  theme_bw() + theme1 +
  theme(strip.text = element_text(size=6))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_gemm_oa_halibut_sector_bycatch_over_time_top20.png"),
       width=6.5, height=5, units="in", dpi=600)

