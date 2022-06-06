

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


# Build data
################################################################################

# Sectors of interest
sort(unique(data_orig$sector))
sectors_do <- c("At-Sea Hake CP", "At-Sea Hake MSCV", "Midwater Hake", "Midwater Hake EM", "Shoreside Hake", "Tribal At-Sea Hake",
                "Directed P Halibut", "OA CA Halibut",
                "LE Sablefish - Hook & Line", "LE Sablefish - Pot",
                "Pink Shrimp", "Ridgeback Prawn Trawl", "Sea Cucumber Trawl")

# Build data
data <- data_orig %>%
  # Reduce to sectors of interest
  filter(sector %in% sectors_do) %>%
  # Recode some species
  mutate(species=stringr::str_to_sentence(species)) %>%
  mutate(species=recode(species,
                        "Sea cucumber unid"="Sea cucumber",
                        "Ca sea cucumber"="Sea cucumber")) %>%
  # Add target species
  mutate(target_spp=recode(sector,
                           "At-Sea Hake CP"="Pacific hake",
                           "At-Sea Hake MSCV"="Pacific hake",
                           "Directed P Halibut"="Pacific halibut",
                           "LE Sablefish - Hook & Line"="Sablefish",
                           "LE Sablefish - Pot"="Sablefish",
                           "Midwater Hake"="Pacific hake",
                           "Midwater Hake EM"="Pacific hake",
                           "OA CA Halibut"="California halibut",
                           "Pink Shrimp"="Pink shrimp",
                           "Ridgeback Prawn Trawl"="Ridgeback prawn",
                           "Sea Cucumber Trawl"="Sea cucumber",
                           "Shoreside Hake"="Pacific hake",
                           "Tribal At-Sea Hake"="Pacific hake")) %>%
  # Market target or non-target
  mutate(target_type=ifelse(species==target_spp, "Target", "Non-target")) %>%
  # Summarize
  group_by(sector, target_spp, target_type, year) %>%
  summarize(landings_mt=sum(landings_mt),
            discards_mt_dead=sum(discards_mt_dead),
            discards_mt_live=sum(discards_mt_live)) %>%
  ungroup() %>%
  # Gather
  gather(key="catch_catg", value="catch_mt", 5:ncol(.)) %>%
  mutate(catch_catg=recode_factor(catch_catg,
                                  "discards_mt_dead"="Dead discards",
                                  "discards_mt_live"="Live discards",
                                  "landings_mt"="Landings")) %>%
  # Mutate
  mutate(catch_type=ifelse(catch_catg=="Landings", "Landings", "Discards")) %>%
  # Arrange
  select(sector, target_spp, target_type, catch_type, catch_catg, year, everything()) %>%
  # Calculate proportion
  group_by(sector, year) %>%
  mutate(catch_prop=catch_mt/sum(catch_mt)) %>%
  ungroup() %>%
  # Order sectors
  mutate(sector=factor(sector, levels=sectors_do))


# Create ratio version
data2 <- data %>%
  # Add label
  mutate(label=paste(target_type, catch_catg, sep="_") %>% gsub("-| ", "_", .) %>% tolower(.) ) %>%
  # Simplify and spread
  select(sector, year, label, catch_mt) %>%
  spread(key="label", value="catch_mt") %>%
  # Calculate ratio
  mutate(non_target_dead_discards=non_target_dead_discards / target_landings,
         non_target_landings=non_target_landings / target_landings,
         non_target_live_discards=non_target_live_discards / target_landings,
         target_dead_discards=target_dead_discards / target_landings,
         target_live_discards=target_live_discards / target_landings,
         target_landings=target_landings / target_landings) %>%
  # Gather
  gather(key="metric", value="ratio", 3:ncol(.)) %>%
  # Remove target
  filter(metric!="target_landings") %>%
  # Muatte
  mutate(target_type=ifelse(grepl("non", metric), "Non-target", "Target"),
         catch_catg=ifelse(grepl("landings", metric), "Landings",
                           ifelse(grepl("dead", metric), "Discards (dead)", "Discards (live)"))) %>%
  mutate(catch_catg=factor(catch_catg, levels=c("Landings", "Discards (live)", "Discards (dead)")))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=5),
                   axis.title=element_text(size=6),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=6),
                   strip.text=element_text(size=6),
                   plot.title=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g <- ggplot(data, aes(x=year, y=catch_prop, fill=target_type, alpha=catch_catg)) +
  facet_wrap(~sector, ncol=5, scales="free_y") +
  geom_bar(stat="identity", color="grey30", lwd=0.1) +
  # Labels
  labs(x="Year", y="Proportion of catch", title="GEMM mortality estimates") +
  # Legend
  scale_fill_discrete(name="Species type") +
  scale_alpha_manual(name="Catch type", values=c(1, 0.7, 0.3) %>% rev()) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.15),
        legend.box = "horizontal")
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_landings_discards_by_sectors_with_1target_spp.png"),
       width=6.5, height=4.5, units="in", dpi=600)


# Plot data
################################################################################


# Plot
g <- ggplot(data2, aes(x=year, y=ratio, group=metric, color=target_type, linetype=catch_catg)) +
  facet_wrap(~sector, ncol=5, scales="free_y") +
  geom_line() +
  # Reference line
  geom_hline(yintercept=1, linetype="dotted", lwd=0.3, color="grey60") +
  # Labels
  labs(x="Year", y="Bycatch ratio", title="GEMM mortality estimates") +
  # Legend
  scale_color_discrete(name="Species type") +
  scale_linetype_discrete(name="Catch type") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.15),
        legend.box = "horizontal")
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_bycatch_ratios_by_sectors_with_1target_spp.png"),
       width=6.5, height=4.5, units="in", dpi=600)




