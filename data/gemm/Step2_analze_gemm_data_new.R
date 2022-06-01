

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

# Halibut by sector
################################################################################

# CA halibut catch/discards by sector
data1 <- data_orig %>%
  # Reduce to CA halibut
  filter(species=="California Halibut") %>%
  # Remove useless sectors
  filter(!sector %in% c("Combined LE & OA CA Halibut")) %>%
  # Summarize annual catch by sector
  group_by(sector_type, sector, year) %>%
  summarise(landings_mt=sum(landings_mt),
            discards_mt_dead=sum(discards_mt_dead),
            discards_mt_live=sum(discards_mt_live)) %>%
  ungroup() %>%
  # Mean annual catch by sector
  group_by(sector_type, sector) %>%
  summarise(landings_mt=mean(landings_mt),
            discards_mt_dead=mean(discards_mt_dead),
            discards_mt_live=mean(discards_mt_live)) %>%
  ungroup() %>%
  # Simplify and order
  mutate(catch_mt=landings_mt+discards_mt_dead+discards_mt_live) %>%
  arrange(desc(catch_mt)) %>%
  filter(catch_mt>0) %>%
  mutate(sector=factor(sector, levels=sector)) %>%
  select(-catch_mt) %>%
  # Gather catch type
  gather(key="catch_type", value="catch_mt", 3:ncol(.)) %>%
  mutate(catch_type=recode_factor(catch_type,
                           "discards_mt_live"="Discards (live)",
                           "discards_mt_dead"="Discards (dead)",
                           "landings_mt"="Landings"))

# Theme
theme1 <- theme(axis.text=element_text(size=6),
                axis.title=element_text(size=8),
                legend.text=element_text(size=6),
                legend.title=element_text(size=8),
                strip.text=element_text(size=8),
                plot.title=element_text(size=10),
                plot.tag=element_text(size=8),
                # Gridlines
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                # Legend
                legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data1, aes(y=sector, x=catch_mt, fill=catch_type)) +
  geom_bar(stat="identity", color="black", lwd=0.1) +
  # Labels
  labs(x="Average annual catch, 2002-2020 (mt)", y="", title="GEMM mortality estimates") +
  # Legend
  scale_fill_manual(name="",
                    values=c("grey10", "grey50", "grey90") %>% rev()) +
  guides(fill = guide_legend(reverse=T)) +
  # Theme
  theme_bw() + theme1 +
  theme(legend.position=c(0.8, 0.8),
        axis.title.y=element_blank())
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_gemm_halibut_by_sector.png"),
       width=5.5, height=4.5, units="in", dpi=600)


# Halibut by sector and year
################################################################################

# Sectors of intereest
sectors_do <- c("California Recreational", "LE CA Halibut", "OA CA Halibut")

# Build data
data2 <- data_orig %>%
  # Halibut
  filter(species=="California Halibut") %>%
  # Simplify
  select(sector, year, landings_mt, discards_mt_dead, discards_mt_live) %>%
  # Gather catch type
  gather(key="catch_type", value="catch_mt", 3:ncol(.)) %>%
  mutate(catch_type=recode_factor(catch_type,
                           "discards_mt_live"="Discards (live)",
                           "discards_mt_dead"="Discards (dead)",
                           "landings_mt"="Landings")) %>%
  # Sectors of interest
  filter(sector %in% sectors_do) %>%
  mutate(sector=factor(sector, levels=sectors_do)) %>%
  # Compute proportion
  group_by(sector, year) %>%
  mutate(catch_prop= catch_mt/sum(catch_mt)) %>%
  ungroup()

# Bycatch ratio
data2_ratio <- data2 %>%
  group_by(sector, year) %>%
  summarize(ratio_dead=catch_mt[catch_type=="Discards (dead)"] / catch_mt[catch_type=="Landings"],
            ratio_live=catch_mt[catch_type=="Discards (live)"] / catch_mt[catch_type=="Landings"]) %>%
  ungroup() %>%
  gather(key="ratio_type", value="ratio", 3:ncol(.)) %>%
  mutate(ratio_type=recode_factor(ratio_type, "ratio_dead"="Discards (dead)", "ratio_live"="Discards (live)"))

# Plot catch
g1 <- ggplot(data2, aes(x=year, y=catch_mt, fill=catch_type)) +
  facet_wrap(~sector, nrow=1) +
  geom_bar(stat="identity", color="black", lwd=0.1) +
  # Labels
  labs(x="", y="Catch (mt)", tag="A") +
  scale_x_continuous(breaks=seq(2000, 2020, 5), lim=c(2000, 2020)) +
  # Legend
  scale_fill_manual(name="",
                    values=c("grey10", "grey50", "grey90") %>% rev()) +
  guides(fill = guide_legend(reverse=T)) +
  # Theme
  theme_bw() + theme1 +
  theme(axis.title.x=element_blank(),
        legend.key.size=unit(0.3, "cm"),
        legend.position = c(0.9, 0.8))
g1

# Plot proportion
g2 <- ggplot(data2, aes(x=year, y=catch_prop, fill=catch_type)) +
  facet_wrap(~sector, nrow=1) +
  geom_bar(stat="identity", color="black", lwd=0.1) +
  # Labels
  labs(x="", y="Proportion of catch", tag="B") +
  scale_x_continuous(breaks=seq(2000, 2020, 5), lim=c(2000, 2020)) +
  # Legend
  scale_fill_manual(name="",
                    values=c("grey10", "grey50", "grey90") %>% rev()) +
  guides(fill = guide_legend(reverse=T)) +
  # Theme
  theme_bw() + theme1 +
  theme(axis.title.x=element_blank(),
        legend.position = "none")
g2

# Plot ratio
g3 <- ggplot(data2_ratio, aes(x=year, y=ratio, color=ratio_type)) +
  facet_wrap(~sector, nrow=1) +
  geom_line() +
  # Reference line
  geom_hline(yintercept = 1, color="grey50", linetype="dotted") +
  # Labels
  labs(x="", y="Bycatch ratio\n(discards / landings)", tag="C") +
  scale_x_continuous(breaks=seq(2000, 2020, 5), lim=c(2000, 2020)) +
  # Legend
  scale_color_manual(name="",
                    values=c("grey30", "grey70")) +
  # Theme
  theme_bw() +theme1 +
  theme(legend.position=c(0.8, 0.8),
        axis.title.x=element_blank())
g3

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, g3, ncol=1)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_gemm_halibut_by_sector_year.png"),
       width=6.5, height=5, units="in", dpi=600)


# OA CA Halibut bycatch ratio distributions
################################################################################

# Theme
theme2 <- theme(axis.text=element_text(size=6),
                axis.title=element_text(size=8),
                strip.text=element_text(size=8),
                plot.title=element_text(size=9),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                # Gridlines
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                # Legend
                legend.background = element_rect(fill=alpha('blue', 0)))

# Build data
data3 <- data_orig %>%
  # Reduce to A CA Halibut sector
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
         species=gsub(" unid", "", species))

# Order species
data3_order <- data3 %>%
  filter(ratio_type=="All catch") %>%
  group_by(species) %>%
  summarize(ratio_med=median(ratio)) %>%
  arrange(desc(ratio_med)) %>%
  slice(1:50)

# Plot data
g <- ggplot(data3 %>% filter(species %in% data3_order$species),
            aes(y=factor(species, levels=data3_order$species), x=ratio)) +
  facet_wrap(~ratio_type, nrow=1) +
  geom_boxplot(fill="grey90", color="grey30", lwd=0.1, outlier.size = 0.5) +
  # Reference line
  geom_vline(xintercept = 1) +
  # Labels
  labs(x="Bycatch ratio\n(bycatch / retained halibut)", y="", title="GEMM OA CA Halibut bycatch ratios") +
  # Axis
  scale_x_continuous(trans="log10",
                     breaks=c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1),
                     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1")) +
  # Theme
  theme_bw() + theme2
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_gemm_oa_halibut_sector_bycatch_ratios.png"),
       width=6.5, height=7.5, units="in", dpi=600)


# Top-20 bycatch ratios over time
################################################################################

# Build data
data4 <- data3 %>%
  filter(species %in% data3_order$species[1:20] & ratio_type=="All catch")

# Plot data
g <- ggplot(data4, aes(x=year, y=ratio)) +
  facet_wrap(~factor(species, levels=data3_order$species[1:20]), ncol=5, scale="free_y") +
  # Reference line
  geom_hline(yintercept = 1, color="grey50", linetype="dotted") +
  # Line
  geom_line() +
  # Labels
  labs(x="", y="Bycatch ratio\n(all bycatch / retained halibut catch)") +
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

