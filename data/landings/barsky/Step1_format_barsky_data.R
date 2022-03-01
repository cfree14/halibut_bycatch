

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/landings/barsky/raw"
outdir <- "data/landings/barsky/processed"
plotdir <- "data/landings/barsky/figures"

# Read data
data1_orig <- readxl::read_excel(file.path(indir, "Barsky_1990.xlsx"), sheet=1)
data2_orig <- readxl::read_excel(file.path(indir, "Barsky_1990.xlsx"), sheet=2)

# PACFIN data
pacfin_orig <- wcfish::pacfin_all6
pacfin_hal <- pacfin_orig %>%
  # Reduce
  filter(state=="California" & comm_name %in% c("California halibut", "Nom. Calif halibut")) %>%
  # Sum by year
  group_by(year) %>%
  summarize(landings_mt=sum(landings_mt)) %>%
  # Convert units
  mutate(landings_kg=landings_mt*1000,
         landings_lb=measurements::conv_unit(landings_kg, "kg", "lbs"))


# Format data
################################################################################

# Format data
data1a <- data1_orig %>%
  # Rename
  rename(year=Year) %>%
  # Gather
  gather(key="port_complex", value="landings_lb", 2:ncol(.))

range(data1a$year)

# Format data
data2 <- data2_orig %>%
  mutate(notes=ifelse(total_check!=0 & is.na(notes), "Total != CA + Mexico", notes)) %>%
  select(-total_check)
range(data2$year)

# Compare totals between two appendices
tots <- data1a %>%
  # Annual landings in Appendix I
  group_by(year) %>%
  summarize(landings_lb1=sum(landings_lb)) %>%
  ungroup() %>%
  # Annual landings in Appendix II
  left_join(data2 %>% select(year, total)) %>%
  rename(landings_lb2=total) %>%
  # Calculate difference
  mutate(landings_lb_diff=landings_lb2-landings_lb1)

# Calculate totals associated with "uknown/other)"
data1_other <- tots %>%
  # Add port complex
  mutate(port_complex="Other/Unknown") %>%
  # Rename and reduce
  rename(landings_lb=landings_lb_diff) %>%
  select(year, port_complex, landings_lb) %>%
  # Overwrite negative landings in 1934
  mutate(landings_lb=pmax(0, landings_lb))

# Merge Appendix I data with computed missing data
data1b <- rbind(data1a, data1_other) %>%
  # Order
  mutate(port_complex=factor(port_complex,
                             levels=c("San Diego", "Los Angeles", "Santa Barbara", "Monterey", "San Francisco", "Other/Unknown") %>% rev()))



# Plot data
################################################################################

# Build data
data_tots <- data2 %>%
  select(year, california, total) %>%
  gather(key="dataset", value="landings_lb", 2:ncol(.)) %>%
  mutate(dataset=recode(dataset,
                        "california"="California only",
                        "total"="Total (includes Mexico)"))

# Proportions
data_prop <- data1b %>%
  group_by(year) %>%
  mutate(landings_prop=landings_lb / sum(landings_lb)) %>%
  ungroup()

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=9),
                   axis.title.x=element_blank(),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=9),
                   plot.title=element_blank(),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(data1b, aes(x=year, y=landings_lb/1e6, fill=port_complex)) +
  # Port bars
  geom_bar(stat="identity", lwd=0.4) +
  # Total lines
  geom_line(data=data_tots, mapping=aes(x=year, y=landings_lb/1e6, linetype=dataset), inherit.aes = F) +
  # Labels
  labs(x="", y="Landings\n(millions of lbs)") +
  # Axis
  scale_x_continuous(breaks=seq(1900, 2020, 10), lim=c(1916, 1990)) +
  # Legend
  scale_linetype_manual(name="Dataset", values=c("dotted", "solid")) +
  scale_fill_manual(name="Port complex", values=c("grey80", RColorBrewer::brewer.pal(5, "Set3"))) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.65))
g1

# Plot data
g2 <- ggplot(data_prop, aes(x=year, y=landings_prop, fill=port_complex)) +
  # Port bars
  geom_bar(stat="identity", lwd=0.4) +
  # Labels
  labs(x="", y="Proportion\nof landings") +
  # Axis
  scale_x_continuous(breaks=seq(1900, 2020, 10), lim=c(1916, 1990)) +
  # Legend
  scale_linetype_manual(name="Dataset", values=c("dashed", "solid")) +
  scale_fill_manual(name="Port complex", values=c("grey80", RColorBrewer::brewer.pal(5, "Set3"))) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Arrange
g <- gridExtra::grid.arrange(g1, g2, heights=c(0.7, 0.3))

# Export
ggsave(g, filename=file.path(plotdir, "halibut_landings_barsky.png"),
       width=6.5, height=4.5, units="in", dpi=600)


# Export data
################################################################################

# Export data
saveRDS(data1a, file=file.path(outdir, "1930_1988_CA_halibut_landings_by_port_complex_raw.Rds"))
saveRDS(data1b, file=file.path(outdir, "1930_1988_CA_halibut_landings_by_port_complex_expanded.Rds"))
saveRDS(data2, file=file.path(outdir, "1916_1988_CA_halibut_landings_by_country.Rds"))




