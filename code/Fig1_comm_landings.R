

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# Get data
barsky_orig <- readRDS("data/landings/barsky/processed/1916_1988_CA_halibut_landings_by_country.Rds")
pacfin_orig <- readRDS("data/landings/pacfin/processed/PACFIN_1981_2020_CA_halibut_landings_by_port_complex.Rds")


# Build data
################################################################################

# Format Barksy
barsky <- barsky_orig %>%
  mutate(dataset="Barsky (1990)") %>%
  select(dataset, year, total) %>%
  rename(landings_lb=total) %>%
  filter(year<=1980)

# Format PACFIN
pacfin <- pacfin_orig %>%
  group_by(year) %>%
  summarize(landings_lb=sum(landings_lb),
            value_usd=sum(revenues_usd)) %>%
  mutate(dataset="PACFIN")

# Merge data
data <- bind_rows(barsky, pacfin) %>%
  arrange(year)

# High points
pts <- data %>%
  filter(year %in% c(1919, 1946, 1964)) %>%
  mutate(label=paste0(year, "\n", round(landings_lb/1e6, 1), " million lbs"))

# Periods
data %>%
  filter(year %in% 1980:2005) %>%
  pull(landings_lb) %>% mean(.) / 1e6
data %>%
  filter(year %in% 2006:2020) %>%
  pull(landings_lb) %>% mean(.) / 1e6


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=10),
                   axis.title.x=element_blank(),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=10),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=year, y=landings_lb/1e6)) +
  geom_line(size=0.8, color="grey40") +
  # Points
  geom_point(data=pts, size=2.5) +
  geom_text(data=pts, aes(label=label), hjust=0, nudge_x = 2, size=2.5) +
  # Labels
  labs(x="", y="Commercial landings\n(millions of pounds)") +
  # Axis
  scale_y_continuous(lim=c(0, NA)) +
  scale_x_continuous(breaks=seq(1910, 2020, 10)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig1_comm_landings.png"),
       width=6.5, height=3, units="in", dpi=600)






