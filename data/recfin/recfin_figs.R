

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "data/recfin/figures"

# Read data
data_orig2 <- wcfish::recfin_cte2
data_orig3 <- wcfish::recfin_cte3


# Format data
################################################################################

# Inspect
table(data_orig2$trip_type)
table(data_orig2$comm_name)

# Format data
data2 <- data_orig2 %>%
  # Reduce to CA halibut
  filter(state=="California" & comm_name=="California Halibut") %>%
  # Summarize by year and status
  group_by(year, status) %>%
  summarize(catch_n=sum(catch_n),
            catch_mt=sum(catch_mt)) %>%
  ungroup() %>%
  # Convert to pounds
  mutate(catch_kg=catch_mt*1000,
         catch_lb=measurements::conv_unit(catch_kg, "kg", "lbs")) %>%
  # Add proportion
  group_by(year) %>%
  mutate(catch_prop=catch_lb/sum(catch_lb)) %>%
  ungroup()


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   # axis.text.y = element_text(angle = 90, hjust = 0.5),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g1 <- ggplot(data2, aes(x=year, y=catch_lb/1e3, fill=status)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="Catch (1000s of lbs)") +
  # Legend
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.15, 0.9))
g1

# Plot data
g2 <- ggplot(data2, aes(x=year, y=catch_prop, fill=status)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="Proportion of catch") +
  # Legend
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, heights=c(0.7, 0.3))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_rec_discards.png"),
       width=4.5, height=4, units="in", dpi=600)





