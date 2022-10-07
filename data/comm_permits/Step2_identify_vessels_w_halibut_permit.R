

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/comm_permits/raw"
outdir <- "data/comm_permits/processed"
plotdir <- "data/comm_permits/figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CDFW_2000_2021_permit_data.Rds"))


# Setup
################################################################################

# Build data
data <- data_orig %>%
  # Reduce to halibut permits
  filter(permit=="Halibut Trawl Permit") %>%
  # Determine permit extent
  select(vessel_id, permit_issue_date) %>%
  rename(issue_date=permit_issue_date) %>%
  unique() %>%
  mutate(issue_year=lubridate::year(issue_date),
         expire_date=paste(issue_year+1, "03-31", sep="-") %>% lubridate::ymd())


g <- ggplot(data, aes(x=issue_year, y=vessel_id %>% as.character())) +
  geom_tile() +
  theme_bw()
g


# Build expanded data
x <- 1
data_exp <- purrr::map_df(1:nrow(data), function(x){

  # Get info
  vessel_id <- data$vessel_id[x]
  date1 <- data$issue_date[x]
  date2 <- data$expire_date[x]

  # Expand dates
  dates <- seq(date1, date2, by="1 day")

  # Record
  df <- tibble(vessel_id=vessel_id,
               date=dates)

})

# Make data unique
data_exp_uniq <- data_exp %>%
  unique()

# Export
saveRDS(data_exp_uniq, file=file.path(outdir, "trawl_permit_key.Rds"))




# Plot data
################################################################################

# Stats
stats <- data_exp_uniq %>%
  count(vessel_id) %>%
  arrange(desc(n))
data_ordered <-data_exp_uniq %>%
  mutate(vessel_id=vessel_id %>% as.character() %>% factor(., levels=stats$vessel_id)) %>%
  filter(date <= "2021-01-01")


# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.y=element_blank(),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot datav
g <- ggplot(data_ordered, aes(x=date, y=vessel_id)) +
  geom_tile() +
  # Labels
  labs(x="Date", y="Vessel") +
  scale_x_date(breaks = "1 year", date_labels="%Y") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_ca_halibut_vessels.png"),
       width=6.5, height=4, units="in", dpi=600)









