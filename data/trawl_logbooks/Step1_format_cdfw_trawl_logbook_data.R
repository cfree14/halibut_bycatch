

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/trawl_logbooks/raw"
outdir <- "data/trawl_logbooks/processed"
plotdir <- "data/trawl_logbooks/figures"

# Read data
list.files(indir)
data_orig <- read.csv(file.path(indir, "MLS_TrawlExtract_03182022.csv"), na.strings = "")



# Helper function
################################################################################

# Function to collapse repeated dates
collapse_dates <- function(x){
  # Break and unique
  z <- purrr::map_chr(x, function(x){
    y <- str_split(x, pattern=", ", simplify = T) %>% as.character() %>% unique()
  })
  return(z)
}
collapse_dates(x=c("2010-02-11, 2010-02-11", "2010-02-18, 2010-02-18, 2010-02-18"))

# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(logbook_id=serial_number,
         vessel=vessel_name,
         return_date=return_date_string,
         set_lat_dd=set_latitude,
         set_long_dd=set_longitude,
         up_lat_dd=up_latitude,
         up_long_dd=up_longitude,
         depth_avg_fathoms=average_depth,
         catch_lb=pounds,
         catch_lb_conv=converted_pounds,
         spp_code=species_code,
         pacfin_code=pac_fin_species_code,
         value_usd=revenue,
         efp_trip_yn=is_efp_trip,
         observed_trip_yn=is_observed_trip) %>%
  # Convert return/departure dates
  mutate(departure_date=lubridate::mdy(departure_date),
         return_date=lubridate::mdy(return_date)) %>%
  # Build tow date
  mutate(tow_date=paste(tow_year, tow_month, tow_day, sep="-") %>% lubridate::ymd()) %>%
  # Fix and convert landing dates
  mutate(landing_date=collapse_dates(x=landing_date),
         landing_date=lubridate::ymd(landing_date)) %>%
  # Fix longitudes
  mutate(set_long_dd=set_long_dd*-1,
         up_long_dd=up_long_dd*-1) %>%
  # Format net type
  mutate(net_type=recode(net_type,
                         "B"="Bottom",
                         "D"="Danish or Scottish seine",
                         "F"="Selective flatfish",
                         "L"="Large footrope",
                         "M"="Midwater",
                         "S"="Small footrope")) %>%
  # Format region
  mutate(region=recode_factor(region,
                               "N"="North (above 40°10'N)",
                               "NC"="North Central (36° to 40°10'N)",
                               "C"="Central (34°27'N to 36°N)",
                               "S"="Southern (below 34°27'N)")) %>%
  # Arrange
  select(logbook_id:return_port_code,
         tow_date, tow_month, tow_day, tow_year, everything())

# Inspect
str(data)
freeR::complete(data)

# Inspect
range(data$tow_date)
table(data$net_type)
table(data$target_strategy)
table(data$observed_trip_yn)
table(data$efp_trip_yn)
table(data$region)

# World
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Plot points
ggplot() +
  # Country
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2, inherit.aes=F) +
  geom_sf(data=mexico, fill="grey80", color="white", lwd=0.2, inherit.aes=F) +
  # Tows
  geom_point(data=data, mapping=aes(x=set_long_dd, y=set_lat_dd), size=0.1, color="red") +
  geom_point(data=data, mapping=aes(x=up_long_dd, y=up_lat_dd), size=0.1, color="blue") +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(y=c(30, 45), x=c(-110, -130)) +
  # Theme
  theme_bw()


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "CDFW_2000_2020_trawl_logbook_data.Rds"))


