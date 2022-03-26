

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/gillnet_logbooks/raw"
outdir <- "data/gillnet_logbooks/processed"
plotdir <- "data/gillnet_logbooks/figures"

# Read data
list.files(indir)
data_orig <- readxl::read_excel(file.path(indir, "GillnetLogs_2000-2020.xlsx"))


# Format data
################################################################################

# Can fill in gaps in vessel info
# Can fix up species info
# Look at keys

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(vessel=vessel_name,
         fg_id=fish_game_receipt_no,
         boat_id=boatno,
         date=fishing_date,
         target_spp_code=tarspc,
         soak_duration_hr=hours_net_soaked,
         block_id=fg_blocks,
         depth_fa=depths,
         net_length_ft=net_length,
         mesh_size_in=mesh_size,
         line_depth_fa=bouy_line_depth,
         species=common_name,
         species_code=mlds_species_code,
         catch_n=num_catch,
         catch_lb=weights) %>%
  # Format status
  mutate(status=tolower(status)) %>%
  # Format predator
  mutate(predator=predator %>% gsub("_", " ", .) %>% stringr::str_to_sentence(.),
         predator=recode(predator,
                         "Harbor"="Harbor seal",
                         "Mako"="Mako shark",
                         "Sea lion - hagfish"="Sea lion, hagfish",
                         "Seals"="Seal"))

# Inspect
str(data)
freeR::complete(data)

# Inspect
range(data$date, na.rm=T)
table(data$drift_set)
table(data$status)
table(data$predator)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "2000_2020_gillnet_logbook_data.Rds"))

