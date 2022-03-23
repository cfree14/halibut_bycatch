

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
list.files(indir)
data_orig <- read.csv(file.path(indir, "VesselPermitExtract.CSV"), as.is=T, na.strings = "")


# Format data
################################################################################

# To-do list
# 1) Complete and harmonize vessel names
# 2) What is CF DOC? Doesn't quite match with ID and NAME yet

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(length_ft=length,
         beam_ft=beam,
         horsepower=horse_power,
         home_port_code=home_port,
         home_port_county=port_county,
         permit=vessel_permit,
         permit_issue_office_code=permit_issue_office,
         reg_issue_office_code=reg_issue_office) %>%
  # Format data
  mutate(permit_issue_date=lubridate::mdy(permit_issue_date),
         reg_issue_date=lubridate::mdy(reg_issue_date),
         effective_date=lubridate::mdy(effective_date)) %>%
  # Format county
  mutate(home_port_county=stringr::str_to_title(home_port_county)) %>%
  # Format permit
  mutate(permit=stringr::str_to_title(permit))

# Inspect
str(data)
freeR::complete(data)

# Inspect
range(data$permit_issue_date, na.rm=T)
table(data$permit)
table(data$home_port_county)
table(data$permit_issue_office_code)
table(data$reg_issue_office_code)


# Vessel key
################################################################################

# Build vessel key
# Length, beam, tonnage, horsepower, passengers appears constant through time
# Home port changes through time
vessel_key <- data %>%
  group_by(vessel_id, vessel_name, length_ft, beam_ft, horsepower, tonnage, passengers) %>%
  summarize(n=n(),
            yr1=min(vessel_year),
            yr2=max(vessel_year)) %>%
  ungroup()

# Confirm that vessel id is unique
anyDuplicated(vessel_key$vessel_id)
freeR::which_duplicated(vessel_key$vessel_id)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "CDFW_2000_2021_permit_data.Rds"))





