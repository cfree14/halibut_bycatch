

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/cdfw_obs/raw"
outdir <- "data/cdfw_obs/processed"
plotdir <- "data/cdfw_obs/figures"

# Read data
list.files(indir)
data_orig1 <- read.csv(file.path(indir, "GNC8389.csv"), as.is=T, na.strings="")
data_orig2 <- read.csv(file.path(indir, "GNM8389.csv"), as.is=T, na.strings="")
data_orig3 <- read.csv(file.path(indir, "GNS8389.csv"), as.is=T, na.strings="")

# Read species key
spp_key <- readRDS("data/cdfw_species/processed/CDFW_species_key.Rds")


# Format keys
################################################################################

# Read port key
port_key_orig <- read.csv(file.path(indir, "CDFW_port_codes.csv"), as.is=T)

# Format port key
port_key <- port_key_orig %>%
  mutate(major_port=stringr::str_to_title(major_port),
         port=stringr::str_to_title(port))

sort(unique(port_key$major_port))


# Format data 1
################################################################################

# Format data
data1 <- data_orig1 %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(fg_id=fgno,
         set_id=setno,
         spp_code_chr=species,
         n_caught=tcat,
         n_discarded_dead=ddead,
         n_discarded_alive=dlive,
         n_damaged=nodam,
         n_kept=nokept,
         n_measured=nokeptsl,
         n_sold=nosold) %>%
  # Format date
  mutate(date=lubridate::ymd(date)) %>%
  # Convert to numeric
  mutate(n_discarded_alive=as.numeric(n_discarded_alive),
         n_damaged=as.numeric(n_damaged),
         n_kept=as.numeric(n_kept)) %>%
  # Add species name
  left_join(spp_key %>% select(spp_code_chr, comm_name_orig), by="spp_code_chr") %>%
  # Arrange
  select(date, fg_id, set_id, spp_code_chr, comm_name_orig, everything()) %>%
  # Check data
  mutate(n_caught_calc=n_discarded_dead+n_discarded_alive+n_kept+n_measured+n_sold, # n_damaged is redundant
         n_check=n_caught-n_caught_calc) %>%
  select(-c(n_caught_calc, n_check))

# Inspect
str(data1)
freeR::complete(data1)

# Data
range(data1$date)

# File links
sort(unique(data1$m_file_link))
sort(unique(data1$s_file_link))

# Inspect unmatched species
data1_spp <- data1 %>%
  # Unique species
  select(spp_code_chr, comm_name_orig) %>%
  unique() %>%
  # Reduce to unmatched species
  filter(is.na(comm_name_orig)) %>%
  # Arrange
  arrange(spp_code_chr)
data1_spp$spp_code_chr

# Export unmatched
write.csv(data1_spp, file=file.path(outdir, "CDFW_1983_1989_gillnet_observer_data_unmatched_spp.csv"), row.names = F)

# Export
save(data1, file=file.path(outdir, "CDFW_1983_1989_gillnet_observer_data.Rds"))


# Format data 2
################################################################################

# Format data
data2 <- data_orig2 %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(fg_id=fgno,
         set_id=setno,
         spp_code_chr=species,
         fork_length_mm=length,
         total_length_mm=altlen,
         disposition=disp) %>%
  # Format date
  mutate(date=lubridate::ymd(date)) %>%
  # Format sex
  mutate(sex=recode(sex,
                    "0"="unknown",
                    "1"="male",
                    "2"="female")) %>%
  # Add species name
  left_join(spp_key %>% select(spp_code_chr, comm_name_orig)) %>%
  # Arrange
  select(date, fg_id, set_id, spp_code_chr, comm_name_orig, everything())


# Inspect
str(data2)
freeR::complete(data2)

# Inspect
sort(unique(data2$sex))
sort(unique(data2$mat))
sort(unique(data2$disp))

# Inspect unmatched species
data2_spp <- data2 %>%
  # Unique species
  select(spp_code_chr, comm_name_orig) %>%
  unique() %>%
  # Reduce to unmatched species
  filter(is.na(comm_name_orig)) %>%
  # Arrange
  arrange(spp_code_chr)
data2_spp$spp_code_chr

# Export unmatched
write.csv(data2_spp,
          file=file.path(outdir, "CDFW_1983_1989_gillnet_observer_length_comps_unmatched_spp.csv"),
          row.names = F)

# Export
save(data2, file=file.path(outdir, "CDFW_1983_1989_gillnet_observer_length_comps.Rds"))


# Format data 3
################################################################################

# Format data
data3 <- data_orig3 %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(set_id=setno,
         fg_id=fgno,
         complete_yn=complete,
         obs_type=otype,
         port_depart_code=dport,
         port_landing_code=lport,
         target_spp_code=tspec,
         lat_loran=lat,
         long_loran=long,
         net_orientation=norient,
         environment=environ,
         bottom_depth_fa=bdepth,
         time_pull=ptime,
         duration=setdur,
         net_type=ntype,
         net_material=nmat,
         net_length=nlen,
         net_depth=ndepth,
         mesh_size1_in=msize1,
         mesh_size2_in=msize2,
         hanging_length_in=hlength,
         suspender_length_ft=slength,
         extender_length_ft=elength) %>%
  # Format date
  mutate(date=lubridate::mdy(date)) %>%
  # Formate complete
  mutate(complete_yn=recode(complete_yn,
                            "1"="yes", "0"="no")) %>%
  # Format observation type
  mutate(obs_type=recode(obs_type,
                         "1"="prearranged",
                         "2"="opportune on board",
                         "3"="opportune not on board",
                         "4"="opportune at sea",
                         "5"="prearranged at sea")) %>%
  # Format net orientation
  mutate(net_orientation=recode(net_orientation,
                                "1"="parallel",
                                "2"="perpendicular",
                                "3"="diagonal",
                                "4"="other",
                                "5"="?")) %>%
  # Format net material
  mutate(net_material=recode(net_material,
                             "1"="monofilament",
                             "2"="multifilament",
                             "3"="combination",
                             "4"="twisted mono")) %>%
  # Format target species key
  mutate(target_spp_code=stringr::str_pad(target_spp_code, width=3, pad="0", side="left")) %>%
  # Add species name
  left_join(spp_key %>% select(spp_code_chr, comm_name_orig), by=c("target_spp_code"="spp_code_chr")) %>%
  rename(target_spp_orig=comm_name_orig) %>%
  # Add port names
  left_join(port_key %>% select(port_id, port), by=c("port_depart_code"="port_id")) %>%
  rename(port_depart=port) %>%
  left_join(port_key %>% select(port_id, port), by=c("port_landing_code"="port_id")) %>%
  rename(port_landing=port) %>%
  # Arrange
  select(date:obs_type,
         port_depart_code, port_depart,
         port_landing_code, port_landing,
         target_spp_code, target_spp_orig, everything())

# Inspect
str(data3)
freeR::complete(data3)

# Export
save(data3, file=file.path(outdir, "CDFW_1983_1989_gillnet_observer_set_info.Rds"))

