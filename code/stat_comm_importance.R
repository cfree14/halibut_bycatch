
# Library
library(tidyverse)

# Get data
data_orig <- wcfish::cdfw_ports

# Landings
data <- data_orig %>%
  filter(year>=2010) %>%
  group_by(comm_name) %>%
  summarize(value_usd=sum(value_usd),
         landings_lb=sum(landings_lb)) %>%
  ungroup() %>%
  arrange(desc(landings_lb))


# Rec data
data_orig2 <- wcfish::recfin_cte2

data2 <- data_orig2 %>%
  filter(state=="California" & year>=2010 & status=="Retained") %>%
  group_by(comm_name) %>%
  summarize(catch_n=sum(catch_n),
            catch_mt=sum(catch_mt)) %>%
  ungroup() %>%
  arrange(desc(catch_mt))
