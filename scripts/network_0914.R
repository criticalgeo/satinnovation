# NETWORK MAPPING
library(networkD3)
library(tidyverse)
library(magrittr)

ucs_sats <- read.csv("data/processed/ucs_sats_corr_0914.csv")

# creating nodes df
nodes <- data.frame(unique(c(ucs_sats$contractor, ucs_sats$opown)))

# creating links data frame, counting # of satellites by use type
links <- ucs_sats %>% 
  group_by(contractor, opown, Users) %>%
  summarize(no_sats = n())
  # slice(2:nrow(links))  for some reason this is behaving strangely

links <- links[2:nrow(links), ] %>% # deselecting first row because no opown / contractor data available for a number of sats
  pivot_wider(names_from = Users, values_from = no_sats, values_fill = NA) 

links <- cbind(links, data.frame(num_sats = rowSums(links[3:20], na.rm = TRUE)))



