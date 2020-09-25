# NETWORK MAPPING
library(networkD3)
library(tidyverse)
library(magrittr)

ucs_sats <- read.csv("data/processed/ucs_sats_corr_0914.csv") %>%
  filter(!grepl("/", opown), !grepl("/", contractor))

# creating nodes df
nodes <- data.frame(name = unique(c(ucs_sats$contractor, ucs_sats$opown))) %>%
  filter(name != "") %>%
  mutate(nodeID = 0:(nrow(.)-1)) 

# creating data frames of total sats for each contractor and operator to join to nodes df
cont_sum <- ucs_sats %>% 
  group_by(contractor) %>%
  summarise(cont_sats = n()) %>%
  slice(2:nrow(.))

op_sum <- ucs_sats %>%
  group_by(opown) %>%
  summarise(op_sats = n()) %>%
  slice(2:nrow(.))

nodes <- nodes %>%
  left_join(cont_sum, by = c("name" = "contractor")) %>%
  left_join(op_sum, by = c("name" = "opown"))

# creating case_when function to determine which node is a contractor, operator or bot
case_func <- function(cont_sats, op_sats) {
  case_when(
    !is.na(cont_sats) & !is.na(op_sats) ~ "both",
    !is.na(op_sats) & is.na(cont_sats) ~ "operator",
    !is.na(cont_sats) & is.na(op_sats) ~ "contractor"
  )
}

# creating new column of type of operator
nodes <- nodes %>%
  mutate(type = case_func(cont_sats, op_sats))

# creating links data frame, counting # of satellites by use type
links <- ucs_sats %>% 
  group_by(contractor, opown, Users) %>%
  summarize(no_sats = n())
  # slice(2:nrow(links))  for some reason this is behaving strangely

links <- links[2:nrow(links), ] %>% # deselecting first row because no opown / contractor data available for a number of sats
  pivot_wider(names_from = Users, values_from = no_sats, values_fill = NA) 

links <- cbind(links, data.frame(num_sats = rowSums(links[3:19], na.rm = TRUE))) %>%
  left_join(nodes[1:2], by = c("opown" = "name")) %>%
  left_join(nodes[1:2], by = c("contractor" = "name"))

names(links)[21:22] <- c("nodeID_op", "nodeID_cont")

p <- forceNetwork(links, nodes, "nodeID_op", "nodeID_cont", "num_sats", "name", Group = "type", Nodesize = "cont_sats", zoom = TRUE, arrows = TRUE, fontFamily = "Proxima Nova", legend = TRUE)
p

# MODERNIZING DATA SET
# writing out CSV to add notes to nodes
# write.csv(nodes, "data/processed/nodes_0925.csv")



  