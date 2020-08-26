library(tidyverse) # data wrangling, etc.
library(magrittr) # data wrangling
library(lubridate) # tidying dates
library(googlesheets4)
library(ggplot2)
library(patchwork)
library(RColorBrewer)
library(extrafont)

###################
###   WRANGLE   ###
###################

# NTS: save CSV with known encoding in text editor before import

# load data 
sats <- read.csv("data/raw/01_ucssatellites_0401.csv", encoding = "UTF-8") %>%
  slice(1:(nrow(.)-4))
# select columns containing actor-related and use information to retrieve actor list and assess relationships
sats %<>% select(c(1:7, 16, 19, 21:23)) 
# change column names 
names(sats) <- c("satname", "UNregcountry", "operator_country", "operator", "users", "purpose", "purp_detail", "launch_mass", "launch_date", "contractor", "contractor_country", "launch_site")

sats[, 1:ncol(sats)] <- lapply(sats[, 1:ncol(sats)], trimws) # removing white space

# converting launch mass column to numeric
sats$launch_mass <- as.numeric(gsub(",", "", sats$launch_mass))

# lubridate launch_date column
sats$launch_date <- mdy(sats$launch_date)
sats$launch_year <- as.numeric(substr(as.character(sats$launch_date), 1, 4))


## VISUALIZING RAW DATA
# create palette

myColors <- colorRampPalette(brewer.pal(12, "Set1"))(16)

# Launch Year & Mass Distribution for All Orbiting Satellites (linear scale) 
ggplot(sats) +
  geom_point(aes(x = launch_year, y = launch_mass, color = users)) + 
  scale_color_manual(name = "User", values = myColors) +
  geom_hline(yintercept = 500, linetype = "dashed", size = 0.5, alpha = 0.5) + # adding horizontal threshold for smallsat size
  annotate("text", 1980, 500, label = "max smallsat size (500kg)", family = "Avenir", fontface = "bold", vjust = -1, size = 3) +
  theme_minimal() + 
  xlab("Launch Year") +
  ylab("Launch Mass (kg)") +
  ggtitle("Launch Year & Mass Distribution for All Orbiting Satellites")+ 
  theme(plot.title = element_text(face = "bold", vjust = 7), 
        legend.title = element_text(face = "bold"), 
        axis.title.x = element_text(vjust = -7),
        axis.title.y = element_text(vjust = 7),
        text = element_text(family = "Proxima Nova"),
        plot.margin = unit(c(2, 0.5, 2, 1.5), units = "cm"))
ggsave(filename = "yearmass_linear.jpg", width = 10, height = 7, units = "in", device = "jpeg", dpi = 200)

# " " (log scale)
ggplot(sats) +
  geom_point(aes(x = launch_year, y = launch_mass, color = users)) + 
  scale_color_manual(name = "User", values = myColors) +
  scale_y_log10()+
  geom_hline(yintercept = 500, linetype = "dashed", size = 0.5, alpha = 0.5) + # adding horizontal threshold for smallsat size
  annotate("text", 1980, 500, label = "max smallsat size (500kg)", family = "Avenir", fontface = "bold", vjust = -1, size = 3) +
  theme_minimal() + 
  xlab("Launch Year") +
  ylab("Launch Mass (kg)") +
  ggtitle("Launch Year & Mass Distribution for All Orbiting Satellites")+ 
  theme(plot.title = element_text(face = "bold", vjust = 7), 
        legend.title = element_text(face = "bold"), 
        axis.title.x = element_text(vjust = -7),
        axis.title.y = element_text(vjust = 7),
        text = element_text(family = "Proxima Nova"),
        plot.margin = unit(c(2, 0.5, 2, 1.5), units = "cm"))
ggsave(filename = "yearmass_log.jpg", width = 10, height = 7, units = "in", device = "jpeg", dpi = 200)

# filtering EO satellites
eosats <- sats %>%
  filter(grepl("Earth Observation", purpose))

ggplot(eosats) +
  geom_point(aes(x = launch_year, y = launch_mass, color = users)) + 
  scale_color_manual(name = "User", values = myColors) +
  scale_y_log10()+
  geom_hline(yintercept = 500, linetype = "dashed", size = 0.5, alpha = 0.5) + # adding horizontal threshold for smallsat size
  annotate("text", 1980, 500, label = "max smallsat size (500kg)", family = "Avenir", fontface = "bold", vjust = -1, size = 3) +
  theme_minimal() + 
  xlab("Launch Year") +
  ylab("Launch Mass (kg)") +
  ggtitle("Launch Year & Mass Distribution for Earth Observation Satellites")+ 
  theme(plot.title = element_text(face = "bold", vjust = 7), 
        legend.title = element_text(face = "bold"), 
        axis.title.x = element_text(vjust = -7),
        axis.title.y = element_text(vjust = 7),
        text = element_text(family = "Proxima Nova"),
        plot.margin = unit(c(2, 0.5, 2, 1.5), units = "cm"))
ggsave(filename = "yearmassEO_log.jpg", width = 10, height = 7, units = "in", device = "jpeg", dpi = 200)

### sorting instances into individual categories to reduce visual clutter ###
user_cats <- c("Civil", "Commercial", "Government", "Military") # creating list of unique categories
# creating individual data frames 
lapply(user_cats, function(x) {
  t <- sats %>% 
    filter(grepl(x, users))
  assign(x, t, envir = .GlobalEnv)
})

lapply(user_cats, function(x) {
  temp <- sats %>%
    filter(grepl(x, users))
  
  p <- ggplot(temp) +
    geom_point(aes(x = launch_year, y = launch_mass)) + 
    geom_hline(yintercept = 500, linetype = "dashed", size = 0.5, alpha = 0.5) + # adding horizontal threshold for smallsat size
    annotate("text", 1980, 500, label = "max smallsat size (500kg)", family = "Avenir", fontface = "bold", vjust = -1, size = 3) +
    theme_minimal() + 
    xlab("Launch Year") +
    ylab("Launch Mass (kg)") +
    ggtitle(x) +
    theme(plot.title = element_text(face = "bold", vjust = 7), 
          legend.title = element_text(face = "bold"), 
          axis.title.x = element_text(vjust = -7),
          axis.title.y = element_text(vjust = 7),
          text = element_text(family = "Proxima Nova"),
          plot.margin = unit(c(2, 0.5, 2, 1.5), units = "cm"))
    
    
    
    
})

# Launch Year and Mass Distribution for EO Satellites
# creating list of purposes
purposes <- unique(sats$purpose) # find unique instances of purposes
purposes <- c(purposes[!grepl("/", purposes)], "Navigation", "Education", "Maritime Tracking") # subset out instances with slashes to avoid additional categories, then adding categories that do not appear on their own 
purposes <- purposes[-c(8)] # removing "Educational" because it will be covered by the "Education" category when doing string search

# creating list of binary data frames that indicate string matches
df_list <- lapply(purposes, function(x) {
  
    df <- data.frame(str_count(sats$purpose, x))
    names(df) <- x
    
    return(df)
  })

# joining these data frames with main df 
sats %<>% cbind(do.call(cbind, df_list))

# summarize by PURPOSE & OPERATOR
operators <- sats %>% 
  group_by(operator) %>%
  summarise_at(vars(purposes[1]:purposes[length(purposes)]), sum) %>%
  left_join(sats %>% group_by(operator) %>% summarize(country = first(operator_country))) %>%
  left_join(sats %>% count(operator))

# many repeat names but will deal with this later

# summarize by PURPOSE & CONTRACTOR
contractor <- sats %>%
  group_by(contractor) %>%
  summarise_at(vars(purposes[1]:purposes[length(purposes)]), sum) %>%
  left_join(sats %>% group_by(contractor) %>% summarize(country = first(contractor_country))) %>%
  left_join(sats %>% count(contractor))

###################
####   WRITE   ####
###################

# write data to project folder
write.csv(operators, "data/processed/ucs_operators.csv")

# write data to Google sheet
url <- "https://docs.google.com/spreadsheets/d/1D5ljlCuC3n-f5Of4l1aMkOPWRsnQ3uFZkAXvdW4GVdA/edit#gid=0"

sheet_write(operators, url, sheet = "Operators")
