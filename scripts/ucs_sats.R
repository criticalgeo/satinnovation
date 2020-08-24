library(tidyverse) # data wrangling, etc.
library(magrittr) # data wrangling
library(lubridate) # tidying dates
library(googlesheets4)

###################
###   WRANGLE   ###
###################

# NTS: save CSV with known encoding in text editor before import

# load data 
sats <- read.csv("data/raw/01_ucssatellites0401.csv", encoding = "UTF-8") 
# select columns containing actor-related and use information to retrieve actor list and assess relationships
sats %<>% select(c(1:7, 19, 21:23)) 
# change column names 
names(sats) <- c("satname", "UNregcountry", "operator_country", "operator", "users", "purpose", "purp_detail", "launch_date", "contractor", "contractor_country", "launch_site" )

# lubridate launch_date column
sats$launch_date <- mdy(sats$launch_date)

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

# summarize by purpose & operator
operators <- sats %>% 
  group_by(operator) %>%
  summarise_at(vars(purposes[1]:purposes[length(purposes)]), sum) %>%
  left_join(sats %>% group_by(operator) %>% summarize(country = first(operator_country)))

operator_names <- operators$operator
unique(sats$purpose)
purposes

test <- tidy_comb_all(operators, operator) %>% tidy_stringdist()
test <- test[test$osa < 25, ]

url <- "https://docs.google.com/spreadsheets/d/1D5ljlCuC3n-f5Of4l1aMkOPWRsnQ3uFZkAXvdW4GVdA/edit#gid=0"
