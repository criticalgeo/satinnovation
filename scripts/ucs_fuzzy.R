# FUZZY NAME MATCHING FOR UCS SATELLITE DATABASE
library(tidyverse) # data wrangling & viz
library(magrittr) # data wrangling
library(tidystringdist)



ucs_sats <- read.csv("data/raw/01_ucssatellites_0401.csv") %>% # load data 
  select(opown = "Operator.Owner", contractor = "Contractor", opown_country = "Operator.Owner.Country", contractor_country = "Contractor.Country") # select operator/owner & contractor column

# writing function to print number of unique names in each column to check progress of fuzzy matching
numuniq <- function(x) {
  c(length(unique(ucs_sats$opown)), length(unique(ucs_sats$contractor)))
}
# numuniq()
# 543 unique op/owners & 472 unique contractors

# removes all punctuation with the exception of forward slashes and parentheses
ucs_sats[] <- lapply(ucs_sats, function(x) {
  gsub("([\\(\\)//])|[[:punct:]]", "\\1", x) 
  })
# numuniq()
#541 468

# remove all line breaks
ucs_sats[] <- lapply(ucs_sats, function(x) {
  gsub("\n", "", x)
})
#541 468

# remove all content in parentheses, including parentheses
ucs_sats[] <- lapply(ucs_sats, function(x) gsub ("\\s*\\([^\\)]+\\)", "", x))
# numuniq()
# 536, 460

# 536, 460

# trim white space
ucs_sats[, 1:ncol(ucs_sats)] <- lapply(ucs_sats[, 1:ncol(ucs_sats)], trimws)
ucs_sats[, 1:ncol(ucs_sats)] <- lapply(ucs_sats[, 1:ncol(ucs_sats)], function(x) gsub("\\s+", " ", x))
# numuniq()
#530, 448

# calculating string dist between all permutations for operators
op_comb <- tidy_comb_all(unique(ucs_sats$opown)) # first creating data frame of name combinations
cont_comb <- tidy_comb_all(unique(ucs_sats$contractor))

op_dist <- tidy_stringdist(op_comb) %>%
  filter(osa != 0)
cont_dist <- tidy_stringdist(cont_comb) %>%
  filter(osa != 0)

# filtering rows with a soundex value of 0 and an osa value < 5
# also removing rows with slashes (to compare later)
osa_sound_op <- op_dist %>%
  filter(soundex == 0 & osa < 5) %>%
  filter(!grepl("/", V1), !grepl("/", V2)) 

# examining these results reveal only rows 10, 13, and 17 as viable matches
# slicing these rows 
osa_sound_op %<>% slice(c(10, 13, 17))

# left column are all good replacements, so will replace all instances of names in right column with those in the left
for (i in 1:nrow(osa_sound_op)) {
  ucs_sats[grep(osa_sound_op$V2[i], ucs_sats$opown), 1] <- osa_sound_op$V1[i]
  ucs_sats[grep(osa_sound_op$V2[i], ucs_sats$contractor), 2] <- osa_sound_op$V1[i]
}
  
# looking at rows with osa value < 5 and soundex value of 1 to see if there are viable matches
osa_op <- op_dist %>%
  filter(soundex == 1 & osa < 5) %>%
  filter(!grepl("/", V1), !grepl("/", V2)) 
# there is nothing! 

