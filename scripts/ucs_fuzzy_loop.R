# PROGRAM TO PERFORM FUZZY NAME MATCHING IN UCS SATELLITE DATABASE #

library(tidyverse) # data wrangling & viz
library(magrittr) # data wrangling
library(tidystringdist) # calculating string distance

# load data
data <- "https://gist.githubusercontent.com/janicekchen/e0635c2b95e44d2e9613add8ab5d4013/raw/83516552c676cc8f907404d27676594a1f0be5ee/01_ucssatellites_0401.csv"
ucs_sats <- read.csv(data) %>% # load data 
  select(opown = "Operator.Owner", contractor = "Contractor", opown_country = "Country.of.Operator.Owner", contractor_country = "Country.of.Contractor") # select operator/owner & contractor column

# split country columns by "/" (for country verification later)
split <- str_split(ucs_sats[, 3], "/") # creates list of string splits
n.obs <- sapply(split, length) # creates list of integers showing length of each list item
seq.max <-seq_len(max(n.obs)) # generates consecutive sequence based on max number of observations
splitdf <- as.data.frame(t(sapply(split, "[", i = seq.max)))
names(splitdf) <- c("oc1", "oc2", "oc3", "oc4", "oc5")
ucs_sats %<>% cbind(splitdf)

split <- str_split(ucs_sats[, 4], "/") # creates list of string splits
n.obs <- sapply(split, length) # creates list of integers showing length of each list item
seq.max <-seq_len(max(n.obs)) # generates consecutive sequence based on max number of observations
splitdf <- as.data.frame(t(sapply(split, "[", i = seq.max)))
names(splitdf) <- c("cc1", "cc2", "cc3", "cc4", "cc5")
ucs_sats %<>% cbind(splitdf)

ucs_sats %<>% select(-c(3:4))

# writing function to print number of unique names in each column to check progress of fuzzy matching
numuniq <- function(x) {
  c(length(unique(ucs_sats$opown)), length(unique(ucs_sats$contractor)))
  
}

# checking current number of unique entries 
numuniq()

#' 
#' First, I will remove all punctuation, with the exception of parentheses and slashes. I will preserve the slashes in order to maintain partnerships between organizations in the data (e.g. NASA/Johns Hopkins). I will preserve the parentheses in order to remove abbreviations of organizations later (e.g. (NASA)). I will also trim all white space and multiple spaces. 
#' 
#' 
# removes all punctuation with the exception of forward slashes and parentheses
ucs_sats[] <- lapply(ucs_sats, function(x) {
  gsub("([\\(\\)//])|[[:punct:]]", "\\1", x) 
  })
numuniq()

# remove all content in parentheses, including parentheses
ucs_sats[] <- lapply(ucs_sats, function(x) gsub ("\\s*\\([^\\)]+\\)", "", x))
numuniq()

# trim white space
ucs_sats[, 1:ncol(ucs_sats)] <- lapply(ucs_sats[, 1:ncol(ucs_sats)], trimws)
ucs_sats[, 1:ncol(ucs_sats)] <- lapply(ucs_sats[, 1:ncol(ucs_sats)], function(x) gsub("\\s+", " ", x))
numuniq()

# removing all instances of "/ ", replacing with "/"
ucs_sats[] <- lapply(ucs_sats, function(x) gsub ("/ ", "/", x))
numuniq()

# removing all instances of "Inc, Incorporated, Ltd, Limited"
list <- c(" Inc", " Incorporated", " Ltd", " Limited", " Corporation", " Corp")
for (i in list) {
  ucs_sats[] <- lapply(ucs_sats, function(x) gsub (i, "", x))
}

numuniq()

#' 
#' Now, I will create a data frame that lists all possible combinations of names and then calculate string distance.
#' 

cont_comb <- tidy_comb_all(unique(ucs_sats$contractor))
op_comb <- tidy_comb_all(unique(ucs_sats$opown))

cont_dist <- tidy_stringdist(cont_comb) %>%
  filter(osa != 0) %>%
  inner_join(ucs_sats[c(2, 8:12)], by = c("V1" = "contractor")) %>% # joining country columns for additional criterion for evaluation
  inner_join(ucs_sats[c(2, 8:12)], by = c("V2" = "contractor"))

names(cont_dist)[13:17] <- c("V11c", "V12c", "V13c", "V14c", "V15c") # renaming columns
names(cont_dist)[18:22] <- c("V21c", "V22c", "V23c", "V24c", "V25c")

op_dist <- tidy_stringdist(op_comb) %>%
  filter(osa != 0) %>%
  left_join(ucs_sats[c(1, 3:7)], by = c("V1" = "opown")) %>%
  left_join(ucs_sats[c(1, 3:7)], by = c("V2" = "opown"))

names(op_dist)[13:17] <- c("V11c", "V12c", "V13c", "V14c", "V15c") # renaming columns
names(op_dist)[18:22] <- c("V21c", "V22c", "V23c", "V24c", "V25c")

dist <- rbind(cont_dist, op_dist) # combining the data frames
dist <- dist[!duplicated(dist[, 1:2]), ] # removing duplicates

# decide on cosine similarity threshold and then filter all rows according to this criteria 
# if country is the same, cosine is less than threshold, and osa is less than 20, then replace V1 with V2 
# if country is the same, cosine is less than threshold and osa is more than 20, trigger ask yes/no

cosine010_us <- dist %>%
  filter(cosine < 0.10, !grepl("/", V1), !grepl("/", V2)) # removing slashes to retrieve single actors (!!RECALCULATE STRING DIST AFTER NAME REPLACEMENTS FOR SLASHES!! because string replacements in the single actor process may have fixed differences in multiactors)

# sort alphabetically 
for (i in 1:nrow(cosine010_us)) {
  cosine010_us[i, 1:2] <-  sort(cosine010_us[i, 1:2])
}
  
cosine010_us <- cosine010_us[!duplicated(cosine010_us), ]

automatched <- data.frame(V1 = character(0), V2 = character(0)) # creating empty data frame that will be populated in for loop with names that have been matched automatically (for verification later)
nomatch <- data.frame(V1 = character(0), V2 = character(0))

for (i in 1:nrow(cosine010_us)) {
  
  countryset1 <- as.character(str_to_lower(cosine010_us[i, 13:17])) # create vector of V1 countries
  countryset1 <- countryset1[!is.na(countryset1)] # remove NA values from vector
  countryset2 <- as.character(str_to_lower(cosine010_us[i, 18:22])) # create vector of V2 countries
  countryset2 <- countryset2[!is.na(countryset2)] # remove NA values
  
  matchcountry <- length(Reduce(intersect, list(countryset1, countryset2))) # find names that are in both vectors, and calculate length — if length is greater than 0, then there is a country match
  
  if (matchcountry > 0) {
    if (cosine010_us[i, "osa"] < 10) {
      ucs_sats[grep(cosine010_us$V2[i], ucs_sats$opown), 1] <- cosine010_us$V1[i]
      ucs_sats[grep(cosine010_us$V2[i], ucs_sats$contractor), 2] <- cosine010_us$V1[i]
      
      cosine010_us[grep(cosine010_us$V2[i], cosine010_us$V1), 1] <- cosine010_us$V1[i] # changing all other instances of V2 in V1 in order to be consistent about changes 
      
      df <- data.frame(V1 = cosine010_us$V1[i], V2 = cosine010_us$V2[i])
      automatched %<>% rbind(df)
      
    } 
    else {
      print(paste(cosine010_us$V1[i], "," , cosine010_us$V2[i], countryset1, "||", countryset2, cosine010_us$V2country[i]))
      t <- askYesNo("Swap values?", default = FALSE)
      # write Yes if Swap, No if want to replace with another value and Cancel if no changes need to be made
      if (is.na(t)) {
        next
      } 
      else if (t == TRUE) {
        ucs_sats[grep(cosine010_us$V2[i], ucs_sats$opown), 1] <- cosine010_us$V1[i]
        ucs_sats[grep(cosine010_us$V2[i], ucs_sats$contractor), 2] <- cosine010_us$V1[i]
        
        cosine010_us[grep(cosine010_us$V2[i], cosine010_us$V1), 1] <- cosine010_us$V1[i]
      } 
      else if (t == FALSE) {
        u <- readline("Replace with something else? Write replacement here or write N:")
        if (u == "N") {
          next
        } else {
          ucs_sats[grep(cosine010_us$V2[i], ucs_sats$opown), 1] <- u
          ucs_sats[grep(cosine010_us$V2[i], ucs_sats$contractor), 2] <- u
          ucs_sats[grep(cosine010_us$V1[i], ucs_sats$opown), 1] <- u
          ucs_sats[grep(cosine010_us$V1[i], ucs_sats$contractor), 2] <- u
          
          cosine010_us[grep(cosine010_us$V1[i], cosine010_us$V1), "V1"] <- u
          cosine010_us[grep(cosine010_us$V2[i], cosine010_us$V1), "V1"] <- u
        }
      }
    }
  } 
  else {
    df <- data.frame(V1 = cosine010_us$V1[i], V2 = cosine010_us$V2[i])
    nomatch %<>% rbind(df)
  }
}

# MANUAL CHECK
df <- as.data.frame(unique(c(ucs_sats$contractor, ucs_sats$operator)))
alenia <- c("Alcatel Alenia Space", "Alcatel Space Industries", "Alenia Aerospazio", "Alenia Spazio")
boeing <- c("Boeing Defense and Space", "Boeing Satellite Development Center", "Boeing Satellite Systems")
cast <- c("China Academy of Space Technology", "Chinese Research Institute for Space Technology", "China Academy of Space Technology \\(CAST")
institute <- c("Institute of Space and Aeronautical Science", "Institute of Space and Aeronautical Science University of Tokyo")
nasa <- c("Goddard Space Flight Center", "National Aeronautics and Space Administration", "NASA/NASA", "NASA Jet Propulsion Laboratory", "NASA JPL", "NASA Earth Science Technology Office", "NASA Earth Science Enterprise", "NASAAmes Research Center", "NASA/Ames Research Center", "	
NASA Langley Research Center")

manualReplace <- function(list, correction) {
  for (x in list) {
    ucs_sats[] <<- lapply(ucs_sats, function(i) gsub(x, correction, i))
  }
}

manualReplace(boeing, "Boeing")
manualReplace(alenia, "Thales Alenia Space")
manualReplace(c("Ball Aerospace and Technologies"), "Ball Aerospace")
manualReplace(cast, "CAST")
manualReplace(c("EADS Space"), "EADS Astrium")
manualReplace(c("GomSpace ApS"), "GomSpace")
manualReplace(institute, "Institute of Space and Astronautical Science")
manualReplace(nasa, "NASA")
manualReplace("National Reconnaissance Laboratory", "National Reconnaissance Office")
manualReplace("INVAP SE", "INVAP")
ucs_sats[1726, 1] <- "Johns Hopkins Applied Physics Laboratory"

# examining automatched data frame to check for matches that should not have happened
wrongmatch <- automatched %>%
  slice(c(3, 9, 21, 24, 34:35))

# Shanghai Academy of Space Flight Technology -> Shanghai Academy of Satellite Technology
# Nihon University -> Nahon University
# Raytheon -> Ratheon
# Herzliya Science Center -> Herliya Science Center


# reading in database with all variables
ucs_satsfull <- read.csv("data/processed/01_ucssatellites_0401.csv")

# joining original data base with corrected name database
ucs_sats_corrected <- cbind(ucs_sats[1:12], ucs_satsfull[c(1:2, 4, 5:21, 23:36)])

# finding wrong matches and correcting them
apply(wrongmatch, 1, function(x) {
  rowno <- grep(x[["V2"]], ucs_sats_corrected$Contractor)
  ucs_sats_corrected[rowno, "contractor"] <<- x[["V2"]]
  
  rowno <- grep(x[["V2"]], ucs_sats_corrected$Operator.Owner)
  ucs_sats_corrected[rowno, "opown"] <<- x[["V2"]]
  
})

# correcting other incorrect names
namecorrect <- data.frame(
  correct = c("Shanghai Academy of Space Flight Technology",
    "Nihon University",
    "Raytheon",
    "Herzliya Science Center"),
  wrong = c("Shanghai Academy of Satellite Technology",
    "Nahon University",
    "Ratheon",
    "Herliya Science Center"
    )
)

apply(namecorrect, 1, function(x) {
  ucs_sats_corrected[] <<- lapply(ucs_sats_corrected, function(i) {
    gsub(x[["wrong"]], x[["correct"]], i)
  })
})

# filtering out duplicate contractor & op-owner columns
ucs_sats_corrected %<>% select(-c("Contractor", "Operator.Owner"))
write.csv(ucs_sats_corrected, "data/processed/ucs_sats_corr_0914.csv")

