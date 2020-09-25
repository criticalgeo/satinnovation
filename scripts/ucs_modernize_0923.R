# REPLACING NAMES 09/23 FROM MANUAL SEARCH (MODERNIZE) #

ucs_sats <- read.csv("data/processed/ucs_sats_corr_0914.csv")
replace <- read.csv("data/processed/name_replace_0923.csv")

apply(replace, 1, function(x) {
  ucs_sats[ucs_sats$opown == x[["database_name"]], 2] <<- x[["replacement"]]
  ucs_sats[ucs_sats$contractor == x[["database_name"]], 3] <<- x[["replacement"]]
})

write.csv(ucs_sats, "data/processed/modern_ucssats_0923.csv")
