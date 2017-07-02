## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## 
## Copyright (C) 2017 by Maciej Andziński <m.andzinski@gmail.com>
##
## Licensed under GNU GPL v3, <https://www.gnu.org/licenses/gpl-3.0.html>
##
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# ---- required modules ----

require(foreign)
require(stringi)
require(dplyr)


# ---- function definitions ----

#
# DESCRIPTION
# This function converts data frame columns into factors, except for columns names' matching given pattern
#
# ARGUMENTS
# d - a data frame
# pattern - a character vector with regex (default: .)
#
# RETURN VALUE
# Data frame d
#
format_data <- function(d, pattern=".") {
  for(k in names(d)) {
    if(!any(stri_detect_regex(k, pattern))) {
      d[,k] <- factor(d[,k]) 
    }
  }
  d
}


#
# DESCRIPTION
# This function finds the most popular values in a data frame columns
#
# ARGUMENTS
# d - a data frame
# fields - a character vector with columns' names (default: all column names of data frame d)
#
# RETURN VALUE
# A named list of lists. List names correspond to fields argument value, each list element contains a list of 2 elements: first is the most frequent value, second is the popularity (0 to 1, a fraction of total number of rows in d)
# 
get_frequent_values <- function(d, fields=names(d)) {
  values = list()
  for(field in fields){
    freq <- sort(table(d[,field], useNA = "ifany"), decreasing = T)[1]/nrow(d)
    values[[field]] <- list(names(freq),unname(freq))
  }
  values
}


#
# DESCRIPTION
# This function checks whether the most popular values returned by get_frequent_values() function are popular egnough (popularity higher than p)
#
# ARGUMENTS
# freq_val - a list of frequent values' lists returned by get_frequent_values() function
# p - popularity (default: 0.5)
#
# RETURN VALUE
# Returns TRUE if all values have popularity higher than p, otherwise returns FALSE
# 
check_frequent_values <- function(freq_val, p=0.5) {
  for(field in names(freq_val)) {
    if(!freq_val[[field]][[2]]>p) {
      return(FALSE)
    }
  }
  TRUE
}


#
# DESCRIPTION
# This function classifies observations (hijack vs no hijack).
#
# ARGUMENTS
# d - a data frame
# freq_val - a list of frequent values' lists returned by get_frequent_values() function
#
# RETURN VALUE
# Data frame d enriched with four columns - two numeric vectors: hijack, hijack_bin and two factors: hijack_f, hijack_bin_f
#
mark_data <- function(d, freq_val) {
  i=0
  d$hijack <- 0
  for(field in names(freq_val)) {
    v <- freq_val[[field]][[1]]
    if(is.na(v)) {
      diff_val <- which(!is.na(d[,field]))
    } else {
      diff_val <- which(d[,field]!=v)
    }
    d[diff_val,]$hijack = d[diff_val,]$hijack + 2^i
    i = i + 1
  }
  d$hijack_bin <- d$hijack
  d[which(d$hijack>0),]$hijack_bin <- 1
  d$hijack_f <- as.factor(d$hijack)
  d$hijack_bin_f <- as.factor(d$hijack_bin)
  d
}


# ---- data processing ----

# load and format data
df_google <- format_data(read.arff("../measurements/datasets/google.arff"), c("_rt$","^ping_min$"))
df_opendns <- format_data(read.arff("../measurements/datasets/opendns.arff"), c("_rt$","^ping_min$"))

# load fields' names taken into consideration in marking process
fields2mark <- readLines("../measurements/datasets/fields2mark")

# for given columns get frequent values
google_freq_val <- get_frequent_values(df_google, fields2mark)
opendns_freq_val <- get_frequent_values(df_opendns, fields2mark)

# mark observations
df_google_marked <- mark_data(df_google, google_freq_val)
df_opendns_marked <- mark_data(df_opendns, opendns_freq_val)

# check frequent values
check_frequent_values(google_freq_val)
check_frequent_values(opendns_freq_val)

# save data
write.arff(x = df_google_marked, file="../measurements/datasets/google-marked.arff")
write.arff(x = df_opendns_marked, file="../measurements/datasets/opendns-marked.arff")
