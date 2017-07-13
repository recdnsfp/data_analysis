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
# This function finds modes (the most frequent values) in data frame columns
#
# ARGUMENTS
# d - a data frame
# fields - a character vector with columns' names (default: all column names of data frame d)
# min_freq - a numeric vector with minimal frequency or NA if no minimal frequency is defined (default: NA)
#
# RETURN VALUE
# A named list of lists. List names correspond to fields argument value, each list element contains a list of 2 elements: first is the most frequent value, second is the popularity (0 to 1, a fraction of total number of rows in d)
#
get_modes <- function(d, fields=names(d), min_freq=NA) {
  values = list()
  for(field in fields){
    freq <- sort(table(d[,field], useNA = "ifany"), decreasing = T)[1]/nrow(d)
    if(is.na(min_freq) | freq>=min_freq) {
      values[[field]] <- list(names(freq),unname(freq))
    }
  }
  values
}




#
# DESCRIPTION
# This function classifies observations (hijack vs no hijack). An observation is marked with hijack=TRUE if the number of deviations is equal or higher than squared root of length(modes)
#
# ARGUMENTS
# d - a data frame
# modes - a list of frequent values' lists returned by get_modes() function
#
# RETURN VALUE
# Data frame d enriched with hijack column (TRUE/FALSE)
#
mark_data <- function(d, modes) {
  d$deviations <- 0
  for(field in names(modes)) {
    v <- modes[[field]][[1]]
    if(is.na(v)) {
      diff_val <- which(!is.na(d[,field]))
    } else {
      diff_val <- which(d[,field]!=v)
    }
    d[diff_val,]$deviations = d[diff_val,]$deviations + 1
  }
  d$hijack = d$deviations >= sqrt(length(modes))
  d
}



#
# DESCRIPTION
# This function returns deviations for given probe_id.
#
# ARGUMENTS
# d - a data frame
# probe_id - a numeric vector
# modes - a list of frequent values' lists returned by get_modes() function
#
# RETURN VALUE
# A data frame with deviations from the modes
#
get_deviations <- function(d, probe_id, modes) {
  p_id <- probe_id
  r <- filter(d, probe_id==p_id)
  stopifnot(nrow(r)==1)
  results <- rbind(data.frame(x="mode"),data.frame(x="deviation"))
  for(field in names(modes)) {
    v <- unlist(r[field])
    m <- modes[[field]][[1]]
    #cat(paste(field, unlist(r[field]), "\n"))
    if(is.na(v) & is.na(m)) next
    if((is.na(v) & !is.na(m)) | (!is.na(v) & is.na(m)) | v != m) {
      nr <- rbind( data.frame(tmp=m), data.frame(tmp=v) )
      names(nr) <- field
      results <- cbind(results, nr)
    }
  }
  results
}



#
# DESCRIPTION
# This function resturns probe_id for observations marked as hijack.
#
# ARGUMENTS
# d - a data frame with observations
#
# RETURN VALUE
# A numeric vector with probe_id
#
get_hijack_probe_id <- function(df) {
  as.numeric(levels(droplevels(unlist(filter(df,hijack==TRUE)$probe_id))))
}



#
# DESCRIPTION
# This function resturns number of observed deviations.
#
# ARGUMENTS
# d - a data frame with observatiobs
# modes - a list of frequent values' lists returned by get_modes() function
#
# RETURN VALUE
# A data frame
#
get_deviations_stats <- function(d,modes) {
  tmp <- sapply(get_hijack_probe_id(d), function(x) { get_deviations(d,x,modes) }) 
  results <- as.data.frame(table(unlist(sapply(tmp,function(x) { names(x)[-1] }))))
  names(results) <- c("field","n")
  total <- sum(results$n)
  results$fraction <- results$n/total
  results
}


# ---- data processing ----

# load and format data
df_google <- format_data(read.arff("../measurements/datasets/google.arff"), c("_rt$","^ping_min$"))
df_opendns <- format_data(read.arff("../measurements/datasets/opendns.arff"), c("_rt$","^ping_min$"))

# for given columns get frequent values
google_modes <- get_modes(df_google, min_freq=0.95)
opendns_modes <- get_modes(df_opendns, min_freq=0.95)

# mark observations
df_google_marked <- mark_data(df_google, google_modes)
df_opendns_marked <- mark_data(df_opendns, opendns_modes)

# calculate deviation stats
google_deviation_stats <- get_deviations_stats(df_google_marked, google_modes)
opendns_deviation_stats <- get_deviations_stats(df_opendns_marked, opendns_modes)

# save data
write.arff(x = df_google_marked, file="../measurements/datasets/google-marked-auto.arff")
write.arff(x = df_opendns_marked, file="../measurements/datasets/opendns-marked-auto.arff")
write.arff(x = google_deviation_stats, file="../measurements/datasets/google-deviation-stats-auto.arff")
write.arff(x = opendns_deviation_stats, file="../measurements/datasets/opendns-deviation-stats-auto.arff")
