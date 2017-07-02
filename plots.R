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
require(ggplot2)
require(bitops)


# ---- data processing ----

# load data
df_google_marked <- read.arff("../measurements/datasets/google-marked.arff")
df_opendns_marked <- read.arff("../measurements/datasets/opendns-marked.arff")
fields2mark <- readLines("../measurements/datasets/fields2mark")

df_all <- cbind(df_google_marked,data.frame(s="google"))
df_all <- rbind(df_all, cbind(df_opendns_marked,data.frame(s="opendns")))


# ---- deviation from the mode ----

i <- 0
labs <- character()
df_plot <- data.frame(field=character(),value=numeric(),s=character())
for(field in fields2mark) {
  ind <- which(bitAnd(df_all$hijack,2^i)>0)
  if(length(ind)>0) {
    df_plot <- rbind(df_plot, data.frame(probe_id=df_all[ind,]$probe_id, s=df_all[ind,]$s, value=field))
    labs <- c(labs,field)
  }
  i = i + 1
}

ggplot(df_plot) + 
  geom_bar(aes(value,fill=s),position="dodge") + 
  scale_x_discrete(labels=labs) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5), legend.title=element_blank()) + 
  xlab("Feature") +
  ylab("Number of observations") +
  ggtitle("Deviation from the mode") +
  ggsave("plots/deviation_from_the_mode.png")
