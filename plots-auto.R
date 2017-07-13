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


# ---- data processing ----

# load data
df_google_marked <- read.arff("../measurements/datasets/google-marked-auto.arff")
df_opendns_marked <- read.arff("../measurements/datasets/opendns-marked-auto.arff")

df_all <- cbind(df_google_marked,data.frame(s="google"))
df_all <- rbind(df_all, cbind(df_opendns_marked,data.frame(s="opendns")))

df_google_marked <- read.arff("../measurements/datasets/google-deviation-stats-auto.arff")
df_opendns_marked <- read.arff("../measurements/datasets/opendns-deviation-stats-auto.arff")

df_all_deviation_stats <- cbind(google_deviation_stats,data.frame(s="google"))
df_all_deviation_stats <- rbind(df_all_deviation_stats, cbind(opendns_deviation_stats,data.frame(s="opendns")))


# ---- deviation stats ----

ggplot(df_all_deviation_stats, aes(x=field,y=fraction,fill=s)) + 
  geom_bar(stat="identity", position = position_dodge(preserve = 'single') ) + 
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank()) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Feature") +
  ylab("Percentage of observations") +
  ggtitle("Deviation stats for observations marked as hijack") +
  ggsave("plots/deviation_stats-auto.png")

