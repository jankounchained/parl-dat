# Creaste a subset of the last 20 years from Malte's DK data

library(tidyverse)

###
### data and rules
###

# reference DF
dan_all = read_csv("dan/data/folketinget_1953_2019_tokenized.csv")

# take periods from 1998-2001 to 2019 - now
leg_of_int = levels(as.factor(dan_all$Period))[15:21]

# LDA results
XXX


###
### make the cut
###

# cut reference DF
dan_sub = dan_all %>%
  filter(Period %in% leg_of_int) %>%
  # arrange by doc_id
  arrange(doc_id)

# check - all good
levels(as.factor(dan_sub$Period))

# cut LDA DF
XXX


###
### export
###

write_csv(dan_sub, "dan/data/dan_sub.csv")
