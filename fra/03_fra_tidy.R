# tidy records & prepare for tokenization
library(tidyverse)


###
### import data
###
fra_02_list <- list.files(path = "fra/data/", pattern = "02_fra", full.names = T)

fra_raw <- map_df(fra_02_list, read_csv)


###
### drop NAs & create reference df
###

fra_ref = fra_raw %>%
  drop_na(name) %>%
  drop_na(speech) %>%
  rownames_to_column()

###
### only speeches + rownames for processing df
###
fra_s <- fra_ref %>%
  select(rowname, speech) %>%
  # clean text column for lemmatization
         # replace § character with word
  mutate(text_c = str_replace_all(speech, "§", "paragraphe"),
         # drop all punctuation except '
         text_c = str_remove_all(text_c, "(?![\\'])\\p{P}"),
         # to lowercase
         text_c = tolower(text_c),
         # remove exces whitespace
         text_c = trimws(text_c),
         # remove whitespace at the start of the speech
         text_c = str_remove(text_c, "^ "))

write_csv(fra_s, "fra/data/03_fra_s.csv")
write_csv(fra_ref, "fra/data/03_fra_ref.csv")
