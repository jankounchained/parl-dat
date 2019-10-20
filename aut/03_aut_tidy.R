library(tidyverse)

###
### XV
###

a = df_XXV %>%
  # extract bold
  mutate(speaker = str_extract(xml_s, "(?<=<b>).*?(?=<\\/b>)")) %>%
  # if record contains hyperlink, it's a speaker
  mutate(speaker2 = case_when(
      str_detect(speaker, "<\\/a>") ~ str_extract(speaker, '(?<=PAD_\\d{5}\\/index.shtml\\">).*?(?=<\\/a>)'),
      TRUE ~ "NA")
    ) 

a2 = a %>%
  na_if("NA")

    # # extract italics
    # speaker = str_extract(speaker, "(?<=<i>).*?(?=<\\/i>)"),
    # # remove whitespace & "-"
    # speaker = str_remove_all(speaker, " -"),
    # speaker_dummy = if_else(is.na(speaker), 0, 1))
