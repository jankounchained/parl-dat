library(tidyverse)

aut_02_paths = list.files(path = "aut/data/raw", pattern = "*.csv", full.names = T)



###
### XV
###

df_XXV = read_csv("aut/data/02_df_XXV.csv")

a = df_XXV %>%
  # remove quotes from XML
  mutate(xml_s2 = str_remove_all(xml_s, '\\"')) %>%
  # extract bold
  mutate(speaker = str_extract(xml_s2, "(?<=\\<b\\>).*?(?=\\<\\/b\\>)")) %>%
  # extract speaker names
      # if record contains hyperlink, it's a speaker
  mutate(
    speaker2 = ifelse(str_detect(speaker, "<\\/a>"),
                      str_extract(speaker, '(?<=PAD_\\d{5}\\/index.shtml\\">).*?(?=<\\/a>)'),
                      NA),
      # presiding MP doesn't have a link
      # extract "Vorsitz{something}" from speaker col
    speaker2 = ifelse(str_detect(speaker, "(?<=>)Vorsitz.*?(?=<)"), 
                      str_extract(speaker, "(?<=>)Vorsitz.*?(?=<)"),
                      speaker2)
    )

prep1 = a %>%
  # filter out agenda items
        # if e.g. "1. Punkt:" is at start of the linke, no speech 
  mutate(text_c = ifelse(str_detect(plain_s, "^\\d\\. Punkt:"),
                       NA,
                       plain_s)) %>%
  # catch scenic comments
  mutate(com = str_extract_all(text_c, "\\((.*?)\\)"),
         com = paste0(com),
         com = ifelse(com == "character(0)", NA, com)) %>%
  # erase speaker names from text
  mutate(text_c = ifelse(str_detect(text_c, speaker2),
                         str_remove(text_c, speaker2),
                         text_c)) #%>%
  # erase comments from text
 # mutate(text_c = str_remove_all(text_c, "\\((.*?)\\)"))


    # # extract italics
    # speaker = str_extract(speaker, "(?<=<i>).*?(?=<\\/i>)"),
    # # remove whitespace & "-"
    # speaker = str_remove_all(speaker, " -"),
    # speaker_dummy = if_else(is.na(speaker), 0, 1))
