library(tidyverse)
library(rvest)
library(zoo)


### 11/1998 - 12/2005

html_doc <- "fra/scraped2/11_cra_1998_1999_98100510.html"

link <- read_html(html_doc)

# IMPORT DATA
# raw_xml <- link %>%
#   html_nodes("p") %>%
#   as.character() %>%
#   enframe() %>%
#   rename(xml = value)

date_doc = str_extract(html_doc, "(?<=\\d{4}_\\d{4}_).*(?=\\.html)")

speech_df <- link %>%
  html_nodes(xpath = "//p[@align='justify']") %>%
  as.character() %>%
  enframe(value = "xml", name = "rowname")

text_df <- link %>%
  html_nodes(xpath = "//p[@align='justify']") %>%
  html_text() %>%
  enframe(value = "text", name = "rowname")

#

speaker_df <- speech_df %>%
  # identify speakers, identify funcitons
         # extract bold text from speech
  mutate(sp_raw = str_extract(xml, "(?<=<b>).*(?=<\\/b>)"),
         # extract italics from sp_raw
         sp_function = str_extract(sp_raw, "(?<=<i>).*(?=<\\/i>)")) %>%
  # sp_raw_name = for name column;
  # sp_raw_tag = for removing speaker tags out of text; 
  # bold_dummy = for removing rows with bold text that is not speaker
         # prepare a column for name extraction
  mutate(sp_raw_name = str_remove(sp_raw, "<i.*?</i>"),
         # remove italics from sp_raw (already in another column)
         sp_raw_tag = str_remove(sp_raw, "<i>"),
         sp_raw_tag = str_remove(sp_raw_tag, "</i>"),
         # dummy 
         bold_dummy = ifelse(str_detect(sp_raw_tag, "<"), 1, 0),
         # remove overflowing xml
         sp_raw_tag = ifelse(str_detect(sp_raw_tag, "<"), NA, sp_raw_tag)
         ) %>%
  # repeat cleaning for sp_raw_name
  mutate(sp_raw_name = str_remove(sp_raw_name, "<i>"),
         sp_raw_name = str_remove(sp_raw_name, "</i>"),
         sp_raw_name = ifelse(str_detect(sp_raw_name, "<"), NA, sp_raw_name)) %>%
  # clean name column
         # remove excessive whitespace
  mutate(name = trimws(sp_raw_name),
         # remove " -"
         name = str_remove(name, " -"),
         # remove ","
         name = str_remove(name, ",")
         ) 

prep1 = full_join(speaker_df, text_df, by = "rowname") %>%
  # clean the text
         # remove speaker tags from speeches
  mutate(text_c = ifelse(!is.na(sp_raw_tag), str_remove(text, sp_raw), text),
         # remove text headers that are still here
         text_c = ifelse(bold_dummy == 1 & !is.na(bold_dummy), NA, text_c),
         # remove excessive whitespace
         text_c = trimws(text_c)
         ) %>%
  # extract scenic comments
         # catch everything in parantheses
  mutate(com = str_extract_all(text_c, "\\((.*?)\\)"),
         # unlist
         com = paste0(com),
         # if list empty = NA
         com = ifelse(com == "character(0)", NA, com),
         # erase scenic comments from speeches
         text_c = str_remove_all(text_c, "\\((.*?)\\)")
         ) %>%
  # add date of session
  mutate(date = date_doc) %>%
  # keep only variables of interest
  select(rowname, date, name, sp_function, text_c, com, text)

prep2 = prep1 %>%
  # filling NAs for speaker
  mutate(name = na.locf(name, na.rm = F)) %>%
  # concentrate multiple paragraphs to single row
         # signal detection: 1 at changing names
  mutate(changing = ifelse(name != lag(name), 1, 0)
         ) %>%
  # group_by() %>%
  # summarise(text_c = str_c(text_c, collapse = " ")) %>%
  # correct variables
  mutate(rowname = as.numeric(rowname),
         name = as.character(name),
         sp_function = as.character(sp_function),
         text_c = as.character(text_c),
         com = as.character(com),
         text = as.character(text))
  
  


  #        %>%
  # mutate(text_c2 = trimws(text_c),
  #        text_c2 = tolower(text_c2),
  #        text_c2 = str_remove_all(text_c2, "[[:punct:]]"))

