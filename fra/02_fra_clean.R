library(tidyverse)
library(rvest)
library(zoo)
library(groupdata2)
library(future.apply)


######
###### PART 1: parsing 11/1998 - 12/2005
######

parsing_in_the_90s <- function(html_doc) {
  
  link <- read_html(html_doc)
  
  # extract session date from html document name
  date_doc = str_extract(html_doc, "(?<=\\d{4}_\\d{4}_).*(?=\\.html)")
  
  speech_df <- link %>%
    # select only nodes with speeches (paragraphs with justified formating)
    html_nodes(xpath = "//p[@align='justify']") %>%
    # get raw xml
    as.character() %>%
    enframe(value = "xml", name = "rowname")
  
  text_df <- link %>%
    # select only nodes with speeches (paragraphs with justified formating)
    html_nodes(xpath = "//p[@align='justify']") %>%
    # get only text without the code
    html_text() %>%
    enframe(value = "text", name = "rowname")
  
  speaker_df <- speech_df %>%
    # identify speakers, identify funcitons
    # extract bold text from speech
    mutate(sp_raw = str_extract(xml, "(?<=\\<b>).*(?=\\<\\/b>)"),
           # extract italics from sp_raw
           sp_function = str_extract(sp_raw, "(?<=\\<i>).*(?=\\<\\/i>)")) %>%
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
           sp_raw_tag = ifelse(str_detect(sp_raw_tag, "<"), NA, sp_raw_tag),
           # ACCIDENT 1, doc 11/271: some idiot left ONE parentheses bold
           sp_raw_tag = ifelse(sp_raw_tag == ")", NA, sp_raw_tag)
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
    mutate(text_c = ifelse(!is.na(sp_raw_tag), str_remove(text, sp_raw_tag), text),
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
    mutate(name = na.locf(name, na.rm = F)) #%>%
    # # concentrate multiple paragraphs to single row
    # # signal detection: 1 at changing names
    # mutate(changing = ifelse(name != lag(name), 1, 0)
    # ) %>%
    # # convert columns to correct type
    # mutate(rowname = as.numeric(rowname),
    #        name = as.character(name),
    #        sp_function = as.character(sp_function),
    #        text_c = as.character(text_c),
    #        com = as.character(com),
    #        text = as.character(text))
  
  df_groups = prep2 %>%
    # exlude paragraphs without a speaker
    drop_na(name) %>%
    # group_by paragraph
    # if one speech is split into multiple rows (same speaker name in adjecent rows):
    # put them into a single group
    group(n = 'auto', 
          method = 'l_starts',
          starts_col = 'name', 
          col_name = 'ind_speech') %>%
    # ungroup data for merging
    ungroup() %>%
    # convert to character to avoid warning message
    mutate(ind_speech = as.character(ind_speech))
  
  df_collapse_speech = df_groups %>%
    # group by paragraph index
    group_by(ind_speech)  %>%
    # collaps paragraphs
    summarise(speech = paste0(text_c, collapse = " ")) %>%
    # ungroup for merging
    ungroup() %>%
    # convert speech index to numeric for correct order after merging
    mutate(ind_speech = as.numeric(ind_speech))
  
  # make a list of rows where speakers change
  starts = find_starts(df_groups, col = 'ind_speech', return_index = T)
  
  fra_raw = df_groups %>%
    # select rows where speker changes
    slice(starts) %>%
    # keep only important variables
    select(ind_speech, date, name, sp_function) %>%
    # convert speech index to numeric for consistency when merging
    mutate(ind_speech = as.numeric(ind_speech)) %>%
    # merge df containing information with df containing collapsed speeches
    full_join(., df_collapse_speech, by = "ind_speech") %>%
    # make a scalable speech index
    mutate(ind_speech = paste0(date, "_", ind_speech)) %>%
    # convert columns to correct type
    mutate(ind_speech = as.character(ind_speech),
           date = as.numeric(date),
           name = as.character(name),
           sp_function = as.character(sp_function),
           speech = as.character(speech))
  
}


###
### list files to parse
###


# list files from 11 legislative period
ls_11 = list.files(path = "fra/scraped2/", pattern = "11_", full.names = T)

# loop over files from 12 legislative period between 2001 and 2005
ls_12_A = lapply(paste0("12_cra_200", 1:5), list.files,
                path = "fra/scraped2/", 
                full.names = T) %>% unlist()


###
### run parsing_in_the_90s
###

fra_raw_11_A = map_df(ls_11[1:420], parsing_in_the_90s)
# 11/421 HAS A DIFFERENT FUCKING FORMAT
fra_raw_11_B = map_df(ls_11[422:906], parsing_in_the_90s)

# leg 12 doesn't work!
fra_raw_12_A = map_df(ls_12_A, parsing_in_the_90s)

# plan(multiprocess)
# fra_raw_11 = future_lapply(ls_11, parsing_in_the_90s,
#               future.packages = c("tidyverse", "groupdata2", "zoo"))

