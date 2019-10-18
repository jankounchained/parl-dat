library(pacman)
p_load(tidyverse, rvest, tidytext, xml2, zoo)

###
# master data
###

# load list of MPs
deu_mpd <- read_xml("deu/MdB-Stammdaten-data/MDB_STAMMDATEN.XML") %>%
  xml_find_all("//NACHNAME") %>%
  xml_text(.) %>%
  trimws(.) %>%
  unique()


# BUG HERE "Herr Kollege Thierse, ich frage Sie:"

# function detects speaker demarcation
name_recognizer <- function(str) {
  
  begining = str_extract(str, ".*?:") %>%
    str_remove(., ":") %>%
    strsplit(., " ") %>%
    unlist() 
  
  return(any(begining %in% deu_mpd))
  
}

# function extracts metadata of the xml document
get_xml_meta <- function(link) {
  
  election_period <- link %>%
    xml_find_all("//wahlperiode") %>%
    xml_text(.)
  
  doc_type <- link %>%
    xml_find_all("//dokumentart") %>%
    xml_text(.)
  
  nr <- link %>%
    xml_find_all("//nr") %>%
    xml_text(.)
  
  datum <- link %>%
    xml_find_all("//datum") %>%
    xml_text(.)
  
  doc_title <- link %>%
    xml_find_all("//titel") %>%
    xml_text(.)
  
  cbind.data.frame(election_period, doc_type, nr, datum, doc_title)
}


###
# particular links
###
link <- read_html("deu/pp14-data/14001.xml")

raw_xml <- link %>%
  xml_find_all("//text") %>%
  xml_text(.) %>%
  str_split(., "\r\n\r\n") %>%
  enframe() %>%
  separate_rows()


names <- raw_xml %>%
  # run name recognized on rows
  mutate(is_speaker = lapply(raw_xml$value, name_recognizer)) %>%
  # keep only/all records after the first row that has hovno == TRUE
  slice(as.numeric(which(.$is_speaker == TRUE)[1]) : n())

prep1 <- names %>%
  # party 
  mutate(party = str_extract(value, "\\(.*\\):"),
         # remove party data from text
         value = ifelse(!is.na(party), str_remove(value, "\\(.*\\)"), value)) %>%
  # is_comment
  mutate(is_comment = str_detect(value, "\\("),
         # remove everything in bracets, or string if first character is bracet
         value = str_remove_all(value, "\\(.*\\)|^\\(.*")) %>%
  # is_meta
  mutate(is_meta = str_detect(value, "Deutscher Bundestag –")) %>%
    # remove meta text
    filter(is_meta != TRUE) %>%
  # speaker
  mutate(speaker = ifelse(is_speaker == TRUE, str_extract(value, ".*?:"), NA),
         # remove speaker names from speeches
         value = ifelse(!is.na(speaker), str_remove(value, ".*?:"), value))


# WORKS, BUT FUCKS UP ORDERING, SEE ABOUT GROUP_BY
# ALSO PUTS ALL SPEECHES THAT DAY UNDER A SPEAKER NAME
# ADD: REMOVE EMPTY ROWS
prep2 <- prep1 %>%
  mutate(speaker = na.locf(speaker, na.rm = F)) %>%
  group_by(speaker, line = cumsum(!is.na(speaker))) %>% 
  summarize(text = str_c(value, collapse = " ")) %>%
  ungroup()
  


