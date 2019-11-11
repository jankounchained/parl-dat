# merge lemmatized subsets, filter stopwords and turn back into tidy format
# clean stopwords

library(tidyverse)
library(tidytext)
library(rvest)


ufull <- list.files(path = "fra/data/from_udpipe/", pattern = "fra", full.names = T)

from_ud <- map_df(ufull, read_csv) %>%
  rename(rowname = doc_id) %>%
  arrange(rowname) 



###
### NAMES
###

names_link_list = paste0("http://www.assemblee-nationale.fr/qui/xml/liste_alpha.asp?legislature=",
                         c("11", "12", "13", "14", "15"))

scrape_french_mps <- function(url) {
  
  Sys.sleep(2)
  
  link = read_html(url)
  
  all_li = link %>%
    # get all links (acquired via selector gadget)
    html_nodes("li") %>%
    # convert to text
    html_text() %>%
    # tibble
    enframe()
  
  names = all_li %>%
    # keep string containing Mr. / Mrs. / Ms.
    filter(str_detect(value, "M.") | str_detect(value, "Mme") | str_detect(value, "Mlle")) %>%
    rename(rowname = name, title_name = value) %>%
    # split name and title
    mutate(name_surname = str_extract(title_name, "(?<=Mme |M. |Mlle ).*")) %>%
    # split every part of the name 
    separate(name_surname, sep=" ", 
             into = c("name1", "name2", "name3", "name4"), remove = FALSE) %>%
    # delete false matches
    filter(!is.na(name_surname))
  
  return(names)
  
}

french_mps = map_df(names_link_list, scrape_french_mps)



###
### OTHER STOPWORDS
###

stopwords_fr <- 
  # stopwords-iso
  c(stopwords::stopwords("fr", source = "stopwords-iso")) %>%
  # MP names and surnames
  c(tolower(french_mps$name1[!is.na(french_mps$name1)])) %>%
  c(tolower(french_mps$name1[!is.na(french_mps$name2)])) %>%
  c(tolower(french_mps$name1[!is.na(french_mps$name3)])) %>%
  c(tolower(french_mps$name1[!is.na(french_mps$name4)])) %>%
  # month names
  c("janvier","février","mars","avril","mai","juin",
    "juillet","août","septembre","octobre","novembre","décembre") %>%
  # addressing
  c("collègue", 
    "député",
    "monsieur", "m.", "madame", "mme", 
    "ministre", "président",
    "vice-président", "vice") %>%
  unlist() %>%
  tibble(word = .)



###
### FILTER STOPWRODS, produce hash and docwise csv
###

unn <- from_ud %>%
  # one token per row
  unnest_tokens(word, text_c, token = "words") %>%
  # remove stopwords
  filter(!word %in% stopwords_fr$word) %>%
  # remove digits
  filter(!str_detect(word, "\\d+")) %>%
  # keep hashes and ID
  select(rowname, word) %>%
  # group by doc for merging
  group_by(rowname) %>%
  # collect hashes back to docs
  summarise(text_c = str_c(word, collapse = " "))

unn_hash <- from_ud %>%
  # one token per row
  unnest_tokens(word, text_c, token = "words") %>%
  # remove stopwords
  filter(!word %in% stopwords_fr$word) %>%
  # remove digits
  filter(!str_detect(word, "\\d+")) %>%
  # keep hashes and ID
  select(rowname, word) 

write_csv(unn, "fra/data/unn.csv")
write_csv(unn_hash, "fra/data/unn_hash.csv")