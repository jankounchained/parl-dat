# merge lemmatized subsets, filter stopwords and turn back into tidy format

library(tidyverse)
library(tidytext)


###
# merge back
###

ufull <- list.files(path = "deu/data/from_udpipe/", pattern = "deu", full.names = T)

from_ud <- map_df(ufull, read_csv) %>%
  rename(rowname = doc_id) %>%
  arrange(rowname) 


###
### NAMES
###

german_mps = read_csv("deu/data/02_deu_raw.csv") %>%
  select(name) %>%
  filter(!duplicated(name)) %>%
  separate(name, sep=" ", 
           into = c("name1", "name2", "name3", "name4"), remove = FALSE)



###
### OTHER STOPWORDS
###

stopwords_de <- 
  # stopwords-iso
  c(stopwords::stopwords("de", source = "stopwords-iso")) %>%
  # MP names and surnames
  c(tolower(german_mps$name1[!is.na(german_mps$name1)])) %>%
  c(tolower(german_mps$name1[!is.na(german_mps$name2)])) %>%
  c(tolower(german_mps$name1[!is.na(german_mps$name3)])) %>%
  c(tolower(german_mps$name1[!is.na(german_mps$name4)])) %>%
  # month names
  c("januar ", "februar", "märz", "april", "mai", 
    "juni", "juli", "august", "september", "oktober", "november", "december") %>%
  # addressing
  c("kollege", "kollegin", 
    "abgeordneter", "abgeordnete",
    "herr", "frau", "dame", 
    "minister", "ministerin", "kanzler", "kanzlerin",
    "alterspräsident", "alterspräsidentin", "alterspräsidenen",
    "präsident", "präsidentin",
    "vorsitzender", "vorsitzende",
    "vizepräsident", "vizepräsidentin",
    "dr") %>%
  unlist() %>%
  tibble(word = .)



###
### FILTER STOPWRODS, produce hash and docwise csv
###

unn <- from_ud %>%
  # one token per row
  unnest_tokens(word, text_c, token = "words") %>%
  # remove stopwords
  filter(!word %in% stopwords_de$word) %>%
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
  filter(!word %in% stopwords_de$word) %>%
  # remove digits
  filter(!str_detect(word, "\\d+")) %>%
  # keep hashes and ID
  select(rowname, word) 

write_csv(unn, "deu/data/unn.csv")
write_csv(unn_hash, "deu/data/unn_hash.csv")
