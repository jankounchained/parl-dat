library(tidyverse)
library(udpipe)
library(groupdata2)
library(future.apply)
library(zoo)

###
### load in, split 
###

country_iso = "fra"

lemma = read_csv(paste0(country_iso,"/data/03_fra_s.csv")) %>%
  groupdata2::group(240)

lemma %>%
  select(rowname, text_c, .groups) %>%
  split(.$.groups) %>%
  walk(~(write_csv(., str_c(country_iso, "/data/to_udpipe/", unique(.$.groups), "_fra_TO.csv"))))


###
### udpipe set up
###

lemmatize <- function(filename) {
  
  part <- read_csv(paste0(country_iso, "/data/to_udpipe/", filename))
  
  ud_model <- udpipe_load_model(paste0(country_iso, "/data/french-gsd-ud-2.4-190531.udpipe"))
  x <- udpipe_annotate(ud_model, x = part$text_c, doc_id = part$rowname)
  
  x <- as_tibble(x) %>%
    select(doc_id, token, lemma) 
  
  x <- x %>%
    mutate(doc_id = as.numeric(doc_id)) %>%
    filter(!is.na(lemma)) %>%
    group_by(doc_id) %>%
    summarise(text_c = str_c(lemma, collapse = " "))
  
  write_csv(x, path = paste0(country_iso, "/data/from_udpipe/", filename))
  
}

ud_model <- udpipe_load_model(paste0(country_iso, "/data/french-gsd-ud-2.4-190531.udpipe"))


###
# run tokenization and lemmatization
###

lfull <- list.files(path = paste0(country_iso, "/data/to_udpipe/"), pattern = "_TO", full.names = F)

plan(multiprocess)
future_lapply(lfull, lemmatize,
              future.packages = c("tidyverse", "udpipe"))
