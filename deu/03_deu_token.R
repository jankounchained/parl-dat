# run token / lemma

library(tidyverse)
library(udpipe)
library(groupdata2)
library(future.apply)
library(zoo)


###
### load in, split 
###

lemma = read_csv("deu/data/02_deu_s.csv") %>%
  groupdata2::group(120)

lemma %>%
  select(rowname, text_c, .groups) %>%
  split(.$.groups) %>%
  walk(~(write_csv(., str_c("deu/data/to_udpipe/", unique(.$.groups), "_deu.csv"))))


###
### udpipe set up
###

lemmatize <- function(filename) {
  
  part <- read_csv(paste0("deu/data/to_udpipe/", filename))
  
  ud_model <- udpipe_load_model("deu/data/german-gsd-ud-2.4-190531.udpipe")
  x <- udpipe_annotate(ud_model, x = part$text_c, doc_id = part$rowname)
  
  x <- as_tibble(x) %>%
    select(doc_id, token, lemma) 
  
  x <- x %>%
    mutate(doc_id = as.numeric(doc_id)) %>%
    filter(!is.na(lemma)) %>%
    group_by(doc_id) %>%
    summarise(text_c = str_c(lemma, collapse = " "))
  
  write_csv(x, path = paste0("deu/data/from_udpipe/", filename))
  
}

ud_model <- udpipe_load_model("deu/data/german-gsd-ud-2.4-190531.udpipe")


###
# run tokenization and lemmatization
###

lfull <- list.files(path = "deu/data/to_udpipe/", pattern = "deu", full.names = F)

plan(multiprocess)
future_lapply(lfull, lemmatize,
               future.packages = c("tidyverse", "udpipe", "zoo"))


###
# missing files
###
lfrom = list.files(path = "deu/data/from_udpipe/", pattern = "deu", full.names = F)

which(!(lfull %in% lfrom))

a = c(lfull[45], lfull[90], lfull[120])

map_df(a, lemmatize)
