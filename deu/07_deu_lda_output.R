# parse vw output

library(tidyverse)


######
###### LDA IN
######

# VW IN
doc_id = read_lines("deu/data/doc_id")

# load in topic distribuiton across docs
doc = read_delim("deu/data/doc_topic.model", delim = " ",
                 col_names = FALSE,
                 col_types = cols())

# take only last pass
n_topics = ncol(doc)
n_docs = nrow(doc) / 10
doc = tail(doc, n_docs)

doc2 <- doc %>%
  mutate(doc_id = doc_id)

write_csv(doc2, "deu/data/deu_ntr_in.csv")
