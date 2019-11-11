# prepare files for vw

library(tidyverse)

###
### CONVERT HASH TO NUMBER, DOC TO ANOTHER NUMBER
###

unn_hash <- read_csv("deu/data/unn_hash.csv")

out <- count(unn_hash, rowname, word) %>%
  ungroup() %>%
  mutate(hash = as.integer(as.factor(word))) %>%
  arrange(as.numeric(rowname, desc(n))) %>%
  unite("freq", hash, n, sep = ":", remove = FALSE)


out %>%
  distinct(hash, word) %>%
  write_csv("deu/data/deu_hash.csv")


out2 <- out %>%
  split(out$rowname) %>%
  map_chr(~str_c(.$freq, collapse = " ")) %>%
  str_c("| ", .)


write_lines(out2, "deu/data/deu_lda.vw")


# doc_id
out3 = unite(out, "freq", hash, n, sep = ":", remove = FALSE)

out3$rowname %>%
  unique() %>%
  write_lines("deu/data/doc_id")