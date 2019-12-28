# prepare files for vw

library(tidyverse)

###
### CONVERT HASH TO NUMBER, DOC TO ANOTHER NUMBER
###

unn_hash <- read_csv("deu/data/unn_hash.csv")

out <- count(unn_hash, rowname, word) %>%
  ungroup() %>%
  mutate(hash = as.integer(as.factor(word))) %>%
  arrange(as.numeric(rowname), desc(n))


out %>%
  distinct(hash, word) %>%
  arrange(hash) %>%
  write_csv("deu/data/deu_hash.csv")


out2 = unite(out, "freq", hash, n, sep = ":", remove = FALSE)


out3 = split(out2, out2$rowname) %>%
  map_chr(~str_c(.$freq, collapse = " ")) %>%
  str_c("| ", .)


write_lines(out3, "deu/data/deu_lda.vw")


out2$rowname %>%
  unique() %>%
  write_lines("deu/data/doc_id")

