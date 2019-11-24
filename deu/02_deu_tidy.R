# tidy records & prepare for tokenization
library(tidyverse)


###
### import data
###
tei_out_list <- list.files(path = "deu/tei-out/", pattern = "*csv", full.names = T)

deu_raw <- map_df(tei_out_list, read_csv)


###
### only speeches + rownames for processing
###
deu_raw_2 <- deu_raw %>%
  select(-rowname) %>%
  rownames_to_column() %>%
  mutate(text_c = str_replace_all(text, "§", "paragraf"),
         text_c = str_remove_all(text_c, "[:punct:]"),
         text_c = tolower(text_c),
         text_c = trimws(text_c))

deu_s = deu_raw_2 %>%
  select(rowname, text) %>%
  mutate(rowname = as.numeric(rowname))

write_csv(deu_raw_2, "deu/data/02_deu_raw.csv")
write_csv(deu_s, "deu/data/02_deu_s.csv")
