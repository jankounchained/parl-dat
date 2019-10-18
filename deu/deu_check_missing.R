colSums(is.na(deu18))

a15 <- deu15 %>%
  filter(is.na(date))

a16 <- deu16 %>%
  filter(is.na(date)) %>%
  mutate(rowname = as.numeric(rowname)) %>%
  mutate(lag_row = lag(rowname),
         rowname_check = rowname - lag_row) %>%
  filter(rowname_check != 1)
  



a17 <- deu17 %>%
  filter(is.na(date))
