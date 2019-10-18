library(tidyverse)
library(tictoc)
library(xml2)

parse_TEI <- function(xml_object, speaker_link) {
  
  # METADATA
  ## extract metadata - about session
  plegislative_period <- xml_find_all(xml_object, "//legislativePeriod") %>%
    xml_text()
  
  pdate <- xml_find_all(xml_object, "//publicationStmt/date") %>%
    xml_text()
  
  
  ## extract metadata - about speaker
  metadata_list <- xml_find_all(xml_object, speaker_link) %>%
    xml_attrs() %>%
    unlist()
  
  meta <- enframe(metadata_list) %>%
    rownames_to_column %>% 
    gather(var, value, -rowname) %>% 
    spread(rowname, value) %>%
    select(-var)
  
  colnames(meta) <- meta[1,]
  
  meta <- meta %>%
    slice(-1)
  
  
  # TEXT
  ## extract text
  ptext <- xml_find_all(xml_object, speaker_link[1]) %>%
    xml_find_all(., ".//p") %>%
    xml_text() %>%
    str_c(collapse = "")
  
  
  # DF
  one_speech <- meta %>%
    mutate(text = ifelse(is_empty(ptext), NA, ptext),
           date = pdate,
           period = plegislative_period)
  
  return(one_speech)
  
  
}


deu_xml_roof <- function(filepath) {
  
  ## load xml_object
  link <- read_xml(filepath)
  
  ## find speaker xpaths
  speaker_paths <- link %>%
    xml_find_all("//sp") %>%
    xml_path()
  
  ## parse tei
  df <- map_df(speaker_paths, parse_TEI, xml_object = link)
  return(df)
}

#plan(multiprocess)
s14 <- list.files(path = "deu/GermaParlTEI-master/14", pattern = "*.xml", full.names = T)
s15 <- list.files(path = "deu/GermaParlTEI-master/15", pattern = "*.xml", full.names = T)
s16 <- list.files(path = "deu/GermaParlTEI-master/16", pattern = "*.xml", full.names = T)
s17 <- list.files(path = "deu/GermaParlTEI-master/17", pattern = "*.xml", full.names = T)
s18 <- list.files(path = "deu/GermaParlTEI-master/18", pattern = "*.xml", full.names = T)


tic()
write_csv(map_df(s14, deu_xml_roof), "deu/tei-out/14.csv")
write_csv(map_df(s15, deu_xml_roof), "deu/tei-out/15.csv")
write_csv(map_df(s16, deu_xml_roof), "deu/tei-out/16.csv")
write_csv(map_df(s17, deu_xml_roof), "deu/tei-out/17.csv")
write_csv(map_df(s18, deu_xml_roof), "deu/tei-out/18.csv")
toc()

deu14 <- read_csv("deu/tei-out/14.csv") %>%
  rownames_to_column() %>%
  write_csv("deu/tei-out/14.csv")
deu15 <- read_csv("deu/tei-out/15.csv") %>%
  rownames_to_column() %>%
  write_csv("deu/tei-out/15.csv")
deu16 <- read_csv("deu/tei-out/16.csv") %>%
  rownames_to_column() %>%
  write_csv("deu/tei-out/16.csv")
deu17 <- read_csv("deu/tei-out/17.csv") %>%
  rownames_to_column() %>%
  write_csv("deu/tei-out/17.csv")
deu18 <- read_csv("deu/tei-out/18.csv") %>%
  rownames_to_column() %>%
  write_csv("deu/tei-out/18.csv")

deu15 <- read_csv("deu/tei-out/15.csv") %>%
  mutate(date = ifelse(rowname %in% 14169:14432, "2003-11-26", date)) %>%
  write_csv("deu/tei-out/15.csv")
