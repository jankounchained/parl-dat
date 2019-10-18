library(pacman)
p_load(tidyverse, xml2, rvest, xmltools)

link <- read_xml("deu/GermaParlTEI-master/14/BT_14_001.xml")

# go all the way to sp node
link %>%
  xml_children() %>%
  xml_children() %>%
  xml_children() %>%
  xml_children() %>%
  xml_view_tree()

  
speaker_link <- link %>%
  xml_find_all("//sp") %>%
  xml_path()


# function
## xml_object: object after read_xml()
## speaker_link: vector of xml paths to speakers in the xml_object
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
  ptext <- xml_find_all(link, speaker_link[1]) %>%
    xml_find_all(., ".//p") %>%
    xml_text() %>%
    str_c(collapse = "")

  
  # DF
  one_speech <- meta %>%
    mutate(text = ptext,
           date = pdate,
           period = plegislative_period)
  
  return(one_speech)
  
  
}


a <- parse_TEI(link, speaker_link[1])
b <- map_df(speaker_link, parse_TEI, xml_object = link)

