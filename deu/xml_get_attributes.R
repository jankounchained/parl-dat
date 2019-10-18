get_xml_meta <- function(link) {
  
  election_period <- link %>%
    xml_find_all("//wahlperiode") %>%
    xml_text(.)
  
  doc_type <- link %>%
    xml_find_all("//dokumentart") %>%
    xml_text(.)
  
  nr <- link %>%
    xml_find_all("//nr") %>%
    xml_text(.)
  
  datum <- link %>%
    xml_find_all("//datum") %>%
    xml_text(.)
  
  doc_title <- link %>%
    xml_find_all("//titel") %>%
    xml_text(.)
  
  cbind.data.frame(election_period, doc_type, nr, datum, doc_title)
  

  
}

get_xml_meta(link)
