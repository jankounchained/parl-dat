### 13/2006 - 14/2008

html_doc <- "fra/scraped2/13_cra_2007_2008_029.html"
 
 link <- read_html(html_doc)
  
  # IMPORT DATA
  raw_xml <- link %>%
    html_nodes("p") %>%
    as.character() %>%
    enframe() %>%
    rename(xml = value)
  
  pres <- link %>%
    html_nodes(xpath = "/html/body/div[6]/div[1]/h5[4]") %>%
    html_text() %>%
    enframe(value = "presiding")
  
  date <- link %>%
    html_nodes(xpath = "//h1[@class='seance']") %>%
    html_text() %>%
    enframe(value = "fr_date")
  
  agenda_item <- link %>%
    html_nodes(xpath = "//h2[@class='titre1']") %>%
    html_text() %>%
    enframe(value = "item")