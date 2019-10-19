library(tidyverse)
library(rvest)
library(downloader)


######
###### PART 1: download open data of records from 2011-2019
######

###
### get links to files
###

opendata_root_link <- "https://echanges.dila.gouv.fr/OPENDATA/Debats/AN/"

opendata_root <- read_html(opendata_root_link)

year_links <- opendata_root %>%
  # go to node pre
  html_node("pre") %>%
  # only links
  html_nodes("a") %>%
  # what are links linking to
  html_attrs() %>%
  enframe() %>%
  # keep only records linking to yearly folders
  filter(str_detect(value, "\\d{4}/")) %>%
  # add start of the link
  mutate(value = paste0(opendata_root_link, value))


make_list_of_file_urls <- function(links) {
  
  Sys.sleep(2)
  
  open <- read_html(links)
  
  file_links <- open %>%
    html_node("pre") %>%
    html_nodes("a") %>%
    html_attrs() %>%
    enframe() %>%
    # keep links to .taz files
    filter(str_detect(value, "\\.taz")) %>%
    # add full url
    mutate(value = paste0(links, value))
  
  return(file_links)
  
}

files_url <- map_df(year_links$value, make_list_of_file_urls)


###
### download fiels
###

for (myurl in files_url$value) {
  
  filename <- paste0(
    "fra/scraped/",
    # extract everything after 4 digits followed by /
    str_extract(myurl,"(?<=\\d{4}\\/).*"))
  
  # mode wb: downloading unix binary 
  download(myurl, filename, mode = "wb")
  Sys.sleep(1)
}



######
###### PART 2: direct scrape of older records
######

###
### get links to individual legislative periods 
###

an_root <- "http://www.assemblee-nationale.fr"
an_leg <- c("11/debats/index.asp", "12/debats/index.asp", "13/debats/index.asp")

ro_period_links <- paste(an_root, an_leg, sep = "/")


###
### get links to index of transcripts
###

get_to_session_site <- function(legislative_period_link) {
  
  Sys.sleep(2)
  
  root_html <- read_html(legislative_period_link)
  
  links <- root_html %>%
    html_nodes("li") %>%
    html_node("a") %>%
    html_attrs() %>%
    enframe()
  
  session_links <- links %>%
    filter(str_detect(value, "cra/"))
  
  return(session_links)

}

session_links <- map_df(ro_period_links, get_to_session_site)

session_links_clean <- session_links %>%
  filter(!duplicated(value)) %>%
  filter(str_detect(value, "\\d{2}\\/cr")) %>%
  mutate(url_value = paste0(an_root, value))


###
### get links from index to html site with session transcript
###

get_to_html_trans <- function(index_link) {
  
  Sys.sleep(2)
  
  # CRA OD 12/2005-2006
  if (str_detect(index_link, 
                 paste("13/cra",
                       "/12/cra/2006-2007/",
                       "/12/cra/2005-2006/",
                       "/12/cra/2005-2006-extra/", 
                       sep = "|"))) {
    
    root_index_link <- read_html(index_link)
    
    trans_href <- root_index_link %>%
      html_nodes(xpath = "//td[@class='seance']") %>%
      html_nodes(xpath = "//h1[@class='seance']") %>%
      html_nodes("a") %>%
      html_attrs() %>%
      enframe()
    
    trans_links <- trans_href %>%
      mutate(trans_url = paste0(index_link, value),
             value = as.character(value))
    
  }
  
  # CRA DO 12/2004-2005 (INCL.)
  else {
    
    root_index_link <- read_html(index_link)
    
    trans_href <- root_index_link %>%
      html_nodes("h5") %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      enframe()
    
    trans_links <- trans_href %>%
      mutate(trans_url = paste0(index_link, value),
             value = as.character(value))
    
  }
  
  return(trans_links)
  
}

trans_links <- map_df(session_links_clean$url_value, get_to_html_trans)

trans_links_clean <- trans_links %>%
  mutate(trans_url = str_remove(trans_url, "#.*"))

###
### download raw html
###

for (myurl in trans_links_clean$trans_url) {
  
  filename <- paste0(
    # folder path
    "fra/scraped2/", 
    # remove punctutaion from the next part of the filename
    str_replace_all(
      # collect everything between .fr/ and .asp
      str_extract(myurl, "(?<=\\.fr\\/).*(?=\\.asp)"),
      # replace / or - with _
      "/|-", "_"
    ),
    # file format
    ".html"
    )
  
  download(myurl, filename)
  Sys.sleep(2)
}
