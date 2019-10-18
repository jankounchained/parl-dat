library(tidyverse)
library(rvest)
library(downloader)


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


###
### PART 2: direct scrape
###


