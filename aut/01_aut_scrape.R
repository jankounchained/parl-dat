library(tidyverse)
library(rvest)
library(downloader)

# manually found links to RSS feed on the Austrian parliamentary database
aut_rss <- read_delim("aut/aut_rss_links.csv", ";", escape_double = FALSE, trim_ws = TRUE)


###
### scrape RSS to get link to sites of individual sessions
###

aut_rss_to_link <- function(rss_link) {
  
  Sys.sleep(2)
  
  rss_feed = read_xml(rss_link)
  
  xml_item = rss_feed %>%
    xml_find_all("//item")
  
  title = rss_feed %>%
    xml_find_all(".//title") %>%
    xml_text() %>%
    enframe() %>%
    filter(str_detect(value, "Sitzung|Nachtrag"))
  
  st_links = rss_feed %>%
    xml_find_all(".//link") %>%
    xml_text() %>%
    enframe() %>%
    filter(str_detect(value, "NRSITZ"))
  
  xl_datum = rss_feed %>%
    xml_find_all(".//pubDate") %>%
    xml_text() %>%
    enframe()
  
  roll = tibble(title = title$value,
                xl_datum = xl_datum$value,
                st_links = st_links$value)
  
  return(roll)
  
}

df_link = map_df(aut_rss$link, aut_rss_to_link)
write_csv(df_link, "aut/df_link.csv")


df_link = read_csv("aut/df_link.csv") %>%
  mutate(st_links = trimws(st_links))



###
### scrape link to html docs of the stenographic reports
###

# define where on site to find the link (acquired by inspect element)
steno_xpath = "//html/body/div[1]/div[5]/div[1]/div[1]/div[3]/div[2]/div[2]/div/ul/li/a[2]"

# funciton that goes on the session site and find the link to the html doc
get_doc_links <- function(site_link, xpath) {
  
  Sys.sleep(2)
  
  session_site = read_html(site_link)
  
  doc_link = session_site %>%
    html_node(xpath = steno_xpath) %>%
    html_attr("href") %>%
    enframe()
  
  return(doc_link)
  
}

# run
doc_links_df = map_df(df_link$st_links, get_doc_links, xpath = steno_xpath)

df_link = df_link %>%
  # add doc_links column
  mutate(doc_links = doc_links_df$value) %>%
  # add url start
  mutate(doc_links = paste0("https://www.parlament.gv.at", doc_links)) %>%
  # add a column with filenames for the downloaderd docs
  mutate(html_filename = paste0(
    str_extract(doc_links, "(?<=VHG/).*?(?=/NRSITZ)"),
    "_",
    str_extract(doc_links, "(?<=SITZ_).*?(?=/)")
    ))
  

###
### downloadR (download whole htmls)
###
library(downloader)
#library(stringr)

for (myurl in df_link$doc_links) {
  
  filename <- paste0(
    "aut/scraped/",
    str_extract(myurl, "(?<=VHG/).*?(?=/NRSITZ)"),
    "_",
    str_extract(myurl, "(?<=SITZ_).*?(?=/)"),
    ".html"
  )
  
  download(myurl, filename)
  Sys.sleep(2)
}
