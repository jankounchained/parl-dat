### 11/1998 - 12/2005

html_doc <- "fra/scraped2/11_cra_1998_1999_98100510.html"

link <- read_html(html_doc)

# IMPORT DATA
raw_xml <- link %>%
  html_nodes("p") %>%
  as.character() %>%
  enframe() %>%
  rename(xml = value)

speech_df <- link %>%
  html_nodes(xpath = "//p[@align='justify']") %>%
  as.character() %>%
  enframe()

speaker_df <- speech_df %>%
         # extract bold text from speech
  mutate(speaker = str_extract(value, "(?<=<b>).*(?=<\\/b>)"),
         # extract italics from speaker
         sp_fun = str_extract(speaker, "(?<=<i>).*(?=<\\/i>)"),
         # delete italics from speaker
         speaker = str_remove(speaker, "<i.*?</i>")
         )


