# new name recognizer
## setup
deu_mpd_surname <- read_xml("deu/MdB-Stammdaten-data/MDB_STAMMDATEN.XML") %>%
  xml_find_all("//NACHNAME") %>%
  xml_text(.) %>%
  trimws(.) %>%
  unique()

deu_mpd_firstname <- read_xml("deu/MdB-Stammdaten-data/MDB_STAMMDATEN.XML") %>%
  xml_find_all("//VORNAME") %>%
  xml_text(.) %>%
  trimws(.) %>%
  unique()

## function
name_recognizer <- function(str) {
  
  begining = str_extract(str, ".*?:") %>%
    str_remove(., ":") %>%
    strsplit(., " ") %>%
    unlist() 
  
  match = enframe(begining)
  
  any(begining %in% deu_mpd)
  
  return()
  
}

str = "Jan Gebhardt (SPD):"
str2 = "Herr kollege Gebhardt, ich frage Sie:"
str3 = "Alterspräsident Jan Gebhardt:"
str4 = "Kerstin Griese, Parl. Staatssekretärin beim Bundesminister für Arbeit und Soziales:"

tic()
begining = str_extract(str3, ".*?:") %>%
  str_replace(., ":", " :") %>%
  str_remove(., ",") %>%
  strsplit(., " ") %>%
  unlist() 

match = enframe(begining) %>%
  mutate(is_name = case_when(value %in% deu_mpd ~ TRUE),
         is_president = case_when(str_detect(value[1], "räsident") ~ TRUE),
         is_party = case_when(str_detect(value, "\\(.*\\)")  ~ TRUE))
  

is_speaker = case_when(any(match$is_name == TRUE) & any(match$is_president == TRUE) ~ TRUE,
                       any(match$is_name == TRUE) & any(match$is_party == TRUE) ~ TRUE,
                       TRUE ~ FALSE)
toc()