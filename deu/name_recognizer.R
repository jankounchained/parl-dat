sample <- "Präsident  Wolfgang  Thierse:  Liebe  Kolleginnenund  Kollegen!  Verehrte  Gäste!  Zuerst  und  vor  allemmöchte  ich  mich  für  das  Vertrauen,  das  Sie  mit  IhrerWahl  in  mich  gesetzt  haben,  bedanken.  Dieses  Vertrau-en  verpflichtet;  ich  will  mir  alle  Mühe  geben,  es  durchFairneß,   durch   Offenheit   und   durch   parteipolitischeNeutralität  in  der  Amtsführung  zu  rechtfertigen.  Ichbitte Sie sehr, liebe Kolleginnen und Kollegen, mich da-bei zu unterstützen."
sample2 <- "Ich frage Sie, Herr Kollege Solms: Nehmen Sie die"

begining = str_extract(sample, ".*?:") %>%
  str_remove(., ":") %>%
  strsplit(., " ") %>%
  unlist() 



name_recognizer <- function(str) {
  
  begining = str_extract(str, ".*?:") %>%
    str_remove(., ":") %>%
    strsplit(., " ") %>%
    unlist() 
  
  return(any(begining %in% deu_mpd))
  
}

name_recognizer(sample2)
