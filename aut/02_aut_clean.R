library(tidyverse)
library(rvest)

###
### testing function
###

prep_tester <- function(dft) {
  
  # nrow
  dft_nrow = nrow(dft)
  
  # testing index
  n_true_index = sum(dft$xml_index == dft$plain_index)
  RES_index = dft_nrow - n_true_index
  
  # testing session meta
  leg_present = sum(str_detect(dft$session_meta, "Gesetzgebungsperiode"))
  RES_leg = dft_nrow - leg_present
  
  year_present = sum(str_detect(dft$session_meta, "\\d{4}"))
  RES_year = dft_nrow - year_present
  
  
  
  
  RES = c("nrow" = dft_nrow,
          "RES_index" = RES_index, 
          "RES_leg" = RES_leg, 
          "RES_year" = RES_year,
          "M_plain" =  median(nchar(dft$plain_s), na.rm = T),
          "M_xml" = median(nchar(dft$xml_s), na.rm = T),
          "plain NA" = sum(is.na(dft$plain_s)),
          "xml NA" = sum(is.na(dft$xml_s)))
  
  return(RES)
  
}


###
### leg XX & XXI
###

#html_doc <- "aut/scraped/XX_00001.html"

e_legXX <- function(html_doc) {
  
  link <- read_html(html_doc)
  
  session_meta = link %>%
    html_nodes(xpath = "/html/body/b[1]") %>%
    html_text() %>%
    paste(., collapse="|") %>%
    enframe(name = "DEL", value = "session_meta")
  
  plain_speech_df = link %>%
    html_nodes(xpath = "//p[@align='JUSTIFY']") %>%
    html_text() %>%
    enframe(name = "plain_index", value = "plain_s")
  
  xml_speech_df = link %>%
    html_nodes(xpath = "//p[@align='JUSTIFY']") %>%
    as.character() %>%
    enframe(name = "xml_index", value = "xml_s")
  
  if (nrow(plain_speech_df) == 0) {
    
    plain_speech_df = "NA" %>%
      enframe(name = "plain_index", value = "plain_s") %>%
      na_if("NA")
    
    xml_speech_df = "NA" %>%
      enframe(name = "xml_index", value = "xml_s") %>%
      na_if("NA")
  }
  
  prep1 <- cbind.data.frame(session_meta, plain_speech_df, xml_speech_df)
  
  return(prep1)
}


ls_XX = list.files(path = "aut/scraped/", pattern = "XX_", full.names = T)
ls_XXI = list.files(path = "aut/scraped/", pattern = "XXI_", full.names = T)

df_XX = map_df(ls_XX, e_legXX)
prep_tester(df_XX)
write_csv(df_XX, "aut/data/raw/02_df_XX.csv")

df_XXI = map_df(ls_XXI, e_legXX)
prep_tester(df_XXI)
write_csv(df_XXI, "aut/data/raw/02_df_XXI.csv")


###
### leg XXII
###

e_legXXII_A <- function(html_doc) {
  
  link <- read_html(html_doc, 
                    # to load the whole doc, otherwise, it takes just a few sections
                    options = c("IGNORE_ENC")) 

  session_meta_LEG = link %>%
    # legislative period is always DB104
    html_nodes(xpath = "//p[@class='DBl04']") %>%
    html_text() 
  
  session_meta_YEAR = link %>%
    # date is always DB105
    html_nodes(xpath = "//p[@class='DBl05']") %>%
    html_text() 
  
  session_meta = enframe(
    paste(session_meta_LEG, session_meta_YEAR, sep = "|"),
    name = "DEL", value = "session_meta")
  
  
  plain_speech_df = link %>%
    # extrarct all <p class = 'F1'>
    # acquired via Slector Gadget
    html_nodes(xpath = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ' , 'F1', ' ' ))]") %>%
    html_text() %>%
    enframe(name = "plain_index", value = "plain_s")
  
  xml_speech_df = link %>%
    html_nodes(xpath = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ' , 'F1', ' ' ))]") %>%
    as.character() %>%
    enframe(name = "xml_index", value = "xml_s")
  
  if (nrow(plain_speech_df) == 0) {
    
    plain_speech_df = "NA" %>%
      enframe(name = "plain_index", value = "plain_s") %>%
      na_if("NA")
    
    xml_speech_df = "NA" %>%
      enframe(name = "xml_index", value = "xml_s") %>%
      na_if("NA")
  }
  
  prep1 <- cbind.data.frame(session_meta, plain_speech_df, xml_speech_df)
  
  return(prep1)
}

# MSONORMAL
e_legXXII_B <- function(html_doc) {
  
  link <- read_html(html_doc, 
                    # to load the whole doc, otherwise, it takes just a few sections
                    options = c("IGNORE_ENC")) 
  
  session_meta_LEG = link %>%
    # legislative period is always DB104
    html_nodes(xpath = "//p[@class='DBl04']") %>%
    html_text() %>%
    paste(., collapse="|")
  
  session_meta_YEAR = link %>%
    # date is always DB105
    html_nodes(xpath = "//p[@class='DBl05']") %>%
    html_text() %>%
    paste(., collapse="|")
  
  session_meta = enframe(
    paste(session_meta_LEG, session_meta_YEAR, sep = "|"),
    name = "DEL", value = "session_meta")
  
  plain_speech_df = link %>%
    html_nodes(xpath = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ' , 'MsoNormal', ' ' ))]") %>%
    html_text() %>%
    enframe(name = "plain_index", value = "plain_s")
  
  xml_speech_df = link %>%
    html_nodes(xpath = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ' , 'MsoNormal', ' ' ))]") %>%
    as.character() %>%
    enframe(name = "xml_index", value = "xml_s")
  
  if (nrow(plain_speech_df) == 0) {
    
    plain_speech_df = "NA" %>%
      enframe(name = "plain_index", value = "plain_s") %>%
      na_if("NA")
    
    xml_speech_df = "NA" %>%
      enframe(name = "xml_index", value = "xml_s") %>%
      na_if("NA")
    
    
  }
  
  prep1 <- cbind.data.frame(session_meta, plain_speech_df, xml_speech_df)
  
  return(prep1)
}


ls_XXII = list.files(path = "aut/scraped/", pattern = "XXII_", full.names = T)

df_XXIIa = map_df(ls_XXII[1:17], e_legXXII_A)
df_XXIIb = map_df(ls_XXII[18:164], e_legXXII_B)

prep_tester(df_XXIIa)
prep_tester(df_XXIIb)

df_XII = full_join(df_XXIIa, df_XXIIb)
write_csv(df_XII, "aut/data/raw/02_df_XXII.csv")


###
### leg XXIII
###

ls_XXIII = list.files(path = "aut/scraped/", pattern = "XXIII_", full.names = T)

df_XXIII = map_df(ls_XXIII, e_legXXII_B)
prep_tester(df_XXIII)

write_csv(df_XXIII, "aut/data/raw/02_df_XXIII.csv")



###
### leg XXIV
###

ls_XXIV = list.files(path = "aut/scraped/", pattern = "XXIV_", full.names = T)

df_XXIV = map_df(ls_XXIV, e_legXXII_B)
prep_tester(df_XXIV)

write_csv(df_XXIV, "aut/data/raw/02_df_df_XXIV.csv")


###
### leg XXV
###

ls_XXV = list.files(path = "aut/scraped/", pattern = "XXV_", full.names = T)

df_XXV = map_df(ls_XXV, e_legXXII_B)
prep_tester(df_XXV)

write_csv(df_XXV, "aut/data/raw/02_df_XXV.csv")
