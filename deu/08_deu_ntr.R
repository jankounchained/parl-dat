# calculate NTR scores for France

library(tidyverse)
library(entropy)
library(future.apply)



######
###### NTR
######

vdiff <- function(x,n,fun) sapply(n, function(i) fun(x,i))
vlag  <- function(x,n) vdiff(x,0:n,dplyr::lag)
vlead <- function(x,n) vdiff(x,0:n,dplyr::lead)

novelty <- function(mat, w) {
  vlag(1:nrow(mat), w) %>%          # produce the lags (same shape as document)
    apply(1, function(idx) {        # then for each row (document)
      lapply(idx[2:length(idx)], function(i) { #for each lag
        if (is.na(i)) return(NA)             #check if it's na (we're at beginning / end of data)
        ## calculate surprise from past to present
        KL.plugin(mat[i,], mat[idx[1],], unit = "log2")
      }) %>%
        unlist() %>%
        mean()
    })}


transience <- function(mat, w) {
  vlead(1:nrow(mat), w) %>%         # produce the leads (same shape as document)
    apply(1, function(idx) {        # then for each row (document)
      lapply(idx[2:length(idx)], function(i) { #for each lead
        if (is.na(i)) return(NA)             #check if it's na (we're at beginning / end of data)
        ## calculate surprise from present to future
        KL.plugin(mat[idx[1],], mat[i,], unit = "log2")
      }) %>%
        unlist() %>%
        mean()
    })}


z <- function(d) (d - mean(d)) / sd(d)

calculate_ntr <- function(doc_subset_path, w) {
  
  import = read_csv(doc_subset_path)
  
  id = import$doc_id
  
  doc = import %>%
    select(-doc_id) %>%
    as.matrix(., ncol = 100)
  
  ntr_output = tibble(doc_id = id) %>%
    mutate(
      novelty = novelty(doc, w),
      transience = transience(doc, w),
      resonance = novelty - transience
    ) %>%
    filter(complete.cases(.)) %>%
    mutate(z_novelty = z(novelty),
           z_transience = z(transience),
           z_resonance = z(resonance)
    )
  
  res_nov_model = lm(z_resonance ~ z_novelty, data = ntr_output)
  ntr_output$delta_R = ntr_output$z_resonance - predict(res_nov_model)
  
  ntr_output %>%
    write_csv(paste0("deu/data/ntr_out/W", as.character(w), "_", "_from_matrix.csv"))
  
}


####
# run
####

timescales <- seq(100, 1000, 50)

plan(multiprocess)
future_lapply(timescales, FUN = calculate_ntr, 
              doc_subset_path = "deu/data/deu_ntr_in.csv",
              future.packages = c("tidyverse", "entropy"))

#calculate_ntr("fra/data/fra_ntr_in.csv", w = 1)
