# calculate shuffeled NTR scores for Germany

library(tidyverse)
library(entropy)
library(future.apply)


######
###### SHUFFLE LDA OUTPUT
######

# read LDA output
deu_lda = read_csv("deu/data/deu_ntr_in.csv")

# set seed for reproducibility
set.seed(42)

# shuffle rows
random_rows <- sample(nrow(deu_lda))
shuffled_speeches <- deu_lda[random_rows, ]

write_csv(shuffled_speeches, "deu/data/shuffled_speeches.csv")

# restart R
rm(list = ls())
.rs.restartR()

######
###### NTR
######

# country for loading / saving files
iso = "deu"

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

calculate_ntr <- function(w) {
  
  import = read_csv("deu/data/shuffled_speeches.csv")
  
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
    write_csv(paste0(iso, "/data/ntr_shuffled/W", as.character(w), "_ntr_shuf.csv"))
  
}


####
# run
####

timescales <- c(300, 900)

plan(multiprocess)
future_lapply(timescales, FUN = calculate_ntr, 
              future.packages = c("tidyverse", "entropy"))
