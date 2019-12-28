# sanity check: shuffled CZpar/leg2013-17

library(tidyverse)
library(entropy)
library(future.apply)


# read LDA output
cze_2013_lda = read_csv("cze/data/ntr_in_13.csv")

# set seed for reproducibility
set.seed(42)

# shuffle rows
random_rows <- sample(nrow(cze_2013_lda))
shuffled_speeches <- cze_2013_lda[random_rows, ]

write_csv(shuffled_speeches, "cze/data/cze_shuff.csv")

# restart R
rm(list = ls())
.rs.restartR()


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
    write_csv(paste0("cze/data/W", w, "_shuff.csv"))
  
}


####
# run
####

calculate_ntr("cze/data/cze_shuff.csv", w = 27)


######
###### plot, check
######


#norm = read_csv("cze/data/1_from_matrix.csv")
#shuff = read_csv("cze/data/cze_shuff.csv")


ntr_test_path = list.files("cze/data/", pattern = "*.csv", full.names = T)


export_plots <- function(path) {
  
  # load in
  ntr_w = read_csv(path)
  
  # extract timeframe
  w = str_extract(path, "(?<=W)\\d+")
  
  
  # RESONANCE ~ NOVELTY
  ggplot(ntr_w, aes(z_novelty, z_resonance)) +
    geom_bin2d(bins = 100) +
    scale_fill_viridis_c(trans = "log", breaks = c(1,10,100,1000),
                         labels = trans_format("log10", 
                                               math_format(expr = 10^.x, format = force))) +
    geom_smooth(method = "lm", colour = "black", alpha = 0, size = 0.5) +
    labs(x = "Novelty (z-scaled)\n", y = "Resonance (z-scaled)", 
         title = paste0("Resonance vs. Novelty, w = ", w),
         subtitle = "with a regression line",
         caption = "Data source: CZ (2013-2017)",
         fill = "speech\ncount") +
    scale_x_continuous(breaks = seq(-4, 8, 2)) +
    scale_y_continuous(breaks = seq(-6, 6, 2)) +
    theme_janco_point() +
    theme(legend.direction = "vertical", legend.position = "right",
          plot.caption = element_text(hjust = 0.5)
    )
  
  ggsave(filename = paste0("rn_", w, ".png"), path = "cze/data/plots/RN/")
  
  
  
  # NOVELTY VS TRANSIENCE
  ggplot(ntr_w, aes(z_novelty, z_transience)) +
    geom_bin2d(bins = 100) +
    scale_fill_viridis_c(trans = "log", breaks = c(1,10,100,1000,10000),
                         labels = trans_format("log10", math_format(expr = 10^.x, format = force))) +
    geom_abline(colour = "black", linetype = "dashed") +
    labs(x = "Novelty (z-scaled)\n", y = "Transience (z-scaled)", 
         title = paste0("Novelty vs. Transience, w =", w),
         subtitle = "with an identity line (x = y)",
         caption = "Data source: CZ (2013-2017)",
         fill = "speech\ncount") +
    scale_x_continuous(breaks = seq(-4, 8, 2)) +
    scale_y_continuous(breaks = seq(-6, 8, 2)) +
    theme_janco_point() +
    theme(legend.direction = "vertical", legend.position = "right",
          plot.caption = element_text(hjust = 0.5)
    )
  
  ggsave(filename = paste0("nt_", w, ".png"), path = "cze/data/plots/NT/")
  
  
}

export_shuff_plots <- function(path) {
  
  # load in
  ntr_w = read_csv(path)
  
  # extract timeframe
  w = str_extract(path, "(?<=W)\\d+")
  
  
  # RESONANCE ~ NOVELTY
  ggplot(ntr_w, aes(z_novelty, z_resonance)) +
    geom_bin2d(bins = 100) +
    scale_fill_viridis_c(trans = "log", breaks = c(1,10,100,1000),
                         labels = trans_format("log10", 
                                               math_format(expr = 10^.x, format = force))) +
    geom_smooth(method = "lm", colour = "black", alpha = 0, size = 0.5) +
    labs(x = "Novelty (z-scaled)\n", y = "Resonance (z-scaled)", 
         title = paste0("Shuffled Resonance vs. Novelty, w = ", w),
         subtitle = "with a regression line",
         caption = "Data source: CZ (2013-2017)",
         fill = "speech\ncount") +
    scale_x_continuous(breaks = seq(-4, 8, 2)) +
    scale_y_continuous(breaks = seq(-6, 6, 2)) +
    theme_janco_point() +
    theme(legend.direction = "vertical", legend.position = "right",
          plot.caption = element_text(hjust = 0.5)
    )
  
  ggsave(filename = paste0("shuff_rn_", w, ".png"), path = "cze/data/plots/shuffled/")
  
  
  
  # NOVELTY VS TRANSIENCE
  ggplot(ntr_w, aes(z_novelty, z_transience)) +
    geom_bin2d(bins = 100) +
    scale_fill_viridis_c(trans = "log", breaks = c(1,10,100,1000,10000),
                         labels = trans_format("log10", math_format(expr = 10^.x, format = force))) +
    geom_abline(colour = "black", linetype = "dashed") +
    labs(x = "Novelty (z-scaled)\n", y = "Transience (z-scaled)", 
         title = paste0("Shuffled Novelty vs. Transience, w =", w),
         subtitle = "with an identity line (x = y)",
         caption = "Data source: French National Assembly debates (1998-2019)",
         fill = "speech\ncount") +
    scale_x_continuous(breaks = seq(-4, 8, 2)) +
    scale_y_continuous(breaks = seq(-6, 8, 2)) +
    theme_janco_point() +
    theme(legend.direction = "vertical", legend.position = "right",
          plot.caption = element_text(hjust = 0.5)
    )
  
  ggsave(filename = paste0("shuff_nt_", w, ".png"), path = "cze/data/plots/shuffled/")
  
  
}

export_plots(ntr_test_path[1])
export_shuff_plots(ntr_test_path[4])
