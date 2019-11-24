library(tidyverse)
library(scales)
library(ggunchained)

ntr_paths = list.files("fra/data/ntr_out", pattern = "*.csv", full.names = T)


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
         caption = "Data source: French National Assembly debates (1998-2019)",
         fill = "speech\ncount") +
    scale_x_continuous(breaks = seq(-4, 8, 2)) +
    scale_y_continuous(breaks = seq(-6, 6, 2)) +
    theme_janco_point() +
    theme(legend.direction = "vertical", legend.position = "right",
          plot.caption = element_text(hjust = 0.5)
    )
  
  ggsave(filename = paste0("rn_", w, ".png"), path = "fra/data/plots/RN/")
  
  
  
  # NOVELTY VS TRANSIENCE
  ggplot(ntr_w, aes(z_novelty, z_transience)) +
    geom_bin2d(bins = 100) +
    scale_fill_viridis_c(trans = "log", breaks = c(1,10,100,1000,10000),
                         labels = trans_format("log10", math_format(expr = 10^.x, format = force))) +
    geom_abline(colour = "black", linetype = "dashed") +
    labs(x = "Novelty (z-scaled)\n", y = "Transience (z-scaled)", 
         title = paste0("Novelty vs. Transience, w =", w),
         subtitle = "with an identity line (x = y)",
         caption = "Data source: French National Assembly debates (1998-2019)",
         fill = "speech\ncount") +
    scale_x_continuous(breaks = seq(-4, 8, 2)) +
    scale_y_continuous(breaks = seq(-6, 8, 2)) +
    theme_janco_point() +
    theme(legend.direction = "vertical", legend.position = "right",
          plot.caption = element_text(hjust = 0.5)
    )
  
  ggsave(filename = paste0("nt_", w, ".png"), path = "fra/data/plots/NT/")
  
  
}

lapply(ntr_paths, export_plots)
