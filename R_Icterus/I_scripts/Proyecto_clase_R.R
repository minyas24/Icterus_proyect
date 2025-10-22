#install.packages("pacman")
library("pacman")
p_load("tidyverse",
       "readr",
       "here",
       "spThin",
       "readr",
       "dplyr",
       "dendextend")

#----------Generación de dendograma------------
dendo_I <- read_csv(here("R_Icterus", "I_data", "10_I_dominicensis_R.csv")) %>% select(-fid, -gbifID, -species, -month, -year) %>% scale( ) %>% as_tibble() %>%  dist( method = "euclidean") %>% hclust(method = "average") %>% as.dendrogram( ) 

#inicia el proceso de exportacion del plot
here("R_Icterus", "I_data", "graphics", "I10_dendo.png") %>%  png(width = 5000, height = 2500)

dendo_I %>% set( what = "labels_cex", value = 0.3) %>% set( "branches_k_color", k = 1, value = c( "orange2")) %>% plot()









