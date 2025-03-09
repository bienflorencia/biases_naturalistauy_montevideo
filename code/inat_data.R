library(tidyverse)

natuy_mdeo <- read_csv('data/observations-536732.csv')

natuy_data_mdeo <- natuy_mdeo %>% 
  # filter records with low positional accuracy
  filter(positional_accuracy <= 500) %>% 
  # filter records with obscured coordinates
  filter(coordinates_obscured == F)


nrow(natuy_data_mdeo) # 10299 registros
nrow(natuy_data_mdeo %>% distinct(taxon_species_name)) # 1387 especies
