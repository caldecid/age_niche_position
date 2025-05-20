
# arranging tables for supplementary --------------------------------------


library(tidyverse)
library(writexl)
library(openxlsx)



# marginality -------------------------------------------------------------

##calling models
biome_marg_results <- read_csv("results/data/processed/biome_marg_results.csv")

biomes <- unique(biome_marg_results$biome)

##saving 
for(i in seq_along(biomes)){
  
  df <- biome_marg_results %>% filter(biome == biomes[i])
  
  openxlsx::write.xlsx(df,
             file = paste0("results/data/processed/biomes/marginality/",
                           biomes[i], ".xlsx"))
  
}


# specialization ----------------------------------------------------------

##calling models
biome_spe_results <- read_csv("results/data/processed/biome_spe_results.csv")

biomes <- unique(biome_spe_results$biome)

##saving 
for(i in seq_along(biomes)){
  
  df <- biome_spe_results %>% filter(biome == biomes[i])
  
  openxlsx::write.xlsx(df,
         file = paste0("results/data/processed/biomes/specialization/",
                                     biomes[i], ".xlsx"))
  
}

