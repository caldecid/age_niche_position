
# Biome pgls by class ------------------------------------------------------

##sourcing the libraries and the directories
source(file.path(getwd(), "/source.R"))

##reading vertebrate data set
vert_enfa_ages <- read_csv(file = "results/data/processed/vert_enfa_ages.csv")

y <- vert_enfa_ages %>% group_by(className, biome) %>% count()

biomes <- unique(vert_enfa_ages$biome)

########"Tropical_&_subtropical_moist_broadleaf_forests" analysis

##mammals


mammals_biome_marg <- fun_biome_pgls(df = vert_enfa_ages, class = "MAMMALIA",
                                   phy = mammals_phy, variable = "marginality")

mammals_biome_marg <- do.call("rbind", mammals_biome_marg)

mammals_biome_marg$variable <- "marginality" 

row.names(mammals_biome_marg) <- NULL


mammals_biome_spe <- fun_biome_pgls(df = vert_enfa_ages, class = "MAMMALIA", 
                                phy = mammals_phy, variable = "specialization")

mammals_biome_spe <- do.call("rbind", mammals_biome_spe)

mammals_biome_spe$variable <- "specialization" 

row.names(mammals_biome_spe) <- NULL


###Reptiles
reptiles_biome_marg <- fun_biome_pgls(df = vert_enfa_ages, class = "REPTILIA",
                                 phy = reptilia_phy, variable = "marginality")


reptiles_biome_marg <- do.call("rbind", reptiles_biome_marg)

reptiles_biome_marg$variable <- "marginality" 

row.names(reptiles_biome_marg) <- NULL


reptiles_biome_spe <- fun_biome_pgls(df = vert_enfa_ages, class = "REPTILIA",
                                phy = reptilia_phy, variable = "specialization")

reptiles_biome_spe <- do.call("rbind", reptiles_biome_spe)

reptiles_biome_spe$variable <- "specialization" 

row.names(reptiles_biome_spe) <- NULL

######AVES

#marginality
birds_biome_marg <- fun_biome_pgls(df = vert_enfa_ages, class = "AVES",
                                   phy = birds_phy, variable = "marginality")

birds_biome_marg <- do.call("rbind", birds_biome_marg)

birds_biome_marg$variable <- "marginality" 

row.names(birds_biome_marg) <- NULL

#specialization
birds_biome_spe <- fun_biome_pgls(df = vert_enfa_ages, class = "AVES",
                                   phy = birds_phy, variable = "specialization")


birds_biome_spe <- do.call("rbind", birds_biome_spe)

birds_biome_spe$variable <- "specialization" 

row.names(birds_biome_spe) <- NULL

#########Amphibians

#marginality
amphibians_biome_marg <- fun_biome_pgls(df = vert_enfa_ages, class = "AMPHIBIA",
                                        phy = amphibia_phy,
                                        variable = "marginality")

amphibians_biome_marg <- do.call("rbind", amphibians_biome_marg)

amphibians_biome_marg$variable <- "marginality" 

row.names(amphibians_biome_marg) <- NULL

#specialization
amphibians_biome_spe <- fun_biome_pgls(df = vert_enfa_ages, class = "AMPHIBIA",
                                        phy = amphibia_phy,
                                        variable = "specialization")

amphibians_biome_spe <- do.call("rbind", amphibians_biome_spe)

amphibians_biome_spe$variable <- "specialization" 

row.names(amphibians_biome_spe) <- NULL


#######unifying results for saving
#marginality
biome_marg_results <- rbind(mammals_biome_marg, reptiles_biome_marg,
                            birds_biome_marg, amphibians_biome_marg)

write_csv(biome_marg_results, file = "results/data/processed/biome_marg_results.csv")

#specialization
biome_spe_results <- rbind(mammals_biome_spe, reptiles_biome_spe,
                           birds_biome_spe, amphibians_biome_spe)

write_csv(biome_spe_results, 
                     file = "results/data/processed/biome_spe_results.csv")


