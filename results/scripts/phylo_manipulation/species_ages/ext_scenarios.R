##sourcing the libraries and the directories
source(file.path(getwd(), "/source.R"))


##calling the marginality and specialization dataset

vertebrates_biome_enfa <- read_csv("results/data/processed/vertebrates_biome_enfa.csv")

##eliminate previous ages
vertebrates_biome_enfa <- vertebrates_biome_enfa %>% select(-tip.mean)

######################correcting species ages#######################

##Mammals

mammals_phy <- read.tree("results/data/raw/phylogenies/mammals/mammals_phy/MamPhy_BDvr_Completed_5911sp_topoCons_FBDasZhouEtAl_v2_tree0000.tre")

##correcting ages
mammals_age <- corrective_species_age(phy = mammals_phy, rho = 0.9)

##mammals dataset
mammals_data <- inner_join(vertebrates_biome_enfa, mammals_age, by = "species")

####Reptiles

reptiles_phy <- read.tree("results/data/raw/phylogenies/reptiles/squamate_phylo/reptiles_phy.tree")

##correcting ages
reptiles_ages <- corrective_species_age(phy = reptiles_phy, rho = 0.9)

##reptiles dataset
reptiles_data <- inner_join(vertebrates_biome_enfa, reptiles_ages, by = "species")

##Amphibians

amphibians_phy <- read.tree("results/data/raw/phylogenies/amphibians/amphibians_phy.tree")

##correcting ages
amphibians_ages <- corrective_species_age(phy = amphibians_phy, rho = 0.9)

##amphibians dataset
amphibians_data <- inner_join(vertebrates_biome_enfa, amphibians_ages, by = "species")

###Birds

birds_phy <- read.tree("results/data/raw/phylogenies/birds_phy.tree")

##correcting ages
bird_ages <- corrective_species_age(phy = birds_phy, rho = 0.9)

##birds dataset
birds_data <- inner_join(vertebrates_biome_enfa, bird_ages, by = "species")

##total dataset
vert_enfa_ages <- rbind(mammals_data, reptiles_data,
                         amphibians_data, birds_data)
##saving
write_csv(vert_enfa_ages, file = "results/data/processed/vert_enfa_ages.csv")
