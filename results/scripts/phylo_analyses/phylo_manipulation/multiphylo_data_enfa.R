############calculating species ages and joining with ENFA metrics############

##sourcing the libraries, directories and functions

source(file.path(getwd(), "/source.R"))

#########The results generated in this script were sent to the server #########


##reading vertebrates niche positions
vertebrates_biome_enfa <- read_csv("results/data/processed/vertebrates_biome_enfa.csv")

##eliminate previous ages
vertebrates_biome_enfa <- vertebrates_biome_enfa %>% select(-tip.mean)


############ Loops for measuring the species ages from 100 phylogenies#########

set.seed(13)

###########Birds##############
##listing files (were not included in the github repo, but are available in https://vertlife.org/data/ )

##reading multiphylo
birds.multiphylo <- ape::read.tree("results/data/raw/phylogenies/birds/HackettStage1Full_1.tre")

##choosing 100 random trees
birds.multiphylo <- birds.multiphylo[sample(1:1000, size = 100, replace = TRUE)]

##list for storing species ages

birds.list.ages <- lapply(birds.multiphylo, corrective_species_age, rho = 0.9)

###joining species with marginality and specialization info
for(i in seq_along(birds.list.ages)) {
  birds.list.ages[[i]] <- inner_join(vertebrates_biome_enfa,
                                     birds.list.ages[[i]], by = "species")
}

##########Amphibians
##listing files (were not included in the github repo, but are available in https://vertlife.org/data/ )
amphi.multiphylo <- ape::read.tree("results/data/raw/phylogenies/amphibians/amph_shl_new_Posterior_7238.1000.trees")

###choosing 100 random trees
amphi.multiphylo <- amphi.multiphylo[sample(1:1000, size = 100, replace = TRUE)]

#list for storing species ages
amphi.list.ages <- vector(mode = "list", length = 100)

##lapply for corrective species ages
amphi.list.ages <- lapply(amphi.multiphylo, corrective_species_age, rho = 0.9)


###joining species with marginality and specialization info
for(i in seq_along(amphi.list.ages)) {
  amphi.list.ages[[i]] <- inner_join(vertebrates_biome_enfa,
                                     amphi.list.ages[[i]], by = "species")
}

######reptiles
##listing files (were not included in the github repo, but are available in https://vertlife.org/data/ )

rep.multiPhylo <- ape::read.tree("results/data/raw/phylogenies/reptiles/squamate_phylo/squam_shl_new_Posterior_9755.10000.trees")

###choosing 100 random trees
rep.multiphylo <- rep.multiPhylo[sample(1:1000, size = 100, replace = TRUE)]

#list for storing species ages
rep.list.ages <- vector(mode = "list", length = 100)

##lapply for corrective species ages
rep.list.ages <- lapply(rep.multiphylo, corrective_species_age, rho = 0.9)

###joining species with marginality and specialization info
for(i in seq_along(rep.list.ages)) {
  rep.list.ages[[i]] <- inner_join(vertebrates_biome_enfa,
                                   rep.list.ages[[i]], by = "species")
}


##mammals

##listing files (were not included in the github repo, but are available in https://vertlife.org/data/ )
mammals_files <- list.files("results/data/raw/phylogenies/mammals/mammals_phy", pattern = "\\.tre$", full.names = TRUE)

##picking 100 random trees
mammals.multiphylo <- as.multiPhylo(lapply(sample(mammals_files, size = 100, replace = TRUE), ape::read.tree))

##lapply for corrective species ages
mammals.list.ages <- lapply(mammals.multiphylo,
                            corrective_species_age, rho = 0.9)

###joining species with marginality and specialization info
for(i in seq_along(mammals.list.ages)) {
  mammals.list.ages[[i]] <- inner_join(vertebrates_biome_enfa,
                                       mammals.list.ages[[i]], by = "species")
}

##saving lists enfa ages
save(mammals.list.ages, birds.list.ages, rep.list.ages, amphi.list.ages,
     file = "results/data/processed/vert_enfa_ages_list.RData")

##saving multiphylo 
save(mammals.multiphylo, birds.multiphylo, rep.multiphylo, amphi.multiphylo,
     file = "results/data/metadata/vert_multiphylo_list.RData")
