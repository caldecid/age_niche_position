##libraries
library(tidyverse)
library(phytools)
library(caper)
library(purrr)

##calling tetrapods enfa for whole neotropics

#aves
aves_enfa_neo <- read_csv("results/data/processed/aves_enfa_whole_neotropics.csv") %>% 
                 rename(neo_marginality = marginality,
                        neo_specialization = specialization)

#mammals
mammals_enfa_neo <- read_csv("results/data/processed/mammals_enfa_whole_neotropics.csv")%>% 
                                       rename(neo_marginality = marginality,
                                               neo_specialization = specialization)

#reptiles
reptiles_enfa_neo <- read_csv("results/data/processed/reptiles_enfa_whole_neotropics.csv")%>% 
                                      rename(neo_marginality = marginality,
                                               neo_specialization = specialization)

#amphibians
amphibians_enfa_neo <- read_csv("results/data/processed/amphibians_enfa_whole_neotropics.csv")%>% 
                                           rename(neo_marginality = marginality,
                                                  neo_specialization = specialization)

##calling RData lists with estimated and corrected ages
load("results/data/processed/vert_enfa_ages_list.RData")

#transforming the list with the ENFA measures from the whole neotropics

#aves
aves.list.ages_joined <- purrr::map(birds.list.ages, ~
                                       left_join(.x, aves_enfa_neo, by = c("species" = "species_name"))
)


##mammals
mammals.list.ages_joined <- purrr::map(mammals.list.ages, ~
                                       left_join(.x, mammals_enfa_neo, by = c("species" = "species_name"))
)

#reptiles
reptiles.list.ages_joined <- purrr::map(rep.list.ages, ~
                                       left_join(.x, reptiles_enfa_neo, by = c("species" = "species_name"))
)

#amphibians
amphi.list.ages_joined <- purrr::map(amphi.list.ages, ~
                                left_join(.x, amphibians_enfa_neo, by = c("species" = "species_name"))
)

##
