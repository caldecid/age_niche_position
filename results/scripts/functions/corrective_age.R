# Correcting species age for three extinction scenarios --------

#' @title Speciation and extinction rates from phylogeny
#'
#' @description This function corrects the species age for three extinction scenarios
#' 
#' @param phy An ultrametric bifurcating phylogenetic tree, in ape "phylo" format.
#' @param rho Sampling fraction
#' 
#' @return A dataset with species names, phylogenetic age, and three columns with the age corrected for three extinciton scenarios

corrective_species_age <- function(phy, rho){
  
 
  #########Calculate phylogenetic age
  
  phylo_age <- calculate_tip_ages(phy)
  
  #tip.age as numeric
  phylo_age$tip.age <- as.numeric(phylo_age$tip.age)
  
  #######Estimate diversification rates
  
  ##ultrametric tree
  phy = force.ultrametric(phy)
  
  ##are there negative branches?
  phy$edge.length[which(phy$edge.length< 0)] = 0
  
  ###low extinction fraction
  low.ext <- estimate_bd(phy, epsilon = 0.10)
  
  ##creating a tibble for posterior merging
  low.ext <- tibble(lambda = low.ext[1],
                       mu = low.ext[2],
                       epsilon = "low")
  
  ##intermediate
  int.ext <- estimate_bd(phy, epsilon = 0.5)
  
  ##creating a tibble for posterior merging
  int.ext <- tibble(lambda = int.ext[1],
                       mu = int.ext[2],
                       epsilon = "int")
  
  ###high
  high.ext <- estimate_bd(phy, epsilon = 0.95)
  
  ##creating a tibble for posterior merging
  high.ext <- tibble(lambda = high.ext[1],
                        mu = high.ext[2],
                        epsilon = "high")
  
  
  #######Probabilistic function for the three extinction scenarios
  
  #low extinction
  ext.low.age <- c()
  
  
  for(i in 1:nrow(phylo_age)){
    ext.low.age[i] <- meanAgeFunction(lambda = low.ext$lambda,
                                         mu = low.ext$mu,
                                         rho = rho,
                                         v = phylo_age$tip.age[i])
  }
  
  
  ##intermediate extinction scenario
  
  ext.int.age <- c()
  
  
  for(i in 1:nrow(phylo_age)){
    ext.int.age[i] <- meanAgeFunction(lambda = int.ext$lambda,
                                         mu = int.ext$mu,
                                         rho = rho,
                                         v = phylo_age$tip.age[i])
  }
  
  
  ##high extinction scenario
  ext.high.age <- c()
  
  
  for(i in 1:nrow(phylo_age)){
    ext.high.age[i] <- meanAgeFunction(lambda = high.ext$lambda,
                                          mu = high.ext$mu,
                                          rho = rho,
                                          v = phylo_age$tip.age[i])
  }
  
  ##binding
  phylo_age <- cbind(phylo_age, ext.low.age, ext.int.age, ext.high.age)
  
  
  names(phylo_age) <- c("species", "Estimated.age", "low.age",
                          "int.age", "high.age")
  
  return(phylo_age)
  
}
