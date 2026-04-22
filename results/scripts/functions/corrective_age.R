# Probabilistic function for correcting species age -----------------------

## see https://github.com/thauffe/SpeciesAge.git

p0t <- function(lambda, mu, rho, t) {
  1 - (rho * (lambda - mu) / (rho * lambda + (lambda * (1 - rho) - mu) * exp((mu - lambda) * t)))
}

# the definite integral, Lambda(t1, t2) 
integratedRate <- function(lambda, mu, rho, t1, t2) {
  2 * (mu * t2 - log(lambda * rho - (lambda * (rho - 1) + mu) * exp((mu - lambda) * t2))) -
    2 * (mu * t1 - log(lambda * rho - (lambda * (rho - 1) + mu) * exp((mu - lambda) * t1))) 
}


ageDensityFunction <- function(lambda, mu, rho, t) {
  # get the integrated rate
  integrated_rate <- integratedRate(lambda, mu, rho, 0, t)
  # compute the density of the age
  age_density <- 2 * lambda * p0t(lambda, mu, rho, t) * dpois(0, integrated_rate)
  return(age_density) 
}



ageFunction <- function(lambda, mu, rho, t, v) {
  # get the integrated rate
  integrated_rate <- integratedRate(lambda, mu, rho, 0, t)
  # compute the density of the age 
  if ( t == v ) {
    age_density <- dpois(0, integrated_rate) 
  } else {
    age_density <- 2 * lambda * p0t(lambda, mu, rho, t) * dpois(0, integrated_rate) }
  return(age_density)
}


meanAgeFunction <- function(lambda, mu, rho, v) {
  # get the integrated rate
  integrated_rate <- integratedRate(lambda, mu, rho, 0, v)
  # compute the probability of zero events 
  p_no_events <- dpois(0, integrated_rate)
  # compute the mean, given that the age is not v 
  mean_with_events <- integrate(function(t) {
    t * ageDensityFunction(lambda, mu, rho, t) }, lower = 0, upper = v)$value
  # combine the probabilities
  mean_age <- p_no_events * v + mean_with_events
  return(mean_age) 
}

medianAgeFunction <- function(lambda, mu, rho, v) {
  # get the integrated rate
  integrated_rate <- integratedRate(lambda, mu, rho, 0, v)
  # compute the probability of zero events 
  p_no_events <- dpois(0, integrated_rate)
  # compute the median by optimization 
  median_age <- optim(par = v / 2, fn = function(t) {
    # compute the probability older than t 
    p <- integrate( function(s) {
      ageDensityFunction(lambda, mu, rho, s) 
    }, lower = t, upper = v)$value + p_no_events
    # compute the distance from the median 
    return(abs(p - 0.5))
  }, lower = 0, upper = v, method = "Brent")$par
  return(median_age) 
}


# SIM
forwardSimulate <- function(lambda, mu, rho, time) {
  # repeat until we get one extant lineage 
  repeat {
    # initialize simulation
    species <- data.frame(start = time, end = NA, status = "alive")
    # simulate forward 
    current_time <- time 
    repeat {
      # get the living species
      current_species <- which(species$status == "alive")
      # terminate if there are no living species
      if (length(current_species) == 0) { 
        break
      }
      # compute the rate of events
      event_rate <- (lambda + mu) * length(current_species)
      # generate a waiting time
      waiting_time <- rexp(1, event_rate) 
      current_time <- current_time - waiting_time
      # terminate if we go beyond the present 
      if (current_time < 0) {
        break 
      }
      # otherwise, perform an event
      if (runif(1) < lambda / (lambda + mu)) {
        # speciation event
        # choose affected species
        this_species <- current_species[sample.int(length(current_species), size = 1)]
        # terminate the species 
        species$status[this_species] <- "dead" 
        species$end[this_species] <- current_time
        # add new species
        new_species <- data.frame(start = c(current_time, current_time), end = NA, status = "alive")
        species <- rbind(species, new_species) 
      } else {
        # extinction event
        # choose affected species
        this_species <- current_species[sample.int(length(current_species), size = 1)]
        # terminate the species 
        species$status[this_species] <- "dead" 
        species$end[this_species] <- current_time
      }
    }
    # set missing times to 0 species$end[is.na(species$end)] <- 0
    # simulate incomplete sampling
    current_species <- which(species$status == "alive")
    species$status[current_species] <- ifelse(rbinom(length(current_species), size = 1, prob = rho) == 1, "alive", "dead")
    # check that there is exactly one extant species 
    if ( sum(species$status == "alive") == 1 ) {
      break 
    }
  }
  # done with simulation 
  return(species$start[species$status == "alive"])
}



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
