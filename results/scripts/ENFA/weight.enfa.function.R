# Function for weighted enfa results --------------------------------------

weighted.enfa <- function(dsm, realm, results){
  #dsm = distribution spatial maps of the species
  #realm = realm
  #results = data.frame from the enfa.separated function 
  
  ###data frame with the species which are present in only one biome
  results.unique <- results[ave(seq_along(results$species), 
                                results$species, FUN = length) == 1, ]%>%
                                arrange(species)
  
  ###species which are  present in more than one biome
  results.rep <- results[-which(results$species %in% 
                        results.unique$species), ] %>%
                        group_by(species) %>%
                        nest(data = c(biome, marginality, specialization))%>%
                        arrange(species)
  
  ##subset dsm for only the repeated species
  dsm.2 <- dsm[which(dsm@data$BINOMIAL %in% results.rep$species),]
  
  ##rename
  nest.tibble <- results.rep
  
  #weighted.marginality
  nest.tibble$w.marginality <- rep(NA, nrow(nest.tibble))
  
  #weighted.specialization
  nest.tibble$w.specialization <- rep(NA, nrow(nest.tibble))
  
  ##progress bar
  pb <- progress_bar$new(total = nrow(nest.tibble))
  
  for(j in 1:nrow(nest.tibble)){
    ##progress bar
    pb$tick()
    
    #creating area column inside the nested tibble
    nest.tibble[[2]][[j]]$area <- rep(NA, nrow(nest.tibble[[2]][[j]]))
    
   
    for(i in 1:nrow(nest.tibble[[2]][[j]])){
      
       #area of the intersection
       nest.tibble[[2]][[j]]$area[i] <- suppressWarnings(try(gArea
                                           (gIntersection(dsm.2[j,],
                                            realm[realm@data$BIOME_D  ==
                                            nest.tibble[[2]][[j]]$biome[i],]))))
    
    }
    
  
  #weighted marginality
  nest.tibble$w.marginality[j] <- 
    weighted.mean(nest.tibble[[2]][[j]]$marginality,
                  (nest.tibble[[2]][[j]]$area)/sum(nest.tibble[[2]][[j]]$area))
    
  #weighted specialization
  nest.tibble$w.specialization[j] <- 
    weighted.mean(nest.tibble[[2]][[j]]$specialization,
                  (nest.tibble[[2]][[j]]$area)/sum(nest.tibble[[2]][[j]]$area))
  }
  ##terminate progress bar
  pb$terminate()
  
  ##renaming columns
  results.rep.1 <- nest.tibble %>% 
                   rename(marginality = w.marginality, 
                          specialization = w.specialization)
  results.unique.1 <- results.unique %>%
                      rename(data = biome) %>%
                      mutate(data = as.list(data))
  
  results.total <- bind_rows(results.rep.1, results.unique.1)
  
  return(results.total)
}


