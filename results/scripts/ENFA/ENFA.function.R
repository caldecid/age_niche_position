#packages
library(ape)
library(BIEN)
library(sf)
library(tidyr)
library(dplyr)
library(raster)
library(maptools)
library(rgdal)
library(stringr)
library(rgeos)
library(cleangeo)
library(CENFA)
library(usdm)
library(svMisc)
library(progress)



# ENFA Function -----------------------------------------------------------

enfa.separated <- function(realm, dsm.ag, clim.l){
  #realm = realm from wwf terrestrial ecosystems (see metadata)
  #dsm.ag = distribution species model aggregated
  #clim.l = climatic variables cropped from the realm
  
  ######PAM for determining the presence of species within biomes
  
  #empty matrix
  pam.biomes <- matrix(, nrow = length(dsm.ag), ncol = length(realm)+1)
  
  #naming columns and first row
  colnames(pam.biomes) <- c("species", realm@data$BIOME_D)
  pam.biomes[,1] <- dsm.ag@data$BINOMIAL
  
  ###Loop for building the PAM of the biomes
  for(i in 1:length(realm)) {
    pam.biomes[,i+1] <- gIntersects(dsm.ag, realm[i,], byid = TRUE)
  }
  
  ##list for keeping vector which define the species present in each biome
  biome.list <- list()
  for(i in 1:length(realm)){
    biome.list[[i]] <- which(pam.biomes[,i+1] == TRUE)
  }
  
  ##empty list for storing the ENFA results for each species in each biome
  
  enfa.bio <- vector("list", length(realm))##one main list for each biome
  
  for(i in 1:length(realm)){
    enfa.bio[[i]] <- vector("list", length(which(pam.biomes[,i+1] == TRUE))) 
    #a nested list for each species present in each biome
  }
  
  ###print progress
  print("starting ENFA")
 
  ##progress bar
  pb <- progress_bar$new(total = length(biome.list))
  
  ##loop for performing the ENFA analysis
  for(j in 1:length(biome.list)){
    
    ##starting progress bar inside nested loop
    pb$tick()
    if(length(biome.list[[j]]) == 0){
      enfa.bio[[j]] <- NULL
    } else {
      for(i in 1:length(biome.list[[j]])){
        
        enfa.bio[[j]][[i]] <- suppressWarnings(try(enfa(x = clim.l[[j]], 
                                       s.dat = dsm.ag[biome.list[[j]][i],],
                                       parallel = TRUE, n = 3)))
      }
      
    }
    
  }
  pb$terminate()
  
  ########################ENFA results###################################
  
  # list storing the principal results of ENFA ------------------------------
  
  ##empty vector for storing the results
  output.l <- list()
  
  for(j in 1:length(enfa.bio)){
    
    output.l[[j]] <- data.frame(matrix(NA, nrow = length(enfa.bio[[j]]),
                                       ncol = 4))
    colnames(output.l[[j]]) <- c("biome", "species", "marginality", 
                                 "specialization")
    for(i in 1:length(enfa.bio[[j]])){
      output.l[[j]][i,1] <- realm@data[["BIOME_D"]][j]
      output.l[[j]][i,2] <- pam.biomes[biome.list[[j]][i],1]
      if(class(enfa.bio[[j]][[i]]) != "enfa"){
        output.l[[j]][i,3] <- NA
        output.l[[j]][i,4] <- NA
      } else{
        output.l[[j]][i,3] <- enfa.bio[[j]][[i]]@marginality
        output.l[[j]][i,4] <- enfa.bio[[j]][[i]]@specialization
      }
      
    }
    
  }  
  
  results <- do.call("rbind", output.l)
  
}

