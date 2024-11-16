##sourcing the libraries and the directories
source(file.path(getwd(), "/source.R"))

# neotropical reptiles ----------------------------------------------------

##terrestrial ecosystems
terrestrial_ecos <- readOGR("Biomes", "wwf_terr_ecos")

###only Neotropics
neotropics <- terrestrial_ecos[which(terrestrial_ecos@data[["REALM"]] == "NT"), ]

##eliminating inexisting biomes, there are no biome 98 and 99
bio98 <- which(neotropics@data$BIOME == 98)
bio99 <- which(neotropics@data$BIOME == 99)
ine.bios <- c(bio98, bio99)
neotropics <- neotropics[-ine.bios, ]

##aggregating the polygons which pertain to the same biome
neotropics.ag <- aggregate(neotropics, by = "BIOME")

###creating a dataframe with the biomes names
BIOME_n <- c(1:14)
BIOME_D <- c("Tropical_&_subtropical_moist_broadleaf_forests", 
             "Tropical_&_subtropical_dry_broadleaf_forests",
             "Tropical_&_subtropical_coniferous_forests", 
             "Temperate_broadleaf_&_mixed_forests",
             "Temperate_coniferous_forests",
             "Boreal_Forests/Taiga",
             "Tropical_&_subtropical_grasslands_savannas_and_shrublands",
             "Temperate_grasslands_savannas_and_ shrublands",
             "Flooded_grasslands_and_savannas",
             "Montane_grasslands_and_shrublands",
             "Tundra", "Mediterranean_forests_woodlands_and_scrub",
             "Deserts_and_xeric_shrublands", "mangroves")
Biome <- data.frame(BIOME_n, BIOME_D)

neotropics.ag <- merge(neotropics.ag, Biome, by.x = "BIOME", by.y = "BIOME_n")

###getting climate data####
w <- raster :: getData('worldclim', var='bio', res=10)

####getting dsm neotropical reptiles###
neotropical_reptiles <- readOGR("neotropical_reptiles", "data_0")

###calling neotropical reptiles
neotropical_reptiles_dsm_climate <- dsm.climate.vectorized(realm 
                                                 = neotropics.ag,
                                                 dsm = neotropical_reptiles,
                                                           w = w)



###calling enfa
dsm.ag.neo.reptiles <- neotropical_reptiles_dsm_climate[[2]]
clim.neotropics <-neotropical_reptiles_dsm_climate[[1]]

neotropical_reptiles_enfa <- enfa.separated(realm = neotropics.ag,
                                            dsm.ag = dsm.ag.neo.reptiles,
                                            clim.l = clim.neotropics)



##weighted ENFA
results.neo.reptiles <- neotropical_reptiles_enfa

neo.reptiles.weighted.enfa <- weighted.enfa(dsm = dsm.ag.neo.reptiles, 
                                          realm = neotropics.ag,
                                          results = results.neo.reptiles)

##joining dataframes
reptiles_biome_enfa <- left_join(neotropical_reptiles_enfa,
                                 neo.reptiles.weighted.enfa,
                                 by = "species")

##saving neotropical enfa reptiles
write_csv(neotropical_reptiles_enfa,
          file = "results/data/metadata/reptiles_biome_enfa.csv")


# neotropical amphibians --------------------------------------------------

###getting climate data####
w <- getData('worldclim', var='bio', res=10)

####getting dsm neotropical amphibians###
neotropical_amphibians <- readOGR("neotropical_amphibians", "data_0")

###calling dsm and climate
neotropical_amphibians_dsm <- dsm.climate.vectorized(realm = neotropics.ag,
                                                           dsm = neotropical_amphibians,
                                                           w = w)
###extracting species polygons and climate raster
dsm.amphibians <- neotropical_amphibians_dsm[[2]]
clim.neotropics <- neotropical_amphibians_dsm[[1]]

###neotropical amphibians enfa
neotropical_amphibians_enfa <- enfa.separated(realm = neotropics.ag,
                                            dsm.ag = dsm.amphibians,
                                            clim.l = clim.neotropics)


##weighted ENFA
results.neo.amphibians <- neotropical_amphibians_enfa

neo.amphibians.weighted.enfa <- weighted.enfa(dsm = dsm.amphibians, 
                                            realm = neotropics.ag,
                                            results = results.neo.amphibians)
##joining df
neotropical_amphibians_enfa <- left_join(neotropical_amphibians_enfa,
                                         neo.amphibians.weighted.enfa,
                                         by = "species")


##saving
write_csv(neotropical_amphibians_enfa,
          file = "results/data/metadata/amphibians_biome_enfa.RData")

# neotropical mammals -----------------------------------------------------


###getting climate data####
w <- getData('worldclim', var='bio', res=10)

####getting dsm neotropical mammals###
neotropical_mammals<- readOGR("neotropical_mammals", "data_0")

###calling dsm and climate
neotropical_mammals_dsm <- dsm.climate.vectorized(realm = neotropics.ag,
                                                     dsm = neotropical_mammals,
                                                     w = w)
###extracting species polygons and climate raster
dsm.mammals <- neotropical_mammals_dsm[[2]]
clim.neotropics <- neotropical_mammals_dsm[[1]]

###neotropical mammals enfa
neotropical_mammals_enfa <- enfa.separated(realm = neotropics.ag,
                                              dsm.ag = dsm.mammals,
                                              clim.l = clim.neotropics)

##weighted ENFA
results.neo.mammal <- neotropical_mammals_enfa

neo.mammal.weighted.enfa <- weighted.enfa(dsm = dsm.mammals, 
                                          realm = neotropics.ag,
                                          results = results.neo.mammal)

##joining dfs
neotropical_mammals_enfa <- left_join(neotropical_mammals_enfa,
                                      neo.mammal.weighted.enfa,
                                      by = "species")

##save
write_csv(neotropical_mammals_enfa,
               file = "results/data/metadata/mammals_biome_enfa.RData")


# neotropical_birds ------------------------------------------------------

##sf for birds
bird_sp <- st_read(dsn = "BOTW.gdb")

##transforming into a spatialpolygondataframe

bird_multipolygon <- st_cast(st_geometry((bird_sp, "MULTIPOLYGON")))
 
bird_spatialpolygon <- as_Spatial(bird_multipolygon)

bird_data <- bird_sp %>% st_drop_geometry()

bird_ranges <- SpatialPolygonsDataFrame(bird_spatialpolygon, data = bird_data,
                                        match.ID = FALSE)

###only birds present in the neotropics
bird_neo <- bird_ranges[which(gIntersects(bird_ranges, aggregate(neotropics.ag),
                                          byid = TRUE)), ]

bird_neo.present <- bird_neo[bird_neo@data$presence == 1, ]
##origin
bird_neo.present <- bird_neo.present[bird_neo.present@data$origin == 1,]
##seasonal
bird_neo.present <- bird_neo.present[bird_neo.present@data$seasonal == 1, ] 

##croping only neotropical ranges
bird_neotropics <- crop(x = bird_neo.present,  y = extent(neotropics.ag))

###simplyfying polygons 

###obtaining dsm data
bird_data <- bird_neotropics@data

###simplyfying the dsm
bird.neo.simple <- gSimplify(bird_neotropics, tol = 0.00001)

###spatialpolygon dataframe
###gSimplify transforms the dsm into a spatialpolygon, so we need the data
bird.simple.data <- SpatialPolygonsDataFrame(bird.neo.simple, data = bird_data)

###aggregating for disolving the polygons of repeated species
bird_neo_ag <- aggregate(bird.simple.data, by = "sci_name")

##minimal area for ENFA functioning properly

bird_neo_min <- bird_neo_ag[gArea(bird_neo_ag, byid = TRUE)
                                > 0.11, ]
###dealing with climate
clim.l <- list()

##empty list for storing the highly correlated climatic variables

clim.vif <- list()

##loop for cropping and storing the weather of each biome

for(i in 1:length(neotropics.ag)){
  
  clim.l[[i]] <- crop(x = w, y = extent(neotropics.ag[i, ]))
  
  ##mask for cropping the exact biome extension
  
  clim.l[[i]] <- mask(clim.l[[i]], neotropics.ag[i, ])
  
  ##detecting highly correlated variables
  
  clim.vif[[i]] <- vifstep(clim.l[[i]])
  
  ##excluding highly correlated variables
  clim.l[[i]] <- exclude(clim.l[[i]], clim.vif[[i]])
}

##adjusting projections
crs(bird_neo_min) <- crs(clim.l[[1]])

##creating list
list.dsm.birds.climate <- list(bird_neo_min, clim.l)
names(list.dsm.birds.climate) <- c("neo_birds", "climate")

save(list.dsm.birds.climate, file = "clime_and_neo_birds.RData")

###spatial polygon dataframe
bird_ranges <- list.dsm.birds.climate[[1]]

##Climate
clim.neo <- list.dsm.birds.climate[[2]]

###ENFA
birds_enfa <- enfa.separated(realm = neotropics.ag, dsm.ag = bird_ranges, 
                             clim.l = clim.neo)

##weighted ENFA
neo.birds.weighted.enfa <- weighted.enfa(dsm = bird_ranges, 
                                          realm = neotropics.ag,
                                          results = birds_enfa)

##joining dfs
neotropical_aves_enfa <- left_join(birds_enfa,
                                      neo.birds.weighted.enfa,
                                      by = "species")

##save
write_csv(neotropical_aves_enfa,
          file = "results/data/metadata/aves_biome_enfa.RData")

####joining all dataframes
vertebrates_biome_enfa <- rbind(neotropical_amphibians_enfa,
                                neotropical_reptiles_enfa,
                                neotropical_mammals_enfa,
                                neotropical_aves_enfa)

##saving
write_csv(vertebrates_biome_enfa, 
          file = "results/data/processed/vertebrates_biome_enfa.csv")