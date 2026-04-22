##sourcing the libraries and the directories
source(file.path(getwd(), "/source.R"))

# neotropical reptiles ----------------------------------------------------

library(tidyverse)
library(terra)
library(sf)
library(geodata)
library(stringr)
library(devtools)
devtools::install_github("rinnan/CENFA")
library(CENFA)
library(sp)
library(parallel)




# Neotropics --------------------------------------------------------------

ecoregions <- st_read("results/data/raw/ecoregions/Global_200_Terrestrial.shp")

biomes <- st_read("results/data/raw/biomes/wwf_terr_ecos.shp")

neotropics <- biomes %>% filter(REALM == "NT")

##how to aggregate all the neotropics biomes in just one area

neotropics <- st_make_valid(neotropics)  # first quick pass

#  Force validation using a geometry repair trick
neotropics_valid <- neotropics %>%
  mutate(geometry = st_buffer(geometry, 0))

#  Now safely dissolve to one polygon
neotropics_union <- neotropics_valid %>%
  summarise(geometry = st_union(geometry)) %>%
  st_make_valid()

#just calculating the area by biome for further analyses
neotropics_equal <- st_transform(neotropics, crs = "ESRI:54009")  # Mollweide equal-area projection

# Calculate area in km² if not already accurate
neotropics_equal <- neotropics_equal %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 10^6)

# Summarize total area per biome
area_by_biome <- neotropics_equal %>%
  group_by(BIOME) %>%
  summarise(total_area_km2 = sum(area_km2, na.rm = TRUE)) %>%
  arrange(desc(total_area_km2)) %>% 
  st_drop_geometry() %>% 
  filter(!BIOME %in% c(99, 98))  %>%
  mutate(BIOME_name = recode(
    BIOME,
    `1`  = "Tropical & Subtropical Moist Broadleaf Forests",
    `2`  = "Tropical & Subtropical Dry Broadleaf Forests",
    `3`  = "Tropical & Subtropical Coniferous Forests",
    `4`  = "Temperate Broadleaf & Mixed Forests",
    `5`  = "Temperate Conifer Forests",
    `6`  = "Boreal Forests/Taiga",
    `7`  = "Tropical & Subtropical Grasslands, Savannas & Shrublands",
    `8`  = "Temperate Grasslands, Savannas & Shrublands",
    `9`  = "Flooded Grasslands & Savannas",
    `10` = "Montane Grasslands & Shrublands",
    `11` = "Tundra",
    `12` = "Mediterranean Forests, Woodlands & Scrub",
    `13` = "Deserts & Xeric Shrublands",
    `14` = "Mangroves"
  )) 

write_csv(area_by_biome, file = "results/data/metadata/area_by_biome.csv")

# Climatic data -----------------------------------------------------------

w <- geodata::worldclim_global(var = "bio", res = 10, path = tempdir())


######## Cropping, masking against the neotropics the climate raster file #########

#cropping 
climate_neotropics <- terra::crop(w, neotropics_union)

#mask to exact boundary
climate_neotropics <- terra::mask(climate_neotropics, neotropics_union)

#detect highly correlated variables
clim_vif <- usdm::vifstep(climate_neotropics)

#excluding 
climate_neo_2 <- usdm::exclude(climate_neotropics, clim_vif)

#rasterizing
climate_raster <- raster::stack(as(climate_neo_2, "Raster"))

#saving
saveRDS(climate_raster, "results/data/metadata/climate_neotropic.rds")

#reading
climate_raster <- readRDS("results/data/metadata/climate_neotropic.rds")

# Mammals' shape files ----------------------------------------------------
mammals_sh <- st_read("results/data/raw/mammals_shapes/MAMMALS_TERRESTRIAL_ONLY.shp")

#modifying species name
mammals_sh$sci_name <- str_replace(mammals_sh$sci_name, " ", "_")

#calling the mammals that have phylogenetic data
mammals_unique <- read_csv("results/data/processed/vert_enfa_unique.csv") %>% 
  filter(className == "MAMMALIA")

#filtering shapes of mammals not present in our analyses
mammals_sh_filter <- mammals_sh %>% filter(sci_name %in% mammals_unique$species)

# Dissolve polygons per species (by scientific name)
mammals_agg <- mammals_sh_filter %>%
  group_by(sci_name) %>%
  summarise(across(everything(), first),  
            geometry = st_union(geometry)) %>%
  ungroup()

# Filter out island species
mammals_neo_sh <- mammals_agg %>% filter(is.na(island),
                                         presence == 1,
                                         origin == 1,
                                         seasonal == 1)


# Exclude species with very small ranges (< 0.15 area units)
mammals_neo_min <- mammals_neo_sh %>%
  filter(SHAPE_Area > 0.15)

# Match coordinate systems
mammals_neo_min <- st_transform(mammals_neo_min, crs = crs(w))

# Simplify geometries
mammals_neo_simple <- st_simplify(mammals_neo_min, 
                                  dTolerance = 0.00001, 
                                  preserveTopology = TRUE)

#rasterizing
mammals_neo_clipped <- st_intersection(mammals_neo_shapes, neotropics_union)

sf::st_geometry(mammals_neo_clipped) <- "geometry"

sf::st_geometry(mammals_neo_clipped)

mammals_sp <- as(mammals_neo_clipped, "Spatial")

# ENFA --------------------------------------------------------------------

# Split polygons by row
species_list <- split(mammals_sp, seq_len(nrow(mammals_sp)))

# --- Parallel setup ---
n_cores <- 4
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(CENFA)
  library(raster)
})
clusterExport(cl, c("climate_raster", "species_list"))

# --- Parallel processing ---
enfa_summary_list <- parLapply(cl, seq_along(species_list), function(i) {
  sp_poly <- species_list[[i]]
  sp_name <- sp_poly$sci_name
  
  # Run ENFA
  enfa_res <- CENFA::enfa(x = climate_raster, s.dat = sp_poly)
  
  # Return only relevant metrics
  data.frame(
    species_name = sp_name,
    marginality = enfa_res@marginality,
    specialization = enfa_res@specialization
  )
})

stopCluster(cl)

# --- Combine results ---
enfa_mammals_summary <- do.call(rbind, enfa_summary_list)

# --- Save to CSV ---
write.csv(enfa_mammals_summary, "results/data/processed/mammals_enfa_whole_neotropics.csv",
          row.names = FALSE)



#amphibians' shape files ----------------------------------------------------
amphibians_sh <- st_read("results/data/raw/amphibians_shapes/data_0.shp")

#modifying species name
amphibians_sh$BINOMIAL <- str_replace(amphibians_sh$BINOMIAL, " ", "_")

#calling the amphibians that have phylogenetic data
amphibians_unique <- read_csv("results/data/processed/vert_enfa_unique.csv") %>% 
  filter(className == "AMPHIBIA")

#filtering shapes of amphibians not present in our analyses
amphibians_sh_filter <- amphibians_sh %>% filter(BINOMIAL %in% amphibians_unique$species)

# Dissolve polygons per species (by scientific name)
amphibians_agg <- amphibians_sh_filter %>%
  group_by(BINOMIAL) %>%
  summarise(across(everything(), first),  
            geometry = st_union(geometry)) %>%
  ungroup()

# Filter out island species
amphibians_neo_sh <- amphibians_agg %>% filter(is.na(ISLAND),
                                         PRESENCE == 1,
                                         ORIGIN == 1,
                                         SEASONAL == 1)


# Match coordinate systems
amphibians_neo_sh <- st_transform(amphibians_neo_sh, crs = crs(w))

# Simplify geometries
amphibians_neo_simple <- st_simplify(amphibians_neo_sh, 
                                  dTolerance = 0.00001, 
                                  preserveTopology = TRUE)

#rasterizing
amphibians_neo_clipped <- st_intersection(amphibians_neo_simple, neotropics_union)

sf::st_geometry(amphibians_neo_clipped) <- "geometry"

sf::st_geometry(amphibians_neo_clipped)

amphibians_spatial <- as(amphibians_neo_clipped, "Spatial")

##saving
saveRDS(amphibians_spatial, "results/data/metadata/amphibians_raster_neo.rds")

amphibians_spatial <- readRDS("results/data/metadata/amphibians_raster_neo.rds")

# ENFA --------------------------------------------------------------------
# Split polygons by row
species_list <- split(amphibians_spatial, seq_len(nrow(amphibians_spatial)))

# --- Parallel setup ---
n_cores <- 4
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(CENFA)
  library(raster)
})
clusterExport(cl, c("climate_raster", "species_list"))

# --- Parallel processing with strong error handling ---
enfa_summary_list <- parLapply(cl, seq_along(species_list), function(i) {
  sp_poly <- species_list[[i]]
  sp_name <- as.character(sp_poly$BINOMIAL)[1]
  
  # Default output (guaranteed one row)
  out <- data.frame(
    species_name = sp_name,
    marginality = NA_real_,
    specialization = NA_real_,
    stringsAsFactors = FALSE
  )
  
  tryCatch({
    enfa_res <- CENFA::enfa(x = climate_raster, s.dat = sp_poly)
    
    # If ENFA succeeds, fill in results
    out$marginality <- enfa_res@marginality
    out$specialization <- enfa_res@specialization
    
  }, error = function(e) {
    message(paste("❌ ENFA failed for", sp_name, ":", e$message))
  })
  
  return(out)
})

stopCluster(cl)

# --- Combine results ---
enfa_amphibians_summary <- do.call(rbind, enfa_summary_list)

# --- Save to CSV ---
write.csv(enfa_amphibians_summary, "results/data/processed/amphibians_enfa_whole_neotropics.csv",
          row.names = FALSE)

#reptiles' shape files ----------------------------------------------------

reptiles_sh <- st_read("results/data/raw/reptiles_shapes/data_0.shp")

#modifying species name
reptiles_sh$BINOMIAL <- str_replace(reptiles_sh$BINOMIAL, " ", "_")

#calling the reptiles that have phylogenetic data
reptiles_unique <- read_csv("results/data/processed/vert_enfa_unique.csv") %>% 
  filter(className == "REPTILIA")


#filtering shapes of reptiles not present in our analyses
reptiles_sh_filter <- reptiles_sh %>% filter(BINOMIAL %in% reptiles_unique$species) 

#evaluating bad polygons
bad <- reptiles_sh_filter[!st_is_valid(reptiles_sh_filter), ]

#removing bad polygons and species
reptiles_sh_filter <- reptiles_sh_filter %>% filter(!BINOMIAL %in% bad$BINOMIAL)

#safe union due to topologies
safe_union <- function(x) {
  tryCatch(st_union(x), error = function(e) st_combine(x))
}

reptiles_agg <- reptiles_sh_filter %>%
  group_by(BINOMIAL) %>%
  summarise(across(everything(), first),
            geometry = safe_union(geometry)) %>%
  ungroup()


# Filter out island species
reptiles_neo_sh <- reptiles_agg %>% filter(is.na(ISLAND),
                                               PRESENCE == 1,
                                               ORIGIN == 1,
                                               SEASONAL == 1)


# Match coordinate systems
reptiles_neo_sh <- st_transform(reptiles_neo_sh, crs = crs(w))

# Simplify geometries
reptiles_neo_simple <- st_simplify(reptiles_neo_sh, 
                                     dTolerance = 0.00001, 
                                     preserveTopology = TRUE)

#rasterizing
reptiles_neo_clipped <- st_intersection(reptiles_neo_simple, neotropics_union)


sf::st_geometry(reptiles_neo_clipped) <- "geometry"

sf::st_geometry(reptiles_neo_clipped)

reptiles_spatial <- as(reptiles_neo_clipped, "Spatial")

##saving
saveRDS(reptiles_spatial, "results/data/metadata/reptiles_raster_neo.rds")

reptiles_spatial <- readRDS("results/data/metadata/reptiles_raster_neo.rds")

# ENFA --------------------------------------------------------------------
# Split polygons by row
species_list <- split(reptiles_spatial, seq_len(nrow(reptiles_spatial)))

# --- Parallel setup ---
n_cores <- 4
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(CENFA)
  library(raster)
})
clusterExport(cl, c("climate_raster", "species_list"))

# --- Parallel processing with strong error handling ---
enfa_summary_list <- parLapply(cl, seq_along(species_list), function(i) {
  sp_poly <- species_list[[i]]
  sp_name <- as.character(sp_poly$BINOMIAL)[1]
  
  # Default output (guaranteed one row)
  out <- data.frame(
    species_name = sp_name,
    marginality = NA_real_,
    specialization = NA_real_,
    stringsAsFactors = FALSE
  )
  
  tryCatch({
    enfa_res <- CENFA::enfa(x = climate_raster, s.dat = sp_poly)
    
    # If ENFA succeeds, fill in results
    out$marginality <- enfa_res@marginality
    out$specialization <- enfa_res@specialization
    
  }, error = function(e) {
    message(paste("❌ ENFA failed for", sp_name, ":", e$message))
  })
  
  return(out)
})

stopCluster(cl)

# --- Combine results ---
enfa_reptiles_summary <- do.call(rbind, enfa_summary_list)

# --- Save to CSV ---
write.csv(enfa_reptiles_summary, "results/data/processed/reptiles_enfa_whole_neotropics.csv",
          row.names = FALSE)
