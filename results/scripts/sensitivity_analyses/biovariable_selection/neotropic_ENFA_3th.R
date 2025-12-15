
# Sensitivity analyses vif ------------------------------------------------

##sourcing the libraries and the directories
source(file.path(getwd(), "/source.R"))

# libraries
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

# sensitivity analyses (VIF rigorous) -------------------------------------

#th = 5
climate_neotropic_5th <- readRDS("C:/Users/carlo/OneDrive/Desktop/Doctorado 2019/ufba/tese/age_niche_position/results/data/metadata/climate_neotropic_5th.rds")

#th = 3
climate_neotropic_3th <- readRDS("C:/Users/carlo/OneDrive/Desktop/Doctorado 2019/ufba/tese/age_niche_position/results/data/metadata/climate_neotropic__3th.rds")


# Mammals' raster files ----------------------------------------------------
mammals_neo_shapes <- readRDS("C:/Users/carlo/OneDrive/Desktop/Doctorado 2019/ufba/tese/age_niche_position/results/data/metadata/mammals_neo_shapes.rds")

mammals_spatial <- as(mammals_neo_shapes, "Spatial")

# ENFA --------------------------------------------------------------------

# Split polygons by row
species_list <- split(mammals_spatial, seq_len(nrow(mammals_spatial)))

# --- Parallel setup ---
n_cores <- 4
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(CENFA)
  library(raster)
})
clusterExport(cl, c("climate_neotropic_5th", "species_list"))

# --- Parallel processing ---
enfa_summary_list <- parLapply(cl, seq_along(species_list), function(i) {
  sp_poly <- species_list[[i]]
  sp_name <- sp_poly$sci_name
  
  # Run ENFA
  enfa_res <- CENFA::enfa(x = climate_neotropic_5th, s.dat = sp_poly)
  
  # Return only relevant metrics
  data.frame(
    species_name = sp_name,
    marginality = enfa_res@marginality,
    specialization = enfa_res@specialization
  )
})

stopCluster(cl)

# --- Combine results ---
enfa_mammals_5th <- do.call(rbind, enfa_summary_list)

# --- Save to CSV ---
write.csv(enfa_mammals_5th,
          "results/data/processed/sensitivity_analyses/mammals_enfa_5th.csv",
          row.names = FALSE)

##th = 3
# --- Parallel setup ---
n_cores <- 4
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(CENFA)
  library(raster)
})
clusterExport(cl, c("climate_neotropic_3th", "species_list"))

# --- Parallel processing ---
enfa_summary_list <- parLapply(cl, seq_along(species_list), function(i) {
  sp_poly <- species_list[[i]]
  sp_name <- sp_poly$sci_name
  
  # Run ENFA
  enfa_res <- CENFA::enfa(x = climate_neotropic_3th, s.dat = sp_poly)
  
  # Return only relevant metrics
  data.frame(
    species_name = sp_name,
    marginality = enfa_res@marginality,
    specialization = enfa_res@specialization
  )
})

stopCluster(cl)

# --- Combine results ---
enfa_mammals_3th <- do.call(rbind, enfa_summary_list)

# --- Save to CSV ---
write.csv(enfa_mammals_3th,
          "results/data/processed/sensitivity_analyses/mammals_enfa_3th.csv",
          row.names = FALSE)


# amphibians sensitivity --------------------------------------------------

amphibians_spatial <- readRDS("results/data/metadata/amphibians_raster_neo.rds")

# ENFA --------------------------------------------------------------------
# Split polygons by row
species_list <- split(amphibians_spatial, seq_len(nrow(amphibians_spatial)))

##th = 5
# --- Parallel setup ---
n_cores <- 4
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(CENFA)
  library(raster)
})
clusterExport(cl, c("climate_neotropic_5th", "species_list"))

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
    enfa_res <- CENFA::enfa(x = climate_neotropic_5th, s.dat = sp_poly)
    
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
enfa_amphibians_5th <- do.call(rbind, enfa_summary_list)

# --- Save to CSV ---
write.csv(enfa_amphibians_5th, "results/data/processed/sensitivity_analyses/amphibians_enfa_5th.csv",
          row.names = FALSE)

#th = 3
# --- Parallel setup ---
n_cores <- 4
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(CENFA)
  library(raster)
})
clusterExport(cl, c("climate_neotropic_3th", "species_list"))

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
    enfa_res <- CENFA::enfa(x = climate_neotropic_3th, s.dat = sp_poly)
    
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
enfa_amphibians_3th <- do.call(rbind, enfa_summary_list)

# --- Save to CSV ---
write.csv(enfa_amphibians_3th, "results/data/processed/sensitivity_analyses/amphibians_enfa_3th.csv",
          row.names = FALSE)


#reptiles' shape files ----------------------------------------------------

reptiles_spatial <- readRDS("results/data/metadata/reptiles_raster_neo.rds")

# ENFA --------------------------------------------------------------------
# Split polygons by row
species_list <- split(reptiles_spatial, seq_len(nrow(reptiles_spatial)))

#th = 5 

# --- Parallel setup ---
n_cores <- 4
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(CENFA)
  library(raster)
})
clusterExport(cl, c("climate_neotropic_5th", "species_list"))

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
    enfa_res <- CENFA::enfa(x = climate_neotropic_5th, s.dat = sp_poly)
    
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
enfa_reptiles_5th <- do.call(rbind, enfa_summary_list)

# --- Save to CSV ---
write.csv(enfa_reptiles_5th, "results/data/processed/sensitivity_analyses/reptiles_enfa_5th.csv",
          row.names = FALSE)

##th = 3

# --- Parallel setup ---
n_cores <- 4
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(CENFA)
  library(raster)
})
clusterExport(cl, c("climate_neotropic_3th", "species_list"))

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
    enfa_res <- CENFA::enfa(x = climate_neotropic_3th, s.dat = sp_poly)
    
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
enfa_reptiles_3th <- do.call(rbind, enfa_summary_list)

# --- Save to CSV ---
write.csv(enfa_reptiles_3th, "results/data/processed/sensitivity_analyses/reptiles_enfa_3th.csv",
          row.names = FALSE)


#############################################################################################

######Aves

aves_spatial <- readRDS("results/data/metadata/aves_raster_neo.rds")

# ENFA --------------------------------------------------------------------
# Split polygons by row
species_list <- split(aves_spatial, seq_len(nrow(aves_spatial)))

#th = 5 

# --- Parallel setup ---
n_cores <- 4
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(CENFA)
  library(raster)
})
clusterExport(cl, c("climate_neotropic_5th", "species_list"))

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
    enfa_res <- CENFA::enfa(x = climate_neotropic_5th, s.dat = sp_poly)
    
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
enfa_aves_5th <- do.call(rbind, enfa_summary_list)

# --- Save to CSV ---
write.csv(enfa_aves_5th, "results/data/processed/sensitivity_analyses/aves_enfa_5th.csv",
          row.names = FALSE)

##th = 3

# --- Parallel setup ---
n_cores <- 4
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(CENFA)
  library(raster)
})
clusterExport(cl, c("climate_neotropic_3th", "species_list"))

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
    enfa_res <- CENFA::enfa(x = climate_neotropic_3th, s.dat = sp_poly)
    
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
enfa_aves_3th <- do.call(rbind, enfa_summary_list)

# --- Save to CSV ---
write.csv(enfa_aves_3th, "results/data/processed/sensitivity_analyses/aves_enfa_3th.csv",
          row.names = FALSE)