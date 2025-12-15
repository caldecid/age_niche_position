
# biome-ENFA for tetrapods (VIF < 3) --------------------------------------

##libraries
library(CENFA)
library(sf)
library(future)
library(future.apply)
library(tidyverse)
library(raster)


# Mammals -----------------------------------------------------------------

##========================================
## SET FUTURE PLAN (1 core per biome)
##========================================
plan(multisession, workers = length(climate_biomes_rasters))

##========================================
## Function to run ENFA for a single biome
##========================================
run_enfa_biome <- function(biome_name) {
  
  message("\n==============================")
  message("Running biome: ", biome_name)
  message("==============================\n")
  
  ##----------------------------------------
  ## TEMP DIRECTORY FOR THIS WORKER
  ##----------------------------------------
  tmpdir <- file.path(tempdir(), biome_name)
  dir.create(tmpdir, showWarnings = FALSE)
  rasterOptions(tmpdir = tmpdir)
  
  ##========================================
  ## Load climate for this biome
  ##========================================
  clim_stack <- climate_biomes_rasters[[biome_name]]
  
  ## biome polygon
  biome_poly_sp <- rasterToPolygons(clim_stack[[1]], dissolve = TRUE)
  biome_poly_sf <- st_as_sf(biome_poly_sp) |> st_make_valid()
  
  ##========================================
  ## Convert mammals + intersect with biome
  ##========================================
  mammals_sf <- st_as_sf(mammals_raster_neo) |> st_make_valid()
  
  idx <- st_intersects(mammals_sf, biome_poly_sf)
  mammals_touching <- mammals_sf[lengths(idx) > 0, ]
  
  mammals_sf_touch <- st_simplify(mammals_touching, dTolerance = 500)
  mammals_sf_clean <- mammals_sf_touch[!st_is_empty(mammals_sf_touch), ]
  
  mammals_sp <- as(mammals_sf_clean, "Spatial")
  
  ##========================================
  ## Prepare result df
  ##========================================
  rep_df <- data.frame(
    species = mammals_sp$sci_name,
    marginality = NA_real_,
    specialization = NA_real_,
    biome = biome_name,
    stringsAsFactors = FALSE
  )
  
  ##========================================
  ## ENFA LOOP
  ##========================================
  for (i in 1:nrow(mammals_sp)) {
    
    sp_name <- mammals_sp[i, ]$sci_name
    
    enfa_res <- try(
      CENFA::enfa(x = clim_stack, s.dat = mammals_sp[i, ]),
      silent = TRUE
    )
    
    if (!inherits(enfa_res, "try-error")) {
      rep_df$marginality[i] <- enfa_res@marginality
      rep_df$specialization[i] <- enfa_res@specialization
    } else {
      message("ENFA failed for: ", sp_name)
    }
    
    ## Clean temp files every 30 species
    if (i %% 30 == 0) {
      raster::removeTmpFiles(h = 0)
    }
  }
  
  ##========================================
  ## Save results
  ##========================================
  outfile <- paste0(
    "results/data/metadata/ENFA_3th/dataframes/mammals/mammals_",
    biome_name, ".csv"
  )
  
  write_csv(rep_df, outfile)
  message("Saved: ", outfile)
  
  ## Final cleanup
  raster::removeTmpFiles(h = 0)
  
  return(outfile)
}

##========================================
## RUN ALL BIOMES IN PARALLEL
##========================================
biome_names <- names(climate_biomes_rasters)

future_lapply(biome_names, run_enfa_biome)



# Birds -------------------------------------------------------------------

##========================================
## SET FUTURE PLAN (1 core per biome)
##========================================
plan(multisession, workers = length(climate_biomes_rasters))

##========================================
## Function to run ENFA for a single biome
##========================================
run_enfa_biome <- function(biome_name) {
  
  message("\n==============================")
  message("Running biome: ", biome_name)
  message("==============================\n")
  
  ##----------------------------------------
  ## TEMP DIRECTORY FOR THIS WORKER
  ##----------------------------------------
  tmpdir <- file.path(tempdir(), biome_name)
  dir.create(tmpdir, showWarnings = FALSE)
  rasterOptions(tmpdir = tmpdir)
  
  ##========================================
  ## Load climate for this biome
  ##========================================
  clim_stack <- climate_biomes_rasters[[biome_name]]
  
  ## biome polygon
  biome_poly_sp <- rasterToPolygons(clim_stack[[1]], dissolve = TRUE)
  biome_poly_sf <- st_as_sf(biome_poly_sp) |> st_make_valid()
  
  ##========================================
  ## Convert aves + intersect with biome
  ##========================================
  aves_sf <- st_as_sf(aves_raster_neo) |> st_make_valid()
  
  idx <- st_intersects(aves_sf, biome_poly_sf)
  aves_touching <- aves_sf[lengths(idx) > 0, ]
  
  aves_sf_touch <- st_simplify(aves_touching, dTolerance = 500)
  aves_sf_clean <- aves_sf_touch[!st_is_empty(aves_sf_touch), ]
  
  aves_sp <- as(aves_sf_clean, "Spatial")
  
  ##========================================
  ## Prepare result df
  ##========================================
  rep_df <- data.frame(
    species = aves_sp$sci_name,
    marginality = NA_real_,
    specialization = NA_real_,
    biome = biome_name,
    stringsAsFactors = FALSE
  )
  
  ##========================================
  ## ENFA LOOP
  ##========================================
  for (i in 1:nrow(aves_sp)) {
    
    sp_name <- aves_sp[i, ]$sci_name
    
    enfa_res <- try(
      CENFA::enfa(x = clim_stack, s.dat = aves_sp[i, ]),
      silent = TRUE
    )
    
    if (!inherits(enfa_res, "try-error")) {
      rep_df$marginality[i] <- enfa_res@marginality
      rep_df$specialization[i] <- enfa_res@specialization
    } else {
      message("ENFA failed for: ", sp_name)
    }
    
    ## Clean temp files every 30 species
    if (i %% 30 == 0) {
      raster::removeTmpFiles(h = 0)
    }
  }
  
  ##========================================
  ## Save results
  ##========================================
  outfile <- paste0(
    "results/data/metadata/ENFA_3th/dataframes/aves/aves_",
    biome_name, ".csv"
  )
  
  write_csv(rep_df, outfile)
  message("Saved: ", outfile)
  
  ## Final cleanup
  raster::removeTmpFiles(h = 0)
  
  return(outfile)
}

##========================================
## RUN ALL BIOMES IN PARALLEL
##========================================
biome_names <- names(climate_biomes_rasters)

future_lapply(biome_names, run_enfa_biome)


# Reptiles ----------------------------------------------------------------


##========================================
## SET FUTURE PLAN (1 core per biome)
##========================================
plan(multisession, workers = length(climate_biomes_rasters) - 1)

##========================================
## Function to run ENFA for a single biome
##========================================
run_enfa_biome <- function(biome_name) {
  
  message("\n==============================")
  message("Running biome: ", biome_name)
  message("==============================\n")
  
  ##----------------------------------------
  ## TEMP DIRECTORY FOR THIS WORKER
  ##----------------------------------------
  tmpdir <- file.path(tempdir(), biome_name)
  dir.create(tmpdir, showWarnings = FALSE)
  rasterOptions(tmpdir = tmpdir)
  
  ##========================================
  ## Load climate for this biome
  ##========================================
  clim_stack <- climate_biomes_rasters[[biome_name]]
  
  ## biome polygon
  biome_poly_sp <- rasterToPolygons(clim_stack[[1]], dissolve = TRUE)
  biome_poly_sf <- st_as_sf(biome_poly_sp) |> st_make_valid()
  
  ##========================================
  ## Convert reptiles + intersect with biome
  ##========================================
  reptiles_sf <- st_as_sf(reptiles_raster_neo) |> st_make_valid()
  
  idx <- st_intersects(reptiles_sf, biome_poly_sf)
  reptiles_touching <- reptiles_sf[lengths(idx) > 0, ]
  
  reptiles_sf_touch <- st_simplify(reptiles_touching, dTolerance = 500)
  reptiles_sf_clean <- reptiles_sf_touch[!st_is_empty(reptiles_sf_touch), ]
  
  reptiles_sp <- as(reptiles_sf_clean, "Spatial")
  
  ##========================================
  ## Prepare result df
  ##========================================
  rep_df <- data.frame(
    species = reptiles_sp$BINOMIAL,
    marginality = NA_real_,
    specialization = NA_real_,
    biome = biome_name,
    stringsAsFactors = FALSE
  )
  
  ##========================================
  ## ENFA LOOP
  ##========================================
  for (i in 1:nrow(reptiles_sp)) {
    
    sp_name <- reptiles_sp[i, ]$BINOMIAL
    
    enfa_res <- try(
      CENFA::enfa(x = clim_stack, s.dat = reptiles_sp[i, ]),
      silent = TRUE
    )
    
    if (!inherits(enfa_res, "try-error")) {
      rep_df$marginality[i] <- enfa_res@marginality
      rep_df$specialization[i] <- enfa_res@specialization
    } else {
      message("ENFA failed for: ", sp_name)
    }
    
    ## Clean temp files every 30 species
    if (i %% 30 == 0) {
      raster::removeTmpFiles(h = 0)
    }
  }
  
  ##========================================
  ## Save results
  ##========================================
  outfile <- paste0(
    "results/data/metadata/ENFA_3th/dataframes/reptiles/rep_",
    biome_name, ".csv"
  )
  
  write_csv(rep_df, outfile)
  message("Saved: ", outfile)
  
  ## Final cleanup
  raster::removeTmpFiles(h = 0)
  
  return(outfile)
}

##========================================
## RUN ALL BIOMES IN PARALLEL
##========================================
biome_names <- names(climate_biomes_rasters)[-11]

future_lapply(biome_names, run_enfa_biome)


# Amphibians --------------------------------------------------------------


##========================================
## SET FUTURE PLAN (1 core per biome)
##========================================
plan(multisession, workers = length(climate_biomes_rasters))

##========================================
## Function to run ENFA for a single biome
##========================================
run_enfa_biome <- function(biome_name) {
  
  message("\n==============================")
  message("Running biome: ", biome_name)
  message("==============================\n")
  
  ##----------------------------------------
  ## TEMP DIRECTORY FOR THIS WORKER
  ##----------------------------------------
  tmpdir <- file.path(tempdir(), biome_name)
  dir.create(tmpdir, showWarnings = FALSE)
  rasterOptions(tmpdir = tmpdir)
  
  ##========================================
  ## Load climate for this biome
  ##========================================
  clim_stack <- climate_biomes_rasters[[biome_name]]
  
  ## biome polygon
  biome_poly_sp <- rasterToPolygons(clim_stack[[1]], dissolve = TRUE)
  biome_poly_sf <- st_as_sf(biome_poly_sp) |> st_make_valid()
  
  ##========================================
  ## Convert amphibians + intersect with biome
  ##========================================
  amphibians_sf <- st_as_sf(amphibians_raster_neo) |> st_make_valid()
  
  idx <- st_intersects(amphibians_sf, biome_poly_sf)
  amphibians_touching <- amphibians_sf[lengths(idx) > 0, ]
  
  amphibians_sf_touch <- st_simplify(amphibians_touching, dTolerance = 500)
  amphibians_sf_clean <- amphibians_sf_touch[!st_is_empty(amphibians_sf_touch), ]
  
  amphibians_sp <- as(amphibians_sf_clean, "Spatial")
  
  ##========================================
  ## Prepare result df
  ##========================================
  rep_df <- data.frame(
    species = amphibians_sp$BINOMIAL,
    marginality = NA_real_,
    specialization = NA_real_,
    biome = biome_name,
    stringsAsFactors = FALSE
  )
  
  ##========================================
  ## ENFA LOOP
  ##========================================
  for (i in 1:nrow(amphibians_sp)) {
    
    sp_name <- amphibians_sp[i, ]$BINOMIAL
    
    enfa_res <- try(
      CENFA::enfa(x = clim_stack, s.dat = amphibians_sp[i, ]),
      silent = TRUE
    )
    
    if (!inherits(enfa_res, "try-error")) {
      rep_df$marginality[i] <- enfa_res@marginality
      rep_df$specialization[i] <- enfa_res@specialization
    } else {
      message("ENFA failed for: ", sp_name)
    }
    
    ## Clean temp files every 30 species
    if (i %% 30 == 0) {
      raster::removeTmpFiles(h = 0)
    }
  }
  
  ##========================================
  ## Save results
  ##========================================
  outfile <- paste0(
    "results/data/metadata/ENFA_3th/dataframes/amphibians/rep_",
    biome_name, ".csv"
  )
  
  write_csv(rep_df, outfile)
  message("Saved: ", outfile)
  
  ## Final cleanup
  raster::removeTmpFiles(h = 0)
  
  return(outfile)
}

##========================================
## RUN ALL BIOMES IN PARALLEL
##========================================
biome_names <- names(climate_biomes_rasters)

future_lapply(biome_names, run_enfa_biome)

