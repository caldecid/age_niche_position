# Sensitivity_biome_ENFA_3th --------------------------------------------------

##sourcing the libraries and the directories
source(file.path(getwd(), "/source.R"))

# libraries
library(tidyverse)
library(terra)
library(sf)
library(geodata)
library(stringr)
library(devtools)
#devtools::install_github("rinnan/CENFA")
library(CENFA)
library(sp)
library(parallel)


w <- geodata::worldclim_global(var = "bio", res = 10, path = tempdir())

###only Neotropics
biomes <- st_read("results/data/raw/biomes/wwf_terr_ecos.shp")

neotropics <- biomes %>% filter(REALM == "NT")

##how to aggregate all the neotropics biomes in just one area

neotropics <- st_make_valid(neotropics)  # first quick pass

#  Force validation using a geometry repair trick
neotropics_valid <- neotropics %>%
  mutate(geometry = st_buffer(geometry, 0))

#eliminating biome 98 and 99
neotropics_valid <- neotropics_valid %>% filter(!BIOME %in% c(98,99))


##aggregating the polygons which pertain to the same biome
neotropics_biome <- neotropics_valid %>%
  group_by(BIOME) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup() %>%
  st_make_valid()

###creating a dataframe with the biomes names
biome_names <- tibble(
  BIOME = 1:14,
  BIOME_NAME = c(
    "Tropical & Subtropical Moist Broadleaf Forests",
    "Tropical & Subtropical Dry Broadleaf Forests",
    "Tropical & Subtropical Coniferous Forests",
    "Temperate Broadleaf & Mixed Forests",
    "Temperate Conifer Forests",
    "Boreal Forests/Taiga",
    "Tropical & Subtropical Grasslands, Savannas & Shrublands",
    "Temperate Grasslands, Savannas & Shrublands",
    "Flooded Grasslands & Savannas",
    "Montane Grasslands & Shrublands",
    "Tundra",
    "Mediterranean Forests, Woodlands & Scrub",
    "Deserts & Xeric Shrublands",
    "Mangroves"
  )
)

#Join names
neotropics_biome <- neotropics_biome %>%
  left_join(biome_names, by = "BIOME")

####################getting climate data########################
w <- geodata::worldclim_global(var = "bio", res = 10, path = tempdir())

#cropping to match the neotropics
clim_neo <- crop(w, neotropics_biome)

##ensure crs match
clim_neo <- terra::project(clim_neo, st_crs(neotropics_biome)$wkt)

#empty list
climate_neo_list <- vector("list", length = nrow(neotropics_biome))

names(climate_neo_list) <- neotropics_biome$BIOME_NAME


##now separating the biomes' climate with VIF(th = 3)
for (b in seq_along(climate_neo_list)) {
  biome_poly <- neotropics_biome[b, ]
  biome_name <- gsub(" ", "_", biome_poly$BIOME_NAME[1])
  message(paste0("Processing ", biome_name))
  
  # Crop + mask
  clim_biome <- crop(clim_neo, biome_poly)
  clim_biome <- mask(clim_biome, biome_poly)
  
  #detect highly correlated variables ######### th = 3 ######################
  clim_vif <- usdm::vifstep(clim_biome, th = 3)
  
  #excluding 
  climate_biome_th3 <- usdm::exclude(clim_biome, clim_vif)
  
  #rasterizing
  climate_raster <- raster::stack(as(climate_biome_th3, "Raster"))
  
  #saving in list
  climate_neo_list[[b]] <- climate_raster
  
  #saving in file
  out_path <- paste0("results/data/metadata/biome_rasters/", biome_name, ".rds")
  saveRDS(climate_raster, out_path)
  
  message("✔ Saved:", out_path)
}

##reading

biome_clim <- list.files("results/data/metadata/biome_rasters")

biome_name <- sub("\\.rds$", "", biome_clim)

climate_neo_list <- list()

for(i in seq_along(biome_clim)) {
  
  climate_neo_list[[i]] <- readRDS(paste0("results/data/metadata/biome_rasters/",
                                          biome_clim[i]))
}

names(climate_neo_list) <- biome_name


