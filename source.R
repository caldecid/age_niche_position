## METADATA ===============================================================
## Description: This script sources the working directory and the files 
## 
## R version: 4.0.2 for Windows
## Date: 2022-12-05
## Author: Carlos Calderon
##=======================================================================##

##libraries
####libraries
library(readr)
library(readxl)
library(tidyverse)
library(ape)
library(phytools)
library(devtools)
library(stringr)
library(geiger)
library(caper)
library(nlme)
library(broom.mixed)
library(nlme)
library(grid)
library(ggpubr)
library(psychometric)
library(FDRestimation)
library(diversitree)
library(picante)
library(phangorn)
library(TreeTools)
library(tidytree)
library(FossilSim)
library(TreeSim)
library(ggridges)
library(ggpubr)
library(multcompView)
library(cowplot)
library(scales)
library(future)
library(future.apply)
library(TreeSimGM)
library(HDInterval)
library(arules)
library(gridExtra)
library(grid)
library(spatstat)
library(modi)
library(diversitree)
library(TreeSim)
library(ggbreak)
library(logr)
library(hexbin)
library(RColorBrewer)
library(writexl)
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

# set working directory -----------


### create variables for directory relative location -------

####### results

##scripts

scripts <- "results/scripts"

functions <- "functions"


functions_dir <- list.files(paste0(scripts, "/", functions), full.names = TRUE)


for(i in seq_along(functions_dir)){
  source(functions_dir[i])
}


##data 

# results/data/raw
raw <- "results/data/raw"

#results/data/processed
pro <- "results/data/processed"



# results/data/meta
meta <- "results/data/metadata"


###### figures files

figures <- "/text/figures"