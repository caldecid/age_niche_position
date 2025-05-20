
# PGLS by class and biome ------------------------------------------------------

# Due to memmory demmand, the script was run on a server and not on my personal computer

##calling packages
required_packages <- c("dplyr", "readr", "phytools", "future.apply",
                       "caper", "furrr")  # Add your packages here

# Check and install missing packages
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, lib = "R/x86_64-pc-linux-gnu-library/4.2")
  }
  library(pkg, lib.loc = "R/x86_64-pc-linux-gnu-library/4.2", character.only = TRUE)
}


# loading metadata

##this data was stored in the server and included for each tetrapod class: 1) a list 
#with 100 dataframes containing the species ages measured and corrected in 100 random trees 
#with their respective ENFA values, 2) the multiphylo with 100 random trees (see methods)

meta <- "niche_position_age/results/data/metadata"

# Get a list of all RData files in the directory
rdata_files <- list.files(meta, pattern = "\\.RData$", full.names = TRUE)

# Load all the RData files
for (file in rdata_files) {
  load(file)
}

##loading ages
load("niche_position_age/results/data/processed/vert_enfa_ages_list.RData")

##plan multissesion
plan(multisession, workers = 25)

##function res
res_pgls <- function(mod, ext, class, biome){
  
  res_df  <- as.data.frame(summary(mod)$coef)
  ##adj r squared
  res_df$adj.r.squared <- as.vector(summary(mod)$adj.r.squared)
  ##phylogenetic signal (lambda)
  res_df$lambda <- as.vector(summary(mod)$param[2])
  ##lambda confidence interval
  res_df$lam_low <- as.vector(summary(mod)$param.CI$lambda$ci.val[1])
  res_df$lam_up <- as.vector(summary(mod)$param.CI$lambda$ci.val[2])
  ## class
  res_df$class <- as.character(class)
  # extinction scenario
  res_df$ext <- as.character(ext)
  #biome
  res_df$biome <- as.character(biome)
  ##estimates
  res_df$term <- rownames(res_df)
  rownames(res_df) <- NULL
  res_df <- res_df %>% dplyr::select(term, Estimate:biome)
  
  return(res_df)
}

########pgls for each biome
fun_biome_pgls <- function(df, class, phy) {
  
  ## Output directory
  output.dir <- paste0("niche_position_age/results/data/processed/biomes2/", class)
  if (!dir.exists(output.dir)) dir.create(output.dir, recursive = TRUE)  
  
  ## Obtaining biomes names
  biomes <- unique(df$biome)
  
  ## Initialize output list
  
  ## Function to fit PGLS models with error handling
  fit_pgls <- function(df_caper, response, predictor, ext) {
    tryCatch({
      model <- pgls(as.formula(paste0("log(", response, " + 1) ~ log(", predictor, " + 1)")), 
                    data = df_caper, lambda = "ML")
      res_pgls(mod = model, ext = ext, class = class, biome = biomes[i])
    }, error = function(e) {
      message(paste("Error in PGLS for", response, "and", predictor, "in biome", biomes[i], ":", e$message))
      return(NULL)
    })
  }
  
  ## Loop for each biome
  for (i in seq_along(biomes)) {
    
    ## Subset dataset for the current biome
    df_biome <- df %>% filter(biome == biomes[i])
    
    ## Skip biome if no species are found
    if (nrow(df_biome) == 0) {
      message(paste("Skipping biome", biomes[i], "as it has no data."))
      next
    }
    
    ## Drop tips from the phylogeny
    phy_biome <- keep.tip(phy, tip = df_biome$species)
    
    ## Arrange dataset for PGLS
    df_enfa_biome <- df_biome %>%
      arrange(match(species, phy_biome$tip.label)) %>%
      dplyr::select(species, marginality, specialization, low.age, int.age, high.age)
    
    ## Create comparative data frame
    df_caper <- tryCatch({
      caper::comparative.data(phy = phy_biome, data = as.data.frame(df_enfa_biome),
                              names.col = "species", vcv = TRUE, warn.dropped = TRUE)
    }, error = function(e) {
      message(paste("Error in comparative.data for biome", biomes[i], ":", e$message))
      return(NULL)
    })
    
    ## Skip iteration if df_caper is NULL
    if (is.null(df_caper)) next
    
    ## Fit models for marginality
    results_marg <- do.call(rbind, Filter(Negate(is.null), list(
      fit_pgls(df_caper, "marginality", "low.age", "low"),
      fit_pgls(df_caper, "marginality", "int.age", "int"),
      fit_pgls(df_caper, "marginality", "high.age", "high")
    )))
    
    if (!is.null(results_marg)) results_marg$variable <- "marginality"
    
    ## Fit models for specialization
    results_sp <- do.call(rbind, Filter(Negate(is.null), list(
      fit_pgls(df_caper, "specialization", "low.age", "low"),
      fit_pgls(df_caper, "specialization", "int.age", "int"),
      fit_pgls(df_caper, "specialization", "high.age", "high")
    )))
    
    if (!is.null(results_sp)) results_sp$variable <- "specialization"
    
    ## Store results
    biome_results <- rbind(results_marg, results_sp)
    
    ## Skip saving if biome_results is empty
    if (is.null(biome_results) || nrow(biome_results) == 0) {
      message(paste("Skipping biome", biomes[i], "as no valid models were created."))
      next
    }
    
    
    
    ## Ensure file name is safe
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    write_csv(biome_results, 
              file = paste0(output.dir, "/", biomes[i], "_", timestamp, ".csv"))
  }
  
}



# Apply the function in parallel and save results
##mammals
furrr::future_map2(mammals.list.ages, mammals.multiphylo, fun_biome_pgls, 
                   class = "MAMMALIA")

##birds
furrr::future_map2(birds.list.ages, birds.multiphylo, fun_biome_pgls, 
                   class = "AVES")

##amphibians
furrr::future_map2(amphi.list.ages, amphi.multiphylo, fun_biome_pgls, 
                   class = "AMPHIBIA")

##reptiles
furrr::future_map2(rep.list.ages, rep.multiphylo, fun_biome_pgls, 
                   class = "REPTILIA")