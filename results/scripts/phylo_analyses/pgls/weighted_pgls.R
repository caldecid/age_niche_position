# PGLS species age vs weighted ENFA measures for each tetrapod class------------


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

##this data was stored in the server and included for each tetrapod class: 
# 100 comparative.data (see 'caper' package) which combines the respective phylogeny
# with the dataset containing the species ages and ENFA measures.
#These comparative.data were generated in the Rscript "

meta <- "niche_position_age/results/data/metadata"

# Get a list of all RData files in the directory
rdata_files <- list.files(meta, pattern = "\\.RData$", full.names = TRUE)

# Load all the RData files
for (file in rdata_files) {
  load(file)
}

##plan multissesion
plan(multisession, workers = 50)

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


comp_pgls <- function(comp_data, taxa, output_dir = "results") {
  if (is.null(comp_data)) return(NULL)
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) dir.create(output_dir)
  
  # Compute Marginality Models
  marg_low <- caper::pgls(log(w.marginality + 1) ~ log(low.age + 1), 
                          data = comp_data, lambda = "ML")
  marg_int <- caper::pgls(log(w.marginality + 1) ~ log(int.age + 1), 
                          data = comp_data, lambda = "ML")
  marg_high <- caper::pgls(log(w.marginality + 1) ~ log(high.age + 1), 
                           data = comp_data, lambda = "ML")
  
  marg_results <- dplyr::bind_rows(
    res_pgls(mod = marg_low, ext = "low", class = "group", biome = "w.marginality"),
    res_pgls(mod = marg_int, ext = "int", class = "group", biome = "w.marginality"),
    res_pgls(mod = marg_high, ext = "high", class = "group", biome = "w.marginality")
  )
  
  # Save marginality results
  write_csv(marg_results, file = file.path(output_dir, paste0(taxa, "_marg_results.csv")))
  
  # Compute Specialization Models
  spe_low <- caper::pgls(log(w.specialization + 1) ~ log(low.age + 1), 
                         data = comp_data, lambda = "ML")
  spe_int <- caper::pgls(log(w.specialization + 1) ~ log(int.age + 1), 
                         data = comp_data, lambda = "ML")
  spe_high <- caper::pgls(log(w.specialization + 1) ~ log(high.age + 1), 
                          data = comp_data, lambda = "ML")
  
  spe_results <- dplyr::bind_rows(
    res_pgls(mod = spe_low, ext = "low", class = "group", biome = "w.specialization"),
    res_pgls(mod = spe_int, ext = "int", class = "group", biome = "w.specialization"),
    res_pgls(mod = spe_high, ext = "high", class = "group", biome = "w.specialization")
  )
  
  # Save specialization results
  write_csv(spe_results, file = file.path(output_dir, paste0(taxa, "_spe_results.csv")))
  
}


##running

amp_names <- c(paste0(rep("amphibians_", 100), 1:100))

# Apply the function in parallel and save results
furrr::future_map2(amp_comp_weighted, amp_names, comp_pgls,
                output_dir =  "niche_position_age/results/data/processed/amphibians")


#birds
bird_names <- c(paste0(rep("birds_", 100), 1:100))

furrr::future_map2(bird_comp_weighted, bird_names, comp_pgls,
                output_dir =  "niche_position_age/results/data/processed/birds")


#reptiles
rep_names <- c(paste0(rep("rep_", 100), 1:100))

#Apply the function in parallel and save results
furrr::future_map2(rep_comp_weighted, rep_names, comp_pgls,
                output_dir =  "niche_position_age/results/data/processed/reptiles")

##mammals
mam_names <- c(paste0(rep("mam_", 100), 1:100))

# Apply the function in parallel and save results
furrr::future_map2(mam_comp_weighted, mam_names, comp_pgls,
                   output_dir =  "niche_position_age/results/data/processed/mammals")