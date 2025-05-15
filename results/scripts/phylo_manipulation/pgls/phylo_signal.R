
# Phylogenetic signal of ENFA measures in Tetrapods -----------------------

# Load necessary packages
library(phytools)       
library(caper)          
library(future.apply)   

# Set up parallel processing (Processes were run in the USP server)
plan(multisession, workers = 50)  # Use available cores

# Function to calculate phylogenetic signal
calc_phylo_signal <- function(trait_values, trees) {
  results_list <- future_lapply(seq_along(trees), function(i) {
    tree <- trees[[i]]
    
    # Match data to tree
    species_in_tree <- intersect(tree$tip.label, names(trait_values))
    trait_subset <- trait_values[species_in_tree]
    
    if (length(trait_subset) > 10) {  # Ensure enough data points
      # Compute Blomberg's K
      K_result <- phylosig(tree, trait_subset, method = "K", test = TRUE)
      
      # Compute Pagel's Lambda
      lambda_result <- phylosig(tree, trait_subset, method = "lambda", test = TRUE)
      
      # Return results as a named list
      return(data.frame(tree_id = i, 
                        K = K_result$K, K_pval = K_result$P, 
                        lambda = lambda_result$lambda, lambda_pval = lambda_result$P))
    } else {
      return(NULL)  # Skip trees with too few species
    }
  })
  
  # Remove NULL values before combining results
  results_list <- Filter(Negate(is.null), results_list)
  
  if (length(results_list) > 0) {
    return(do.call(rbind, results_list))  # Convert list output to a dataframe
  } else {
    return(data.frame(tree_id = integer(), K = numeric(), K_pval = numeric(),
                      lambda = numeric(), lambda_pval = numeric()))  # Return empty df
  }
}


# Create named vectors for each trait
marginality <- setNames(vert_enfa_unique$w.marginality, vert_enfa_unique$species)
specialization <- setNames(vert_enfa_unique$w.specialization, vert_enfa_unique$species)

# Lists to store results
signal_results <- list()

# Loop through each tetrapod class and run parallelized phylogenetic signal estimation
tetrapod_classes <- list(
  Reptiles = rep.multiphylo,
  Birds = birds.multiphylo,
  Mammals = mammals.multiphylo
)

for (class_name in names(tetrapod_classes)) {
  cat("Processing:", class_name, "\n")
  
  trees <- tetrapod_classes[[class_name]]
  
  # Parallelized calculation for marginality
  signal_results[[class_name]]$marginality <- calc_phylo_signal(marginality, trees)
  
  # Parallelized calculation for specialization
  signal_results[[class_name]]$specialization <- calc_phylo_signal(specialization, trees)
}

# Save results
saveRDS(signal_results, "niche_position_age/results/data/processed/phylogenetic_signal_results_parallel.rds")

# Shut down parallel workers
plan(sequential)