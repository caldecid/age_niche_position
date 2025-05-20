
# Comparative.data for weighted PGLS --------------------------------------

library(doParallel)
library(foreach)

##This script was run in the USP server

comp_phy_fun_parallel <- function(group.list, multi.phylo) {
  cl <- makeCluster(5) ##warning; using 5 cores
  registerDoParallel(cl)
  
  results <- foreach(j = seq_along(group.list), .packages = c("caper", "dplyr")) %dopar% {
    tryCatch({
      group_enfa_unique <- distinct(group.list[[j]], species, .keep_all = TRUE)
      group_phy_unique <- keep.tip(multi.phylo[[j]], tip = group_enfa_unique$species)
      
      group_enfa_unique_2 <- group_enfa_unique %>% 
        arrange(match(species, group_phy_unique$tip.label)) %>% 
        dplyr::select(species, w.marginality, w.specialization, low.age, int.age, high.age)
      
      caper::comparative.data(
        phy = group_phy_unique,
        data = as.data.frame(group_enfa_unique_2),
        names.col = "species", vcv = TRUE, warn.dropped = TRUE
      )
    }, error = function(e) {
      message(paste("Error in iteration", j, ":", e$message))
      NULL
    })
  }
  
  stopCluster(cl)
  return(results)
}

####mammals
mam_comp_weighted <- comp_phy_fun_parallel(group.list = mammals.list.ages,
                                           multi.phylo = mammals.multiphylo)


save(mam_comp_weighted,
     file = "results/data/metadata/mam_comparative_weighted.RData")

###bird
bird_comp_weighted <- comp_phy_fun_parallel(group.list = birds.list.ages,
                                            multi.phylo = birds.multiphylo)

save(bird_comp_weighted,
     file = "results/data/metadata/bird_comparative_weighted.RData")

#####amphibians
amp_comp_weighted <- comp_phy_fun_parallel(group.list = amphi.list.ages,
                                           multi.phylo = amphi.multiphylo)

save(amp_comp_weighted,
     file = "results/data/metadata/amp_comparative_weighted.RData")


###reptiles
rep_comp_weighted <- comp_phy_fun_parallel(group.list = rep.list.ages,
                                           multi.phylo = rep.multiphylo)

save(rep_comp_weighted,
     file = "results/data/metadata/rep_comparative_weighted.RData")