# Calculate tip ages
calculate_tip_ages <- function(tree){
  phy.age <- picante::node.age(tree)
  BL.position <- cbind(phy.age$edge,phy.age$age, tree$edge.length)
  dist.tip <- max(phy.age$age)-BL.position[,3]
  BL.positions <- cbind(BL.position,dist.tip)
  ages<- BL.positions[,5] + BL.positions[,4]
  BL.positions <- cbind(BL.positions,ages)
  node.ages<- as.data.frame(BL.positions)
  names(node.ages) <- c("parental.node","daughter.node","dist.root","BL","dist.tip","mrca.age")
  ## node.ages is a data frame listing as variables the identity of parental and
  #daughter nodes, the distance from the root and from the present of each node,
  #the branch length and the age of the most recent common ancestor
  species.ages<- node.ages[node.ages[,2] < length(tree$tip)+1,]
  row.names(species.ages) <- tree$tip[species.ages$daughter.node]
  ## species ages is node.ages data frame reduced to the tips (species)
  species.ages <- species.ages[order(row.names(species.ages)),]
  output.table <- as.data.frame(cbind(row.names(species.ages),species.ages$mrca.age))
  colnames(output.table) <- c('tip','tip.age')
  return(output.table)
}


# New ultrametric fixing rooting 
force.ultrametric<-function(tree,method=c("nnls","extend"),...){
  require(phangorn)
  if(hasArg(message)) message<-list(...)$message
  else message<-TRUE
  if(message){
    cat("***************************************************************\n")
    cat("*                          Note:                              *\n")
    cat("*    force.ultrametric does not include a formal method to    *\n")
    cat("*    ultrametricize a tree & should only be used to coerce    *\n")
    cat("*   a phylogeny that fails is.ultramtric due to rounding --   *\n")
    cat("*    not as a substitute for formal rate-smoothing methods.   *\n")
    cat("***************************************************************\n")
  }
  method<-method[1]
  if(method=="nnls") tree<-nnls.tree(cophenetic(tree),tree,
                                     method="ultrametric",rooted=is.rooted(tree),trace=0)
  else if(method=="extend"){
    h<-diag(vcv(tree))
    d<-max(h)-h
    ii<-sapply(1:Ntip(tree),function(x,y) which(y==x),
               y=tree$edge[,2])
    tree$edge.length[ii]<-tree$edge.length[ii]+d
  } else 
    cat("method not recognized: returning input tree\n\n")
  tree
}


# Make output invisible
hush <- function(code) {
  sink("NULL")
  tmp <- code
  sink()
  tmp
}


# Estimating lambda and mu from an extinction fr and sampling fr  --------

#' @title Speciation and extinction rates from phylogeny
#'
#' @description This function estimates speciation and extinction rate from a phylogeny under a given extinction and sampling fraction
#' 
#' @param phy An ultrametric bifurcating phylogenetic tree, in ape "phylo" format.
#' @param epsilon Extinction fraction
#' @param rho Sampling fraction
#' @param ml_optim Method to use for optimisation. May be one of "optim", "subplex", "nlminb", "nlm" (partial unambigious string is allowed).
#' 
#' @return A named vector of two parameters, lambda and mu.


estimate_bd <- function(phy, epsilon = 0, rho = 1, ml_optim = 'subplex') {
  if(epsilon >= 1) {
    stop("Extinction fraction should not be greater than 1")
  }
  # Birth-death likelihood
  bd_lik <- make.bd(tree = phy, sampling.f = rho) 
  # Constrain extinction fraction
  con <- paste0("mu ~ ", epsilon, " * lambda")
  bd_lik <- constrain(bd_lik, con)
  # Initial birth rate for ML search with yule rate
  b_init <- phy$Nnode / sum(phy$edge.length)
  if (epsilon > 0) {
    b_init <- b_init / epsilon
  }
  # Finde maximum likelihood rates
  bd_fit <- find.mle(bd_lik, x.init = b_init, method = ml_optim)
  # Format birth and death rate for output
  lambda <- coef(bd_fit)
  mu <- epsilon * lambda
  bd_rates <- c(lambda, mu)
  names(bd_rates) <- c("lambda", "mu")
  return(bd_rates)
}


# Transforming a pgls model into a results dataframe  --------

#' @title PGLS results
#'
#' @description This function transforms a pgls model (caper) into a results dataframe
#' 
#' @param mod PGLS model
#' @param ext Extinction scenario
#' @param class Taxonomical class
#' @param biome biome/weighted
#'  
#' @return A results dataframe

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
  res_df <- res_df %>% select(term, Estimate:biome)
  
  return(res_df)
}


# PGLS models and results for each class and every biome  --------

#' @title PGLS for each class and every viome
#'
#' @description This function fits pgls models for each class and every biome
#' 
#' @param df dataset (vert_enfa_ages)
#' @param class class name ("MAMMALIA"/"REPTILIA"/"AMPHIBIA"/"AVES")
#' @param phy phylogeny
#' @param variable variable (marginality/specialization)
#' 
#'  
#' @return A list containing each biome dataframe

fun_biome_pgls <- function(df, class, phy, variable) {
  
  
  ##obtaining biomes names
  biomes <- unique(df$biome)
  
  ##each biome has three extinction scenarios
  list_biome <- vector("list", length = length(biomes))
  
  names(list_biome) <- biomes
  
  ##loop for evaluating each biome
  for(i in seq_along(biomes)) {
    
    ##dataset
    df_biome <- df %>% filter(className == class,
                              biome == biomes[i])
    
    ##dropping tips
    phy_biome <- keep.tip(phy, tip = df_biome$species)
    
    ##arranging dataset
    df_enfa_biome <- df_biome %>% 
      arrange(match(species, phy_biome$tip.label)) %>% 
      select(species, !!variable,
             low.age, int.age, high.age)
    
    ##Comparative data frame
    df_caper <- caper::comparative.data(phy = phy_biome,
                                        data = as.data.frame(df_enfa_biome),
                                        names.col = "species", vcv =TRUE ,
                                        warn.dropped = TRUE)
    
    
    ###low extinction scenario
    model_low <- pgls(as.formula(paste0("log(",
                                        variable, "+ 1) ~ log(low.age+1)")),
                      data = df_caper, lambda = "ML")
    
    ##results dataframe
    results_low <- res_pgls(mod = model_low, ext = "low",
                            class = class, biome = biomes[i])
    
    
    
    
    
    
    ###intermediate extinction
    model_int <- pgls(as.formula(paste0("log(",
                                        variable, "+ 1) ~ log(int.age+1)")),
                      data = df_caper, lambda = "ML")
    
    ##results dataframe
    results_int <- res_pgls(mod = model_int, ext = "int",
                            class = class, biome = biomes[i])
    
    
    ##high extinction
    model_high <- pgls(as.formula(paste0("log(",
                                         variable, "+ 1) ~ log(high.age+1)")),
                       data = df_caper, lambda = "ML")
    
    ##results dataframe
    results_high <- res_pgls(mod = model_high, ext = "high",
                             class = class, biome = biomes[i])
    
    
    
    list_biome[[i]] <- rbind(results_low, results_int, results_high)
    print(biomes[[i]])
  }
  
  return(list_biome)
} 
