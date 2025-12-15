# correlations ENFA biome 3th ------------------------------------------------

library(tidyverse)
library(corrplot)


# amphibians --------------------------------------------------------------

#amphibians dataset (th = 10)
amphibians_biome_enfa <- read_csv("results/data/metadata/amphibians_biome_enfa.csv")


##amphibians dataset (th = 3)

amphibians_files <- list.files("results/data/processed/sensitivity_analyses/biome_3th/amphibians")

# Folder where your CSV files are stored
folder_amphibians <- "results/data/processed/sensitivity_analyses/biome_3th/amphibians"

# Read and rbind all files
amphibians_biome_enfa_3th <- do.call(rbind, lapply(amphibians_files, function(f) {
  read.csv(file.path(folder_amphibians, f), stringsAsFactors = FALSE)
})) %>% drop_na() %>% 
  rename(marginality_3th = marginality,
         specialization_3th = specialization)

#saving
write_csv(amphibians_biome_enfa_3th, file = "results/data/processed/sensitivity_analyses/biome_3th/amphibians/amphibians_enfa_3th.csv")

#cleaning the biome strings
normalize_biome <- function(x) {
  x %>%
    tolower() %>%
    gsub(",", "", .) %>%
    gsub("&", "and", .) %>%
    gsub("\\s+", "_", .) %>%      # collapse spaces to underscores
    gsub("_+", "_", .) %>%        # collapse multiple underscores
    trimws()
}


amphibians_biome_enfa <- amphibians_biome_enfa %>% mutate(biome_clean = normalize_biome(biome))

amphibians_enfa_biome_3th <- amphibians_biome_enfa_3th %>% mutate(biome_clean = normalize_biome(biome))

#amphibians merged
amphibians_merged <- amphibians_biome_enfa %>% 
                     left_join(amphibians_enfa_biome_3th,
                               by = c("species", "biome_clean")) %>% 
                    drop_na()


#general trend
#marginality
amphibians_merged %>% ggplot(aes(x = marginality, y = marginality_3th)) +
                geom_point(alpha = 0.7)+
               geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                           color = "red")


#specialization
amphibians_merged %>% ggplot(aes(x = specialization, y = specialization_3th)) +
  geom_point(alpha = 0.7)+
  xlim(0,50)+
  ylim(0,50)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "red")

##separate by biome for further correlation
biomes <- unique(amphibians_merged$biome_clean)
amphibians_biome <- vector("list", length= length(biomes))
names(amphibians_biome) <- biomes

#assigning a list element for each dataframe defined by biome
for(i in seq_along(biomes)){
   amphibians_biome[[i]] <- amphibians_merged %>% filter(biome_clean == biomes[i])
}

names(amphibians_biome) <- tools::toTitleCase(gsub("_", " ", names(amphibians_biome)))

#creating empty dataframe
df_cor_amphibians <- data.frame(
  biome = character(11),
  marginality_3th = numeric(11),
  specialization_3th = numeric(11),
  stringsAsFactors = FALSE
)

df_cor_amphibians$class <- "amphibians"

########Implement a for loop for generating figures for each biome###########
for(i in seq_along(amphibians_biome)){
  #marginality correlation
  marg_cols <- amphibians_biome[[i]] %>%
    dplyr::select(starts_with("marginality"))
  
  #specialization correlation
  spec_cols <- amphibians_biome[[i]] %>%
    dplyr::select(starts_with("specialization"))
  
  # Compute correlation matrices (using Pearson)
  
  #marginality
  cor_marg <- cor(marg_cols, use = "pairwise.complete.obs", method = "pearson")
  colnames(cor_marg) <- rownames(cor_marg) <- c(
    "VIF ≤ 10",
    "VIF ≤ 3")
  
  #specialization
  cor_spec <- cor(spec_cols, use = "pairwise.complete.obs", method = "pearson")
  colnames(cor_spec) <- rownames(cor_spec) <- c(
    "VIF ≤ 10",
    "VIF ≤ 3"
  )
  
  df_cor_amphibians[i, 1] <- names(amphibians_biome[i])
  df_cor_amphibians[i, 2] <- cor_marg[2,1]
  df_cor_amphibians[i, 3] <- cor_spec[2,1]
  
  # marginality
#saving as png
  png(paste0("figures/biome_3th/amphibians/cor_marg_",names(amphibians_biome[i]),
  ".png"), 
      width = 12, height = 12,
      units = "cm", pointsize = 8, res = 300)
  
  corrplot(cor_marg,
           method = "number",
           type = "upper",
           tl.col = "black",
           tl.cex = 0.9,  # (optional) adjust text size
           title = "Amphibians – Marginality correlations",
           mar = c(0, 0, 2, 0))
  mtext(paste0(names(amphibians_biome[i])), 
        side = 1,               
        adj = 0,                
        line = 1,             
        cex = 0.9,              
        font = 2)  
  
  dev.off()
  
 
  
#specialization
png(paste0("figures/biome_3th/amphibians/cor_spe_",names(amphibians_biome[i]),
             ".png"), 
      width = 12, height = 12,
      units = "cm", pointsize = 8, res = 300)
corrplot(cor_spec,
           method = "number",
           type = "upper",
           tl.col = "black",
           tl.cex = 0.9,  # (optional) adjust text size
           title = "Amphibians – Specialization correlations",
           mar = c(0, 0, 2, 0))
mtext(paste0(names(amphibians_biome[i])), 
      side = 1,               
      adj = 0,                
      line = 1,             
      cex = 0.9,              
      font = 2)  

dev.off()
  
}

#saving
write_csv(df_cor_amphibians, "results/data/processed/sensitivity_analyses/biome_3th/df_cor_amphibians.csv")


# reptiles ----------------------------------------------------------------


#reptiles dataset (th = 10)
reptiles_biome_enfa <- read_csv("results/data/metadata/reptiles_biome_enfa.csv")


##reptiles dataset (th = 3)

reptiles_files <- list.files("results/data/processed/sensitivity_analyses/biome_3th/reptiles")

# Folder where your CSV files are stored
folder_reptiles <- "results/data/processed/sensitivity_analyses/biome_3th/reptiles"

# Read and rbind all files
reptiles_biome_enfa_3th <- do.call(rbind, lapply(reptiles_files, function(f) {
  read.csv(file.path(folder_reptiles, f), stringsAsFactors = FALSE)
})) %>% drop_na() %>% 
  rename(marginality_3th = marginality,
         specialization_3th = specialization)

#saving
write_csv(reptiles_biome_enfa_3th, file = "results/data/processed/sensitivity_analyses/biome_3th/reptiles/reptiles_enfa_3th.csv")

#cleaning the biome strings
normalize_biome <- function(x) {
  x %>%
    tolower() %>%
    gsub(",", "", .) %>%
    gsub("&", "and", .) %>%
    gsub("\\s+", "_", .) %>%      # collapse spaces to underscores
    gsub("_+", "_", .) %>%        # collapse multiple underscores
    trimws()
}


reptiles_biome_enfa <- reptiles_biome_enfa %>% mutate(biome_clean = normalize_biome(biome))

reptiles_enfa_biome_3th <- reptiles_biome_enfa_3th %>% mutate(biome_clean = normalize_biome(biome))

#reptiles merged
reptiles_merged <- reptiles_biome_enfa %>% 
  left_join(reptiles_enfa_biome_3th,
            by = c("species", "biome_clean")) %>% 
  drop_na()



#general trend
#marginality
reptiles_merged %>% ggplot(aes(x = marginality, y = marginality_3th)) +
  geom_point(alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "red")


#specialization
reptiles_merged %>% ggplot(aes(x = specialization, y = specialization_3th)) +
  geom_point(alpha = 0.7)+
  xlim(0,50)+
  ylim(0,50)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "red")

##separate by biome for further correlation
biomes <- unique(reptiles_merged$biome_clean)
reptiles_biome <- vector("list", length= length(biomes))
names(reptiles_biome) <- biomes

#assigning a list element for each dataframe defined by biome
for(i in seq_along(biomes)){
  reptiles_biome[[i]] <- reptiles_merged %>% filter(biome_clean == biomes[i])
}

names(reptiles_biome) <- tools::toTitleCase(gsub("_", " ", names(reptiles_biome)))

#creating empty dataframe
df_cor_reptiles <- data.frame(
  biome = character(11),
  marginality_3th = numeric(11),
  specialization_3th = numeric(11),
  stringsAsFactors = FALSE
)

df_cor_reptiles$class <- "reptiles"

########Implement a for loop for generating figures for each biome###########
for(i in seq_along(reptiles_biome)){
  #marginality correlation
  marg_cols <- reptiles_biome[[i]] %>%
    dplyr::select(starts_with("marginality"))
  
  #specialization correlation
  spec_cols <- reptiles_biome[[i]] %>%
    dplyr::select(starts_with("specialization"))
  
  # Compute correlation matrices (using Pearson)
  
  #marginality
  cor_marg <- cor(marg_cols, use = "pairwise.complete.obs", method = "pearson")
  colnames(cor_marg) <- rownames(cor_marg) <- c(
    "VIF ≤ 10",
    "VIF ≤ 3")
  
  #specialization
  cor_spec <- cor(spec_cols, use = "pairwise.complete.obs", method = "pearson")
  colnames(cor_spec) <- rownames(cor_spec) <- c(
    "VIF ≤ 10",
    "VIF ≤ 3"
  )
  
  df_cor_reptiles[i, 1] <- names(reptiles_biome[i])
  df_cor_reptiles[i, 2] <- cor_marg[2,1]
  df_cor_reptiles[i, 3] <- cor_spec[2,1]
  
  # marginality
  #saving as png
  png(paste0("figures/biome_3th/reptiles/cor_marg_",names(reptiles_biome[i]),
             ".png"), 
      width = 12, height = 12,
      units = "cm", pointsize = 8, res = 300)
  
  corrplot(cor_marg,
           method = "number",
           type = "upper",
           tl.col = "black",
           tl.cex = 0.9,  # (optional) adjust text size
           title = "reptiles – Marginality correlations",
           mar = c(0, 0, 2, 0))
  mtext(paste0(names(reptiles_biome[i])), 
        side = 1,               
        adj = 0,                
        line = 1,             
        cex = 0.9,              
        font = 2)  
  
  dev.off()
  
  
  
  #specialization
  png(paste0("figures/biome_3th/reptiles/cor_spe_",names(reptiles_biome[i]),
             ".png"), 
      width = 12, height = 12,
      units = "cm", pointsize = 8, res = 300)
  corrplot(cor_spec,
           method = "number",
           type = "upper",
           tl.col = "black",
           tl.cex = 0.9,  # (optional) adjust text size
           title = "reptiles – Specialization correlations",
           mar = c(0, 0, 2, 0))
  mtext(paste0(names(reptiles_biome[i])), 
        side = 1,               
        adj = 0,                
        line = 1,             
        cex = 0.9,              
        font = 2)  
  
  dev.off()
  
}

#saving
write_csv(df_cor_reptiles, "results/data/processed/sensitivity_analyses/biome_3th/df_cor_reptiles.csv")

# mammals ----------------------------------------------------------------

#mammals dataset (th = 10)
mammals_biome_enfa <- read_csv("results/data/metadata/mammals_biome_enfa.csv")


##mammals dataset (th = 3)

mammals_files <- list.files("results/data/processed/sensitivity_analyses/biome_3th/mammals")

# Folder where your CSV files are stored
folder_mammals <- "results/data/processed/sensitivity_analyses/biome_3th/mammals"

# Read and rbind all files
mammals_biome_enfa_3th <- do.call(rbind, lapply(mammals_files, function(f) {
  read.csv(file.path(folder_mammals, f), stringsAsFactors = FALSE)
})) %>% drop_na() %>% 
  rename(marginality_3th = marginality,
         specialization_3th = specialization)

#saving
write_csv(mammals_biome_enfa_3th, file = "results/data/processed/sensitivity_analyses/biome_3th/mammals/mammals_enfa_3th.csv")

#cleaning the biome strings
normalize_biome <- function(x) {
  x %>%
    tolower() %>%
    gsub(",", "", .) %>%
    gsub("&", "and", .) %>%
    gsub("\\s+", "_", .) %>%      # collapse spaces to underscores
    gsub("_+", "_", .) %>%        # collapse multiple underscores
    trimws()
}


mammals_biome_enfa <- mammals_biome_enfa %>% mutate(biome_clean = normalize_biome(biome))

mammals_enfa_biome_3th <- mammals_biome_enfa_3th %>% mutate(biome_clean = normalize_biome(biome))

#mammals merged
mammals_merged <- mammals_biome_enfa %>% 
  left_join(mammals_enfa_biome_3th,
            by = c("species", "biome_clean")) %>% 
  drop_na()



#general trend
#marginality
mammals_merged %>% ggplot(aes(x = marginality, y = marginality_3th)) +
  geom_point(alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "red")


#specialization
mammals_merged %>% ggplot(aes(x = specialization, y = specialization_3th)) +
  geom_point(alpha = 0.7)+
  xlim(0,50)+
  ylim(0,50)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "red")

##separate by biome for further correlation
biomes <- unique(mammals_merged$biome_clean)
mammals_biome <- vector("list", length= length(biomes))
names(mammals_biome) <- biomes

#assigning a list element for each dataframe defined by biome
for(i in seq_along(biomes)){
  mammals_biome[[i]] <- mammals_merged %>% filter(biome_clean == biomes[i])
}

names(mammals_biome) <- tools::toTitleCase(gsub("_", " ", names(mammals_biome)))

#creating empty dataframe
df_cor_mammals <- data.frame(
  biome = character(11),
  marginality_3th = numeric(11),
  specialization_3th = numeric(11),
  stringsAsFactors = FALSE
)

df_cor_mammals$class <- "mammals"

########Implement a for loop for generating figures for each biome###########
for(i in seq_along(mammals_biome)){
  #marginality correlation
  marg_cols <- mammals_biome[[i]] %>%
    dplyr::select(starts_with("marginality"))
  
  #specialization correlation
  spec_cols <- mammals_biome[[i]] %>%
    dplyr::select(starts_with("specialization"))
  
  # Compute correlation matrices (using Pearson)
  
  #marginality
  cor_marg <- cor(marg_cols, use = "pairwise.complete.obs", method = "pearson")
  colnames(cor_marg) <- rownames(cor_marg) <- c(
    "VIF ≤ 10",
    "VIF ≤ 3")
  
  #specialization
  cor_spec <- cor(spec_cols, use = "pairwise.complete.obs", method = "pearson")
  colnames(cor_spec) <- rownames(cor_spec) <- c(
    "VIF ≤ 10",
    "VIF ≤ 3"
  )
  
  df_cor_mammals[i, 1] <- names(mammals_biome[i])
  df_cor_mammals[i, 2] <- cor_marg[2,1]
  df_cor_mammals[i, 3] <- cor_spec[2,1]
  
  # marginality
  #saving as png
  png(paste0("figures/biome_3th/mammals/cor_marg_",names(mammals_biome[i]),
             ".png"), 
      width = 12, height = 12,
      units = "cm", pointsize = 8, res = 300)
  
  corrplot(cor_marg,
           method = "number",
           type = "upper",
           tl.col = "black",
           tl.cex = 0.9,  # (optional) adjust text size
           title = "mammals – Marginality correlations",
           mar = c(0, 0, 2, 0))
  mtext(paste0(names(mammals_biome[i])), 
        side = 1,               
        adj = 0,                
        line = 1,             
        cex = 0.9,              
        font = 2)  
  
  dev.off()
  
  
  
  #specialization
  png(paste0("figures/biome_3th/mammals/cor_spe_",names(mammals_biome[i]),
             ".png"), 
      width = 12, height = 12,
      units = "cm", pointsize = 8, res = 300)
  corrplot(cor_spec,
           method = "number",
           type = "upper",
           tl.col = "black",
           tl.cex = 0.9,  # (optional) adjust text size
           title = "mammals – Specialization correlations",
           mar = c(0, 0, 2, 0))
  mtext(paste0(names(mammals_biome[i])), 
        side = 1,               
        adj = 0,                
        line = 1,             
        cex = 0.9,              
        font = 2)  
  
  dev.off()
  
}

#saving
write_csv(df_cor_mammals, "results/data/processed/sensitivity_analyses/biome_3th/df_cor_mammals.csv")

# aves ----------------------------------------------------------------

#aves dataset (th = 10)
aves_biome_enfa <- read_csv("results/data/metadata/aves_biome_enfa.csv")


##aves dataset (th = 3)

aves_files <- list.files("results/data/processed/sensitivity_analyses/biome_3th/aves")

# Folder where your CSV files are stored
folder_aves <- "results/data/processed/sensitivity_analyses/biome_3th/aves"

# Read and rbind all files
aves_biome_enfa_3th <- do.call(rbind, lapply(aves_files, function(f) {
  read.csv(file.path(folder_aves, f), stringsAsFactors = FALSE)
})) %>% drop_na() %>% 
  rename(marginality_3th = marginality,
         specialization_3th = specialization)

#saving
#saving
write_csv(aves_biome_enfa_3th, file = "results/data/processed/sensitivity_analyses/biome_3th/aves/aves_enfa_3th.csv")

#cleaning the biome strings
normalize_biome <- function(x) {
  x %>%
    tolower() %>%
    gsub(",", "", .) %>%
    gsub("&", "and", .) %>%
    gsub("\\s+", "_", .) %>%      # collapse spaces to underscores
    gsub("_+", "_", .) %>%        # collapse multiple underscores
    trimws()
}


aves_biome_enfa <- aves_biome_enfa %>% mutate(biome_clean = normalize_biome(biome))

aves_enfa_biome_3th <- aves_biome_enfa_3th %>% mutate(biome_clean = normalize_biome(biome))

#aves merged
aves_merged <- aves_biome_enfa %>% 
  left_join(aves_enfa_biome_3th,
            by = c("species", "biome_clean")) %>% 
  drop_na()



#general trend
#marginality
aves_merged %>% ggplot(aes(x = marginality, y = marginality_3th)) +
  geom_point(alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "red")


#specialization
aves_merged %>% ggplot(aes(x = specialization, y = specialization_3th)) +
  geom_point(alpha = 0.7)+
  xlim(0,50)+
  ylim(0,50)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "red")

##separate by biome for further correlation
biomes <- unique(aves_merged$biome_clean)
aves_biome <- vector("list", length= length(biomes))
names(aves_biome) <- biomes

#assigning a list element for each dataframe defined by biome
for(i in seq_along(biomes)){
  aves_biome[[i]] <- aves_merged %>% filter(biome_clean == biomes[i])
}

names(aves_biome) <- tools::toTitleCase(gsub("_", " ", names(aves_biome)))

#creating empty dataframe
df_cor_aves <- data.frame(
  biome = character(11),
  marginality_3th = numeric(11),
  specialization_3th = numeric(11),
  stringsAsFactors = FALSE
)

df_cor_aves$class <- "aves"

########Implement a for loop for generating figures for each biome###########
for(i in seq_along(aves_biome)){
  #marginality correlation
  marg_cols <- aves_biome[[i]] %>%
    dplyr::select(starts_with("marginality"))
  
  #specialization correlation
  spec_cols <- aves_biome[[i]] %>%
    dplyr::select(starts_with("specialization"))
  
  # Compute correlation matrices (using Pearson)
  
  #marginality
  cor_marg <- cor(marg_cols, use = "pairwise.complete.obs", method = "pearson")
  colnames(cor_marg) <- rownames(cor_marg) <- c(
    "VIF ≤ 10",
    "VIF ≤ 3")
  
  #specialization
  cor_spec <- cor(spec_cols, use = "pairwise.complete.obs", method = "pearson")
  colnames(cor_spec) <- rownames(cor_spec) <- c(
    "VIF ≤ 10",
    "VIF ≤ 3"
  )
  
  df_cor_aves[i, 1] <- names(aves_biome[i])
  df_cor_aves[i, 2] <- cor_marg[2,1]
  df_cor_aves[i, 3] <- cor_spec[2,1]
  
  # marginality
  #saving as png
  png(paste0("figures/biome_3th/aves/cor_marg_",names(aves_biome[i]),
             ".png"), 
      width = 12, height = 12,
      units = "cm", pointsize = 8, res = 300)
  
  corrplot(cor_marg,
           method = "number",
           type = "upper",
           tl.col = "black",
           tl.cex = 0.9,  # (optional) adjust text size
           title = "aves – Marginality correlations",
           mar = c(0, 0, 2, 0))
  mtext(paste0(names(aves_biome[i])), 
        side = 1,               
        adj = 0,                
        line = 1,             
        cex = 0.9,              
        font = 2)  
  
  dev.off()
  
  
  
  #specialization
  png(paste0("figures/biome_3th/aves/cor_spe_",names(aves_biome[i]),
             ".png"), 
      width = 12, height = 12,
      units = "cm", pointsize = 8, res = 300)
  corrplot(cor_spec,
           method = "number",
           type = "upper",
           tl.col = "black",
           tl.cex = 0.9,  # (optional) adjust text size
           title = "aves – Specialization correlations",
           mar = c(0, 0, 2, 0))
  mtext(paste0(names(aves_biome[i])), 
        side = 1,               
        adj = 0,                
        line = 1,             
        cex = 0.9,              
        font = 2)  
  
  dev.off()
  
}

#saving
write_csv(df_cor_aves, "results/data/processed/sensitivity_analyses/biome_3th/df_cor_aves.csv")

###general correlation table
df_cor_general <- rbind(df_cor_mammals, df_cor_aves, df_cor_reptiles, 
                        df_cor_amphibians)

write_csv(df_cor_general, "results/data/processed/sensitivity_analyses/biome_3th/df_cor_general.csv")





####### joining list.ages ########

##### mammals
mammals.list.ages <-  map(
                            mammals.list.ages,
                ~ .x %>% mutate(biome_clean = normalize_biome(biome))
                        )

mammals.list.ages_joined <- map(mammals.list.ages, ~ 
                        left_join(.x, mammals_enfa_biome_3th, by = c("species", "biome_clean"))
)


##aves

birds.list.ages <-  map(
  birds.list.ages,
  ~ .x %>% mutate(biome_clean = normalize_biome(biome))
)

birds.list.ages_joined <- map(birds.list.ages, ~ 
                                  left_join(.x, aves_enfa_biome_3th, by = c("species", "biome_clean"))
)

####reptiles

reptiles.list.ages <-  map(
  rep.list.ages,
  ~ .x %>% mutate(biome_clean = normalize_biome(biome))
)

reptiles.list.ages_joined <- map(reptiles.list.ages, ~ 
                                  left_join(.x, reptiles_enfa_biome_3th, by = c("species", "biome_clean"))
)

######amphibians

amphibians.list.ages <-  map(
  amphi.list.ages,
  ~ .x %>% mutate(biome_clean = normalize_biome(biome))
)

amphibians.list.ages_joined <- map(amphibians.list.ages, ~ 
                                  left_join(.x, amphibians_enfa_biome_3th, by = c("species", "biome_clean"))
)

save(mammals.list.ages_joined, birds.list.ages_joined, 
        amphibians.list.ages_joined, reptiles.list.ages_joined,
       file = "results/data/processed/sensitivity_analyses/biome_3th/vert_enfa_ages_biome_3th.RData")

#####we are going to use as threshold 0.7 to rerun the models

df_cor_general %>% filter(marginality_3th < 0.7)


