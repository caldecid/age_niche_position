library(corrplot)

# Correlations ------------------------------------------------------------

vert_enfa_unique <- read_csv("results/data/processed/vert_enfa_unique.csv")
##weighted by biome dataset
vert_enfa_unique <- vert_enfa_unique %>% dplyr::select(species, w.marginality, w.specialization) %>%
  rename(species_name = species,
         marginality_weighted = w.marginality, 
         specialization_weighted = w.specialization)


#######mammals

mammals_enfa_10th <- read_csv("results/data/processed/mammals_enfa_whole_neotropics.csv") %>% 
                     rename(marginality_10th = marginality,
                            specialization_10th = specialization)

mammals_enfa_5th <- read_csv("results/data/processed/sensitivity_analyses/mammals_enfa_5th.csv")%>% 
  rename(marginality_5th = marginality,
         specialization_5th = specialization)

mammals_enfa_3th <- read_csv("results/data/processed/sensitivity_analyses/neotropic/mammals_enfa_3th.csv")%>% 
  rename(marginality_3th = marginality,
         specialization_3th = specialization)


##binding 
mammals_enfa_sen <- left_join(mammals_enfa_10th, mammals_enfa_3th, by = "species_name") %>% 
                    
                    left_join(vert_enfa_unique, by = "species_name")


# Select the relevant columns
marg_cols <- mammals_enfa_sen %>%
  dplyr::select(starts_with("marginality"))

spec_cols <- mammals_enfa_sen %>%
  dplyr::select(starts_with("specialization"))

# Compute correlation matrices (using Pearson)
cor_marg <- cor(marg_cols, use = "pairwise.complete.obs", method = "pearson")
colnames(cor_marg) <- rownames(cor_marg) <- c(
  "Neotropic (th = 10)",
  #"VIF ≤ 5",
  "Neotropic (th = 3)",
  "Weighted"
)
cor_marg_mam <- as.data.frame(cor_marg)
cor_marg_mam$class <- "Mammals"

cor_spec <- cor(spec_cols, use = "pairwise.complete.obs", method = "pearson")
colnames(cor_spec) <- rownames(cor_spec) <- c(
  "Neotropic (th = 10)",
  #"VIF ≤ 5",
  "Neotropic (th = 3)",
  "Weighted"
)

cor_spe_mam <- as.data.frame(cor_spec)
cor_spe_mam$class <- "Mammals"

# marginality
corrplot(cor_marg,
         method = "number",
         type = "upper",
         tl.col = "black",
         tl.cex = 0.9,  # (optional) adjust text size
         title = "Mammals – Marginality correlations",
         mar = c(0, 0, 2, 0))

#specialization
corrplot(cor_spec,
         method = "number",
         type = "upper",
         tl.col = "black",
         tl.cex = 0.9,  # (optional) adjust text size
         title = "Mammals – Specialization correlations",
         mar = c(0, 0, 2, 0))



#######amphibians

amphibians_enfa_10th <- read_csv("results/data/processed/amphibians_enfa_whole_neotropics.csv") %>% 
  rename(marginality_10th = marginality,
         specialization_10th = specialization)


amphibians_enfa_3th <- read_csv("results/data/processed/sensitivity_analyses/neotropic/amphibians_enfa_3th.csv")%>% 
  rename(marginality_3th = marginality,
         specialization_3th = specialization)


##binding 
amphibians_enfa_sen <- left_join(amphibians_enfa_10th, amphibians_enfa_3th, by = "species_name") %>% 
  
  left_join(vert_enfa_unique, by = "species_name") %>% 
   drop_na()


# Select the relevant columns
marg_cols_amphibians <- amphibians_enfa_sen %>%
  dplyr::select(starts_with("marginality"))

spec_cols_amphibians <- amphibians_enfa_sen %>%
  dplyr::select(starts_with("specialization"))

# Compute correlation matrices (using Pearson)
cor_marg_amphibians <- cor(marg_cols_amphibians,
                           use = "pairwise.complete.obs", method = "pearson")

colnames(cor_marg_amphibians) <- rownames(cor_marg_amphibians) <- c(
  "Neotropic (th = 10)",
  
  "Neotropic (th = 3)",
  "Weighted"
)

cor_marg_amphibians <- as.data.frame(cor_marg_amphibians)
cor_marg_amphibians$class <- "amphibians"


cor_spec_amphibians <- cor(spec_cols_amphibians,
                           use = "pairwise.complete.obs", method = "pearson")


colnames(cor_spec_amphibians) <- rownames(cor_spec_amphibians) <- c(
  "Neotropic (th = 10)",
  
  "Neotropic (th = 3)",
  "Weighted"
)

cor_spec_amphibians <- as.data.frame(cor_spec_amphibians)
cor_spec_amphibians$class <- "amphibians"

# marginality
corrplot(cor_marg_amphibians,
         method = "number",
         type = "upper",
         tl.col = "black",
         tl.cex = 0.9,  # (optional) adjust text size
         title = "Amphibians – Marginality correlations",
         mar = c(0, 0, 2, 0))

#specialization
corrplot(cor_spec_amphibians,
         method = "number",
         type = "upper",
         tl.col = "black",
         tl.cex = 0.9,  # (optional) adjust text size
         title = "Amphibians – Specialization correlations",
         mar = c(0, 0, 2, 0))


#######reptiles

reptiles_enfa_10th <- read_csv("results/data/processed/reptiles_enfa_whole_neotropics.csv") %>% 
  rename(marginality_10th = marginality,
         specialization_10th = specialization)



reptiles_enfa_3th <- read_csv("results/data/processed/sensitivity_analyses/neotropic/reptiles_enfa_3th.csv")%>% 
  rename(marginality_3th = marginality,
         specialization_3th = specialization)


##binding 
reptiles_enfa_sen <- left_join(reptiles_enfa_10th, reptiles_enfa_3th, by = "species_name") %>% 
  
  left_join(vert_enfa_unique, by = "species_name") %>% 
  drop_na()


# Select the relevant columns
marg_cols_reptiles <- reptiles_enfa_sen %>%
  dplyr::select(starts_with("marginality"))

spec_cols_reptiles <- reptiles_enfa_sen %>%
  dplyr::select(starts_with("specialization"))

# Compute correlation matrices (using Pearson)
cor_marg_reptiles <- cor(marg_cols_reptiles,
                           use = "pairwise.complete.obs", method = "pearson")

colnames(cor_marg_reptiles) <- rownames(cor_marg_reptiles) <- c(
  "Neotropic (th = 10)",

  "Neotropic (th = 3)",
  "Weighted"
)

cor_marg_reptiles <- as.data.frame(cor_marg_reptiles)
cor_marg_reptiles$class <- "reptiles"

cor_spec_reptiles <- cor(spec_cols_reptiles,
                           use = "pairwise.complete.obs", method = "pearson")

colnames(cor_spec_reptiles) <- rownames(cor_spec_reptiles) <- c(
  "Neotropic (th = 10)",
 
  "Neotropic (th = 3)",
  "Weighted"
)

cor_spec_reptiles <- as.data.frame(cor_spec_reptiles)
cor_spec_reptiles$class <- "reptiles"


# marginality
corrplot(cor_marg_reptiles,
         method = "number",
         type = "upper",
         tl.col = "black",
         tl.cex = 0.9,  # (optional) adjust text size
         title = "Reptiles – Marginality correlations",
         mar = c(0, 0, 2, 0))

#specialization
corrplot(cor_spec_reptiles,
         method = "number",
         type = "upper",
         tl.col = "black",
         tl.cex = 0.9,  # (optional) adjust text size
         title = "Reptiles – Specialization correlations",
         mar = c(0, 0, 2, 0))


#######aves

aves_enfa_10th <- read_csv("results/data/processed/aves_enfa_whole_neotropics.csv") %>% 
  rename(marginality_10th = marginality,
         specialization_10th = specialization)



aves_enfa_3th <- read_csv("results/data/processed/sensitivity_analyses/neotropic/aves_enfa_3th.csv")%>% 
  rename(marginality_3th = marginality,
         specialization_3th = specialization)


##binding 
aves_enfa_sen <- left_join(aves_enfa_10th, aves_enfa_3th, by = "species_name") %>% 
 
  left_join(vert_enfa_unique, by = "species_name") %>% 
  drop_na()


# Select the relevant columns
marg_cols_aves <- aves_enfa_sen %>%
  dplyr::select(starts_with("marginality")) 

spec_cols_aves <- aves_enfa_sen %>%
  dplyr::select(starts_with("specialization"))

# Compute correlation matrices (using Pearson)
cor_marg_aves <- cor(marg_cols_aves,
                           use = "pairwise.complete.obs", method = "pearson")


colnames(cor_marg_aves) <- rownames(cor_marg_aves) <- c(
  "Neotropic (th = 10)",
  
  "Neotropic (th = 3)",
  "Weighted"
)

cor_marg_aves <- as.data.frame(cor_marg_aves)
cor_marg_aves$class <- "Birds"

cor_spec_aves <- cor(spec_cols_aves,
                           use = "pairwise.complete.obs", method = "pearson")

colnames(cor_spec_aves) <- rownames(cor_spec_aves) <- c(
  "Neotropic (th = 10)",
 
  "Neotropic (th = 3)",
  "Weighted"
)

cor_spec_aves <- as.data.frame(cor_spec_aves)
cor_spec_aves$class <- "Birds"

# marginality
corrplot(cor_marg_aves,
         method = "number",
         type = "upper",
         tl.col = "black",
         tl.cex = 0.9,  # (optional) adjust text size
         title = "Birds – Marginality correlations",
         mar = c(0, 0, 2, 0))

#specialization
corrplot(cor_spec_aves,
         method = "number",
         type = "upper",
         tl.col = "black",
         tl.cex = 0.9,  # (optional) adjust text size
         title = "Birds – Specialization correlations",
         mar = c(0, 0, 2, 0))


############## General correlations ####################

#specialization
cor_specialization <- rbind(cor_spe_mam, cor_spec_aves, cor_spec_reptiles,
                            cor_spec_amphibians)

cor_specialization$cor<- rownames(cor_specialization)

rownames(cor_specialization) <- NULL

#saving
write_xlsx(cor_specialization,
      "results/data/processed/sensitivity_analyses/neotropic/cor_specialization.xlsx")

 
#marginality
cor_marginality <- rbind(cor_marg_mam, cor_marg_aves, cor_marg_reptiles,
                         cor_marg_amphibians)

cor_marginality$cor <- rownames(cor_marginality)

rownames(cor_marginality) <- NULL

#saving
write_xlsx(cor_marginality,
           "results/data/processed/sensitivity_analyses/neotropic/cor_marginality.xlsx")
