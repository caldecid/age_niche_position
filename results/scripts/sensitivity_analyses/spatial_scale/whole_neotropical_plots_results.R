
##sourcing the libraries and the directories
source(file.path(getwd(), "/source.R"))

##libraries
library(tidyverse)
library(phytools)
library(caper)
library(purrr)
library(dunn.test)

##calling area by biome for some analyses
area_by_biome <- read_csv(file = "results/data/metadata/area_by_biome.csv")

##calling tetrapods enfa for whole neotropics

#aves
aves_enfa_neo <- read_csv("results/data/processed/aves_enfa_whole_neotropics.csv") %>% 
  rename(neo_marginality = marginality,
         neo_specialization = specialization)

aves_enfa_neo$class <- "Aves"

#mammals
mammals_enfa_neo <- read_csv("results/data/processed/mammals_enfa_whole_neotropics.csv")%>% 
  rename(neo_marginality = marginality,
         neo_specialization = specialization)

mammals_enfa_neo$class <- "Mammals"

#reptiles
reptiles_enfa_neo <- read_csv("results/data/processed/reptiles_enfa_whole_neotropics.csv")%>% 
  rename(neo_marginality = marginality,
         neo_specialization = specialization)

reptiles_enfa_neo$class <- "Reptiles"

#amphibians
amphibians_enfa_neo <- read_csv("results/data/processed/amphibians_enfa_whole_neotropics.csv")%>% 
  rename(neo_marginality = marginality,
         neo_specialization = specialization)

amphibians_enfa_neo$class <- "Amphibians"

###rbinding for figures
tetrapod_neo <- rbind(mammals_enfa_neo, aves_enfa_neo, reptiles_enfa_neo,
                      amphibians_enfa_neo)

tetrapod_neo$class <- factor(tetrapod_neo$class,
                             levels = c("Mammals",
                                        "Aves",
                                        "Reptiles",
                                        "Amphibians"),
                             ordered = TRUE)


##figure

#color categories
class_colors <- c(
  "Mammals" = "#E66101",
  "Aves" = "#E69F00",   
  
  "Reptiles" = "#018571", 
  "Amphibians" = "#7fbf7b"
)

ggplot(tetrapod_neo, aes(x = class, y = neo_specialization,
                             fill = class)) +
  geom_boxplot()+#outlier.shape  = NA) +
  scale_fill_manual(values = class_colors)+
  ylim(0,20)+
  xlab(NULL)+
  ylab("Marginality")+
  theme_bw()+
  #mynamestheme+
  theme(legend.position = "none",
        axis.title.y = element_text(margin = margin(r = 13)),
       # axis.text.x = element_blank(),
        axis.title = element_text(size = 17),
        axis.text.y = element_text(size = 16))



##calling RData lists with estimated and corrected ages
load("results/data/processed/vert_enfa_ages_list.RData")

#transforming the list with the ENFA measures from the whole neotropics

#aves
aves.list.ages_joined <- purrr::map(birds.list.ages, ~
                                      left_join(.x, aves_enfa_neo, by = c("species" = "species_name"))
)


##mammals
mammals.list.ages_joined <- purrr::map(mammals.list.ages, ~
                                         left_join(.x, mammals_enfa_neo, by = c("species" = "species_name"))
)

#reptiles
reptiles.list.ages_joined <- purrr::map(rep.list.ages, ~
                                          left_join(.x, reptiles_enfa_neo, by = c("species" = "species_name"))
)

#amphibians
amphi.list.ages_joined <- purrr::map(amphi.list.ages, ~
                                       left_join(.x, amphibians_enfa_neo, by = c("species" = "species_name"))
)

#saving as RData
save(mammals.list.ages_joined, aves.list.ages_joined, reptiles.list.ages_joined,
     amphi.list.ages_joined, file = "results/data/metadata/tetrapod_list_neo_ages.RData")


#######looking correlations

#######aves
aves_unique <- aves.list.ages_joined[[1]] 
    #%>%
  #distinct(species, .keep_all = TRUE) 
  

#######mammals
mammals_unique <- mammals.list.ages_joined[[1]] 
#%>%
  #distinct(species, .keep_all = TRUE) 


#######reptiles
reptiles_unique <- reptiles.list.ages_joined[[1]] 
#%>%
  #distinct(species, .keep_all = TRUE) 

#######amphibians
amphibians_unique <- amphi.list.ages_joined[[1]] #%>%
  #distinct(species, .keep_all = TRUE) 
  

##rbinding
tetrapod_unique <- rbind(aves_unique, mammals_unique, reptiles_unique,
                         amphibians_unique) %>% 
                  drop_na() %>% #recoding biomes name
     mutate(biome_acronym = recode(biome,
                                "Tropical_&_subtropical_moist_broadleaf_forests" = "TSMBF",
                                "Montane_grasslands_and_shrublands" = "MGS",
                                "Mediterranean_forests_woodlands_and_scrub" = "MFWS",
                                "Deserts_and_xeric_shrublands" = "DXS",
                                "mangroves" = "MAN",
                                "Tropical_&_subtropical_dry_broadleaf_forests" = "TSDBF",
                                "Tropical_&_subtropical_coniferous_forests" = "TSCF",
                                "Temperate_broadleaf_&_mixed_forests" = "TBMF",
                                "Tropical_&_subtropical_grasslands_savannas_and_shrublands" = "TSGSS",
                                "Temperate_grasslands_savannas_and_ shrublands" = "TGSS",
                                "Flooded_grasslands_and_savannas" = "FGS"
  ))


#factor
tetrapod_unique$class <- factor(
                   tetrapod_unique$class,
                  levels = c("Mammals", "Aves", "Reptiles", "Amphibians"),
                   ordered = TRUE)

#factorizing according to which biome has greater area in the Neotropics

#acronym_order <- area_by_biome$BIOME_name
acronym_order <- c("TSMBF", "TSGSS", "TGSS", "DXS", "TSDBF", 
                   "MGS", "TBMF", "TSCF", "FGS", "MFWS", "MAN")

tetrapod_unique$biome_acronym <- factor(tetrapod_unique$biome_acronym,
                                        levels = acronym_order,
                                        ordered = TRUE)
# Define class colors
class_colors <- c(
  "Mammals" = "#E66101",
  "Aves" = "#E69F00",
  "Reptiles" = "#018571",
  "Amphibians" = "#7fbf7b"
)

##Plotting

#Neotropical marginality vs Biome marginality

p_marg <- ggplot(tetrapod_unique, aes(x = neo_marginality, y = marginality, color = class)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  facet_grid(class ~ biome_acronym, scales = "fixed", space = "fixed") +
  scale_x_continuous(limits = c(0, 6), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 6), expand = c(0, 0)) +
  scale_color_manual(values = class_colors) +
  coord_fixed(ratio = 1) +
  theme_bw(base_size = 12) +
  theme(
    strip.text.y = element_blank(),        # removes facet labels
    panel.spacing = unit(1, "lines"),
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5)
  ) +
  labs(
    x = "Whole neotropical marginality",
    y = "Biome marginality",
    
  )

#saving
#svg file
svg("figures/whole_neo/neo_marg_biome.svg",
    width = 40, height = 12)
p_marg
dev.off()

#png file
png("figures/whole_neo/neo_marg_biome.png", 
    width = 40, height = 12,
    units = "cm", pointsize = 8, res = 300)
p_marg
dev.off()


#Neotropical specialization vs Biome specialization

p_spe <- ggplot(tetrapod_unique, aes(x = neo_specialization, y = specialization, color = class)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  facet_grid(class ~ biome_acronym, scales = "fixed", space = "fixed") +
  scale_x_continuous(limits = c(0, 60), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 60), expand = c(0, 0)) +
  scale_color_manual(values = class_colors) +
  coord_fixed(ratio = 1) +
  theme_bw(base_size = 12) +
  theme(
    strip.text.y = element_blank(),        # removes facet labels
    panel.spacing = unit(1, "lines"),
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6.5),
    axis.text.y = element_text(size = 8),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5)
  ) +
  labs(
    x = "Whole neotropical specialization",
    y = "Biome specialization"
  )

#saving
#svg file
svg("figures/whole_neo/neo_spe_biome.svg",
    width = 44, height = 12)
p_spe
dev.off()

#png file
png("figures/whole_neo/neo_spe_biome.png", 
    width = 44, height = 12,
    units = "cm", pointsize = 8, res = 300)
p_spe
dev.off()



#####now comparation of the weighted measures across classes

# --- Plot 1: Marginality ---
p1 <-  tetrapod_unique %>% #just one value per species for not overplotting
      distinct(species, .keep_all = TRUE)  %>% 
   ggplot(aes(x = neo_marginality, y = w.marginality, color = class)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  facet_grid(class ~ ., scales = "fixed", space = "fixed") +
  scale_x_continuous(limits = c(0, 6), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 6), expand = c(0, 0)) +
  scale_color_manual(values = class_colors) +
  coord_fixed(ratio = 1) +
  theme_bw(base_size = 12) +
  theme(
    strip.text.y = element_blank(),        # removes facet labels
    panel.spacing = unit(1, "lines"),
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5)
  ) +
  labs(
    x = "Neotropical marginality",
    y = "Weighted marginality",
   
  )

# --- Plot 2: Specialization ---
p2 <- tetrapod_unique %>% #just one value per species for not overplotting
  distinct(species, .keep_all = TRUE)  %>% 
  ggplot(aes(x = neo_specialization, y = w.specialization, color = class)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  facet_grid(class ~ ., scales = "fixed", space = "fixed") +
  scale_x_continuous(limits = c(0, 60), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 60), expand = c(0, 0)) +
  scale_color_manual(values = class_colors) +
  coord_fixed(ratio = 1) +
  theme_bw(base_size = 12) +
  theme(
    strip.text.y = element_blank(),        # removes facet labels
    panel.spacing = unit(1, "lines"),
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5)
  ) +
  labs(
    x = "Neotropical specialization",
    y = "Weighted specialization",
    
  )

# --- Combine side by side ---
combined_plot <- p1 | p2 


#svg file
svg("figures/whole_neo/neo_vs_weighted.svg",
    width = 14, height = 20)
combined_plot
dev.off()

#png file
png("figures/whole_neo/neo_vs_weighted.png", 
    width = 14, height = 20,
    units = "cm", pointsize = 8, res = 300)
combined_plot
dev.off()

######## ANOVA Kruskal-Wallis ############

#filtering repeated species names, the ones that occupy more than one biome
tetrapod_df <- tetrapod_unique %>% 
                 distinct(species, .keep_all = TRUE)

##neotropical marginality
neo.marg <- kruskal.test(neo_marginality ~ class,
                       data = tetrapod_df) ## it is significant


dunn.test(tetrapod_df$neo_marginality, tetrapod_df$class,
          method = "holm")

###plotting

#pivot longer for plotting
tetra_marg <- tetrapod_df %>% 
                  select(species, class, w.marginality, neo_marginality) %>% 
                    pivot_longer(cols = c(w.marginality, neo_marginality),
                           names_to = "type", values_to = "marginality")

#plot
box_marg <- ggplot(tetra_marg, aes(x = class, y = marginality, fill = class)) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~type, ncol = 1, labeller = labeller(type = c(
    "w.marginality" = "Biome-weighted marginality",
    "neo_marginality" = "Neotropical marginality"
  ))) +
  scale_fill_manual(values = class_colors) +
  ylim(0, 6) +
  theme_bw() +
  #mynamestheme +
  ylab("Marginality")+
  xlab(NULL)+
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "none",
    axis.title.y = element_text(margin = margin(r = 13)),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 14)
  )

#saving

#svg file
svg("figures/whole_neo/box_marg.svg",
    width = 12, height = 20)
box_marg
dev.off()

#png file
png("figures/whole_neo/box_marg.png", 
    width = 12, height = 20,
    units = "cm", pointsize = 8, res = 300)
box_marg
dev.off()


#######neotropical specialization
neo.spe.anova <- kruskal.test(neo_specialization ~ class,
                         data = tetrapod_df) ## it is significant


dunn.test(tetrapod_df$neo_specialization, tetrapod_df$class,
          method = "holm")

#pivot longer for plotting
tetra_spe <- tetrapod_df %>% 
  select(species, class, w.specialization, neo_specialization) %>% 
  pivot_longer(cols = c(w.specialization, neo_specialization),
               names_to = "type", values_to = "specialization")

#plot
box_spe <- ggplot(tetra_spe, aes(x = class, y = specialization, fill = class)) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~type, ncol = 1, labeller = labeller(type = c(
    "w.specialization" = "Biome-weighted specialization",
    "neo_specialization" = "Neotropical specialization"
  ))) +
  scale_fill_manual(values = class_colors) +
  ylim(0, 25) +
  theme_bw() +
  #mynamestheme +
  ylab("Specialization")+
  xlab(NULL)+
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "none",
    axis.title.y = element_text(margin = margin(r = 13)),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 14)
  )

#saving

#svg file
svg("figures/whole_neo/box_spe.svg",
    width = 12, height = 20)
box_spe
dev.off()

#png file
png("figures/whole_neo/box_spe.png", 
    width = 12, height = 20,
    units = "cm", pointsize = 8, res = 300)
box_spe
dev.off()



# PGLS results ------------------------------------------------------------

##############amphibians
amphi_files <- list.files("results/data/processed/whole_neotropic_pgls/amphibians")

setwd("results/data/processed/whole_neotropic_pgls/amphibians")

amphi_results <- do.call(rbind, lapply(amphi_files, read.csv))

amphi_results$class <- "Amphibians"

amphi_results <- amphi_results %>% rename(p_value = Pr...t..) #%>% 
#                 filter(term != "(Intercept)")


##summary
#marginality
summary_amphi_marg <- amphi_results %>% filter(biome == "neo_marginality") %>%
  group_by(ext) %>% 
  summarize(
    mean_B = mean(Estimate),
    median_B = median(Estimate),
    CI_lower_B = quantile(Estimate, 0.025),
    CI_upper_B = quantile(Estimate, 0.975),
    mean_p = mean(p_value),
    median_p = median(p_value),
    CI_lower_p = quantile(p_value, 0.025),
    CI_upper_p = quantile(p_value, 0.975),
    prop_significant = mean(p_value < 0.05),
    mean_R = mean(adj.r.squared),
    median_R = median(adj.r.squared),
    CI_lower_R = quantile(adj.r.squared, 0.025),
    CI_upper_R = quantile(adj.r.squared, 0.975),
    mean_lambda = mean(lambda),
    median_lambda = median(lambda),
    CI_lower_lambda = mean(lam_low),
    CI_upper_lambda = mean(lam_up))

summary_amphi_marg$class <- "amphibians"

summary_amphi_marg$enfa <- "marginality"


#specialization
summary_amphi_spe <- amphi_results %>% filter(biome == "neo_specialization") %>%
  group_by(ext) %>% 
  summarize(
    mean_B = mean(Estimate),
    median_B = median(Estimate),
    CI_lower_B = quantile(Estimate, 0.025),
    CI_upper_B = quantile(Estimate, 0.975),
    mean_p = mean(p_value),
    median_p = median(p_value),
    CI_lower_p = quantile(p_value, 0.025),
    CI_upper_p = quantile(p_value, 0.975),
    prop_significant = mean(p_value < 0.05),
    mean_R = mean(adj.r.squared),
    median_R = median(adj.r.squared),
    CI_lower_R = quantile(adj.r.squared, 0.025),
    CI_upper_R = quantile(adj.r.squared, 0.975),
    mean_lambda = mean(lambda),
    median_lambda = median(lambda),
    CI_lower_lambda = mean(lam_low),
    CI_upper_lambda = mean(lam_up))

summary_amphi_spe$class <- "amphibians"

summary_amphi_spe$enfa <- "specialization"



#reptiles
repti_files <- list.files("results/data/processed/whole_neotropic_pgls/reptiles")

setwd("results/data/processed/whole_neotropic_pgls/reptiles")

repti_results <- do.call(rbind, lapply(repti_files, read.csv))

repti_results$class <- "Reptiles"


repti_results <- repti_results %>% rename(p_value = Pr...t..) 
                                #%>% 
                            #filter(term != "(Intercept)")

##summary
#marginality
summary_repti_marg <- repti_results %>% filter(biome == "neo_marginality") %>%
  group_by(ext) %>% 
  summarize(
    mean_B = mean(Estimate),
    median_B = median(Estimate),
    CI_lower_B = quantile(Estimate, 0.025),
    CI_upper_B = quantile(Estimate, 0.975),
    mean_p = mean(p_value),
    median_p = median(p_value),
    CI_lower_p = quantile(p_value, 0.025),
    CI_upper_p = quantile(p_value, 0.975),
    prop_significant = mean(p_value < 0.05),
    mean_R = mean(adj.r.squared),
    median_R = median(adj.r.squared),
    CI_lower_R = quantile(adj.r.squared, 0.025),
    CI_upper_R = quantile(adj.r.squared, 0.975),
    mean_lambda = mean(lambda),
    median_lambda = median(lambda),
    CI_lower_lambda = mean(lam_low),
    CI_upper_lambda = mean(lam_up))

summary_repti_marg$class <- "reptiles"

summary_repti_marg$enfa <- "marginality"


#specialization
summary_repti_spe <- repti_results %>% filter(biome == "neo_specialization") %>%
  group_by(ext) %>% 
  summarize(
    mean_B = mean(Estimate),
    median_B = median(Estimate),
    CI_lower_B = quantile(Estimate, 0.025),
    CI_upper_B = quantile(Estimate, 0.975),
    mean_p = mean(p_value),
    median_p = median(p_value),
    CI_lower_p = quantile(p_value, 0.025),
    CI_upper_p = quantile(p_value, 0.975),
    prop_significant = mean(p_value < 0.05),
    mean_R = mean(adj.r.squared),
    median_R = median(adj.r.squared),
    CI_lower_R = quantile(adj.r.squared, 0.025),
    CI_upper_R = quantile(adj.r.squared, 0.975),
    mean_lambda = mean(lambda),
    median_lambda = median(lambda),
    CI_lower_lambda = mean(lam_low),
    CI_upper_lambda = mean(lam_up))

summary_repti_spe$class <- "reptiles"

summary_repti_spe$enfa <- "specialization"



#aves
aves_files <- list.files("results/data/processed/whole_neotropic_pgls/aves")

setwd("results/data/processed/whole_neotropic_pgls/aves")

aves_results <- do.call(rbind, lapply(aves_files, read.csv))

aves_results$class <- "aves"

aves_results <- aves_results %>% rename(p_value = Pr...t..) 
                                      #%>% 
                                 #filter(term != "(Intercept)")

##summary
#marginality
summary_aves_marg <- aves_results %>% filter(biome == "neo_marginality") %>%
  group_by(ext) %>% 
  summarize(
    mean_B = mean(Estimate),
    median_B = median(Estimate),
    CI_lower_B = quantile(Estimate, 0.025),
    CI_upper_B = quantile(Estimate, 0.975),
    mean_p = mean(p_value),
    median_p = median(p_value),
    CI_lower_p = quantile(p_value, 0.025),
    CI_upper_p = quantile(p_value, 0.975),
    prop_significant = mean(p_value < 0.05),
    mean_R = mean(adj.r.squared),
    median_R = median(adj.r.squared),
    CI_lower_R = quantile(adj.r.squared, 0.025),
    CI_upper_R = quantile(adj.r.squared, 0.975),
    mean_lambda = mean(lambda),
    median_lambda = median(lambda),
    CI_lower_lambda = mean(lam_low),
    CI_upper_lambda = mean(lam_up))

summary_aves_marg$class <- "aves"

summary_aves_marg$enfa <- "marginality"


#specialization
summary_aves_spe <- aves_results %>% filter(biome == "neo_specialization") %>%
  group_by(ext) %>% 
  summarize(
    mean_B = mean(Estimate),
    median_B = median(Estimate),
    CI_lower_B = quantile(Estimate, 0.025),
    CI_upper_B = quantile(Estimate, 0.975),
    mean_p = mean(p_value),
    median_p = median(p_value),
    CI_lower_p = quantile(p_value, 0.025),
    CI_upper_p = quantile(p_value, 0.975),
    prop_significant = mean(p_value < 0.05),
    mean_R = mean(adj.r.squared),
    median_R = median(adj.r.squared),
    CI_lower_R = quantile(adj.r.squared, 0.025),
    CI_upper_R = quantile(adj.r.squared, 0.975),
    mean_lambda = mean(lambda),
    median_lambda = median(lambda),
    CI_lower_lambda = mean(lam_low),
    CI_upper_lambda = mean(lam_up))

summary_aves_spe$class <- "aves"

summary_aves_spe$enfa <- "specialization"


#mammals
mammals_files <- list.files("results/data/processed/whole_neotropic_pgls/mammals")

setwd("results/data/processed/whole_neotropic_pgls/mammals")

mammals_results <- do.call(rbind, lapply(mammals_files, read.csv))

mammals_results$class <- "mammals"

mammals_results <- mammals_results %>% rename(p_value = Pr...t..) #%>% 
                        #filter(term != "(Intercept)")

##summary
#marginality
summary_mammals_marg <- mammals_results %>% filter(biome == "neo_marginality") %>%
  group_by(ext) %>% 
  summarize(
    mean_B = mean(Estimate),
    median_B = median(Estimate),
    CI_lower_B = quantile(Estimate, 0.025),
    CI_upper_B = quantile(Estimate, 0.975),
    mean_p = mean(p_value),
    median_p = median(p_value),
    CI_lower_p = quantile(p_value, 0.025),
    CI_upper_p = quantile(p_value, 0.975),
    prop_significant = mean(p_value < 0.05),
    mean_R = mean(adj.r.squared),
    median_R = median(adj.r.squared),
    CI_lower_R = quantile(adj.r.squared, 0.025),
    CI_upper_R = quantile(adj.r.squared, 0.975),
    mean_lambda = mean(lambda),
    median_lambda = median(lambda),
    CI_lower_lambda = mean(lam_low),
    CI_upper_lambda = mean(lam_up))

summary_mammals_marg$class <- "mammals"

summary_mammals_marg$enfa <- "marginality"


#specialization
summary_mammals_spe <- mammals_results %>% filter(biome == "neo_specialization") %>%
  group_by(ext) %>% 
  summarize(
    mean_B = mean(Estimate),
    median_B = median(Estimate),
    CI_lower_B = quantile(Estimate, 0.025),
    CI_upper_B = quantile(Estimate, 0.975),
    mean_p = mean(p_value),
    median_p = median(p_value),
    CI_lower_p = quantile(p_value, 0.025),
    CI_upper_p = quantile(p_value, 0.975),
    prop_significant = mean(p_value < 0.05),
    mean_R = mean(adj.r.squared),
    median_R = median(adj.r.squared),
    CI_lower_R = quantile(adj.r.squared, 0.025),
    CI_upper_R = quantile(adj.r.squared, 0.975),
    mean_lambda = mean(lambda),
    median_lambda = median(lambda),
    CI_lower_lambda = mean(lam_low),
    CI_upper_lambda = mean(lam_up))

summary_mammals_spe$class <- "mammals"

summary_mammals_spe$enfa <- "specialization"


# Confidence interval plots -----------------------------------------------

############# Neotropical marginality
summary_neo_marg <- rbind(summary_mammals_marg, summary_aves_marg,
                          summary_repti_marg, summary_amphi_marg)

summary_neo_marg$class <-  factor(summary_neo_marg$class, levels = c("amphibians",
                                                                 "reptiles",
                                                                 "aves",
                                                                 "mammals"),
                                  labels = c("Amphibians",
                                             "Reptiles",
                                             "Aves",
                                             "Mammals"),
                                ordered = TRUE)

############### neotropical specialization


summary_neo_spe <- rbind(summary_mammals_spe, summary_aves_spe,
                         summary_repti_spe, summary_amphi_spe)

summary_neo_spe$class <-  factor(summary_neo_spe$class, levels = c("amphibians",
                                                               "reptiles",
                                                               "aves",
                                                               "mammals"),
                               labels = c("Amphibians",
                                          "Reptiles",
                                          "Aves",
                                          "Mammals"),
                               ordered = TRUE)


##Binding summaries
sum_neo_tot <- rbind(summary_neo_marg, summary_neo_spe)

write_csv(sum_neo_tot, "results/data/processed/whole_neotropic_pgls/summary_pgls.csv")

#saving for building tables

#high extinction
sum_high <- sum_neo_tot %>% filter(ext == "high")
writexl::write_xlsx(sum_high, "results/data/processed/whole_neotropic_pgls/summary_high.xlsx")

#intermediate extinction
sum_int <- sum_neo_tot %>% filter(ext == "int")
writexl::write_xlsx(sum_int, "results/data/processed/whole_neotropic_pgls/summary_int.xlsx")

#low
sum_low <- sum_neo_tot %>% filter(ext == "low")
writexl::write_xlsx(sum_low, "results/data/processed/whole_neotropic_pgls/summary_low.xlsx")

########################## plots #####################

##defining colors for each class
class_colors <- c(
  "Mammals" = "#E66101",
  "Aves" = "#E69F00",   # Strong golden yellow
  
  "Reptiles" = "#018571", # Strong teal green
  "Amphibians" = "#7fbf7b"
)

##High extinction
high_CI <- sum_neo_tot %>% filter(ext == "high") %>% 
  ggplot(aes(y = class, color = class)) +
  geom_point(aes(x = median_B), size = 6, position = position_dodge(width = 0.3)) +
  geom_linerange(aes(xmin = CI_lower_B, xmax = CI_upper_B), linewidth = 1.8)+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = class_colors, 
                     guide = "none") +
  labs(x = "Beta", y = NULL, color = NULL) +
  xlim(-0.2, 0.2)+
  mynamestheme+
  theme(legend.position = "none",
        axis.text.y.left = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "gray98"),  # Light gray background
        panel.grid.major = element_line(color = "gray90"), # Subtle grid for reference
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(size = 15),  # Increase x-axis text size
        axis.title.x = element_text(size = 16))+  # Increase y-axis title size)+
  facet_wrap(~enfa, ncol = 2, labeller = as_labeller(c(
    marginality = "Neotropical marginality",
    specialization = "Neotropical specialization"
  )))

#saving as png
png("figures/whole_neo/high_neo_CI.png", 
    width = 25, height = 15,
    units = "cm", pointsize = 8, res = 300)

high_CI

dev.off()


#saving as svg

svg("figures/whole_neo/high_neo_CI.svg",
    width = 10, height = 5)
high_CI
dev.off()


###intermediate extinction
int_CI <- sum_neo_tot %>% filter(ext == "int") %>% 
  ggplot(aes(y = class, color = class)) +
  geom_point(aes(x = median_B), size = 6, position = position_dodge(width = 0.3)) +
  geom_linerange(aes(xmin = CI_lower_B, xmax = CI_upper_B), linewidth = 1.8)+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = class_colors, 
                     guide = "none") +
  labs(x = "Beta", y = NULL, color = NULL) +
  xlim(-0.2, 0.2)+
  mynamestheme+
  ggtitle("Intermediate extinction")+
  theme(legend.position = "none",
        axis.text.y.left = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "gray98"),  # Light gray background
        panel.grid.major = element_line(color = "gray90"), # Subtle grid for reference
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(size = 15),  # Increase x-axis text size
        axis.title.x = element_text(size = 16))+  # Increase y-axis title size)+
  facet_wrap(~enfa, ncol = 2, labeller = as_labeller(c(
    marginality = "Neotropical marginality",
    specialization = "Neotropical specialization"
  )))

#saving as png
png("figures/whole_neo/int_neo_CI.png", 
    width = 25, height = 15,
    units = "cm", pointsize = 8, res = 300)

int_CI

dev.off()


#saving as svg

svg("figures/whole_neo/int_neo_CI.svg",
    width = 10, height = 5)
int_CI
dev.off()

###low extinction

low_CI <- sum_neo_tot %>% filter(ext == "low") %>% 
  ggplot(aes(y = class, color = class)) +
  geom_point(aes(x = median_B), size = 6, position = position_dodge(width = 0.3)) +
  geom_linerange(aes(xmin = CI_lower_B, xmax = CI_upper_B), linewidth = 1.8)+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = class_colors, 
                     guide = "none") +
  labs(x = "Beta", y = NULL, color = NULL) +
  xlim(-0.2, 0.2)+
  mynamestheme+
  ggtitle("Low extinction")+
  theme(legend.position = "none",
        axis.text.y.left = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "gray98"),  # Light gray background
        panel.grid.major = element_line(color = "gray90"), # Subtle grid for reference
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(size = 15),  # Increase x-axis text size
        axis.title.x = element_text(size = 16))+  # Increase y-axis title size)+
  facet_wrap(~enfa, ncol = 2, labeller = as_labeller(c(
    marginality = "Neotropical marginality",
    specialization = "Neotropical specialization"
  )))

#saving as png
png("figures/whole_neo/low_neo_CI.png", 
    width = 25, height = 15,
    units = "cm", pointsize = 8, res = 300)

low_CI

dev.off()


#saving as svg

svg("figures/whole_neo/low_neo_CI.svg",
    width = 10, height = 5)
low_CI
dev.off()




###P values distribution

#general results
gen_results <- rbind(amphi_results, repti_results, aves_results, 
                     mammals_results) 


##saving
write_csv(gen_results, "results/data/processed/whole_neotropic_pgls/general_pgls.csv")

