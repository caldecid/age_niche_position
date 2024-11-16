
# Plotting biomes results -------------------------------------------------


##sourcing the libraries and the directories
source(file.path(getwd(), "/source.R"))

# marginality -------------------------------------------------------------

##calling dataframe
biome_marg_results <- read_csv("data/processed/biome_marg_results.csv")

#filtering
biome_marg_results <- biome_marg_results %>% rename("p_value" = "Pr(>|t|)")

#renaming
biome_marg_results$term <- factor(biome_marg_results$term, levels = c("(Intercept)",
                                                                      "log(low.age + 1)",
                                                                      "log(int.age + 1)",
                                                                      "log(high.age + 1)") ,
                                  labels = c("Intercept",
                                             "Age",
                                             "Age",
                                             "Age"))

sig_marg_res <- biome_marg_results %>% filter(term == "Age",
                                              p_value <= 0.05,
                                              adj.r.squared > 0)



##seven significant biomes
biomes_sig_marg <- unique(sig_marg_res$biome)

# specialization ----------------------------------------------------------

##calling dataframe
biome_spe_results <- read_csv("data/processed/biome_spe_results.csv")

biome_spe_results <- biome_spe_results %>% rename("p_value" = "Pr(>|t|)")

biome_spe_results$term <- factor(biome_spe_results$term, levels = c("(Intercept)",
                                                                    "log(low.age + 1)",
                                                                    "log(int.age + 1)",
                                                                    "log(high.age + 1)") ,
                                 labels = c("Intercept",
                                            "Age",
                                            "Age",
                                            "Age"))

sig_spe_res <- biome_spe_results %>% filter(term == "Age",
                                            p_value <= 0.05,
                                            adj.r.squared > 0)

##seven biomes with significant results
biomes_sig_spe <- unique(sig_spe_res$biome) 



# Marginality -------------------------------------------------------------




# Birds -------------------------------------------------------------------

####################high extinction#############################

biomes_sig_marg_birds <- sig_marg_res %>% filter(class == "AVES") %>%
                                             pull(biome) %>% unique()


##pivoting
vert_pivot_birds <- vert_enfa_ages %>% filter(className == "AVES",
                                        biome %in% biomes_sig_marg_birds) %>% 
  pivot_longer(cols = ends_with(".age"),
               names_to = "ext_age",
               values_to = "Ages") %>% 
  filter(ext_age != "Estimated.age")

##factors
vert_pivot_birds$ext_age <- factor(vert_pivot_birds$ext_age, 
                             levels = c("low.age",
                                        "int.age",
                                        "high.age"),
                             ordered = TRUE)

vert_pivot_birds$className <- factor(vert_pivot_birds$className,
                               levels = c("AVES"),
                               labels = c("Birds"
                                          ),
                               ordered = TRUE)
vert_pivot_birds$biome <- factor(vert_pivot_birds$biome,
                           levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                      "Tropical_&_subtropical_dry_broadleaf_forests",
                                      "Tropical_&_subtropical_coniferous_forests",
                                      "Tropical_&_subtropical_grasslands_savannas_and_shrublands",
                                      "Deserts_and_xeric_shrublands"),
                           labels = c("Tropical & subtropical moist broadleaf forests",
                                      "Tropical & subtropical dry broadleaf forests",
                                      "Tropical & subtropical coniferous forests",
                                      "Tropical & subtropical grasslands savannas and shrublands",
                                      "Deserts and xeric shrublands"),
                           ordered = TRUE)


## df elements of models
ele_marg_birds <- biome_marg_results %>% select(term, Estimate, class, ext, biome) %>% 
  filter(class == "AVES",
         biome %in% biomes_sig_marg_birds) %>% 
  pivot_wider(names_from = "term",
              values_from=  "Estimate") %>% 
  rename(className = class)

##changing factors name for matching with vert_enfa
ele_marg_birds$className <- factor(ele_marg_birds$className, levels = c("AVES"),
                                   labels = c("Birds"))

##High extinciton scenario
ele_marg_birds_high <- ele_marg_birds %>% filter(ext == "high")

##Biomes
ele_marg_birds_high$biome <- factor(ele_marg_birds_high$biome,
                                   levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                              "Tropical_&_subtropical_dry_broadleaf_forests",
                                              "Tropical_&_subtropical_coniferous_forests",
                                              "Tropical_&_subtropical_grasslands_savannas_and_shrublands",
                                              "Deserts_and_xeric_shrublands"),
                                   labels = c("Tropical & subtropical moist broadleaf forests",
                                              "Tropical & subtropical dry broadleaf forests",
                                              "Tropical & subtropical coniferous forests",
                                              "Tropical & subtropical grasslands savannas and shrublands",
                                              "Deserts and xeric shrublands"),
                                   ordered = TRUE)

png("text/figures/marginality/biomes/bird_model.png",
    width = 50, height = 10, units = "cm", 
    pointsize = 8, res = 300)


plot.birds.high.marg <- vert_pivot_birds %>% filter(ext_age == "high.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(marginality+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Marginality")+
  facet_grid(className~biome, scales = "free", switch = "y")+
  geom_abline(data = ele_marg_birds_high, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle(NULL)+
  scale_y_continuous(position = "right")+
  theme_bw() +
  mynamestheme+
  theme(legend.position = "none",
        strip.text.y = element_text(angle= 90, size = 12,
                                    face = "bold"))
        

dev.off()


#################################Intermediate extinction ######################


biomes_sig_marg_birds <- sig_marg_res %>% filter(class == "AVES",
                                                 ext == "int") %>%
                                        pull(biome) %>% unique()


##pivoting
vert_pivot_birds <- vert_enfa_ages %>% filter(className == "AVES",
                                              biome %in% biomes_sig_marg_birds) %>% 
  pivot_longer(cols = ends_with(".age"),
               names_to = "ext_age",
               values_to = "Ages") %>% 
  filter(ext_age != "Estimated.age")

##factors
vert_pivot_birds$ext_age <- factor(vert_pivot_birds$ext_age, 
                                   levels = c("low.age",
                                              "int.age",
                                              "high.age"),
                                   ordered = TRUE)

vert_pivot_birds$className <- factor(vert_pivot_birds$className,
                                     levels = c("AVES"),
                                     labels = c("Birds"
                                     ),
                                     ordered = TRUE)
vert_pivot_birds$biome <- factor(vert_pivot_birds$biome,
                                 levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                            "Tropical_&_subtropical_dry_broadleaf_forests",
                                            "Tropical_&_subtropical_coniferous_forests",
                                            "Tropical_&_subtropical_grasslands_savannas_and_shrublands",
                                            "Deserts_and_xeric_shrublands"),
                                 labels = c("Tropical & subtropical moist broadleaf forests",
                                            "Tropical & subtropical dry broadleaf forests",
                                            "Tropical & subtropical coniferous forests",
                                            "Tropical & subtropical grasslands savannas and shrublands",
                                            "Deserts and xeric shrublands"),
                                 ordered = TRUE)


## df elements of models
ele_marg_birds <- biome_marg_results %>% select(term, Estimate, class, ext, biome) %>% 
  filter(class == "AVES",
         biome %in% biomes_sig_marg_birds) %>% 
  pivot_wider(names_from = "term",
              values_from=  "Estimate") %>% 
  rename(className = class)

##changing factors name for matching with vert_enfa
ele_marg_birds$className <- factor(ele_marg_birds$className, levels = c("AVES"),
                                   labels = c("Birds"))

##Int extinction scenario
ele_marg_birds_int <- ele_marg_birds %>% filter(ext == "int")

##Biomes
ele_marg_birds_int$biome <- factor(ele_marg_birds_int$biome,
                                    levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                               "Tropical_&_subtropical_dry_broadleaf_forests",
                                               "Tropical_&_subtropical_coniferous_forests",
                                               "Tropical_&_subtropical_grasslands_savannas_and_shrublands",
                                               "Deserts_and_xeric_shrublands"),
                                    labels = c("Tropical & subtropical moist broadleaf forests",
                                               "Tropical & subtropical dry broadleaf forests",
                                               "Tropical & subtropical coniferous forests",
                                               "Tropical & subtropical grasslands savannas and shrublands",
                                               "Deserts and xeric shrublands"),
                                    ordered = TRUE)

png("text/figures/marginality/biomes/bird_model_int.png",
    width = 50, height = 10, units = "cm", 
    pointsize = 8, res = 300)


plot.birds.int.marg <- vert_pivot_birds %>% filter(ext_age == "int.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(marginality+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Marginality")+
  facet_grid(className~biome, scales = "free", switch = "y")+
  geom_abline(data = ele_marg_birds_int, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle(NULL)+
  scale_y_continuous(position = "right")+
  theme_bw() +
  mynamestheme+
  theme(legend.position = "none",
        strip.text.y = element_text(angle= 90, size = 12,
                                    face = "bold"))


dev.off()

#############Low extinction###########################

biomes_sig_marg_birds <- sig_marg_res %>% filter(class == "AVES") %>%
  pull(biome) %>% unique()


##pivoting
vert_pivot_birds <- vert_enfa_ages %>% filter(className == "AVES",
                                              biome %in% biomes_sig_marg_birds) %>% 
  pivot_longer(cols = ends_with(".age"),
               names_to = "ext_age",
               values_to = "Ages") %>% 
  filter(ext_age != "Estimated.age")

##factors
vert_pivot_birds$ext_age <- factor(vert_pivot_birds$ext_age, 
                                   levels = c("low.age",
                                              "int.age",
                                              "high.age"),
                                   ordered = TRUE)

vert_pivot_birds$className <- factor(vert_pivot_birds$className,
                                     levels = c("AVES"),
                                     labels = c("Birds"
                                     ),
                                     ordered = TRUE)
vert_pivot_birds$biome <- factor(vert_pivot_birds$biome,
                                 levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                            "Tropical_&_subtropical_dry_broadleaf_forests",
                                            "Tropical_&_subtropical_coniferous_forests",
                                            "Tropical_&_subtropical_grasslands_savannas_and_shrublands",
                                            "Deserts_and_xeric_shrublands"),
                                 labels = c("Tropical & subtropical moist broadleaf forests",
                                            "Tropical & subtropical dry broadleaf forests",
                                            "Tropical & subtropical coniferous forests",
                                            "Tropical & subtropical grasslands savannas and shrublands",
                                            "Deserts and xeric shrublands"),
                                 ordered = TRUE)


## df elements of models
ele_marg_birds <- biome_marg_results %>% select(term, Estimate, class, ext, biome) %>% 
  filter(class == "AVES",
         biome %in% biomes_sig_marg_birds) %>% 
  pivot_wider(names_from = "term",
              values_from=  "Estimate") %>% 
  rename(className = class)

##changing factors name for matching with vert_enfa
ele_marg_birds$className <- factor(ele_marg_birds$className, levels = c("AVES"),
                                   labels = c("Birds"))

##low extinciton scenario
ele_marg_birds_low <- ele_marg_birds %>% filter(ext == "low")

##Biomes
ele_marg_birds_low$biome <- factor(ele_marg_birds_low$biome,
                                    levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                               "Tropical_&_subtropical_dry_broadleaf_forests",
                                               "Tropical_&_subtropical_coniferous_forests",
                                               "Tropical_&_subtropical_grasslands_savannas_and_shrublands",
                                               "Deserts_and_xeric_shrublands"),
                                    labels = c("Tropical & subtropical moist broadleaf forests",
                                               "Tropical & subtropical dry broadleaf forests",
                                               "Tropical & subtropical coniferous forests",
                                               "Tropical & subtropical grasslands savannas and shrublands",
                                               "Deserts and xeric shrublands"),
                                    ordered = TRUE)

png("text/figures/marginality/biomes/bird_model_low.png",
    width = 50, height = 10, units = "cm", 
    pointsize = 8, res = 300)


plot.birds.low.marg <- vert_pivot_birds %>% filter(ext_age == "low.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(marginality+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Marginality")+
  facet_grid(className~biome, scales = "free", switch = "y")+
  geom_abline(data = ele_marg_birds_low, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle(NULL)+
  scale_y_continuous(position = "right")+
  theme_bw() +
  mynamestheme+
  theme(legend.position = "none",
        strip.text.y = element_text(angle= 90, size = 12,
                                    face = "bold"))


dev.off()




# Reptiles ----------------------------------------------------------------


#########High extinction###########################
biomes_sig_marg_rep <- sig_marg_res %>% filter(class == "REPTILIA") %>%
                                              pull(biome) %>% unique()


##pivoting
vert_pivot_reptiles <- vert_enfa_ages %>% filter(className == "REPTILIA",
                                        biome %in% biomes_sig_marg_rep) %>% 
  pivot_longer(cols = ends_with(".age"),
               names_to = "ext_age",
               values_to = "Ages") %>% 
  filter(ext_age != "Estimated.age")

##factors
vert_pivot_reptiles$ext_age <- factor(vert_pivot_reptiles$ext_age, 
                             levels = c("low.age",
                                        "int.age",
                                        "high.age"),
                             ordered = TRUE)

vert_pivot_reptiles$className <- factor(vert_pivot_reptiles$className,
                               levels = c("REPTILIA"),
                               labels = c("Reptiles"
                               ),
                               ordered = TRUE)
vert_pivot_reptiles$biome <- factor(vert_pivot_reptiles$biome,
                           levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                      "Tropical_&_subtropical_dry_broadleaf_forests"),
                           labels = c("Tropical & subtropical moist broadleaf forests",
                                      "Tropical & subtropical dry broadleaf forests"),
                           ordered = TRUE)


## df elements of models
ele_marg_reptiles <- biome_marg_results %>% select(term, Estimate, class, ext, biome) %>% 
  filter(class == "REPTILIA",
         biome %in% biomes_sig_marg_rep) %>% 
  pivot_wider(names_from = "term",
              values_from=  "Estimate") %>% 
  rename(className = class)

##changing factors name for matching with vert_enfa
ele_marg_reptiles$className <- factor(ele_marg_reptiles$className, levels = c("REPTILIA"),
                                   labels = c("Reptiles"))

##High extinciton scenario
ele_marg_reptiles_high <- ele_marg_reptiles %>% filter(ext == "high")

##Biomes
ele_marg_reptiles_high$biome <- factor(ele_marg_reptiles_high$biome,
                                   levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                              "Tropical_&_subtropical_dry_broadleaf_forests"),
                                   labels = c("Tropical & subtropical moist broadleaf forests",
                                              "Tropical & subtropical dry broadleaf forests"),
                                   ordered = TRUE)


png("text/figures/marginality/biomes/reptile_model.png",
    width = 20, height = 10, units = "cm", 
    pointsize = 8, res = 300)


plot.reptiles.high.marg <- vert_pivot_reptiles %>% filter(ext_age == "high.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(marginality+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Marginality")+
  facet_grid(className~biome, scales = "free", switch = "y")+
  geom_abline(data = ele_marg_reptiles_high, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle(NULL)+
  scale_y_continuous(position = "right")+
  theme_bw() +
  mynamestheme+
  theme(legend.position = "none",
        strip.text.y = element_text(angle= 90, size = 12,
                                    face = "bold"))


dev.off()



#############################intermediate extinction#########################
biomes_sig_marg_rep <- sig_marg_res %>% filter(class == "REPTILIA",
                                               ext == "int") %>%
  pull(biome) %>% unique()


##pivoting
vert_pivot_reptiles <- vert_enfa_ages %>% filter(className == "REPTILIA",
                                                 biome %in% biomes_sig_marg_rep) %>% 
  pivot_longer(cols = ends_with(".age"),
               names_to = "ext_age",
               values_to = "Ages") %>% 
  filter(ext_age != "Estimated.age")

##factors
vert_pivot_reptiles$ext_age <- factor(vert_pivot_reptiles$ext_age, 
                                      levels = c("low.age",
                                                 "int.age",
                                                 "high.age"),
                                      ordered = TRUE)

vert_pivot_reptiles$className <- factor(vert_pivot_reptiles$className,
                                        levels = c("REPTILIA"),
                                        labels = c("Reptiles"
                                        ),
                                        ordered = TRUE)
vert_pivot_reptiles$biome <- factor(vert_pivot_reptiles$biome,
                                    levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                               "Tropical_&_subtropical_dry_broadleaf_forests"),
                                    labels = c("Tropical & subtropical moist broadleaf forests",
                                               "Tropical & subtropical dry broadleaf forests"),
                                    ordered = TRUE)


## df elements of models
ele_marg_reptiles <- biome_marg_results %>% select(term, Estimate, class, ext, biome) %>% 
  filter(class == "REPTILIA",
         biome %in% biomes_sig_marg_rep) %>% 
  pivot_wider(names_from = "term",
              values_from=  "Estimate") %>% 
  rename(className = class)

##changing factors name for matching with vert_enfa
ele_marg_reptiles$className <- factor(ele_marg_reptiles$className, levels = c("REPTILIA"),
                                      labels = c("Reptiles"))

##int extinciton scenario
ele_marg_reptiles_int <- ele_marg_reptiles %>% filter(ext == "int")

##Biomes
ele_marg_reptiles_int$biome <- factor(ele_marg_reptiles_int$biome,
                                       levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                                  "Tropical_&_subtropical_dry_broadleaf_forests"),
                                       labels = c("Tropical & subtropical moist broadleaf forests",
                                                  "Tropical & subtropical dry broadleaf forests"),
                                       ordered = TRUE)


png("text/figures/marginality/biomes/reptile_model_int.png",
    width = 20, height = 10, units = "cm", 
    pointsize = 8, res = 300)


plot.reptiles.int.marg <- vert_pivot_reptiles %>% filter(ext_age == "int.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(marginality+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Marginality")+
  facet_grid(className~biome, scales = "free", switch = "y")+
  geom_abline(data = ele_marg_reptiles_int, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle(NULL)+
  scale_y_continuous(position = "right")+
  theme_bw() +
  mynamestheme+
  theme(legend.position = "none",
        strip.text.y = element_text(angle= 90, size = 12,
                                    face = "bold"))


dev.off()

####################Low extinction###################################
biomes_sig_marg_rep <- sig_marg_res %>% filter(class == "REPTILIA",
                                               ext == "low") %>%
  pull(biome) %>% unique()


##pivoting
vert_pivot_reptiles <- vert_enfa_ages %>% filter(className == "REPTILIA",
                                                 biome %in% biomes_sig_marg_rep) %>% 
  pivot_longer(cols = ends_with(".age"),
               names_to = "ext_age",
               values_to = "Ages") %>% 
  filter(ext_age != "Estimated.age")

##factors
vert_pivot_reptiles$ext_age <- factor(vert_pivot_reptiles$ext_age, 
                                      levels = c("low.age",
                                                 "int.age",
                                                 "high.age"),
                                      ordered = TRUE)

vert_pivot_reptiles$className <- factor(vert_pivot_reptiles$className,
                                        levels = c("REPTILIA"),
                                        labels = c("Reptiles"
                                        ),
                                        ordered = TRUE)
vert_pivot_reptiles$biome <- factor(vert_pivot_reptiles$biome,
                                    levels = c("Tropical_&_subtropical_moist_broadleaf_forests"),
                                    labels = c("Tropical & subtropical moist broadleaf forests"),
                                    ordered = TRUE)


## df elements of models
ele_marg_reptiles <- biome_marg_results %>% select(term, Estimate, class, ext, biome) %>% 
  filter(class == "REPTILIA",
         biome %in% biomes_sig_marg_rep) %>% 
  pivot_wider(names_from = "term",
              values_from=  "Estimate") %>% 
  rename(className = class)

##changing factors name for matching with vert_enfa
ele_marg_reptiles$className <- factor(ele_marg_reptiles$className, levels = c("REPTILIA"),
                                      labels = c("Reptiles"))

##low extinciton scenario
ele_marg_reptiles_low <- ele_marg_reptiles %>% filter(ext == "low")

##Biomes
ele_marg_reptiles_low$biome <- factor(ele_marg_reptiles_low$biome,
                                       levels = c("Tropical_&_subtropical_moist_broadleaf_forests"
                                                  ),
                                       labels = c("Tropical & subtropical moist broadleaf forests"),
                                       ordered = TRUE)


png("text/figures/marginality/biomes/reptile_model_low.png",
    width = 10, height = 10, units = "cm", 
    pointsize = 8, res = 300)


plot.reptiles.low.marg <- vert_pivot_reptiles %>% filter(ext_age == "low.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(marginality+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Marginality")+
  facet_grid(className~biome, scales = "free", switch = "y")+
  geom_abline(data = ele_marg_reptiles_low, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle(NULL)+
  scale_y_continuous(position = "right")+
  theme_bw() +
  mynamestheme+
  theme(legend.position = "none",
        strip.text.y = element_text(angle= 90, size = 12,
                                    face = "bold"))


dev.off()




# amphibians --------------------------------------------------------------


#########high extinction#########################################
biomes_sig_marg_amphibians <- sig_marg_res %>%
                     filter(class == "AMPHIBIA") %>% pull(biome) %>% unique()


##pivoting
vert_pivot_amphibians <- vert_enfa_ages %>% filter(className == "AMPHIBIA",
                                                 biome %in% biomes_sig_marg_amphibians) %>% 
  pivot_longer(cols = ends_with(".age"),
               names_to = "ext_age",
               values_to = "Ages") %>% 
  filter(ext_age != "Estimated.age")

##factors
vert_pivot_amphibians$ext_age <- factor(vert_pivot_amphibians$ext_age, 
                                      levels = c("low.age",
                                                 "int.age",
                                                 "high.age"),
                                      ordered = TRUE)

vert_pivot_amphibians$className <- factor(vert_pivot_amphibians$className,
                                        levels = c("AMPHIBIA"),
                                        labels = c("Amphibians"
                                        ),
                                        ordered = TRUE)
vert_pivot_amphibians$biome <- factor(vert_pivot_amphibians$biome,
                                    levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                               "Temperate_grasslands_savannas_and_ shrublands",
                                               "Mediterranean_forests_woodlands_and_scrub"),
                                    labels = c("Tropical & subtropical moist broadleaf forests",
                                               "Temperate grasslands savannas and shrublands",
                                               "Mediterranean forests woodlands and scrub"),
                                    ordered = TRUE)


## df elements of models
ele_marg_amphibians <- biome_marg_results %>% select(term, Estimate, class, ext, biome) %>% 
  filter(class == "AMPHIBIA",
         biome %in% biomes_sig_marg_amphibians) %>% 
  pivot_wider(names_from = "term",
              values_from=  "Estimate") %>% 
  rename(className = class)

##changing factors name for matching with vert_enfa
ele_marg_amphibians$className <- factor(ele_marg_amphibians$className, levels = c("AMPHIBIA"),
                                      labels = c("Amphibians"))

##High extinciton scenario
ele_marg_amphibians_high <- ele_marg_amphibians %>% filter(ext == "high")

##Biomes
ele_marg_amphibians_high$biome <- factor(ele_marg_amphibians_high$biome,
                                         levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                                    "Temperate_grasslands_savannas_and_ shrublands",
                                                    "Mediterranean_forests_woodlands_and_scrub"),
                                         labels = c("Tropical & subtropical moist broadleaf forests",
                                                    "Temperate grasslands savannas and shrublands",
                                                    "Mediterranean forests woodlands and scrub" ),
                                         ordered = TRUE)


png("text/figures/marginality/biomes/amphibians_model.png",
    width = 30, height = 10, units = "cm", 
    pointsize = 8, res = 300)


plot.amphibians.high.marg <- vert_pivot_amphibians %>% filter(ext_age == "high.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(marginality+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Marginality")+
  facet_grid(className~biome, scales = "free", switch = "y")+
  geom_abline(data = ele_marg_amphibians_high, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle(NULL)+
  scale_y_continuous(position = "right")+
  theme_bw() +
  mynamestheme+
  theme(legend.position = "none",
        strip.text.y = element_text(angle=0, size = 12,
                                    face = "bold"))

dev.off()

############intermediate extinction########################
biomes_sig_marg_amphibians <- sig_marg_res %>%
  filter(class == "AMPHIBIA") %>% pull(biome) %>% unique()


##pivoting
vert_pivot_amphibians <- vert_enfa_ages %>% filter(className == "AMPHIBIA",
                                                   biome %in% biomes_sig_marg_amphibians) %>% 
  pivot_longer(cols = ends_with(".age"),
               names_to = "ext_age",
               values_to = "Ages") %>% 
  filter(ext_age != "Estimated.age")

##factors
vert_pivot_amphibians$ext_age <- factor(vert_pivot_amphibians$ext_age, 
                                        levels = c("low.age",
                                                   "int.age",
                                                   "high.age"),
                                        ordered = TRUE)

vert_pivot_amphibians$className <- factor(vert_pivot_amphibians$className,
                                          levels = c("AMPHIBIA"),
                                          labels = c("Amphibians"
                                          ),
                                          ordered = TRUE)
vert_pivot_amphibians$biome <- factor(vert_pivot_amphibians$biome,
                                      levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                                 "Temperate_grasslands_savannas_and_ shrublands",
                                                 "Mediterranean_forests_woodlands_and_scrub"),
                                      labels = c("Tropical & subtropical moist broadleaf forests",
                                                 "Temperate grasslands savannas and shrublands",
                                                 "Mediterranean forests woodlands and scrub"),
                                      ordered = TRUE)


## df elements of models
ele_marg_amphibians <- biome_marg_results %>% select(term, Estimate, class, ext, biome) %>% 
  filter(class == "AMPHIBIA",
         biome %in% biomes_sig_marg_amphibians) %>% 
  pivot_wider(names_from = "term",
              values_from=  "Estimate") %>% 
  rename(className = class)

##changing factors name for matching with vert_enfa
ele_marg_amphibians$className <- factor(ele_marg_amphibians$className, levels = c("AMPHIBIA"),
                                        labels = c("Amphibians"))

##int extinciton scenario
ele_marg_amphibians_int <- ele_marg_amphibians %>% filter(ext == "int")

##Biomes
ele_marg_amphibians_int$biome <- factor(ele_marg_amphibians_int$biome,
                                         levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                                    "Temperate_grasslands_savannas_and_ shrublands",
                                                    "Mediterranean_forests_woodlands_and_scrub"),
                                         labels = c("Tropical & subtropical moist broadleaf forests",
                                                    "Temperate grasslands savannas and shrublands",
                                                    "Mediterranean forests woodlands and scrub" ),
                                         ordered = TRUE)


png("text/figures/marginality/biomes/amphibians_model_int.png",
    width = 30, height = 10, units = "cm", 
    pointsize = 8, res = 300)


plot.amphibians.int.marg <- vert_pivot_amphibians %>% filter(ext_age == "int.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(marginality+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Marginality")+
  facet_grid(className~biome, scales = "free", switch = "y")+
  geom_abline(data = ele_marg_amphibians_int, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle(NULL)+
  scale_y_continuous(position = "right")+
  theme_bw() +
  mynamestheme+
  theme(legend.position = "none",
        strip.text.y = element_text(angle=0, size = 12,
                                    face = "bold"))

dev.off()


#################Low extinction###################

biomes_sig_marg_amphibians <- sig_marg_res %>%
  filter(class == "AMPHIBIA") %>% pull(biome) %>% unique()


##pivoting
vert_pivot_amphibians <- vert_enfa_ages %>% filter(className == "AMPHIBIA",
                                                   biome %in% biomes_sig_marg_amphibians) %>% 
  pivot_longer(cols = ends_with(".age"),
               names_to = "ext_age",
               values_to = "Ages") %>% 
  filter(ext_age != "Estimated.age")

##factors
vert_pivot_amphibians$ext_age <- factor(vert_pivot_amphibians$ext_age, 
                                        levels = c("low.age",
                                                   "int.age",
                                                   "high.age"),
                                        ordered = TRUE)

vert_pivot_amphibians$className <- factor(vert_pivot_amphibians$className,
                                          levels = c("AMPHIBIA"),
                                          labels = c("Amphibians"
                                          ),
                                          ordered = TRUE)
vert_pivot_amphibians$biome <- factor(vert_pivot_amphibians$biome,
                                      levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                                 "Temperate_grasslands_savannas_and_ shrublands",
                                                 "Mediterranean_forests_woodlands_and_scrub"),
                                      labels = c("Tropical & subtropical moist broadleaf forests",
                                                 "Temperate grasslands savannas and shrublands",
                                                 "Mediterranean forests woodlands and scrub"),
                                      ordered = TRUE)


## df elements of models
ele_marg_amphibians <- biome_marg_results %>% select(term, Estimate, class, ext, biome) %>% 
  filter(class == "AMPHIBIA",
         biome %in% biomes_sig_marg_amphibians) %>% 
  pivot_wider(names_from = "term",
              values_from=  "Estimate") %>% 
  rename(className = class)

##changing factors name for matching with vert_enfa
ele_marg_amphibians$className <- factor(ele_marg_amphibians$className, levels = c("AMPHIBIA"),
                                        labels = c("Amphibians"))

##low extinciton scenario
ele_marg_amphibians_low <- ele_marg_amphibians %>% filter(ext == "low")

##Biomes
ele_marg_amphibians_low$biome <- factor(ele_marg_amphibians_low$biome,
                                         levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                                    "Temperate_grasslands_savannas_and_ shrublands",
                                                    "Mediterranean_forests_woodlands_and_scrub"),
                                         labels = c("Tropical & subtropical moist broadleaf forests",
                                                    "Temperate grasslands savannas and shrublands",
                                                    "Mediterranean forests woodlands and scrub" ),
                                         ordered = TRUE)


png("text/figures/marginality/biomes/amphibians_model_low.png",
    width = 30, height = 10, units = "cm", 
    pointsize = 8, res = 300)


plot.amphibians.low.marg <- vert_pivot_amphibians %>% filter(ext_age == "low.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(marginality+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Marginality")+
  facet_grid(className~biome, scales = "free", switch = "y")+
  geom_abline(data = ele_marg_amphibians_low, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle(NULL)+
  scale_y_continuous(position = "right")+
  theme_bw() +
  mynamestheme+
  theme(legend.position = "none",
        strip.text.y = element_text(angle=0, size = 12,
                                    face = "bold"))

dev.off()

# beta --------------------------------------------------------------------

#############high extinction#########################

png("text/figures/marginality/biomes/beta_high.png",
    width = 10, height = 10, units = "cm", 
    pointsize = 8, res = 300)

 beta_marg %>% filter(ext == "high") %>%
  ggplot(aes(x = Estimate, fill = class ))+
  geom_rect(aes(xmin = -Inf,
                xmax = 0, 
                ymin = -Inf,
                ymax = +Inf),
            fill = "#f0f0f0",
            color = "#f0f0f0")+
  geom_vline(xintercept = 0, color = "black", linewidth = 1,
             linetype = "dashed")+
  geom_histogram(color = "black", bins = 20) +
  scale_fill_manual(name = NULL,
                    labels = c("Birds", "Reptiles", "Amphibians"),
                    values = c(
                      "#fdae61",
                      "#008837",
                      "#a6dba0"))+
  xlim(-0.5,0.5)+
  xlab("Beta")+
  ylab("Count")+
  theme_classic()+
  #scale_y_continuous(expand = c(0,0))+
  mynamestheme




dev.off()

###########intermediate extinction#################
png("text/figures/marginality/biomes/beta_int.png",
    width = 10, height = 10, units = "cm", 
    pointsize = 8, res = 300)

beta_marg %>% filter(ext == "int") %>%
  ggplot(aes(x = Estimate, fill = class ))+
  geom_rect(aes(xmin = -Inf,
                xmax = 0, 
                ymin = -Inf,
                ymax = +Inf),
            fill = "#f0f0f0",
            color = "#f0f0f0")+
  geom_vline(xintercept = 0, color = "black", linewidth = 1,
             linetype = "dashed")+
  geom_histogram(color = "black", bins = 20) +
  scale_fill_manual(name = NULL,
                    labels = c( "Birds", "Reptiles", "Amphibians"),
                    values = c(
                      "#fdae61",
                      "#008837",
                      "#a6dba0"))+
  xlim(-0.5, 0.5)+
  xlab("Beta")+
  ylab("Count")+
  theme_classic()+
  #scale_y_continuous(expand = c(0,0))+
  mynamestheme


dev.off()

###low extinction
png("text/figures/marginality/biomes/beta_low.png",
    width = 10, height = 10, units = "cm", 
    pointsize = 8, res = 300)

beta_marg %>% filter(ext == "low") %>%
  ggplot(aes(x = Estimate, fill = class ))+
  geom_rect(aes(xmin = -Inf,
                xmax = 0, 
                ymin = -Inf,
                ymax = +Inf),
            fill = "#f0f0f0",
            color = "#f0f0f0")+
  geom_vline(xintercept = 0, color = "black", linewidth = 1,
             linetype = "dashed")+
  geom_histogram(color = "black", bins = 20) +
  scale_fill_manual(name = NULL,
                    labels = c( "Birds", "Reptiles", "Amphibians"),
                    values = c(
                      "#fdae61",
                      "#008837",
                      "#a6dba0"))+
  xlim(-0.5,0.5)+
  xlab("Beta")+
  ylab("Count")+
  theme_classic()+
  #scale_y_continuous(expand = c(0,0))+
  mynamestheme


dev.off()


# specialization ----------------------------------------------------------


# mammals -------------------------------------------------------------------

###########high extinction##############

biomes_sig_spe_mammals <- sig_spe_res %>% filter(class == "MAMMALIA") %>% pull(biome) %>% unique()


##pivoting
vert_pivot_mammals <- vert_enfa_ages %>% filter(className == "MAMMALIA",
                                              biome %in% biomes_sig_spe_mammals) %>% 
  pivot_longer(cols = ends_with(".age"),
               names_to = "ext_age",
               values_to = "Ages") %>% 
  filter(ext_age != "Estimated.age")

##factors
vert_pivot_mammals$ext_age <- factor(vert_pivot_mammals$ext_age, 
                                   levels = c("low.age",
                                              "int.age",
                                              "high.age"),
                                   ordered = TRUE)

vert_pivot_mammals$className <- factor(vert_pivot_mammals$className,
                                     levels = c("MAMMALIA"),
                                     labels = c("Mammals"
                                     ),
                                     ordered = TRUE)

vert_pivot_mammals$biome <- factor(vert_pivot_mammals$biome,
                                 levels = c("Tropical_&_subtropical_moist_broadleaf_forests"),
                                 labels = c("Tropical & subtropical moist broadleaf forests"),
                                 ordered = TRUE)


## df elements of models
ele_spe_mammals <- biome_spe_results %>% select(term, Estimate, class, ext, biome) %>% 
  filter(class == "MAMMALIA",
         biome %in% biomes_sig_spe_mammals) %>% 
  pivot_wider(names_from = "term",
              values_from=  "Estimate") %>% 
  rename(className = class)

##changing factors name for matching with vert_enfa
ele_spe_mammals$className <- factor(ele_spe_mammals$className, levels = c("MAMMALIA"),
                                   labels = c("Mammals"))

##High extinciton scenario
ele_spe_mammals_high <- ele_spe_mammals %>% filter(ext == "high")

##Biomes
ele_spe_mammals_high$biome <- factor(ele_spe_mammals_high$biome,
                                    levels = c("Tropical_&_subtropical_moist_broadleaf_forests"),
                                    labels = c("Tropical & subtropical moist broadleaf forests"),
                                    ordered = TRUE)

png("text/figures/specialization/biomes/mammals_model_high.png",
    width = 10, height = 10, units = "cm", 
    pointsize = 8, res = 300)


plot.mammals.high.spe <- vert_pivot_mammals %>% filter(ext_age == "high.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(specialization+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Specialization")+
  facet_grid(className~biome, scales = "free", switch = "y")+
  geom_abline(data = ele_spe_mammals_high, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle(NULL)+
  scale_y_continuous(position = "right")+
  theme_bw() +
  mynamestheme+
  theme(legend.position = "none",
        strip.text.y = element_text(angle= 90, size = 14,
                                    face = "bold"))


dev.off()


######################Intermediate########################
###########high extinction##############

biomes_sig_spe_mammals <- sig_spe_res %>% filter(class == "MAMMALIA",
                                                 ext == "int") %>%
                                         pull(biome) %>% unique()


##pivoting
vert_pivot_mammals <- vert_enfa_ages %>% filter(className == "MAMMALIA",
                                            biome %in% biomes_sig_spe_mammals) %>% 
  pivot_longer(cols = ends_with(".age"),
               names_to = "ext_age",
               values_to = "Ages") %>% 
  filter(ext_age != "Estimated.age")

##factors
vert_pivot_mammals$ext_age <- factor(vert_pivot_mammals$ext_age, 
                                     levels = c("low.age",
                                                "int.age",
                                                "high.age"),
                                     ordered = TRUE)

vert_pivot_mammals$className <- factor(vert_pivot_mammals$className,
                                       levels = c("MAMMALIA"),
                                       labels = c("Mammals"
                                       ),
                                       ordered = TRUE)

vert_pivot_mammals$biome <- factor(vert_pivot_mammals$biome,
                                   levels = c("Tropical_&_subtropical_moist_broadleaf_forests"),
                                   labels = c("Tropical & subtropical moist broadleaf forests"),
                                   ordered = TRUE)


## df elements of models
ele_spe_mammals <- biome_spe_results %>% select(term, Estimate, class, ext, biome) %>% 
  filter(class == "MAMMALIA",
         biome %in% biomes_sig_spe_mammals) %>% 
  pivot_wider(names_from = "term",
              values_from=  "Estimate") %>% 
  rename(className = class)

##changing factors name for matching with vert_enfa
ele_spe_mammals$className <- factor(ele_spe_mammals$className, levels = c("MAMMALIA"),
                                    labels = c("Mammals"))

##int extinciton scenario
ele_spe_mammals_int <- ele_spe_mammals %>% filter(ext == "int")

##Biomes
ele_spe_mammals_int$biome <- factor(ele_spe_mammals_int$biome,
                                     levels = c("Tropical_&_subtropical_moist_broadleaf_forests"),
                                     labels = c("Tropical & subtropical moist broadleaf forests"),
                                     ordered = TRUE)

png("text/figures/specialization/biomes/mammals_model_int.png",
    width = 10, height = 10, units = "cm", 
    pointsize = 8, res = 300)


plot.mammals.int.spe <- vert_pivot_mammals %>% filter(ext_age == "int.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(specialization+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Specialization")+
  facet_grid(className~biome, scales = "free", switch = "y")+
  geom_abline(data = ele_spe_mammals_int, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle(NULL)+
  scale_y_continuous(position = "right")+
  theme_bw() +
  mynamestheme+
  theme(legend.position = "none",
        strip.text.y = element_text(angle= 90, size = 14,
                                    face = "bold"))


dev.off()


###########low extinction##############

biomes_sig_spe_mammals <- sig_spe_res %>% filter(class == "MAMMALIA") %>%
                                 pull(biome) %>% unique()


##pivoting
vert_pivot_mammals <- vert_enfa_ages %>% filter(className == "MAMMALIA",
                                                biome %in% biomes_sig_spe_mammals) %>% 
  pivot_longer(cols = ends_with(".age"),
               names_to = "ext_age",
               values_to = "Ages") %>% 
  filter(ext_age != "Estimated.age")

##factors
vert_pivot_mammals$ext_age <- factor(vert_pivot_mammals$ext_age, 
                                     levels = c("low.age",
                                                "int.age",
                                                "high.age"),
                                     ordered = TRUE)

vert_pivot_mammals$className <- factor(vert_pivot_mammals$className,
                                       levels = c("MAMMALIA"),
                                       labels = c("Mammals"
                                       ),
                                       ordered = TRUE)

vert_pivot_mammals$biome <- factor(vert_pivot_mammals$biome,
                                   levels = c("Tropical_&_subtropical_moist_broadleaf_forests"),
                                   labels = c("Tropical & subtropical moist broadleaf forests"),
                                   ordered = TRUE)


## df elements of models
ele_spe_mammals <- biome_spe_results %>% select(term, Estimate, class, ext, biome) %>% 
  filter(class == "MAMMALIA",
         biome %in% biomes_sig_spe_mammals) %>% 
  pivot_wider(names_from = "term",
              values_from=  "Estimate") %>% 
  rename(className = class)

##changing factors name for matching with vert_enfa
ele_spe_mammals$className <- factor(ele_spe_mammals$className, levels = c("MAMMALIA"),
                                    labels = c("Mammals"))

##low extinciton scenario
ele_spe_mammals_low <- ele_spe_mammals %>% filter(ext == "low")

##Biomes
ele_spe_mammals_low$biome <- factor(ele_spe_mammals_low$biome,
                                     levels = c("Tropical_&_subtropical_moist_broadleaf_forests"),
                                     labels = c("Tropical & subtropical moist broadleaf forests"),
                                     ordered = TRUE)

png("text/figures/specialization/biomes/mammals_model_low.png",
    width = 10, height = 10, units = "cm", 
    pointsize = 8, res = 300)


plot.mammals.low.spe <- vert_pivot_mammals %>% filter(ext_age == "low.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(specialization+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Specialization")+
  facet_grid(className~biome, scales = "free", switch = "y")+
  geom_abline(data = ele_spe_mammals_low, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle(NULL)+
  scale_y_continuous(position = "right")+
  theme_bw() +
  mynamestheme+
  theme(legend.position = "none",
        strip.text.y = element_text(angle= 90, size = 14,
                                    face = "bold"))


dev.off()

# birds -------------------------------------------------------------------

###############high extinction#########################

biomes_sig_spe_birds <- sig_spe_res %>% filter(class == "AVES",
                                               ext == "high") %>%
                                           pull(biome) %>% unique()


##pivoting
vert_pivot_birds <- vert_enfa_ages %>% filter(className == "AVES",
                                                biome %in% biomes_sig_spe_birds) %>% 
  pivot_longer(cols = ends_with(".age"),
               names_to = "ext_age",
               values_to = "Ages") %>% 
  filter(ext_age != "Estimated.age")

##factors
vert_pivot_birds$ext_age <- factor(vert_pivot_birds$ext_age, 
                                     levels = c("low.age",
                                                "int.age",
                                                "high.age"),
                                     ordered = TRUE)

vert_pivot_birds$className <- factor(vert_pivot_birds$className,
                                       levels = c("AVES"),
                                       labels = c("Birds"
                                       ),
                                       ordered = TRUE)

vert_pivot_birds$biome <- factor(vert_pivot_birds$biome,
                                   levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                              
                                              "Tropical_&_subtropical_grasslands_savannas_and_shrublands",
                                              "Flooded_grasslands_and_savannas",
                                              "mangroves"),
                                   labels = c("Tropical & subtropical moist broadleaf forests",
                                              
                                              "Tropical & subtropical grasslands savannas and shrublands",
                                              "Flooded grasslands and savannas",
                                              "Mangroves"),
                                   ordered = TRUE)


## df elements of models
ele_spe_birds <- biome_spe_results %>% select(term, Estimate, class, ext, biome) %>% 
  filter(class == "AVES",
         biome %in% biomes_sig_spe_birds) %>% 
  pivot_wider(names_from = "term",
              values_from=  "Estimate") %>% 
  rename(className = class)

##changing factors name for matching with vert_enfa
ele_spe_birds$className <- factor(ele_spe_birds$className, levels = c("AVES"),
                                    labels = c("Birds"))

##High extinciton scenario
ele_spe_birds_high <- ele_spe_birds %>% filter(ext == "high")

##Biomes
ele_spe_birds_high$biome <- factor(ele_spe_birds_high$biome,
                                   levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                              
                                              "Tropical_&_subtropical_grasslands_savannas_and_shrublands",
                                              "Flooded_grasslands_and_savannas",
                                              "mangroves"),
                                   labels = c("Tropical & subtropical moist broadleaf forests",
                                              "Tropical & subtropical grasslands savannas and shrublands",
                                              "Flooded grasslands and savannas",
                                              "Mangroves"),
                                     ordered = TRUE)


png("text/figures/specialization/biomes/birds_model_high.png",
    width = 40, height = 10, units = "cm", 
    pointsize = 8, res = 300)


plot.birds.high.spe <- vert_pivot_birds %>% filter(ext_age == "high.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(specialization+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Specialization")+
  facet_grid(className~biome, scales = "free", switch = "y")+
  geom_abline(data = ele_spe_birds_high, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle(NULL)+
  scale_y_continuous(position = "right")+
  theme_bw() +
  mynamestheme+
  theme(legend.position = "none",
        strip.text.y = element_text(angle= 90, size = 14,
                                    face = "bold"))


dev.off()

###############int extinction#########################

biomes_sig_spe_birds <- sig_spe_res %>% filter(class == "AVES",
                                               ext == "int") %>%
  pull(biome) %>% unique()


##pivoting
vert_pivot_birds <- vert_enfa_ages %>% filter(className == "AVES",
                                              biome %in% biomes_sig_spe_birds) %>% 
  pivot_longer(cols = ends_with(".age"),
               names_to = "ext_age",
               values_to = "Ages") %>% 
  filter(ext_age != "Estimated.age")

##factors
vert_pivot_birds$ext_age <- factor(vert_pivot_birds$ext_age, 
                                   levels = c("low.age",
                                              "int.age",
                                              "high.age"),
                                   ordered = TRUE)

vert_pivot_birds$className <- factor(vert_pivot_birds$className,
                                     levels = c("AVES"),
                                     labels = c("Birds"
                                     ),
                                     ordered = TRUE)

vert_pivot_birds$biome <- factor(vert_pivot_birds$biome,
                                 levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                            "Tropical_&_subtropical_dry_broadleaf_forests",
                                            "Tropical_&_subtropical_grasslands_savannas_and_shrublands",
                                            "Flooded_grasslands_and_savannas",
                                            "mangroves"),
                                 labels = c("Tropical & subtropical moist broadleaf forests",
                                            "Tropical & subtropical dry broadleaf forests",
                                            "Tropical & subtropical grasslands savannas and shrublands",
                                            "Flooded grasslands and savannas",
                                            "Mangroves"),
                                 ordered = TRUE)


## df elements of models
ele_spe_birds <- biome_spe_results %>% select(term, Estimate, class, ext, biome) %>% 
  filter(class == "AVES",
         biome %in% biomes_sig_spe_birds) %>% 
  pivot_wider(names_from = "term",
              values_from=  "Estimate") %>% 
  rename(className = class)

##changing factors name for matching with vert_enfa
ele_spe_birds$className <- factor(ele_spe_birds$className, levels = c("AVES"),
                                  labels = c("Birds"))

##int extinciton scenario
ele_spe_birds_int <- ele_spe_birds %>% filter(ext == "int")

##Biomes
ele_spe_birds_int$biome <- factor(ele_spe_birds_int$biome,
                                   levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                              "Tropical_&_subtropical_dry_broadleaf_forests", 
                                              "Tropical_&_subtropical_grasslands_savannas_and_shrublands",
                                              "Flooded_grasslands_and_savannas",
                                              "mangroves"),
                                   labels = c("Tropical & subtropical moist broadleaf forests",
                                              "Tropical & subtropical dry broadleaf forests",
                                              "Tropical & subtropical grasslands savannas and shrublands",
                                              "Flooded grasslands and savannas",
                                              "Mangroves"),
                                   ordered = TRUE)


png("text/figures/specialization/biomes/birds_model_int.png",
    width = 50, height = 10, units = "cm", 
    pointsize = 8, res = 300)


plot.birds.int.spe <- vert_pivot_birds %>% filter(ext_age == "int.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(specialization+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Specialization")+
  facet_grid(className~biome, scales = "free", switch = "y")+
  geom_abline(data = ele_spe_birds_int, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle(NULL)+
  scale_y_continuous(position = "right")+
  theme_bw() +
  mynamestheme+
  theme(legend.position = "none",
        strip.text.y = element_text(angle= 90, size = 14,
                                    face = "bold"))


dev.off()

###############low extinction#########################

biomes_sig_spe_birds <- sig_spe_res %>% filter(class == "AVES",
                                               ext == "low") %>%
  pull(biome) %>% unique()


##pivoting
vert_pivot_birds <- vert_enfa_ages %>% filter(className == "AVES",
                                              biome %in% biomes_sig_spe_birds) %>% 
  pivot_longer(cols = ends_with(".age"),
               names_to = "ext_age",
               values_to = "Ages") %>% 
  filter(ext_age != "Estimated.age")

##factors
vert_pivot_birds$ext_age <- factor(vert_pivot_birds$ext_age, 
                                   levels = c("low.age",
                                              "int.age",
                                              "high.age"),
                                   ordered = TRUE)

vert_pivot_birds$className <- factor(vert_pivot_birds$className,
                                     levels = c("AVES"),
                                     labels = c("Birds"
                                     ),
                                     ordered = TRUE)

vert_pivot_birds$biome <- factor(vert_pivot_birds$biome,
                                 levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                            "Tropical_&_subtropical_dry_broadleaf_forests",
                                            "Tropical_&_subtropical_grasslands_savannas_and_shrublands",
                                            "Flooded_grasslands_and_savannas",
                                            "mangroves"),
                                 labels = c("Tropical & subtropical moist broadleaf forests",
                                            "Tropical & subtropical dry broadleaf forests",
                                            "Tropical & subtropical grasslands savannas and shrublands",
                                            "Flooded grasslands and savannas",
                                            "Mangroves"),
                                 ordered = TRUE)


## df elements of models
ele_spe_birds <- biome_spe_results %>% select(term, Estimate, class, ext, biome) %>% 
  filter(class == "AVES",
         biome %in% biomes_sig_spe_birds) %>% 
  pivot_wider(names_from = "term",
              values_from=  "Estimate") %>% 
  rename(className = class)

##changing factors name for matching with vert_enfa
ele_spe_birds$className <- factor(ele_spe_birds$className, levels = c("AVES"),
                                  labels = c("Birds"))

##low extinciton scenario
ele_spe_birds_low <- ele_spe_birds %>% filter(ext == "low")

##Biomes
ele_spe_birds_low$biome <- factor(ele_spe_birds_low$biome,
                                  levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                             "Tropical_&_subtropical_dry_broadleaf_forests", 
                                             "Tropical_&_subtropical_grasslands_savannas_and_shrublands",
                                             "Flooded_grasslands_and_savannas",
                                             "mangroves"),
                                  labels = c("Tropical & subtropical moist broadleaf forests",
                                             "Tropical & subtropical dry broadleaf forests",
                                             "Tropical & subtropical grasslands savannas and shrublands",
                                             "Flooded grasslands and savannas",
                                             "Mangroves"),
                                  ordered = TRUE)


png("text/figures/specialization/biomes/birds_model_low.png",
    width = 50, height = 10, units = "cm", 
    pointsize = 8, res = 300)


plot.birds.low.spe <- vert_pivot_birds %>% filter(ext_age == "low.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(specialization+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Specialization")+
  facet_grid(className~biome, scales = "free", switch = "y")+
  geom_abline(data = ele_spe_birds_low, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle(NULL)+
  scale_y_continuous(position = "right")+
  theme_bw() +
  mynamestheme+
  theme(legend.position = "none",
        strip.text.y = element_text(angle= 90, size = 14,
                                    face = "bold"))


dev.off()


# Reptiles ----------------------------------------------------------------

#######high extinction#################
biomes_sig_spe_reptiles <- sig_spe_res %>% filter(class == "REPTILIA",
                                                  ext == "high") %>%
  pull(biome) %>% unique()


##pivoting
vert_pivot_reptiles <- vert_enfa_ages %>% filter(className == "REPTILIA",
                                              biome %in% biomes_sig_spe_reptiles) %>% 
  pivot_longer(cols = ends_with(".age"),
               names_to = "ext_age",
               values_to = "Ages") %>% 
  filter(ext_age != "Estimated.age")

##factors
vert_pivot_reptiles$ext_age <- factor(vert_pivot_reptiles$ext_age, 
                                   levels = c("low.age",
                                              "int.age",
                                              "high.age"),
                                   ordered = TRUE)

vert_pivot_reptiles$className <- factor(vert_pivot_reptiles$className,
                                     levels = c("REPTILIA"),
                                     labels = c("Reptiles"
                                     ),
                                     ordered = TRUE)

vert_pivot_reptiles$biome <- factor(vert_pivot_reptiles$biome,
                                 levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                            "Temperate_grasslands_savannas_and_ shrublands",
                                            "Montane_grasslands_and_shrublands"),
                                 labels = c("Tropical & subtropical moist broadleaf forests",
                                            "Temperate grasslands savannas and shrublands",
                                            "Montane grasslands and shrublands"),
                                 ordered = TRUE)


## df elements of models
ele_spe_reptiles <- biome_spe_results %>% select(term, Estimate, class, ext, biome) %>% 
  filter(class == "REPTILIA",
         biome %in% biomes_sig_spe_reptiles) %>% 
  pivot_wider(names_from = "term",
              values_from=  "Estimate") %>% 
  rename(className = class)

##changing factors name for matching with vert_enfa
ele_spe_reptiles$className <- factor(ele_spe_reptiles$className, levels = c("REPTILIA"),
                                  labels = c("Reptiles"))

##High extinciton scenario
ele_spe_reptiles_high <- ele_spe_reptiles %>% filter(ext == "high")

##Biomes
ele_spe_reptiles_high$biome <- factor(ele_spe_reptiles_high$biome,
                                      levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                                 "Temperate_grasslands_savannas_and_ shrublands",
                                                 "Montane_grasslands_and_shrublands"),
                                      labels = c("Tropical & subtropical moist broadleaf forests",
                                                 "Temperate grasslands savannas and shrublands",
                                                 "Montane grasslands and shrublands"),
                                   ordered = TRUE)


png("text/figures/specialization/biomes/reptiles_model_high.png",
    width = 30, height = 10, units = "cm", 
    pointsize = 8, res = 300)


plot.reptiles.high.spe <- vert_pivot_reptiles %>% filter(ext_age == "high.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(specialization+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Specialization")+
  facet_grid(className~biome, scales = "free", switch = "y")+
  geom_abline(data = ele_spe_reptiles_high, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle(NULL)+
  scale_y_continuous(position = "right")+
  theme_bw() +
  mynamestheme+
  theme(legend.position = "none",
        strip.text.y = element_text(angle= 90, size = 14,
                                    face = "bold"))


dev.off()

#######int extinction#################
biomes_sig_spe_reptiles <- sig_spe_res %>% filter(class == "REPTILIA",
                                                  ext == "int") %>%
  pull(biome) %>% unique()


##pivoting
vert_pivot_reptiles <- vert_enfa_ages %>% filter(className == "REPTILIA",
                                                 biome %in% biomes_sig_spe_reptiles) %>% 
  pivot_longer(cols = ends_with(".age"),
               names_to = "ext_age",
               values_to = "Ages") %>% 
  filter(ext_age != "Estimated.age")

##factors
vert_pivot_reptiles$ext_age <- factor(vert_pivot_reptiles$ext_age, 
                                      levels = c("low.age",
                                                 "int.age",
                                                 "high.age"),
                                      ordered = TRUE)

vert_pivot_reptiles$className <- factor(vert_pivot_reptiles$className,
                                        levels = c("REPTILIA"),
                                        labels = c("Reptiles"
                                        ),
                                        ordered = TRUE)

vert_pivot_reptiles$biome <- factor(vert_pivot_reptiles$biome,
                                    levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                               "Temperate_grasslands_savannas_and_ shrublands",
                                               "Montane_grasslands_and_shrublands"),
                                    labels = c("Tropical & subtropical moist broadleaf forests",
                                               "Temperate grasslands savannas and shrublands",
                                               "Montane grasslands and shrublands"),
                                    ordered = TRUE)


## df elements of models
ele_spe_reptiles <- biome_spe_results %>% select(term, Estimate, class, ext, biome) %>% 
  filter(class == "REPTILIA",
         biome %in% biomes_sig_spe_reptiles) %>% 
  pivot_wider(names_from = "term",
              values_from=  "Estimate") %>% 
  rename(className = class)

##changing factors name for matching with vert_enfa
ele_spe_reptiles$className <- factor(ele_spe_reptiles$className, levels = c("REPTILIA"),
                                     labels = c("Reptiles"))

##int extinciton scenario
ele_spe_reptiles_int <- ele_spe_reptiles %>% filter(ext == "int")

##Biomes
ele_spe_reptiles_int$biome <- factor(ele_spe_reptiles_int$biome,
                                      levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                                 "Temperate_grasslands_savannas_and_ shrublands",
                                                 "Montane_grasslands_and_shrublands"),
                                      labels = c("Tropical & subtropical moist broadleaf forests",
                                                 "Temperate grasslands savannas and shrublands",
                                                 "Montane grasslands and shrublands"),
                                      ordered = TRUE)


png("text/figures/specialization/biomes/reptiles_model_int.png",
    width = 30, height = 10, units = "cm", 
    pointsize = 8, res = 300)


plot.reptiles.int.spe <- vert_pivot_reptiles %>% filter(ext_age == "int.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(specialization+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Specialization")+
  facet_grid(className~biome, scales = "free", switch = "y")+
  geom_abline(data = ele_spe_reptiles_int, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle(NULL)+
  scale_y_continuous(position = "right")+
  theme_bw() +
  mynamestheme+
  theme(legend.position = "none",
        strip.text.y = element_text(angle= 90, size = 14,
                                    face = "bold"))


dev.off()

#######low extinction#################
biomes_sig_spe_reptiles <- sig_spe_res %>% filter(class == "REPTILIA",
                                                  ext == "low") %>%
  pull(biome) %>% unique()


##pivoting
vert_pivot_reptiles <- vert_enfa_ages %>% filter(className == "REPTILIA",
                                                 biome %in% biomes_sig_spe_reptiles) %>% 
  pivot_longer(cols = ends_with(".age"),
               names_to = "ext_age",
               values_to = "Ages") %>% 
  filter(ext_age != "Estimated.age")

##factors
vert_pivot_reptiles$ext_age <- factor(vert_pivot_reptiles$ext_age, 
                                      levels = c("low.age",
                                                 "int.age",
                                                 "high.age"),
                                      ordered = TRUE)

vert_pivot_reptiles$className <- factor(vert_pivot_reptiles$className,
                                        levels = c("REPTILIA"),
                                        labels = c("Reptiles"
                                        ),
                                        ordered = TRUE)

vert_pivot_reptiles$biome <- factor(vert_pivot_reptiles$biome,
                                    levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                               "Temperate_grasslands_savannas_and_ shrublands",
                                               "Montane_grasslands_and_shrublands"),
                                    labels = c("Tropical & subtropical moist broadleaf forests",
                                               "Temperate grasslands savannas and shrublands",
                                               "Montane grasslands and shrublands"),
                                    ordered = TRUE)


## df elements of models
ele_spe_reptiles <- biome_spe_results %>% select(term, Estimate, class, ext, biome) %>% 
  filter(class == "REPTILIA",
         biome %in% biomes_sig_spe_reptiles) %>% 
  pivot_wider(names_from = "term",
              values_from=  "Estimate") %>% 
  rename(className = class)

##changing factors name for matching with vert_enfa
ele_spe_reptiles$className <- factor(ele_spe_reptiles$className, levels = c("REPTILIA"),
                                     labels = c("Reptiles"))

##low extinction scenario
ele_spe_reptiles_low <- ele_spe_reptiles %>% filter(ext == "low")

##Biomes
ele_spe_reptiles_low$biome <- factor(ele_spe_reptiles_low$biome,
                                      levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                                 "Temperate_grasslands_savannas_and_ shrublands",
                                                 "Montane_grasslands_and_shrublands"),
                                      labels = c("Tropical & subtropical moist broadleaf forests",
                                                 "Temperate grasslands savannas and shrublands",
                                                 "Montane grasslands and shrublands"),
                                      ordered = TRUE)


png("text/figures/specialization/biomes/reptiles_model_low.png",
    width = 30, height = 10, units = "cm", 
    pointsize = 8, res = 300)


plot.reptiles.low.spe <- vert_pivot_reptiles %>% filter(ext_age == "low.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(specialization+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Specialization")+
  facet_grid(className~biome, scales = "free", switch = "y")+
  geom_abline(data = ele_spe_reptiles_low, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle(NULL)+
  scale_y_continuous(position = "right")+
  theme_bw() +
  mynamestheme+
  theme(legend.position = "none",
        strip.text.y = element_text(angle= 90, size = 14,
                                    face = "bold"))


dev.off()


# AMPHIBIA ----------------------------------------------------------------

##########high extinction ##########################

biomes_sig_spe_amphibians <- sig_spe_res %>% filter(class == "AMPHIBIA",
                                                    ext == "high") %>%
  pull(biome) %>% unique()


##pivoting
vert_pivot_amphibians <- vert_enfa_ages %>% filter(className == "AMPHIBIA",
                                              biome %in% biomes_sig_spe_amphibians) %>% 
  pivot_longer(cols = ends_with(".age"),
               names_to = "ext_age",
               values_to = "Ages") %>% 
  filter(ext_age != "Estimated.age")

##factors
vert_pivot_amphibians$ext_age <- factor(vert_pivot_amphibians$ext_age, 
                                   levels = c("low.age",
                                              "int.age",
                                              "high.age"),
                                   ordered = TRUE)

vert_pivot_amphibians$className <- factor(vert_pivot_amphibians$className,
                                     levels = c("AMPHIBIA"),
                                     labels = c("Amphibians"
                                     ),
                                     ordered = TRUE)

vert_pivot_amphibians$biome <- factor(vert_pivot_amphibians$biome,
                                 levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                            "Tropical_&_subtropical_grasslands_savannas_and_shrublands",
                                            "Temperate_grasslands_savannas_and_ shrublands"),
                                 labels = c("Tropical & subtropical moist broadleaf forests",
                                            "Tropical & subtropical grasslands savannas and shrublands",
                                            "Temperate grasslands savannas and shrublands"),
                                 ordered = TRUE)


## df elements of models
ele_spe_amphibians <- biome_spe_results %>% select(term, Estimate, class, ext, biome) %>% 
  filter(class == "AMPHIBIA",
         biome %in% biomes_sig_spe_amphibians) %>% 
  pivot_wider(names_from = "term",
              values_from=  "Estimate") %>% 
  rename(className = class)

##changing factors name for matching with vert_enfa
ele_spe_amphibians$className <- factor(ele_spe_amphibians$className, levels = c("AMPHIBIA"),
                                  labels = c("Amphibians"))

##High extinciton scenario
ele_spe_amphibians_high <- ele_spe_amphibians %>% filter(ext == "high")

##Biomes
ele_spe_amphibians_high$biome <- factor(ele_spe_amphibians_high$biome,
                                        levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                                   "Tropical_&_subtropical_grasslands_savannas_and_shrublands",
                                                   "Temperate_grasslands_savannas_and_ shrublands"),
                                        labels = c("Tropical & subtropical moist broadleaf forests",
                                                   "Tropical & subtropical grasslands savannas and shrublands",
                                                   "Temperate grasslands savannas and shrublands"),
                                   ordered = TRUE)


png("text/figures/specialization/biomes/amphibians_model_high.png",
    width = 30, height = 10, units = "cm", 
    pointsize = 8, res = 300)


plot.amphibians.high.spe <- vert_pivot_amphibians %>% filter(ext_age == "high.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(specialization+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Specialization")+
  facet_grid(className~biome, scales = "free", switch = "y")+
  geom_abline(data = ele_spe_amphibians_high, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle(NULL)+
  scale_y_continuous(position = "right")+
  theme_bw() +
  mynamestheme+
  theme(legend.position = "none",
        strip.text.y = element_text(angle= 90, size = 14,
                                    face = "bold"))


dev.off()

##########int extinction ##########################

biomes_sig_spe_amphibians <- sig_spe_res %>% filter(class == "AMPHIBIA",
                                                    ext == "int") %>%
  pull(biome) %>% unique()


##pivoting
vert_pivot_amphibians <- vert_enfa_ages %>% filter(className == "AMPHIBIA",
                                                   biome %in% biomes_sig_spe_amphibians) %>% 
  pivot_longer(cols = ends_with(".age"),
               names_to = "ext_age",
               values_to = "Ages") %>% 
  filter(ext_age != "Estimated.age")

##factors
vert_pivot_amphibians$ext_age <- factor(vert_pivot_amphibians$ext_age, 
                                        levels = c("low.age",
                                                   "int.age",
                                                   "high.age"),
                                        ordered = TRUE)

vert_pivot_amphibians$className <- factor(vert_pivot_amphibians$className,
                                          levels = c("AMPHIBIA"),
                                          labels = c("Amphibians"
                                          ),
                                          ordered = TRUE)

vert_pivot_amphibians$biome <- factor(vert_pivot_amphibians$biome,
                                      levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                                 "Tropical_&_subtropical_grasslands_savannas_and_shrublands",
                                                 "Temperate_grasslands_savannas_and_ shrublands"),
                                      labels = c("Tropical & subtropical moist broadleaf forests",
                                                 "Tropical & subtropical grasslands savannas and shrublands",
                                                 "Temperate grasslands savannas and shrublands"),
                                      ordered = TRUE)


## df elements of models
ele_spe_amphibians <- biome_spe_results %>% select(term, Estimate, class, ext, biome) %>% 
  filter(class == "AMPHIBIA",
         biome %in% biomes_sig_spe_amphibians) %>% 
  pivot_wider(names_from = "term",
              values_from=  "Estimate") %>% 
  rename(className = class)

##changing factors name for matching with vert_enfa
ele_spe_amphibians$className <- factor(ele_spe_amphibians$className, levels = c("AMPHIBIA"),
                                       labels = c("Amphibians"))

##int extinciton scenario
ele_spe_amphibians_int <- ele_spe_amphibians %>% filter(ext == "int")

##Biomes
ele_spe_amphibians_int$biome <- factor(ele_spe_amphibians_int$biome,
                                        levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                                   "Tropical_&_subtropical_grasslands_savannas_and_shrublands",
                                                   "Temperate_grasslands_savannas_and_ shrublands"),
                                        labels = c("Tropical & subtropical moist broadleaf forests",
                                                   "Tropical & subtropical grasslands savannas and shrublands",
                                                   "Temperate grasslands savannas and shrublands"),
                                        ordered = TRUE)


png("text/figures/specialization/biomes/amphibians_model_int.png",
    width = 30, height = 10, units = "cm", 
    pointsize = 8, res = 300)


plot.amphibians.int.spe <- vert_pivot_amphibians %>% filter(ext_age == "int.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(specialization+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Specialization")+
  facet_grid(className~biome, scales = "free", switch = "y")+
  geom_abline(data = ele_spe_amphibians_int, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle(NULL)+
  scale_y_continuous(position = "right")+
  theme_bw() +
  mynamestheme+
  theme(legend.position = "none",
        strip.text.y = element_text(angle= 90, size = 14,
                                    face = "bold"))


dev.off()

##########low extinction ##########################

biomes_sig_spe_amphibians <- sig_spe_res %>% filter(class == "AMPHIBIA",
                                                    ext == "low") %>%
  pull(biome) %>% unique()


##pivoting
vert_pivot_amphibians <- vert_enfa_ages %>% filter(className == "AMPHIBIA",
                                                   biome %in% biomes_sig_spe_amphibians) %>% 
  pivot_longer(cols = ends_with(".age"),
               names_to = "ext_age",
               values_to = "Ages") %>% 
  filter(ext_age != "Estimated.age")

##factors
vert_pivot_amphibians$ext_age <- factor(vert_pivot_amphibians$ext_age, 
                                        levels = c("low.age",
                                                   "int.age",
                                                   "high.age"),
                                        ordered = TRUE)

vert_pivot_amphibians$className <- factor(vert_pivot_amphibians$className,
                                          levels = c("AMPHIBIA"),
                                          labels = c("Amphibians"
                                          ),
                                          ordered = TRUE)

vert_pivot_amphibians$biome <- factor(vert_pivot_amphibians$biome,
                                      levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                                 "Tropical_&_subtropical_grasslands_savannas_and_shrublands"
                                                 ),
                                      labels = c("Tropical & subtropical moist broadleaf forests",
                                                 "Tropical & subtropical grasslands savannas and shrublands"),
                                      ordered = TRUE)


## df elements of models
ele_spe_amphibians <- biome_spe_results %>% select(term, Estimate, class, ext, biome) %>% 
  filter(class == "AMPHIBIA",
         biome %in% biomes_sig_spe_amphibians) %>% 
  pivot_wider(names_from = "term",
              values_from=  "Estimate") %>% 
  rename(className = class)

##changing factors name for matching with vert_enfa
ele_spe_amphibians$className <- factor(ele_spe_amphibians$className, levels = c("AMPHIBIA"),
                                       labels = c("Amphibians"))

##low extinciton scenario
ele_spe_amphibians_low <- ele_spe_amphibians %>% filter(ext == "low")

##Biomes
ele_spe_amphibians_low$biome <- factor(ele_spe_amphibians_low$biome,
                                        levels = c("Tropical_&_subtropical_moist_broadleaf_forests",
                                                   "Tropical_&_subtropical_grasslands_savannas_and_shrublands"
                                                  ),
                                        labels = c("Tropical & subtropical moist broadleaf forests",
                                                   "Tropical & subtropical grasslands savannas and shrublands"
                                                   ),
                                        ordered = TRUE)


png("text/figures/specialization/biomes/amphibians_model_low.png",
    width = 20, height = 10, units = "cm", 
    pointsize = 8, res = 300)


plot.amphibians.low.spe <- vert_pivot_amphibians %>% filter(ext_age == "low.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(specialization+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Specialization")+
  facet_grid(className~biome, scales = "free", switch = "y")+
  geom_abline(data = ele_spe_amphibians_low, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle(NULL)+
  scale_y_continuous(position = "right")+
  theme_bw() +
  mynamestheme+
  theme(legend.position = "none",
        strip.text.y = element_text(angle= 90, size = 14,
                                    face = "bold"))


dev.off()






# beta histogram ----------------------------------------------------------


#############High extinction######################
png("text/figures/specialization/biomes/model_beta_high.png",
    width = 10, height = 10, units = "cm", 
    pointsize = 8, res = 300)

beta_spe %>% filter(ext == "high") %>%
  ggplot(aes(x = Estimate, fill = class ))+
  geom_rect(aes(xmin = -Inf,
                xmax = 0, 
                ymin = -Inf,
                ymax = +Inf),
            fill = "#f0f0f0",
            color = "#f0f0f0")+
  geom_vline(xintercept = 0, color = "black", linewidth = 1,
             linetype = "dashed")+
  geom_histogram(color = "black", bins = 15) +
  scale_fill_manual(name = NULL,
                    labels = c("Mammals", "Birds", "Reptiles", "Amphibians"),
                    values = c( "red",
                                "#fdae61",
                                "#008837",
                                "#a6dba0"))+
  #xlim(-1.2,1.2)+
  xlab("Beta")+
  ylab("Count")+
  #ggtitle("Specialization (High extinction)")+
  theme_classic()+
  #scale_y_continuous(expand = c(0,0))+
  mynamestheme

dev.off()

#############int extinction######################
png("text/figures/specialization/biomes/model_beta_int.png",
    width = 10, height = 10, units = "cm", 
    pointsize = 8, res = 300)

beta_spe %>% filter(ext == "int") %>%
  ggplot(aes(x = Estimate, fill = class ))+
  geom_rect(aes(xmin = -Inf,
                xmax = 0, 
                ymin = -Inf,
                ymax = +Inf),
            fill = "#f0f0f0",
            color = "#f0f0f0")+
  geom_vline(xintercept = 0, color = "black", linewidth = 1,
             linetype = "dashed")+
  geom_histogram(color = "black", bins = 15) +
  scale_fill_manual(name = NULL,
                    labels = c("Mammals", "Birds", "Reptiles", "Amphibians"),
                    values = c( "red",
                                "#fdae61",
                                "#008837",
                                "#a6dba0"))+
  #xlim(-1.2,1.2)+
  xlab("Beta")+
  ylab("Count")+
  #ggtitle("Specialization (int extinction)")+
  theme_classic()+
  #scale_y_continuous(expand = c(0,0))+
  mynamestheme

dev.off()

#############low extinction######################
png("text/figures/specialization/biomes/model_beta_low.png",
    width = 10, height = 10, units = "cm", 
    pointsize = 8, res = 300)

beta_spe %>% filter(ext == "low") %>%
  ggplot(aes(x = Estimate, fill = class ))+
  geom_rect(aes(xmin = -Inf,
                xmax = 0, 
                ymin = -Inf,
                ymax = +Inf),
            fill = "#f0f0f0",
            color = "#f0f0f0")+
  geom_vline(xintercept = 0, color = "black", linewidth = 1,
             linetype = "dashed")+
  geom_histogram(color = "black", bins = 15) +
  scale_fill_manual(name = NULL,
                    labels = c("Mammals", "Birds", "Reptiles", "Amphibians"),
                    values = c( "red",
                                "#fdae61",
                                "#008837",
                                "#a6dba0"))+
  xlim(-0.4,0.4)+
  xlab("Beta")+
  ylab("Count")+
  #ggtitle("Specialization (low extinction)")+
  theme_classic()+
  #scale_y_continuous(expand = c(0,0))+
  mynamestheme

dev.off()
