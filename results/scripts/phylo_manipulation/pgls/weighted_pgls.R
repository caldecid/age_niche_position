# Weighted marginality and specialization ---------------------------------

##sourcing the libraries and the directories
source(file.path(getwd(), "/source.R"))

##reading vertebrate data set
vert_enfa_ages <- read_csv(file = "results/data/processed/vert_enfa_ages.csv")


# Mammals --------------------------------------------------------------

mammals_enfa_age <- vert_enfa_ages %>% filter(className == "MAMMALIA")

##phylogeny
mammals_phy <- read.tree("results/data/raw/phylogenies/mammals/mammals_phy/MamPhy_BDvr_Completed_5911sp_topoCons_FBDasZhouEtAl_v2_tree0000.tre")

##removing duplicated species name
mammals_enfa_unique <- distinct(mammals_enfa_age, species, .keep_all = TRUE)

##dropping tips
mammals_phy_unique <- keep.tip(mammals_phy, tip = mammals_enfa_unique$species)

##arranging dataset
mammals_enfa_unique_2 <- mammals_enfa_unique %>% 
                     arrange(match(species, mammals_phy_unique$tip.label)) %>% 
                     select(species, w.marginality,
                            w.specialization,
                            low.age, int.age, high.age)

##Comparative data frame
mdat <- caper::comparative.data(phy = mammals_phy_unique,
                                data = as.data.frame(mammals_enfa_unique_2),
                                names.col = "species", vcv =TRUE ,
                                warn.dropped = TRUE)

###low extinction
mdat_low <- pgls(log(w.marginality + 1) ~ log(low.age+1),
                        data = mdat, lambda = "ML")

##results dataframe
mdat_low_marg_res <- res_pgls(mod = mdat_low, ext = "low",
                               class = "mammals", biome = "w.marg")


###intermediate extinction
mdat_int <- pgls(log(w.marginality + 1) ~ log(int.age+1),
                 data = mdat, lambda = "ML")

##results dataframe
mdat_int_marg_res <- res_pgls(mod = mdat_int, ext = "int",
                              class = "mammals", biome = "w.marg")

##high extinction
mdat_high <- pgls(log(w.marginality + 1) ~ log(high.age+1),
                  data = mdat, lambda = "ML")

##results dataframe
mdat_high_marg_res <- res_pgls(mod = mdat_high, ext = "high",
                              class = "mammals", biome = "w.marg")

##########################specialization####################################
###low extinction
mdat_low_spe <- pgls(log(w.specialization + 1) ~ log(low.age+1),
                 data = mdat, lambda = "ML")

##results dataframe
mdat_low_spe_res <- res_pgls(mod = mdat_low_spe, ext = "low",
                              class = "mammals", biome = "w.spe")


###intermediate extinction
mdat_int_spe <- pgls(log(w.specialization + 1) ~ log(int.age+1),
                 data = mdat, lambda = "ML")

##results dataframe
mdat_int_spe_res <- res_pgls(mod = mdat_int_spe, ext = "int",
                             class = "mammals", biome = "w.spe")

##high extinction
mdat_high_spe <- pgls(log(w.specialization + 1) ~ log(high.age+1),
                  data = mdat, lambda = "ML")

##results dataframe
mdat_high_spe_res <- res_pgls(mod = mdat_high_spe, ext = "high",
                             class = "mammals", biome = "w.spe")

# aves --------------------------------------------------------------

aves_enfa_age <- vert_enfa_ages %>% filter(className == "AVES")

##phylogeny
aves_phy <- read.tree("results/data/raw/phylogenies/birds_phy.tree")


##removing duplicated species name
aves_enfa_unique <- distinct(aves_enfa_age, species, .keep_all = TRUE)

##dropping tips
aves_phy_unique <- keep.tip(aves_phy, tip = aves_enfa_unique$species)

##arranging dataset
aves_enfa_unique_2 <- aves_enfa_unique %>% 
  arrange(match(species, aves_phy_unique$tip.label)) %>% 
  select(species, w.marginality,
         w.specialization,
         low.age, int.age, high.age)

##Comparative data frame
avesdat <- caper::comparative.data(phy = aves_phy_unique,
                                data = as.data.frame(aves_enfa_unique_2),
                                names.col = "species", vcv =TRUE ,
                                warn.dropped = TRUE)

###low extinction
avesdat_low <- pgls(log(w.marginality + 1) ~ log(low.age+1),
                 data = avesdat, lambda = "ML")

##results dataframe
avesdat_low_marg_res <- res_pgls(mod = avesdat_low, ext = "low",
                             class = "aves", biome = "w.marg")


###intermediate extinction
avesdat_int <- pgls(log(w.marginality + 1) ~ log(int.age+1),
                 data = avesdat, lambda = "ML")

##results dataframe
avesdat_int_marg_res <- res_pgls(mod = avesdat_int, ext = "int",
                                 class = "aves", biome = "w.marg")

##high extinction
avesdat_high <- pgls(log(w.marginality + 1) ~ log(high.age+1),
                  data = avesdat, lambda = "ML")

##results dataframe
avesdat_high_marg_res <- res_pgls(mod = avesdat_high, ext = "high",
                                 class = "aves", biome = "w.marg")

##########################specialization####################################
###low extinction
avesdat_low_spe <- pgls(log(w.specialization + 1) ~ log(low.age+1),
                     data = avesdat, lambda = "ML")

##results dataframe
avesdat_low_spe_res <- res_pgls(mod = avesdat_low_spe, ext = "low",
                                 class = "aves", biome = "w.spe")


###intermediate extinction
avesdat_int_spe <- pgls(log(w.specialization + 1) ~ log(int.age+1),
                     data = avesdat, lambda = "ML")

##results dataframe
avesdat_int_spe_res <- res_pgls(mod = avesdat_int_spe, ext = "int",
                                class = "aves", biome = "w.spe")

##high extinction
avesdat_high_spe <- pgls(log(w.specialization + 1) ~ log(high.age+1),
                      data = avesdat, lambda = "ML")

##results dataframe
avesdat_high_spe_res <- res_pgls(mod = avesdat_high_spe, ext = "high",
                                class = "aves", biome = "w.spe")

# amphibia --------------------------------------------------------------

amphibia_enfa_age <- vert_enfa_ages %>% filter(className == "AMPHIBIA")

##phylogenetic tree
amphibia_phy <- read.tree("results/data/raw/phylogenies/amphibians/amphibians_phy.tree")

##removing duplicated species name
amphibia_enfa_unique <- distinct(amphibia_enfa_age, species, .keep_all = TRUE)

##dropping tips
amphibia_phy_unique <- keep.tip(amphibia_phy, tip = amphibia_enfa_unique$species)

##arranging dataset
amphibia_enfa_unique_2 <- amphibia_enfa_unique %>% 
  arrange(match(species, amphibia_phy_unique$tip.label)) %>% 
  select(species, w.marginality,
         w.specialization,
         low.age, int.age, high.age)

##Comparative data frame
amphidat <- caper::comparative.data(phy = amphibia_phy_unique,
                                data = as.data.frame(amphibia_enfa_unique_2),
                                names.col = "species", vcv =TRUE ,
                                warn.dropped = TRUE)

###low extinction
amphidat_low <- pgls(log(w.marginality + 1) ~ log(low.age+1),
                 data = amphidat, lambda = "ML")

##results dataframe
amphidat_low_marg_res <- res_pgls(mod = amphidat_low, ext = "low",
                                  class = "amphibians", biome = "w.marg")


###intermediate extinction
amphidat_int <- pgls(log(w.marginality + 1) ~ log(int.age+1),
                 data = amphidat, lambda = "ML")

##results dataframe
amphidat_int_marg_res <- res_pgls(mod = amphidat_int, ext = "int",
                                  class = "amphibians", biome = "w.marg")

##high extinction
amphidat_high <- pgls(log(w.marginality + 1) ~ log(high.age+1),
                  data = amphidat, lambda = "ML")

##results dataframe
amphidat_high_marg_res <- res_pgls(mod = amphidat_high, ext = "high",
                                  class = "amphibians", biome = "w.marg")

##########################specialization####################################
###low extinction
amphidat_low_spe <- pgls(log(w.specialization + 1) ~ log(low.age+1),
                     data = amphidat, lambda = "ML")

##results dataframe
amphidat_low_spe_res <- res_pgls(mod = amphidat_low_spe, ext = "low",
                                  class = "amphibians", biome = "w.spe")


###intermediate extinction
amphidat_int_spe <- pgls(log(w.specialization + 1) ~ log(int.age+1),
                     data = amphidat, lambda = "ML")

##results dataframe
amphidat_int_spe_res <- res_pgls(mod = amphidat_int_spe, ext = "int",
                                 class = "amphibians", biome = "w.spe")

##high extinction
amphidat_high_spe <- pgls(log(w.specialization + 1) ~ log(high.age+1),
                      data = amphidat, lambda = "ML")

##results dataframe
amphidat_high_spe_res <- res_pgls(mod = amphidat_high_spe, ext = "high",
                                 class = "amphibians", biome = "w.spe")

# reptilia --------------------------------------------------------------

reptilia_enfa_age <- vert_enfa_ages %>% filter(className == "REPTILIA")

##phylogeny
reptilia_phy <- read.tree("results/data/raw/phylogenies/reptiles/squamate_phylo/reptiles_phy.tree")

##removing duplicated species name
reptilia_enfa_unique <- distinct(reptilia_enfa_age, species, .keep_all = TRUE)

##dropping tips
reptilia_phy_unique <- keep.tip(reptilia_phy, tip = reptilia_enfa_unique$species)

##arranging dataset
reptilia_enfa_unique_2 <- reptilia_enfa_unique %>% 
  arrange(match(species, reptilia_phy_unique$tip.label)) %>% 
  select(species, w.marginality,
         w.specialization,
         low.age, int.age, high.age)

##Comparative data frame
repdat <- caper::comparative.data(phy = reptilia_phy_unique,
                                data = as.data.frame(reptilia_enfa_unique_2),
                                names.col = "species", vcv =TRUE ,
                                warn.dropped = TRUE)

###low extinction
repdat_low <- pgls(log(w.marginality + 1) ~ log(low.age+1),
                 data = repdat, lambda = "ML")

##results dataframe
repdat_low_marg_res <- res_pgls(mod = repdat_low, ext = "low",
                                 class = "reptiles", biome = "w.marg")


###intermediate extinction
repdat_int <- pgls(log(w.marginality + 1) ~ log(int.age+1),
                 data = repdat, lambda = "ML")

##results dataframe
repdat_int_marg_res <- res_pgls(mod = repdat_int, ext = "int",
                                class = "reptiles", biome = "w.marg")

##high extinction
repdat_high <- pgls(log(w.marginality + 1) ~ log(high.age+1),
                  data = repdat, lambda = "ML")

##results dataframe
repdat_high_marg_res <- res_pgls(mod = repdat_high, ext = "high",
                                class = "reptiles", biome = "w.marg")

##########################specialization####################################
###low extinction
repdat_low_spe <- pgls(log(w.specialization + 1) ~ log(low.age+1),
                     data = repdat, lambda = "ML")

##results dataframe
repdat_low_spe_res <- res_pgls(mod = repdat_low_spe, ext = "low",
                                class = "reptiles", biome = "w.spe")


###intermediate extinction
repdat_int_spe <- pgls(log(w.specialization + 1) ~ log(int.age+1),
                     data = repdat, lambda = "ML")

##results dataframe
repdat_int_spe_res <- res_pgls(mod = repdat_int_spe, ext = "int",
                               class = "reptiles", biome = "w.spe")

##high extinction
repdat_high_spe <- pgls(log(w.specialization + 1) ~ log(high.age+1),
                      data = repdat, lambda = "ML")

##results dataframe
repdat_high_spe_res <- res_pgls(mod = repdat_high_spe, ext = "high",
                               class = "reptiles", biome = "w.spe")



#######binding results dataframes

##marginality
w.marg.results <- rbind(mdat_low_marg_res, mdat_int_marg_res,mdat_high_marg_res,
                        avesdat_low_marg_res, avesdat_int_marg_res, avesdat_high_marg_res,
                        amphidat_low_marg_res, amphidat_int_marg_res,
                        amphidat_high_marg_res, repdat_low_marg_res,
                        repdat_int_marg_res, repdat_high_marg_res)

##modifications
w.marg.results <- w.marg.results %>% rename("p_value" = "Pr(>|t|)")

w.marg.results$term <- factor(w.marg.results$term, levels = c("(Intercept)",
                                                              "log(low.age + 1)",
                                                              "log(int.age + 1)",
                                                              "log(high.age + 1)") ,
                              labels = c("Intercept",
                                         "Age",
                                         "Age",
                                         "Age"))



##save
write_xlsx(w.marg.results, path = "results/data/processed/w.marg.results.xlsx")



##specialization
w.spe.results <- rbind(mdat_low_spe_res, mdat_int_spe_res,mdat_high_spe_res,
                        avesdat_low_spe_res, avesdat_int_spe_res,
                        avesdat_high_spe_res,
                        amphidat_low_spe_res, amphidat_int_spe_res,
                        amphidat_high_spe_res, repdat_low_spe_res,
                        repdat_int_spe_res, repdat_high_spe_res)

##modifications
w.spe.results <- w.spe.results %>% rename("p_value" = "Pr(>|t|)")


w.spe.results$term <- factor(w.spe.results$term, levels = c("(Intercept)",
                                                              "log(low.age + 1)",
                                                              "log(int.age + 1)",
                                                              "log(high.age + 1)") ,
                              labels = c("Intercept",
                                         "Age",
                                         "Age",
                                         "Age"))

write_xlsx(w.spe.results, path = "results/data/processed/w.spe.results.xlsx")

##filtering significant results
x2 <- w.spe.results %>% filter(term != "(Intercept)", p_value <= 0.05)
###All are significant except Mammals in low and intermediate extinction



# Plotting ----------------------------------------------------------------
#########THEMES#########################

mynamestheme <- theme(strip.text = element_text(family = "serif", size = (10)),
                      plot.title = element_text(family = "serif", size = (14),
                                                face = "bold", hjust = 0.5
                      ),
                      axis.title = element_text(family = "serif", size = (12),
                                                face = "bold"),
                      axis.text = element_text(family = "serif", size = (10)),
                      legend.title = element_text(family = "serif", size = (11),
                                                  face = "bold"),
                      legend.text = element_text(family = "serif", size = (10)),
                      legend.position = "bottom",
                      legend.background = element_rect(fill="white",
                                                       size=.5, linetype="dotted"))

##vertebrates unique
vert_enfa_unique <-  distinct(vert_enfa_ages, species, .keep_all = TRUE)

##pivoting
vert_enfa_pivot <- vert_enfa_unique %>% pivot_longer(cols = ends_with(".age"),
                                                     names_to = "ext_age",
                                                     values_to = "Ages") %>% 
  filter(ext_age != "Estimated.age")

##factors
vert_enfa_pivot$ext_age <- factor(vert_enfa_pivot$ext_age, 
                                  levels = c("low.age",
                                             "int.age",
                                             "high.age"),
                                  ordered = TRUE)

vert_enfa_pivot$className <- factor(vert_enfa_pivot$className,
                                    levels = c("MAMMALIA",
                                               "AVES",
                                               "REPTILIA",
                                               "AMPHIBIA"),
                                    labels = c("Mammals",
                                               "Birds",
                                               "Reptiles",
                                               "Amphibians"),
                                    ordered = TRUE)

####Marginality (high extinction)

##plotting only significant relationships (all except mammals)

## df elements of models
ele_marg <- w.marg.results %>% select(term, Estimate, class, ext) %>% 
                                filter(class != "mammals") %>% 
                             pivot_wider(names_from = "term",
                                         values_from=  "Estimate") %>% 
                      rename(className = class)

##changing factors name for matching with vert_enfa
ele_marg$className <- factor(ele_marg$className, levels = c("aves", "reptiles",
                                                 "amphibians"),
                        labels = c("Birds", "Reptiles", "Amphibians"))

##plot for a high extinction scenario
ele_marg_high <- ele_marg %>% filter(ext == "high")

##plot
png("text/figures/marginality/weighted_enfa/w.marg_high.png",
    width = 20, height = 12, units = "cm", 
    pointsize = 8, res = 300)


vert_enfa_pivot %>% filter(className != "Mammals",
                           ext_age == "high.age") %>% 
ggplot(aes(x = log(Ages+1), y = log(marginality+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Weighted Marginality")+
  facet_wrap(~className, scales = "free")+
  geom_abline(data = ele_marg_high, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle("High extinction scenario")+
  theme_bw() +
  mynamestheme

dev.off()


######Intermediate extinction scenario
##plot for a int extinction scenario
ele_marg_int <- ele_marg %>% filter(ext == "int")

##plot
png("text/figures/marginality/weighted_enfa/w.marg_int.png",
    width = 20, height = 12, units = "cm", 
    pointsize = 8, res = 300)


vert_enfa_pivot %>% filter(className != "Mammals",
                           ext_age == "int.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(marginality+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Weighted Marginality")+
  facet_wrap(~className, scales = "free")+
  geom_abline(data = ele_marg_int, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle("Intermediate extinction scenario")+
  theme_bw() +
  mynamestheme

dev.off()

## low extinction scenario
##plot for a low extinction scenario
ele_marg_low <- ele_marg %>% filter(ext == "low")

##plot
png("text/figures/marginality/weighted_enfa/w.marg_low.png",
    width = 20, height = 12, units = "cm", 
    pointsize = 8, res = 300)


vert_enfa_pivot %>% filter(className != "Mammals",
                           ext_age == "low.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(marginality+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Weighted Marginality")+
  facet_wrap(~className, scales = "free")+
  geom_abline(data = ele_marg_low, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle("Low extinction scenario")+
  theme_bw() +
  mynamestheme

dev.off()


#######Specialization

## df elements of models
ele_spe <- w.spe.results %>% select(term, Estimate, class, ext) %>% 
  pivot_wider(names_from = "term",
              values_from=  "Estimate") %>% 
  rename(className = class)

##changing factors name for matching with vert_enfa
ele_spe$className <- factor(ele_spe$className, levels = c("mammals",
                                                            "aves", "reptiles",
                                                            "amphibians"),
                             labels = c("Mammals",
                                        "Birds", "Reptiles", "Amphibians"))

##plot for a high extinction scenario
ele_spe_high <- ele_spe %>% filter(ext == "high")

##plot
png("text/figures/specialization/weighted_enfa/w.spe_high.png",
    width = 20, height = 18, units = "cm", 
    pointsize = 8, res = 300)


vert_enfa_pivot %>% filter(ext_age == "high.age") %>% 
  ggplot(aes(x = log(Ages+1), y = log(specialization+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Weighted Specialization")+
  facet_wrap(~className, scales = "free")+
  geom_abline(data = ele_spe_high, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle("High extinction scenario")+
  theme_bw() +
  mynamestheme

dev.off()

#####Intermediate scenario (No mammals)
##plot for a high extinction scenario
ele_spe_int <- ele_spe %>% filter(className != "Mammals" ,
                                  ext == "int")

##plot
png("text/figures/specialization/weighted_enfa/w.spe_int.png",
    width = 20, height = 12, units = "cm", 
    pointsize = 8, res = 300)


vert_enfa_pivot %>% filter(ext_age == "int.age",
                           className != "Mammals") %>% 
  ggplot(aes(x = log(Ages+1), y = log(specialization+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Weighted Specialization")+
  facet_wrap(~className, scales = "free")+
  geom_abline(data = ele_spe_int, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle("Intermediate extinction scenario")+
  theme_bw() +
  mynamestheme

dev.off()

#####Low ext scenario (No mammals)
##plot for a low extinction scenario
ele_spe_low <- ele_spe %>% filter(className != "Mammals" ,
                                  ext == "low")

##plot
png("text/figures/specialization/weighted_enfa/w.spe_low.png",
    width = 20, height = 12, units = "cm", 
    pointsize = 8, res = 300)


vert_enfa_pivot %>% filter(ext_age == "low.age",
                           className != "Mammals") %>% 
  ggplot(aes(x = log(Ages+1), y = log(specialization+1)))+
  geom_bin2d() +
  scico::scale_fill_scico(palette = "vik",
                          name = "Richness")+
  theme_bw()+
  #geom_smooth(method = "lm", color = "red")+
  xlab("Species ages")+
  ylab("Weighted Specialization")+
  facet_wrap(~className, scales = "free")+
  geom_abline(data = ele_spe_low, aes(slope = Age, intercept = Intercept),
              color = "red", size = 1)+
  ggtitle("Low extinction scenario")+
  theme_bw() +
  mynamestheme

dev.off()
