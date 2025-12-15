
# Biomes 3th --------------------------------------------------------------
library(tidyverse)

### results dataframes

#marginality (3th)
sum_biomes_marg_3th <- read_csv("results/data/processed/sensitivity_analyses/ENFA_3th/sum_biomes_marg_3th.csv")

##biomes that have proportion of significant models > 0.8
sig_marg_3th <- sum_biomes_marg_3th %>% 
                               filter(prop_significant > 0.8) ## all sig relationships are in birds, 
                                                            ## and none is Mediterranean ..., which
                                                            ## was the only biome that had a correlation
                                                            ## < 0.7 regarding the marginality_10th
                                                            ## all relationships are negative


#specialization (3th)
sum_biomes_spe_3th <- read_csv("results/data/processed/sensitivity_analyses/ENFA_3th/sum_biomes_spe_3th.csv")

##biomes that have proportion of significant models > 0.8
sig_spe_3th <- sum_biomes_spe_3th %>% 
                              filter(prop_significant > 0.8) ### all sig relationships are in birds, 
                                                              ## the three biomes had a correlation
                                                               ## < 0.7 regarding the marginality_10th
                                                              ## all relationships are negative,
                                                              ## lets plot
##factor
sig_spe_3th$biome <- factor(sig_spe_3th$biome,
                            levels = c("Tropical & subtropical grasslands savannas and shrublands",
                                       "Montane grasslands and shrublands",
                                       "Deserts and xeric shrublands"))

sig_spe_3th <- sig_spe_3th %>% rename(biome_clean = biome)

######calling bird results
birds_string <- list.files(path = "results/data/processed/sensitivity_analyses/ENFA_3th/AVES")
 
list_birds <- vector(mode = "list", length = length(birds_string))
 
for(i in seq_along(birds_string)){
   
   list_birds[[i]] <- read_csv(paste0("results/data/processed/sensitivity_analyses/ENFA_3th/AVES/",
                                    birds_string[i]))
   
}


birds_results <- do.call("rbind", list_birds)

birds_results$class <- "birds"

#filtering 
birds_spe <- birds_results %>% filter(variable == "specialization_3th") %>% 
    filter(term == "(Intercept)") %>% 
  mutate(biome_clean = gsub("_", " ", biome),
         biome_clean = gsub("&", "&", biome_clean),     # keep & as is
         biome_clean = gsub("  ", " ", biome_clean)) %>% 
   filter(biome_clean %in% c("tropical and subtropical grasslands savannas and shrublands",
                             "montane grasslands and shrublands",
                             "deserts and xeric shrublands"))
##mean intercept
bird_intercept <- birds_spe %>% 
                   group_by(biome_clean) %>% 
                  summarise(mean_intercept = mean(Estimate))


bird_intercept$biome_clean <- factor(bird_intercept$biome_clean,
                                     levels = c("tropical and subtropical grasslands savannas and shrublands",
                                                "montane grasslands and shrublands",
                                                "deserts and xeric shrublands"))

bird_intercept$biome_clean <- factor(bird_intercept$biome_clean,
                                     labels = levels(sig_spe_3th$biome))

sig_spe <- left_join(sig_spe_3th, bird_intercept, by = "biome_clean")

##birds Specialization_3th and high_age
birds_3th <- birds.list.ages_joined[[1]] %>% 
                                     filter(biome_clean %in% c("tropical_and_subtropical_grasslands_savannas_and_shrublands",
                                                               "montane_grasslands_and_shrublands",
                                                               "deserts_and_xeric_shrublands")) %>% 
                  drop_na() %>% 
                  mutate(log_spe = log1p(specialization_3th),
                         log_high_age = log1p(high.age))

#factors for plotting
birds_3th$biome_clean <- factor(birds_3th$biome_clean, 
                                levels = c("tropical_and_subtropical_grasslands_savannas_and_shrublands",
                                           "montane_grasslands_and_shrublands",
                                           "deserts_and_xeric_shrublands"))
birds_3th$biome_clean <- factor(birds_3th$biome_clean,
                                labels = levels(sig_spe$biome_clean))


# range of predictor
x_range <- birds_3th %>%
  group_by(biome_clean) %>%
  summarise(min_x = min(log_high_age),
            max_x = max(log_high_age)) %>% 
  mutate(biome_clean = as.factor(biome_clean))

# merge ranges with coefficients
pred_input <- sig_spe %>% 
  left_join(x_range, by = "biome_clean")

# generate grid of x and predicted y
pred_lines <- pred_input %>%
  mutate(x = map2(min_x, max_x, ~seq(.x, .y, length.out = 200))) %>%
  unnest(x) %>%
  mutate(
    fit = mean_intercept + median_B * x,
    lower = mean_intercept + CI_lower_B * x,
    upper = mean_intercept + CI_upper_B * x
  )

pred_lines$biome_clean <- factor(pred_lines$biome_clean, levels = levels(sig_spe$biome_clean))




##plotting
biome_spe_3th <- ggplot() +
  geom_point(data = birds_3th,
             aes(x = log_high_age, y = log_spe),
             alpha = 0.3, size = 1,
             color = "#E69F00") +
  
  geom_ribbon(data = pred_lines,
              aes(x = x, ymin = lower, ymax = upper, fill = biome_clean),
              alpha = 0.4, colour = "gray") +
  
  geom_line(data = pred_lines,
            aes(x = x, y = fit),
            color = "#E69F00",
            size = 1) +
  
  facet_wrap(~ biome_clean, ncol = 2,
             scales = "free") +
  
  labs(x = "Species age", 
       y = "Specialization (3th)",
       fill = "Biome") +
  
  theme_bw(base_size = 13) +
  mynamestheme+
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "white"))

#saving
svg("figures/Sup/biome_3th/biome_spe_vs_ages_3th.svg",
    width = 10, height = 8)

biome_spe_3th

dev.off()




##### CI for plotting ##########
sig_spe <- sig_spe %>% 
  mutate(biome_clean = str_wrap(biome_clean, width = 25))

sig_spe$biome_clean <- factor(sig_spe$biome_clean,
                                   levels = c("Deserts and xeric\nshrublands",
                                              "Montane grasslands and\nshrublands",
                                              "Tropical & subtropical\ngrasslands savannas and\nshrublands"))

##plotting CI biomes
CI_spe_3th <- ggplot(sig_spe, aes(x = biome_clean,
                                        y = median_B)) +
  geom_point(size = 5.5,
             position = position_dodge(width = 1), 
             color = "#E69F00") +  # Points for median_B
  geom_errorbar(aes(ymin = CI_lower_B,
                    ymax = CI_upper_B), 
                width = 0.2, size = 0.5,
                position = position_dodge(width = 1),
                color = "#E69F00") +  # Error bars
  theme_bw() +
  labs(x = NULL, y = "Beta") +
  ylim(-0.30, 0.30) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.75) +
  coord_flip() +
  scale_x_discrete(position = "top") + 
  mynamestheme +
  theme(
    panel.background = element_rect(fill = "gray98"),  # Light gray background
    panel.grid.major.y = element_line(color = "grey79",
                                      size = 0.4),
    panel.grid.major.x = element_blank(),# Subtle grid for reference
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 16),  # Increase x-axis text size
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 17),
    #axis.text.y.right = element_text(size = 15), 
    strip.text = element_text(size = 16),
    legend.position = "none")


##saving
svg("figures/Sup/biome_3th/CI_spe_biomes.svg",
    width = 6, height = 6)

CI_spe_3th

dev.off()
