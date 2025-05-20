# Plotting biomes results -------------------------------------------------


##sourcing the libraries and the directories
source(file.path(getwd(), "/source.R"))



###############################################################################
#########   most commented lines are parts of the script  ###################
########    calling results of data generated from the server ##################
########   The Rscript that generated these results is       #################
########     the "biomes_pgls.R" described in the repo     ##################
##############################################################################


##calling data


# Mammals -----------------------------------------------------------------

# mammals_string <- list.files(path = "C:/Users/carlo/OneDrive/Desktop/niche_position2/biomes2/MAMMALIA")
# 
# list_mam <- vector(mode = "list", length = length(mammals_string))
# 
# for(i in seq_along(mammals_string)){
#   
#   list_mam[[i]] <- read_csv(paste0("C:/Users/carlo/OneDrive/Desktop/niche_position2/biomes2/MAMMALIA/",
#                                    mammals_string[i]))
#   
# }
# 
# ######## marginality ############
# mam_results <- do.call("rbind", list_mam)
# 
# mam_results$class <- "Mammals"
# 
# 
# ##marginality
# mam_marg <- mam_results %>% filter(variable == "marginality") %>% 
#   mutate(term = str_replace(term, "^log\\(.*\\)$", "Beta")) %>% 
#   filter(term == "Beta") %>% 
#   dplyr::rename(p_value = `Pr(>|t|)`)
# 
# mam_marg$ext <- factor(mam_marg$ext, levels = c("low",
#                                                     "int",
#                                                     "high"),
#                          ordered = TRUE)
# 
# write_csv(mam_marg, file = "results/data/processed/biomes/mam_marg_biomes.csv")

#read
mam_marg_biomes <- read_csv("results/data/processed/biomes/mam_marg_biomes.csv")

##summarizing
sum_mam_marg <- mam_marg_biomes %>% group_by(biome, ext) %>% 
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

sum_mam_marg$class <- "mammals"

sum_mam_marg$variable <- "marginality"


########### specialization #######################

# mam_spe <- mam_results %>% filter(variable == "specialization") %>% 
#   mutate(term = str_replace(term, "^log\\(.*\\)$", "Beta")) %>% 
#   filter(term == "Beta") %>% 
#   dplyr::rename(p_value = `Pr(>|t|)`)
# 
# mam_spe$ext <- factor(mam_spe$ext, levels = c("low",
#                                                 "int",
#                                                 "high"),
#                        ordered = TRUE)
# 
# write_csv(mam_spe, file = "results/data/processed/biomes/mam_spe_biomes.csv")

#read
mam_spe_biomes <- read_csv("results/data/processed/biomes/mam_spe_biomes.csv")


##summarizing
sum_mam_spe <- mam_spe %>% group_by(biome, ext) %>% 
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

sum_mam_spe$class <- "mammals"

sum_mam_spe$variable <- "specialization"



# Birds -------------------------------------------------------------------

# birds_string <- list.files(path = "C:/Users/carlo/OneDrive/Desktop/niche_position2/biomes2/AVES")
# 
# list_birds <- vector(mode = "list", length = length(birds_string))
# 
# for(i in seq_along(birds_string)){
#   
#   list_birds[[i]] <- read_csv(paste0("C:/Users/carlo/OneDrive/Desktop/niche_position2/biomes2/AVES/",
#                                    birds_string[i]))
#   
# }
# 
# ######## marginality ############
# birds_results <- do.call("rbind", list_birds)
# 
# birds_results$class <- "birds"
# 
# 
# ##marginality
# birds_marg <- birds_results %>% filter(variable == "marginality") %>% 
#   mutate(term = str_replace(term, "^log\\(.*\\)$", "Beta")) %>% 
#   filter(term == "Beta") %>% 
#   dplyr::rename(p_value = `Pr(>|t|)`)
# 
# birds_marg$ext <- factor(birds_marg$ext, levels = c("low",
#                                                 "int",
#                                                 "high"),
#                        ordered = TRUE)
# 
# #write
# write_csv(birds_marg, file = "results/data/processed/biomes/birds_marg_biomes.csv")

#read
birds_marg_biomes <- read_csv("results/data/processed/biomes/birds_marg_biomes.csv")



##summarizing
sum_birds_marg <- birds_marg_biomes %>% group_by(biome, ext) %>% 
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

sum_birds_marg$class <- "birds"

sum_birds_marg$variable <- "marginality"


########### specialization #######################

# birds_spe <- birds_results %>% filter(variable == "specialization") %>%
#   mutate(term = str_replace(term, "^log\\(.*\\)$", "Beta")) %>%
#   filter(term == "Beta") %>%
#   dplyr::rename(p_value = `Pr(>|t|)`)
# 
# birds_spe$ext <- factor(birds_spe$ext, levels = c("low",
#                                               "int",
#                                               "high"),
#                       ordered = TRUE)
# 
# #write
# write_csv(birds_spe, file = "results/data/processed/biomes/birds_spe_biomes.csv")

#read
birds_spe_biomes <- read_csv("results/data/processed/biomes/birds_spe_biomes.csv")


##summarizing
sum_birds_spe <- birds_spe_biomes %>% group_by(biome, ext) %>% 
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

sum_birds_spe$class <- "birds"

sum_birds_spe$variable <- "specialization"


# reptiles -------------------------------------------------------------------

reptiles_string <- list.files(path = "C:/Users/carlo/OneDrive/Desktop/niche_position2/biomes2/REPTILIA")

list_reptiles <- vector(mode = "list", length = length(reptiles_string))

for(i in seq_along(reptiles_string)){
  
  list_reptiles[[i]] <- read_csv(paste0("C:/Users/carlo/OneDrive/Desktop/niche_position2/biomes2/REPTILIA/",
                                        reptiles_string[i]))
  
}

######## marginality ############
# reptiles_results <- do.call("rbind", list_reptiles)
# 
# reptiles_results$class <- "reptiles"
# 
# 
# ##marginality
# reptiles_marg <- reptiles_results %>% filter(variable == "marginality") %>% 
#   mutate(term = str_replace(term, "^log\\(.*\\)$", "Beta")) %>% 
#   filter(term == "Beta") %>% 
#   dplyr::rename(p_value = `Pr(>|t|)`)
# 
# reptiles_marg$ext <- factor(reptiles_marg$ext, levels = c("low",
#                                                     "int",
#                                                     "high"),
#                          ordered = TRUE)
# 
# 
# #write
# write_csv(reptiles_marg, file = "results/data/processed/biomes/reptiles_marg_biomes.csv")

#read
reptiles_marg_biomes <- read_csv("results/data/processed/biomes/reptiles_marg_biomes.csv")


##summarizing
sum_reptiles_marg <- reptiles_marg_biomes %>% group_by(biome, ext) %>% 
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

sum_reptiles_marg$class <- "reptiles"

sum_reptiles_marg$variable <- "marginality"


########### specialization #######################

# reptiles_spe <- reptiles_results %>% filter(variable == "specialization") %>% 
#   mutate(term = str_replace(term, "^log\\(.*\\)$", "Beta")) %>% 
#   filter(term == "Beta") %>% 
#   dplyr::rename(p_value = `Pr(>|t|)`)
# 
# reptiles_spe$ext <- factor(reptiles_spe$ext, levels = c("low",
#                                                   "int",
#                                                   "high"),
#                         ordered = TRUE)
# 
# #write
# write_csv(reptiles_spe, file = "results/data/processed/biomes/reptiles_spe_biomes.csv")

#read
reptiles_spe_biomes <- read_csv("results/data/processed/biomes/reptiles_spe_biomes.csv")


##summarizing
sum_reptiles_spe <- reptiles_spe_biomes %>% group_by(biome, ext) %>% 
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

sum_reptiles_spe$class <- "reptiles"

sum_reptiles_spe$variable <- "specialization"


# amphibians -------------------------------------------------------------------

# amphibians_string <- list.files(path = "C:/Users/carlo/OneDrive/Desktop/niche_position2/biomes2/AMPHIBIA")
# 
# list_amphibians <- vector(mode = "list", length = length(amphibians_string))
# 
# for(i in seq_along(amphibians_string)){
#   
#   list_amphibians[[i]] <- read_csv(paste0("C:/Users/carlo/OneDrive/Desktop/niche_position2/biomes2/AMPHIBIA/",
#                                      amphibians_string[i]))
#   
# }
# 
# ######## marginality ############
# amphibians_results <- do.call("rbind", list_amphibians)
# 
# amphibians_results$class <- "amphibians"
# 
# 
# ##marginality
# amphibians_marg <- amphibians_results %>% filter(variable == "marginality") %>% 
#   mutate(term = str_replace(term, "^log\\(.*\\)$", "Beta")) %>% 
#   filter(term == "Beta") %>% 
#   dplyr::rename(p_value = `Pr(>|t|)`)
# 
# amphibians_marg$ext <- factor(amphibians_marg$ext, levels = c("low",
#                                                     "int",
#                                                     "high"),
#                          ordered = TRUE)
# 
# 
# #write
# write_csv(amphibians_marg, file = "results/data/processed/biomes/amphibians_marg_biomes.csv")

#read
amphibians_marg_biomes <- read_csv("results/data/processed/biomes/amphibians_marg_biomes.csv")


##summarizing
sum_amphibians_marg <- amphibians_marg_biomes %>% group_by(biome, ext) %>% 
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

sum_amphibians_marg$class <- "amphibians"

sum_amphibians_marg$variable <- "marginality"


########### specialization #######################

# amphibians_spe <- amphibians_results %>% filter(variable == "specialization") %>% 
#   mutate(term = str_replace(term, "^log\\(.*\\)$", "Beta")) %>% 
#   filter(term == "Beta") %>% 
#   dplyr::rename(p_value = `Pr(>|t|)`)
# 
# amphibians_spe$ext <- factor(amphibians_spe$ext, levels = c("low",
#                                                   "int",
#                                                   "high"),
#                         ordered = TRUE)
# 
# #write
# write_csv(amphibians_spe, file = "results/data/processed/biomes/amphibians_spe_biomes.csv")

#read
amphibians_spe_biomes <- read_csv("results/data/processed/biomes/amphibians_spe_biomes.csv")


##summarizing
sum_amphibians_spe <- amphibians_spe %>% group_by(biome, ext) %>% 
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

sum_amphibians_spe$class <- "amphibians"

sum_amphibians_spe$variable <- "specialization"


# general summaries -------------------------------------------------------


###############marginality###########

sum_biomes_marg <- rbind(sum_mam_marg, sum_birds_marg, sum_reptiles_marg,
                         sum_amphibians_marg)


#biomes 
biomes <- unique(sum_biomes_marg$biome)  

sum_biomes_marg$biome <- factor(sum_biomes_marg$biome, labels = c("Deserts and xeric shrublands",
                                                                  "Flooded grasslands and savannas",
                                                                  "Mediterranean forests woodlands and scrub",
                                                                  "Montane grasslands and shrublands",
                                                                  "Temperate broadleaf & mixed forests",
                                                                  "Temperate grasslands savannas and shrublands",
                                                                  "Tropical & subtropical coniferous forests",
                                                                  "Tropical & subtropical dry broadleaf forests",
                                                                  "Tropical & subtropical grasslands savannas and shrublands",
                                                                  "Tropical & subtropical moist broadleaf forests",
                                                                  "Mangroves"))
write_csv(sum_biomes_marg, file = "results/data/processed/biomes/marginality/sum_biomes_marg.csv")

##significant table, intermediate extinction and the proportion of significant pvalues equal or above 0.5
sum_biomes_marg_sig <- sum_biomes_marg %>% filter(ext == "high",
                                                  prop_significant >= 0.5) %>% 
  mutate(significance_level = case_when(
    prop_significant >= 0.95 ~ "Strict (≥95%)",
    prop_significant >= 0.80 ~ "Moderate (≥80%)",
    prop_significant >= 0.50 ~ "Lenient (≥50%)"))





write_xlsx(sum_biomes_marg_sig, path = "results/data/processed/biomes/marginality/sum_biomes_sig.xlsx")


#########specialization##########
sum_biomes_spe <- rbind(sum_mam_spe, sum_birds_spe, sum_reptiles_spe,
                        sum_amphibians_spe)


#biomes 
biomes <- unique(sum_biomes_spe$biome)  

sum_biomes_spe$biome <- factor(sum_biomes_spe$biome, labels = c("Deserts and xeric shrublands",
                                                                "Flooded grasslands and savannas",
                                                                "Mediterranean forests woodlands and scrub",
                                                                "Montane grasslands and shrublands",
                                                                "Temperate broadleaf & mixed forests",
                                                                "Temperate grasslands savannas and shrublands",
                                                                "Tropical & subtropical coniferous forests",
                                                                "Tropical & subtropical dry broadleaf forests",
                                                                "Tropical & subtropical grasslands savannas and shrublands",
                                                                "Tropical & subtropical moist broadleaf forests",
                                                                "Mangroves"))
write_csv(sum_biomes_spe, file = "results/data/processed/biomes/specialization/sum_biomes_spe.csv")

##significant table, intermediate extinction and the proportion of significant pvalues equal or above 0.5
sum_biomes_spe_sig <- sum_biomes_spe %>% filter(ext == "high",
                                                prop_significant >= 0.5) %>% 
  mutate(significance_level = case_when(
    prop_significant >= 0.95 ~ "Strict (≥95%)",
    prop_significant >= 0.80 ~ "Moderate (≥80%)",
    prop_significant >= 0.50 ~ "Lenient (≥50%)"
  ))


write_xlsx(sum_biomes_spe_sig, path = "results/data/processed/biomes/specialization/sum_biomes_sig.xlsx")


#########plots as error bars for the beta #############

#binding significative results

df <- rbind(sum_biomes_marg_sig, sum_biomes_spe_sig)



# Convert significance to factor for proper ordering in legend
df$significance_level <- factor(df$significance_level, 
                                levels = c("Strict (≥95%)", "Moderate (≥80%)", 
                                           "Lenient (≥50%)"))


#ordering factors for ploting
df$biome <- factor(df$biome, levels = c(
  "Deserts and xeric shrublands",
  "Mediterranean forests woodlands and scrub",
  "Temperate broadleaf & mixed forests",
  "Flooded grasslands and savannas",
  "Tropical & subtropical grasslands savannas and shrublands",
  "Tropical & subtropical coniferous forests",
  "Mangroves",
  "Tropical & subtropical moist broadleaf forests"))

#breaking the biomes text in the figure
df$biome <- str_wrap(df$biome, width = 25)

df$class <- factor(df$class, levels = c("reptiles","birds","mammals" ))

# Define custom colors for classes
class_colors <- c(
  "birds" = "#E69F00",   # Strong golden yellow
  
  "reptiles" = "#018571" # Strong teal green
)

##principal result
png("text/figures/biomes_CI_high.png", 
    width = 30, height = 20,
    units = "cm", pointsize = 8, res = 300)


ggplot(df, aes(x = biome, y = median_B, color = class,
               shape = significance_level)) +
  geom_point(size = 5.5, position = position_dodge(width = 1)) +  # Points for median_B
  geom_errorbar(aes(ymin = CI_lower_B, ymax = CI_upper_B), 
                width = 0.4, position = position_dodge(width = 1)) +  # Error bars
  theme_bw() +
  labs(x = NULL, y = "Beta", color = "Class") +
  ylim(-0.45, 0.45) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.75) +
  coord_flip() +
  facet_wrap(~variable, labeller = as_labeller(c(
    marginality = "Marginality",
    specialization = "Specialization"
  ))) +
  scale_color_manual(values = class_colors, 
                     guide = "none") +
  scale_shape_manual(values = c(
    "Strict (≥95%)" = 16,    # Solid Circle
    "Moderate (≥80%)" = 17,  # Triangle
    "Lenient (≥50%)" = 15    # Square
  ), name = NULL) +
  
  mynamestheme +
  theme(
    panel.background = element_rect(fill = "gray98"),  # Light gray background
    panel.grid.major.y = element_line(color = "grey49",
                                      size = 0.75),
    panel.grid.major.x = element_blank(),# Subtle grid for reference
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 16),  # Increase x-axis text size
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 17),
    strip.text = element_text(size = 16),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.text = element_text(size = 14),
    legend.position = "bottom")


dev.off()


####################supplementary#######################


## Intermediate extinction 

##significant table, intermediate extinction and the proportion of significant pvalues equal or above 0.5
sum_biomes_marg_sig <- sum_biomes_marg %>% filter(ext == "int",
                                                  prop_significant >= 0.5) %>% 
  mutate(significance_level = case_when(
    prop_significant >= 0.95 ~ "Strict (≥95%)",
    prop_significant >= 0.80 ~ "Moderate (≥80%)",
    prop_significant >= 0.50 ~ "Lenient (≥50%)"
  ))


sum_biomes_spe_sig <- sum_biomes_spe %>% filter(ext == "int",
                                                prop_significant >= 0.5) %>% 
  mutate(significance_level = case_when(
    prop_significant >= 0.95 ~ "Strict (≥95%)",
    prop_significant >= 0.80 ~ "Moderate (≥80%)",
    prop_significant >= 0.50 ~ "Lenient (≥50%)"
  ))



df <- rbind(sum_biomes_marg_sig, sum_biomes_spe_sig)



# Convert significance to factor for proper ordering in legend
df$significance_level <- factor(df$significance_level, 
                                levels = c("Strict (≥95%)", "Moderate (≥80%)", 
                                           "Lenient (≥50%)"))


#ordering factors for ploting
df$biome <- factor(df$biome, levels = c(
  "Deserts and xeric shrublands",
  "Mediterranean forests woodlands and scrub",
  "Temperate broadleaf & mixed forests",
  "Flooded grasslands and savannas",
  "Tropical & subtropical grasslands savannas and shrublands",
  "Tropical & subtropical coniferous forests",
  "Mangroves",
  "Tropical & subtropical moist broadleaf forests"))

#breaking the biomes text in the figure
df$biome <- str_wrap(df$biome, width = 25)

df$class <- factor(df$class, levels = c("reptiles","birds","mammals" ))

# Define custom colors for classes
class_colors <- c(
  "birds" = "#E69F00",   # Strong golden yellow
  "mammals" = "#E66101", # Deep orange
  "reptiles" = "#018571" # Strong teal green
)

##principal result
png("text/figures/Sup/biomes_CI_int.png", 
    width = 30, height = 20,
    units = "cm", pointsize = 8, res = 300)


ggplot(df, aes(x = biome, y = median_B, color = class,
               shape = significance_level)) +
  geom_point(size = 5.5, position = position_dodge(width = 1)) +  # Points for median_B
  geom_errorbar(aes(ymin = CI_lower_B, ymax = CI_upper_B), 
                width = 0.4, position = position_dodge(width = 1)) +  # Error bars
  theme_bw() +
  labs(x = NULL, y = "Beta", color = "Class") +
  ylim(-0.45, 0.45) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  facet_wrap(~variable, labeller = as_labeller(c(
    marginality = "Marginality",
    specialization = "Specialization"
  ))) +
  scale_color_manual(values = class_colors, 
                     guide = "none") +
  scale_shape_manual(values = c(
    "Strict (≥95%)" = 16,    # Solid Circle
    "Moderate (≥80%)" = 17,  # Triangle
    "Lenient (≥50%)" = 15    # Square
  ), name = NULL) +
  mynamestheme +
  theme(
    panel.background = element_rect(fill = "gray98"),  # Light gray background
    panel.grid.major = element_line(color = "gray90"), # Subtle grid for reference
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 16),  # Increase x-axis text size
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 17),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.text = element_text(size = 14) )

dev.off()


## Low extinction 

##significant table, intermediate extinction and the proportion of significant pvalues equal or above 0.5
sum_biomes_marg_sig <- sum_biomes_marg %>% filter(ext == "low",
                                                  prop_significant >= 0.5) %>% 
  mutate(significance_level = case_when(
    prop_significant >= 0.95 ~ "Strict (≥95%)",
    prop_significant >= 0.80 ~ "Moderate (≥80%)",
    prop_significant >= 0.50 ~ "Lenient (≥50%)"
  ))


sum_biomes_spe_sig <- sum_biomes_spe %>% filter(ext == "low",
                                                prop_significant >= 0.5) %>% 
  mutate(significance_level = case_when(
    prop_significant >= 0.95 ~ "Strict (≥95%)",
    prop_significant >= 0.80 ~ "Moderate (≥80%)",
    prop_significant >= 0.50 ~ "Lenient (≥50%)"
  ))



df <- rbind(sum_biomes_marg_sig, sum_biomes_spe_sig)



# Convert significance to factor for proper ordering in legend
df$significance_level <- factor(df$significance_level, 
                                levels = c("Strict (≥95%)", "Moderate (≥80%)", 
                                           "Lenient (≥50%)"))


#ordering factors for ploting
df$biome <- factor(df$biome, levels = c(
  "Deserts and xeric shrublands",
  "Mediterranean forests woodlands and scrub",
  "Temperate broadleaf & mixed forests",
  "Flooded grasslands and savannas",
  "Tropical & subtropical grasslands savannas and shrublands",
  "Tropical & subtropical coniferous forests",
  "Mangroves",
  "Tropical & subtropical moist broadleaf forests"))

#breaking the biomes text in the figure
df$biome <- str_wrap(df$biome, width = 25)

df$class <- factor(df$class, levels = c("amphibians","reptiles","birds","mammals" ))

# Define custom colors for classes
class_colors <- c(
  "birds" = "#E69F00",   # Strong golden yellow
  "mammals" = "#E66101", # Deep orange
  "reptiles" = "#018571", # Strong teal green
  "amphibians" = "#7fbf7b"
)

##principal result
png("text/figures/Sup/biomes_CI_low.png", 
    width = 30, height = 20,
    units = "cm", pointsize = 8, res = 300)

ggplot(df, aes(x = biome, y = median_B, color = class,
               shape = significance_level)) +
  geom_point(size = 5.5, position = position_dodge(width = 1)) +  # Points for median_B
  geom_errorbar(aes(ymin = CI_lower_B, ymax = CI_upper_B), 
                width = 0.4, position = position_dodge(width = 1)) +  # Error bars
  theme_bw() +
  labs(x = NULL, y = "Beta", color = "Class") +
  ylim(-0.45, 0.45) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  facet_wrap(~variable, labeller = as_labeller(c(
    marginality = "Marginality",
    specialization = "Specialization"
  ))) +
  scale_color_manual(values = class_colors, 
                     guide = "none") +
  scale_shape_manual(values = c(
    "Strict (≥95%)" = 16,    # Solid Circle
    "Moderate (≥80%)" = 17,  # Triangle
    "Lenient (≥50%)" = 15    # Square
  ), name = NULL) +
  mynamestheme +
  theme(
    panel.background = element_rect(fill = "gray98"),  # Light gray background
    panel.grid.major = element_line(color = "gray90"), # Subtle grid for reference
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 16),  # Increase x-axis text size
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 17),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.text = element_text(size = 14) )



dev.off()