#########reading results from the weighted pgls in 100 random trees####

##sourcing the libraries and the directories
source(file.path(getwd(), "/source.R"))

###############################################################################
#########   most commented lines are parts of the script  ###################
########    calling results of data generated from the server ##################
########   The Rscript that generated these results is       #################
########     the "weighted_pgls.R" described in the repo     ##################
##############################################################################

# Mammals -----------------------------------------------------------------

##This data was obtained from the server and represents the PGLS models for the 100 random trees

#mam_w_string <- list.files(path = "C:/Users/carlo/OneDrive/Desktop/niche_position2/processed/mammals")

#list_mam <- vector(mode = "list", length = length(mam_w_string))

#for(i in seq_along(mam_w_string)){

#list_mam[[i]] <- read_csv(paste0("C:/Users/carlo/OneDrive/Desktop/niche_position2/processed/mammals/",
#  mam_w_string[i]))

#}

##dataset

#mam_w_results <- do.call("rbind", list_mam)

#mam_w_results$class <- "Mammals"

# weighted marginality ----------------------------------------------------


#mam_w_marg <- mam_w_results %>% filter(biome == "w.marginality") %>% 
#mutate(term = str_replace(term, "^log\\(.*\\)$", "Beta")) %>% 
#filter(term == "Beta") %>% 
# dplyr::rename(p_value = `Pr(>|t|)`)

#mam_w_marg$ext <- as.factor(mam_w_marg$ext)

#mam_w_marg$ext <- factor(mam_w_marg$ext, levels = c("low",
# "int",
#   "high"),
#ordered = TRUE)
#mam_w_marg$class <- "Mammals"

##saving
#write_csv(mam_w_marg,
# file = "results/data/processed/weighted_enfa/mammals_marginality.csv")

##reading 
mam_w_marg <- read_csv("results/data/processed/weighted_enfa/mammals_marginality.csv")


###summary
summary_mam_marg <- mam_w_marg %>% group_by(ext) %>% 
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

summary_mam_marg$class <- "mammals"

summary_mam_marg$variable <- "marginality"


# weighted specialization -------------------------------------------------


#mam_w_spe <- mam_w_results %>% filter(biome == "w.specialization") %>% 
#mutate(term = str_replace(term, "^log\\(.*\\)$", "Beta")) %>% 
#filter(term == "Beta") %>% 
# dplyr::rename(p_value = `Pr(>|t|)`)

#mam_w_spe$ext <- as.factor(mam_w_spe$ext)

#mam_w_spe$ext <- factor(mam_w_spe$ext, levels = c("low",
# "int",
#  "high"),
#ordered = TRUE)

##saving
#write_csv(mam_w_spe,
# file = "results/data/processed/weighted_enfa/mammals_specialization.csv")

#reading
mam_w_spe <- read_csv("results/data/processed/weighted_enfa/mammals_specialization.csv")

###summary

summary_mam_spe <- mam_w_spe %>% group_by(ext) %>% 
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

summary_mam_spe$class <- "mammals"

summary_mam_spe$variable <- "specialization"


# Birds -------------------------------------------------------------------


# birds_w_string <- list.files(path = "C:/Users/carlo/OneDrive/Desktop/niche_position2/processed/birds")
# 
# list_birds <- vector(mode = "list", length = length(birds_w_string))
# 
# for(i in seq_along(birds_w_string)){
#   
#   list_birds[[i]] <- read_csv(paste0("C:/Users/carlo/OneDrive/Desktop/niche_position2/processed/birds/",
#                                    birds_w_string[i]))
#   
# }
# 
# ##dataset
# birds_w_results <- do.call("rbind", list_birds)
# 
# ##class
# birds_w_results$class <- "Birds"

# weighted marginality ----------------------------------------------------


# birds_w_marg <- birds_w_results %>% filter(biome == "w.marginality") %>% 
#   mutate(term = str_replace(term, "^log\\(.*\\)$", "Beta")) %>% 
#   filter(term == "Beta") %>% 
#   dplyr::rename(p_value = `Pr(>|t|)`)
# 
# birds_w_marg$ext <- as.factor(birds_w_marg$ext)
# 
# birds_w_marg$ext <- factor(birds_w_marg$ext, levels = c("low",
#                                                     "int",
#                                                     "high"),
#                          ordered = TRUE)
# 
# ##saving
# write_csv(birds_w_marg,
#           file = "results/data/processed/weighted_enfa/birds_marginality.csv")

birds_w_marg <- read_csv("results/data/processed/weighted_enfa/birds_marginality.csv")


###summary
summary_birds_marg <- birds_w_marg %>% group_by(ext) %>% 
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


summary_birds_marg$class <- "birds"

summary_birds_marg$variable <- "marginality"

# weighted specialization -------------------------------------------------


# birds_w_spe <- birds_w_results %>% filter(biome == "w.specialization") %>% 
#   mutate(term = str_replace(term, "^log\\(.*\\)$", "Beta")) %>% 
#   filter(term == "Beta") %>% 
#   dplyr::rename(p_value = `Pr(>|t|)`)
# 
# birds_w_spe$ext <- as.factor(birds_w_spe$ext)
# 
# birds_w_spe$ext <- factor(birds_w_spe$ext, levels = c("low",
#                                                   "int",
#                                                   "high"),
#                         ordered = TRUE)
# 
# ##saving
# write_csv(birds_w_spe,
#           file = "results/data/processed/weighted_enfa/birds_specialization.csv")

birds_w_spe <- read_csv("results/data/processed/weighted_enfa/birds_specialization.csv")


###summary

summary_birds_spe <- birds_w_spe %>% group_by(ext) %>% 
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

summary_birds_spe$class <- "birds"

summary_birds_spe$variable <- "specialization"


# Reptiles ----------------------------------------------------------------



# reptiles_w_string <- list.files(path = "C:/Users/carlo/OneDrive/Desktop/niche_position2/processed/reptiles")
# 
# list_reptiles <- vector(mode = "list", length = length(reptiles_w_string))
# 
# for(i in seq_along(reptiles_w_string)){
#   
#   list_reptiles[[i]] <- read_csv(paste0("C:/Users/carlo/OneDrive/Desktop/niche_position2/processed/reptiles/",
#                                      reptiles_w_string[i]))
#   
# }
# 
# ##dataset
# reptiles_w_results <- do.call("rbind", list_reptiles)
# 
# 
# reptiles_w_results$class <- "Reptiles"
# 
# 
# # weighted marginality ----------------------------------------------------
# 
# 
# reptiles_w_marg <- reptiles_w_results %>% filter(biome == "w.marginality") %>% 
#   mutate(term = str_replace(term, "^log\\(.*\\)$", "Beta")) %>% 
#   filter(term == "Beta") %>% 
#   dplyr::rename(p_value = `Pr(>|t|)`)
# 
# reptiles_w_marg$ext <- as.factor(reptiles_w_marg$ext)
# 
# reptiles_w_marg$ext <- factor(reptiles_w_marg$ext, levels = c("low",
#                                                         "int",
#                                                         "high"),
#                            ordered = TRUE)
# 
# ##saving
# write_csv(reptiles_w_marg,
#           file = "results/data/processed/weighted_enfa/reptiles_marginality.csv")

#reading
reptiles_w_marg <- read_csv("results/data/processed/weighted_enfa/reptiles_marginality.csv")

###summary
summary_reptiles_marg <- reptiles_w_marg %>% group_by(ext) %>% 
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


summary_reptiles_marg$class <- "reptiles"

summary_reptiles_marg$variable <- "marginality"



# weighted specialization -------------------------------------------------


# reptiles_w_spe <- reptiles_w_results %>% filter(biome == "w.specialization") %>% 
#   mutate(term = str_replace(term, "^log\\(.*\\)$", "Beta")) %>% 
#   filter(term == "Beta") %>% 
#   dplyr::rename(p_value = `Pr(>|t|)`)
# 
# reptiles_w_spe$ext <- as.factor(reptiles_w_spe$ext)
# 
# reptiles_w_spe$ext <- factor(reptiles_w_spe$ext, levels = c("low",
#                                                       "int",
#                                                       "high"),
#                           ordered = TRUE)
# 
# ##saving
# write_csv(reptiles_w_spe,
#           file = "results/data/processed/weighted_enfa/reptiles_specialization.csv")

#reading
reptiles_w_spe <- read_csv("results/data/processed/weighted_enfa/reptiles_specialization.csv")

###summary

summary_reptiles_spe <- reptiles_w_spe %>% group_by(ext) %>% 
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

summary_reptiles_spe$class <- "reptiles"

summary_reptiles_spe$variable <- "specialization"



# Amphibians --------------------------------------------------------------

# amp_w_string <- list.files(path = "C:/Users/carlo/OneDrive/Desktop/niche_position2/processed/amphibians")
# 
# list_amp <- vector(mode = "list", length = length(amp_w_string))
# 
# for(i in seq_along(amp_w_string)){
#   
#   list_amp[[i]] <- read_csv(paste0("C:/Users/carlo/OneDrive/Desktop/niche_position2/processed/amphibians/",
#                                    amp_w_string[i]))
#   
# }
# 
# ##dataset
# amp_w_results <- do.call("rbind", list_amp)
# 
# amp_w_results$class <- "Amphibians"
# 
# # weighted marginality ----------------------------------------------------
# 
# 
# amp_w_marg <- amp_w_results %>% filter(biome == "w.marginality") %>% 
#   mutate(term = str_replace(term, "^log\\(.*\\)$", "Beta")) %>% 
#   filter(term == "Beta") %>% 
#   dplyr::rename(p_value = `Pr(>|t|)`)
# 
# amp_w_marg$ext <- as.factor(amp_w_marg$ext)
# 
# amp_w_marg$ext <- factor(amp_w_marg$ext, levels = c("low",
#                                                     "int",
#                                                     "high"),
#                          ordered = TRUE)
# 
# ##saving
# write_csv(amp_w_marg,
#           file = "results/data/processed/weighted_enfa/amphibians_marginality.csv")

#reading
amp_w_marg <- read_csv("results/data/processed/weighted_enfa/amphibians_marginality.csv")

###summary
summary_amp_marg <- amp_w_marg %>% group_by(ext) %>% 
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

summary_amp_marg$class <- "amphibians"

summary_amp_marg$variable <- "marginality"


# weighted specialization -------------------------------------------------


# amp_w_spe <- amp_w_results %>% filter(biome == "w.specialization") %>% 
#   mutate(term = str_replace(term, "^log\\(.*\\)$", "Beta")) %>% 
#   filter(term == "Beta") %>% 
#   dplyr::rename(p_value = `Pr(>|t|)`)
# 
# amp_w_spe$ext <- as.factor(amp_w_spe$ext)
# 
# amp_w_spe$ext <- factor(amp_w_spe$ext, levels = c("low",
#                                                   "int",
#                                                   "high"),
#                         ordered = TRUE)
# 
# 
# #saving
# write_csv(amp_w_spe,
#           file = "results/data/processed/weighted_enfa/amphibians_specialization.csv")
#reading
amp_w_spe <- read_csv("results/data/processed/weighted_enfa/amphibians_specialization.csv")

###summary

summary_amp_spe <- amp_w_spe %>% group_by(ext) %>% 
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


summary_amp_spe$class <- "amphibians"

summary_amp_spe$variable <- "specialization"


# Confidence interval plots -----------------------------------------------

############# Weighted marginality
summary_w_marg <- rbind(summary_mam_marg, summary_birds_marg, summary_reptiles_marg,
                        summary_amp_marg)

summary_w_marg$class <-  factor(summary_w_marg$class, levels = c("Amphibians",
                                                                 "Reptiles",
                                                                 "Birds",
                                                                 "Mammals"),
                                ordered = TRUE)


##saving
write_csv(summary_w_marg, file = "results/data/processed/weighted_enfa/sum_weighted_marg.csv")


############### Weighted specialization


summary_w_spe <- rbind(summary_mam_spe, summary_birds_spe, summary_reptiles_spe,
                       summary_amp_spe)

summary_w_spe$class <-  factor(summary_w_spe$class, levels = c("Amphibians",
                                                               "Reptiles",
                                                               "Birds",
                                                               "Mammals"),
                               labels = c("Amphibians",
                                          "Reptiles",
                                          "Birds",
                                          "Mammals"),
                               ordered = TRUE)


##saving
write_csv(summary_w_spe, file = "results/data/processed/weighted_enfa/sum_weighted_spe.csv")



##########################New plots #####################

##Binding summaries
sum_tot <- rbind(summary_w_marg, summary_w_spe)


##defining colors for each class
class_colors <- c(
  "Mammals" = "#E66101",
  "Birds" = "#E69F00",   # Strong golden yellow
  
  "Reptiles" = "#018571", # Strong teal green
  "Amphibians" = "#7fbf7b"
)

##High extinction
png("text/figures/weighted_CI.png", 
    width = 25, height = 15,
    units = "cm", pointsize = 8, res = 300)


sum_tot %>% filter(ext == "high") %>% 
  ggplot(aes(y = class, color = class)) +
  geom_point(aes(x = median_B), size = 5, position = position_dodge(width = 0.3)) +
  geom_linerange(aes(xmin = CI_lower_B, xmax = CI_upper_B), linewidth = 1.5)+
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
  facet_wrap(~variable, ncol = 2, labeller = as_labeller(c(
    marginality = "Marginality",
    specialization = "Specialization"
  )))

dev.off()


###P values distribution

#general results
gen_results <- rbind(mam_w_results, birds_w_results,
                     reptiles_w_results, amp_w_results) %>% 
  
  dplyr::rename(p_value = `Pr(>|t|)`,
                variable = "biome")


##saving
write_csv(gen_results, "results/data/processed/weighted_enfa/general_results.csv")

gen_results <- gen_results %>% mutate(term = str_replace(term,
                                                         "^log\\(.*\\)$", "Beta")) %>% 
  filter(term == "Beta")

gen_results$ext <- factor(gen_results$ext, levels = c("low",
                                                      "int",
                                                      "high"),
                          ordered = TRUE)
gen_results$class <- factor(gen_results$class, levels = c("Mammals",
                                                          "Birds",
                                                          "Reptiles",
                                                          "Amphibians"))

##filling purpose
x_1 <- gen_results %>% filter(ext == "high") %>% 
  group_by(class, variable) %>% 
  summarise( prop_significant = mean(p_value < 0.05))

gen_high <- gen_results %>% filter(ext == "high") %>% 
  left_join(x_1, by = c("class", "variable")) %>% 
  mutate(alpha_value = case_when(
    prop_significant >= 0.95 ~ 1,
    prop_significant >= 0.80 & prop_significant < 0.95 ~ 0.5,
    TRUE ~ 0
  ))

##p-values

png("text/figures/Sup/weighted_p_values_high.png", 
    width = 15 , height = 20,
    units = "cm", pointsize = 8, res = 300)

gen_high %>% 
  ggplot(aes(x = p_value, fill = class, alpha = alpha_value)) +
  geom_histogram(binwidth = 0.05, position = "identity", 
                 aes(color = class), size = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = class_colors) +
  scale_color_manual(values = class_colors)+# Use your colors for each class
  scale_alpha_continuous(range = c(0.1, 1)) + 
  geom_vline(aes(xintercept = 0.05), linetype = "dashed", color = "black", linewidth = 1.5)+
  labs(title = "High extinction", x = "p-value", y = NULL) +
  facet_grid(class ~ variable, scales = "free_y",
             labeller = as_labeller(c(
               w.marginality = "Marginality",
               w.specialization = "Specialization"
             )) ) +  # Facet by tetrapod and ENFA measure
  theme_bw()+
  mynamestheme+
  theme(strip.text.y = element_blank(),
        strip.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 16),  # Increase x-axis text size
        axis.title.x = element_text(size = 18))
dev.off()




########### intermediate #######

png("text/figures/Sup/weighted_CI_int.png", 
    width = 25, height = 15,
    units = "cm", pointsize = 8, res = 300)


sum_tot %>% filter(ext == "int") %>% 
  ggplot(aes(y = class, color = class)) +
  geom_point(aes(x = median_B), size = 5, position = position_dodge(width = 0.3)) +
  geom_linerange(aes(xmin = CI_lower_B, xmax = CI_upper_B), linewidth = 1.5)+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = class_colors, 
                     guide = "none") +
  labs(title = "Intermediate extinction" ,x = "Beta", y = NULL, color = NULL) +
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
  facet_wrap(~variable, ncol = 2, labeller = as_labeller(c(
    marginality = "Marginality",
    specialization = "Specialization"
  )))

dev.off()


###P values distribution

#general results
gen_results <- rbind(mam_w_results, birds_w_results,
                     reptiles_w_results, amp_w_results) %>% 
  mutate(term = str_replace(term, "^log\\(.*\\)$", "Beta")) %>% 
  filter(term == "Beta") %>% 
  dplyr::rename(p_value = `Pr(>|t|)`)

gen_results$ext <- factor(gen_results$ext, levels = c("low",
                                                      "int",
                                                      "high"),
                          ordered = TRUE)
gen_results$class <- factor(gen_results$class, levels = c("Mammals",
                                                          "Birds",
                                                          "Reptiles",
                                                          "Amphibians"))

##filling purpose
x_1 <- gen_results %>% filter(ext == "int") %>% 
  group_by(class, variable) %>% 
  summarise( prop_significant = mean(p_value < 0.05))

gen_int <- gen_results %>% filter(ext == "int") %>% 
  left_join(x_1, by = c("class", "variable")) %>% 
  mutate(alpha_value = case_when(
    prop_significant >= 0.95 ~ 1,
    prop_significant >= 0.80 & prop_significant < 0.95 ~ 0.5,
    TRUE ~ 0
  ))

##p values

png("text/figures/Sup/weighted_p_values_int.png", 
    width = 15 , height = 20,
    units = "cm", pointsize = 8, res = 300)

gen_int %>% ggplot(aes(x = p_value, fill = class, alpha = alpha_value)) +
  geom_histogram(binwidth = 0.05, position = "identity", 
                 aes(color = class), size = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = class_colors) +
  scale_color_manual(values = class_colors)+# Use your colors for each class
  scale_alpha_continuous(range = c(0.1, 1)) + 
  geom_vline(aes(xintercept = 0.05), linetype = "dashed", color = "black", linewidth = 1.5)+
  labs(title = "Intermediate extinction", x = "p-value", y = NULL) +
  facet_grid(class ~ variable, scales = "free_y",
             labeller = as_labeller(c(
               w.marginality = "Marginality",
               w.specialization = "Specialization"
             )) ) +  # Facet by tetrapod and ENFA measure
  theme_bw()+
  mynamestheme+
  theme(strip.text.y = element_blank(),
        strip.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 16),  # Increase x-axis text size
        axis.title.x = element_text(size = 18))
dev.off()


##################### low ##########################

png("text/figures/Sup/weighted_CI_low.png", 
    width = 25, height = 15,
    units = "cm", pointsize = 8, res = 300)


sum_tot %>% filter(ext == "low") %>% 
  ggplot(aes(y = class, color = class)) +
  geom_point(aes(x = median_B), size = 5, position = position_dodge(width = 0.3)) +
  geom_linerange(aes(xmin = CI_lower_B, xmax = CI_upper_B), linewidth = 1.5)+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = class_colors, 
                     guide = "none") +
  labs(title = "Low extinction", x = "Beta", y = NULL, color = NULL) +
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
  facet_wrap(~variable, ncol = 2, labeller = as_labeller(c(
    marginality = "Marginality",
    specialization = "Specialization"
  )))

dev.off()


###P values distribution

#general results
gen_results <- rbind(mam_w_results, birds_w_results,
                     reptiles_w_results, amp_w_results) %>% 
  mutate(term = str_replace(term, "^log\\(.*\\)$", "Beta")) %>% 
  filter(term == "Beta") %>% 
  dplyr::rename(p_value = `Pr(>|t|)`)

gen_results$ext <- factor(gen_results$ext, levels = c("low",
                                                      "int",
                                                      "high"),
                          ordered = TRUE)
gen_results$class <- factor(gen_results$class, levels = c("Mammals",
                                                          "Birds",
                                                          "Reptiles",
                                                          "Amphibians"))

##filling purpose
x_1 <- gen_results %>% filter(ext == "low") %>% 
  group_by(class, variable) %>% 
  summarise( prop_significant = mean(p_value < 0.05))


gen_low <- gen_results %>% filter(ext == "low") %>% 
  left_join(x_1, by = c("class", "variable")) %>% 
  mutate(alpha_value = case_when(
    prop_significant >= 0.95 ~ 1,
    prop_significant >= 0.80 & prop_significant < 0.95 ~ 0.5,
    TRUE ~ 0
  ))

##p values

png("text/figures/Sup/weighted_p_values_low.png", 
    width = 15 , height = 20,
    units = "cm", pointsize = 8, res = 300)

gen_low %>% ggplot(aes(x = p_value, fill = class, alpha = alpha_value)) +
  geom_histogram(binwidth = 0.05, position = "identity", 
                 aes(color = class), size = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = class_colors) +
  scale_color_manual(values = class_colors)+# Use your colors for each class
  scale_alpha_continuous(range = c(0.1, 1)) + 
  geom_vline(aes(xintercept = 0.05), linetype = "dashed", color = "black", linewidth = 1.5)+
  labs(title = "Low extinction", x = "p-value", y = NULL) +
  facet_grid(class ~ variable, scales = "free_y",
             labeller = as_labeller(c(
               w.marginality = "Marginality",
               w.specialization = "Specialization"
             )) ) +  # Facet by tetrapod and ENFA measure
  theme_bw()+
  mynamestheme+
  theme(strip.text.y = element_blank(),
        strip.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 16),  # Increase x-axis text size
        axis.title.x = element_text(size = 18))
dev.off()
