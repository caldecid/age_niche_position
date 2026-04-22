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
sum_mam_spe <- mam_spe_biomes %>% group_by(biome, ext) %>% 
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

#reptiles_string <- list.files(path = "C:/Users/carlo/OneDrive/Desktop/niche_position2/biomes2/REPTILIA")

#list_reptiles <- vector(mode = "list", length = length(reptiles_string))

#for(i in seq_along(reptiles_string)){
  
  #list_reptiles[[i]] <- read_csv(paste0("C:/Users/carlo/OneDrive/Desktop/niche_position2/biomes2/REPTILIA/",
                                     #   reptiles_string[i]))
  
#}

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
sum_amphibians_spe <- amphibians_spe_biomes %>% group_by(biome, ext) %>% 
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
                                                  prop_significant >= 0.8) %>% 
  mutate(significance_level = case_when(
    prop_significant >= 0.95 ~ "Strict (≥95%)",
    prop_significant >= 0.80 ~ "Moderate (≥80%)"))





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
                                                prop_significant >= 0.8) %>% 
  mutate(significance_level = case_when(
    prop_significant >= 0.95 ~ "Strict (≥95%)",
    prop_significant >= 0.80 ~ "Moderate (≥80%)"#,
    #prop_significant >= 0.50 ~ "Lenient (≥50%)"
  ))


write_xlsx(sum_biomes_spe_sig, path = "results/data/processed/biomes/specialization/sum_biomes_sig.xlsx")

####Only birds have significant relationships for both ENFA metrics

# Marginality pgls --------------------------------------------------------

##calling general data
vert_enfa_ages <- read_csv("results/data/processed/vert_enfa_ages.csv")

##list all model results from birds

aves.files <- list.files("results/data/processed/biomes/AVES")

list.aves <- vector("list", length = length(aves.files))

for(i in seq_along(list.aves)){
  
  list.aves[[i]] <- read_csv(paste0("results/data/processed/biomes/AVES/",
                                    aves.files[i]))
}

#binding dataframes
aves.results <- do.call(rbind, list.aves)

#saving
write_csv(aves.results, file ="results/data/processed/biomes/birds_general_biomes.csv")

#modifying terms and filtering
aves_marg <- aves.results %>% 
    mutate(term = str_replace(term, "^log\\(.*\\)$", "Beta")) %>% 
    dplyr::rename(p_value = `Pr(>|t|)`) %>% 
    filter(variable == "marginality",
           ext == "high")

aves_marg$term <- ifelse(aves_marg$term == "(Intercept)", 
                        "Intercept", 
                        aves_marg$term)


#summarizing
sum_marg_aves <- aves_marg %>% group_by(biome) %>% filter(term == "Beta") %>%  
  summarize(
    mean_B = mean(Estimate),
    median_B = median(Estimate),
    CI_lower_B = quantile(Estimate, 0.025),
    CI_upper_B = quantile(Estimate, 0.975),
    #mean_intercept = mean(Intercept),
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

#obtaining intercept
sum_int_marg <- aves_marg %>% group_by(biome) %>% 
  filter(term == "Intercept") %>%  
  summarize(mean_intercept = mean(Estimate))

sum_marg_aves <- left_join(sum_marg_aves, sum_int_marg, by = "biome")

##significant biomes
sig_marg_aves <- sum_marg_aves %>% filter(prop_significant >= 0.8)


#modifying biomes names for plotting 
sig_marg_aves$biome <- factor(sig_marg_aves$biome, labels = c("Deserts and xeric shrublands",
                                                              "Tropical & subtropical dry broadleaf forests",
                                                              "Tropical & subtropical grasslands savannas and shrublands",
                                                              "Tropical & subtropical moist broadleaf forests"))

sig_marg_aves$biome <- factor(sig_marg_aves$biome, levels = c("Tropical & subtropical moist broadleaf forests",
                                                              "Tropical & subtropical grasslands savannas and shrublands",
                                                              "Tropical & subtropical dry broadleaf forests",
                                                              "Deserts and xeric shrublands"),
                              ordered = TRUE)

sig_marg_aves <- sig_marg_aves %>%   rename(biome_clean = biome)


######Modifying general table
aves_enfa <- vert_enfa_ages %>% filter(className == "AVES") %>% 
  mutate(
    Class = case_when(
      className == "AVES"     ~ "Birds",
      TRUE ~ className
    ),
    log_marg   = log1p(marginality),
    log_spe = log1p(specialization),
    log_high_age = log1p(high.age)
  )

aves_enfa <- aves_enfa %>%
  mutate(biome_clean = gsub("_", " ", biome),
         biome_clean = gsub("&", "&", biome_clean),     # keep & as is
         biome_clean = gsub("  ", " ", biome_clean))    # remove double spaces

#filtering for only significant biomes
aves_enfa_marg <- aves_enfa %>% 
                  filter(biome_clean %in% sig_marg_aves$biome_clean)

#ordering factor
aves_enfa_marg$biome_clean <- factor(aves_enfa_marg$biome_clean,
                                     levels = levels(sig_marg_aves$biome_clean))

# range of predictor
x_range <- aves_enfa %>%
  group_by(biome_clean) %>%
  summarise(min_x = min(log_high_age),
            max_x = max(log_high_age))

# merge ranges with coefficients
pred_input <- sig_marg_aves %>% 
  left_join(x_range, by = "biome_clean")

# generate grid of x and predicted y
pred_lines <- pred_input %>%
  mutate(x = map2(min_x, max_x, ~seq(.x, .y, length.out = 200))) %>%
  unnest(x) %>%
  mutate(
    fit = mean_intercept + mean_B * x,
    lower = mean_intercept + CI_lower_B * x,
    upper = mean_intercept + CI_upper_B * x
  )


pred_lines$biome_clean <- factor(pred_lines$biome_clean, levels = levels(sig_marg_aves$biome_clean))

##plotting
biome_marg <- ggplot() +
  geom_point(data = aves_enfa_marg,
             aes(x = log_high_age, y = log_marg),
             alpha = 0.3, size = 1,
             color = "#E69F00") +
  
  geom_ribbon(data = pred_lines,
              aes(x = x, ymin = lower, ymax = upper, fill = biome_clean),
              alpha = 0.2, colour = "gray") +
  
  geom_line(data = pred_lines,
            aes(x = x, y = fit),
            color = "#E69F00",
            size = 1) +
  
  facet_wrap(~ biome_clean, ncol = 2,
             scales = "free") +
  
  labs(x = "Species age", 
       y = "Marginality",
       fill = "Biome") +
  
  theme_bw(base_size = 13) +
  mynamestheme+
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "white"))

#saving
svg("figures/biome_marg_vs_ages.svg",
    width = 10, height = 8)

biome_marg

dev.off()


##### CI for marginality ##########
sig_marg_aves <- sig_marg_aves %>% mutate(biome_clean = str_wrap(biome_clean, width = 25))

CI_marg_aves <- ggplot(sig_marg_aves, aes(x = biome_clean, y = median_B)) +
  geom_point(size = 5.5, position = position_dodge(width = 1), 
             color = "#E69F00") +  # Points for median_B
  geom_errorbar(aes(ymin = CI_lower_B, ymax = CI_upper_B), 
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
svg("figures/CI_marg_biomes.svg",
    width = 6, height = 4)

CI_marg_aves

dev.off()




# Specialization ----------------------------------------------------------

aves.results  <- read_csv("results/data/processed/biomes/birds_general_biomes.csv")

#modifying terms and filtering
aves_spe <- aves.results %>% 
  mutate(term = str_replace(term, "^log\\(.*\\)$", "Beta")) %>% 
  dplyr::rename(p_value = `Pr(>|t|)`) %>% 
  filter(variable == "specialization",
         ext == "high")

aves_spe$term <- ifelse(aves_spe$term == "(Intercept)", 
                         "Intercept", 
                         aves_spe$term)


#summarizing
sum_spe_aves <- aves_spe %>% group_by(biome) %>% filter(term == "Beta") %>%  
  summarize(
    mean_B = mean(Estimate),
    median_B = median(Estimate),
    CI_lower_B = quantile(Estimate, 0.025),
    CI_upper_B = quantile(Estimate, 0.975),
    #mean_intercept = mean(Intercept),
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

#obtaining intercept
sum_int_spe <- aves_spe %>% group_by(biome) %>% 
  filter(term == "Intercept") %>%  
  summarize(mean_intercept = mean(Estimate))

sum_spe_aves <- left_join(sum_spe_aves, sum_int_spe, by = "biome")

##significant biomes
sig_spe_aves <- sum_spe_aves %>% filter(prop_significant >= 0.8)

write_xlsx(sig_spe_aves, "results/data/processed/biomes/sig_results_specializaiton.xlsx")

#modifying biomes names for plotting 
sig_spe_aves$biome <- factor(sig_spe_aves$biome, labels = c("Deserts and xeric shrublands",
                                                            "Flooded grasslands and savannas",
                                                            "Montane grasslands and shrublands",
                                                            "Tropical & subtropical grasslands savannas and shrublands",
                                                            "Tropical & subtropical moist broadleaf forests",
                                                            "Mangroves"))

sig_spe_aves$biome <- factor(sig_spe_aves$biome, levels = c("Tropical & subtropical moist broadleaf forests",
                                                            "Mangroves",
                                                            "Tropical & subtropical grasslands savannas and shrublands",
                                                            "Flooded grasslands and savannas",
                                                            "Montane grasslands and shrublands",
                                                            "Deserts and xeric shrublands"))

sig_spe_aves <- sig_spe_aves %>% rename(biome_clean = biome)


######Modifying general table
aves_enfa <- vert_enfa_ages %>% filter(className == "AVES") %>% 
  mutate(
    Class = case_when(
      className == "AVES"     ~ "Birds",
      TRUE ~ className
    ),
    log_marg   = log1p(marginality),
    log_spe = log1p(specialization),
    log_high_age = log1p(high.age)
  )

aves_enfa <- aves_enfa %>%
  mutate(biome_clean = gsub("_", " ", biome),
         biome_clean = gsub("&", "&", biome_clean),     # keep & as is
         biome_clean = gsub("  ", " ", biome_clean))    # remove double spaces

##change mangroves
aves_enfa <- aves_enfa %>%
  mutate(biome_clean = case_when(
    biome_clean == "mangroves" ~ "Mangroves",
    TRUE ~ biome_clean
  ))


#filtering for only significant biomes
aves_enfa_spe <- aves_enfa %>% 
  filter(biome_clean %in% sig_spe_aves$biome_clean)

#ordering factor
aves_enfa_spe$biome_clean <- factor(aves_enfa_spe$biome_clean,
                                     levels = levels(sig_spe_aves$biome_clean))



# range of predictor
x_range <- aves_enfa_spe %>%
  group_by(biome_clean) %>%
  summarise(min_x = min(log_high_age),
            max_x = max(log_high_age)) %>% 
  mutate(biome_clean = as.factor(biome_clean))

# merge ranges with coefficients
pred_input <- sig_spe_aves %>% 
  left_join(x_range, by = "biome_clean")

# generate grid of x and predicted y
pred_lines <- pred_input %>%
  mutate(x = map2(min_x, max_x, ~seq(.x, .y, length.out = 200))) %>%
  unnest(x) %>%
  mutate(
    fit = mean_intercept + mean_B * x,
    lower = mean_intercept + CI_lower_B * x,
    upper = mean_intercept + CI_upper_B * x
  )

pred_lines$biome_clean <- factor(pred_lines$biome_clean, levels = levels(sig_spe_aves$biome_clean))


pred_lines$biome_clean <- factor(pred_lines$biome_clean, labels = c("Tropical & subtropical moist broadleaf forests",
                                                                    "Mangroves",
                                                                    "Tropical & subtropical grasslands savannas and shrublands",
                                                                    "Flooded grasslands and savannas",
                                                                    "Montane grasslands and shrublands",
                                                                    "Deserts and xeric shrublands"))

##plotting
biome_spe <- ggplot() +
  geom_point(data = aves_enfa_spe,
             aes(x = log_high_age, y = log_spe),
             alpha = 0.3, size = 1,
             color = "#E69F00") +
  
  geom_ribbon(data = pred_lines,
              aes(x = x, ymin = lower, ymax = upper, fill = biome_clean),
              alpha = 0.2, colour = "gray") +
  
  geom_line(data = pred_lines,
            aes(x = x, y = fit),
            color = "#E69F00",
            size = 1) +
  
  facet_wrap(~ biome_clean, ncol = 2,
             scales = "free") +
  
  labs(x = "Species age", 
       y = "Specialization",
       fill = "Biome") +
  
  theme_bw(base_size = 13) +
  mynamestheme+
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "white"))

#saving
svg("figures/biome_spe_vs_ages.svg",
    width = 10, height = 10)

biome_spe

dev.off()


##### CI for marginality ##########
sig_spe_aves <- sig_spe_aves %>% 
  mutate(biome_clean = str_wrap(biome_clean, width = 25))

sig_spe_aves$biome_clean <- factor(sig_spe_aves$biome_clean,
                                   levels = c("Deserts and xeric\nshrublands",
                                              "Montane grasslands and\nshrublands",
                                              "Flooded grasslands and\nsavannas",
                                              "Tropical & subtropical\ngrasslands savannas and\nshrublands",
                                              "Mangroves",
                                              "Tropical & subtropical\nmoist broadleaf forests"))

##plotting CI biomes
CI_spe_aves <- ggplot(sig_spe_aves, aes(x = biome_clean,
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
svg("figures/CI_spe_biomes.svg",
    width = 6, height = 6)

CI_spe_aves

dev.off()


