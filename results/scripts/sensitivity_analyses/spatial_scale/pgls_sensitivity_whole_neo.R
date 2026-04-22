##pgls results from sensitivity 3th amphibians and reptiles (whole neotropics)##########

##sourcing the libraries and the directories
source(file.path(getwd(), "/source.R"))

# libraries
library(tidyverse)
library(terra)
library(sf)
library(geodata)
library(stringr)
library(devtools)
devtools::install_github("rinnan/CENFA")
library(CENFA)
library(sp)
library(parallel)



# Amphibians 3th ----------------------------------------------------------

amphi_files <- list.files("results/data/processed/sensitivity_analyses/whole_neotropics/pgls_3th/amphibians")

amphi_results <- purrr::map(paste0("results/data/processed/sensitivity_analyses/whole_neotropics/pgls_3th/amphibians/", amphi_files), read_csv)

amphi_results <- do.call(rbind, amphi_results)

amphi_results$class <- "amphibians"

write_csv(amphi_results, file = "results/data/processed/sensitivity_analyses/whole_neotropics/pgls_3th/amphi_results.csv")

###summary
amphi_sum <- amphi_results %>% filter(term == "log(high.age + 1)") %>% 
                   mutate(term = str_replace(term, "^log\\(.*\\)$", "Beta")) %>% 
                            dplyr::rename(p_value = `Pr(>|t|)`) %>% 
                  group_by(biome) %>% 
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

#factor and renaming variables
amphi_sum$biome <- factor(amphi_sum$biome, 
                          levels = c("neo_marginality",
                                     "neo_specialization"),
                          labels = c("Marginality (th = 3)",
                                     "Specialization (th = 3)"),
                          ordered = TRUE)

amphi_sum$class <- "Amphibians"

# reptiles 3th ----------------------------------------------------------

reptiles_files <- list.files("results/data/processed/sensitivity_analyses/whole_neotropics/pgls_3th/reptiles")

reptiles_results <- purrr::map(paste0("results/data/processed/sensitivity_analyses/whole_neotropics/pgls_3th/reptiles/", reptiles_files), read_csv)

reptiles_results <- do.call(rbind, reptiles_results)

reptiles_results$class <- "reptiles"

write_csv(reptiles_results, file = "results/data/processed/sensitivity_analyses/whole_neotropics/pgls_3th/reptiles_results.csv")
 
##summary
reptiles_sum <- reptiles_results %>% filter(term == "log(high.age + 1)") %>% 
  mutate(term = str_replace(term, "^log\\(.*\\)$", "Beta")) %>% 
  dplyr::rename(p_value = `Pr(>|t|)`) %>% 
  group_by(biome) %>% 
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

#factor and renaming variables
reptiles_sum$biome <- factor(reptiles_sum$biome, 
                          levels = c("neo_marginality",
                                     "neo_specialization"),
                          labels = c("Marginality (th = 3)",
                                     "Specialization (th = 3)"),
                          ordered = TRUE)

reptiles_sum$class <- "Reptiles"

#binding 
sum_tot <- rbind(amphi_sum, reptiles_sum)

#saving
write_csv(sum_tot, "results/data/processed/sensitivity_analyses/whole_neotropics/pgls_3th/summary_pgls.csv")

write_xlsx(sum_tot, "results/data/processed/sensitivity_analyses/whole_neotropics/pgls_3th/summary_pgls.xlsx")


########plotting

##defining colors for each class
class_colors <- c(
  "Mammals" = "#E66101",
  "Birds" = "#E69F00",   # Strong golden yellow
  
  "Reptiles" = "#018571", # Strong teal green
  "Amphibians" = "#7fbf7b"
)


##figure
whole_3th_pgls_plot <- sum_tot %>%  ggplot(aes(y = class, color = class)) +
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
  facet_wrap(~biome, ncol = 2#, 
             #labeller = as_labeller(c(
    #marginality = "Marginality",
    #specialization = "Specialization"
  )#))

##High extinction
png("figures/Sup/sensitivity/whole_3th_CI.png", 
    width = 25, height = 15,
    units = "cm", pointsize = 8, res = 300)

whole_3th_pgls_plot


dev.off()

svg("figures/Sup/sensitivity/whole_3th_CI.svg",
    width = 10, height = 5)

whole_3th_pgls_plot

dev.off()
