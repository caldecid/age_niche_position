
# ANOVA for weighted measures ---------------------------------------------


##sourcing the libraries and the directories
source(file.path(getwd(), "/source.R"))

##reading vertebrate enfa unique 
vert_enfa_unique <- read_csv(file = "results/data/processed/vert_enfa_unique.csv")


###########phylogenetic signal in marginality ###############################

##mammals
mam_signal_marg <- phylogenetic_signal_results_parallel[["Mammals"]][["marginality"]]

mam_signal_marg$class <- "Mammals"

##birds
bird_signal_marg <- phylogenetic_signal_results_parallel[["Birds"]][["marginality"]]

bird_signal_marg$class <- "Birds"

####reptiles 
rep_signal_marg <- phylogenetic_signal_results_parallel[["Reptiles"]][["marginality"]]

rep_signal_marg$class <- "Reptiles"

##amphibians
amp_signal_marg <- signal_results[["Amphibians"]][["marginality"]]

amp_signal_marg$class <- "Amphibians"


##binding
marg_signal <- rbind(mam_signal_marg, bird_signal_marg, rep_signal_marg,
                     amp_signal_marg)

##saving 
write_csv(marg_signal, file = "results/data/processed/marg_phylo_signal.csv")


sum_marg_signal <- marg_signal %>% group_by(class) %>% 
  summarise(
    median_K = median(K),
    CI_lower_K = quantile(K, 0.025),
    CI_upper_K = quantile(K, 0.975),
    median_K_pval = median(K_pval),
    
    median_lambda = median(lambda),
    CI_lower_lambda = quantile(lambda, 0.025),
    CI_upper_lambda = quantile(lambda, 0.975),
    median_lambda_pval = median(lambda_pval))

sum_marg_signal$variable <- "marginality"

########phylogenetic signal in specialization############################

##mammals
mam_signal_spe <- phylogenetic_signal_results_parallel[["Mammals"]][["specialization"]]

mam_signal_spe$class <- "Mammals"

##birds
bird_signal_spe <- phylogenetic_signal_results_parallel[["Birds"]][["specialization"]]

bird_signal_spe$class <- "Birds"

####reptiles 
rep_signal_spe <- phylogenetic_signal_results_parallel[["Reptiles"]][["specialization"]]

rep_signal_spe$class <- "Reptiles"

##amphibians
amp_signal_spe <- signal_results[["Amphibians"]][["specialization"]]

amp_signal_spe$class <- "Amphibians"


##binding
spe_signal <- rbind(mam_signal_spe, bird_signal_spe, rep_signal_spe,
                    amp_signal_spe)

##saving
write_csv(spe_signal, 
          file = "results/data/processed/specialization_phylo_signal.csv")


###summarizing
sum_spe_signal <- spe_signal %>% group_by(class) %>% 
  summarise(
    median_K = median(K),
    CI_lower_K = quantile(K, 0.025),
    CI_upper_K = quantile(K, 0.975),
    median_K_pval = median(K_pval),
    
    median_lambda = median(lambda),
    CI_lower_lambda = quantile(lambda, 0.025),
    CI_upper_lambda = quantile(lambda, 0.975),
    median_lambda_pval = median(lambda_pval))

sum_spe_signal$variable <- "specialization"

########Binding summaries

sum_phylo_signal <- rbind(sum_marg_signal, sum_spe_signal)


##saving
writexl::write_xlsx(sum_phylo_signal,
                    path = "results/data/processed/sum_phylo_signal.xlsx")




# Kruskal Wallis ----------------------------------------------------------



vert_enfa_unique$className <- factor(vert_enfa_unique$className, 
                                     levels = c("AMPHIBIA",
                                                "REPTILIA",
                                                "AVES", 
                                                "MAMMALIA"),
                                     labels = c( "Amphibians",
                                                 "Reptiles",
                                                 "Birds",
                                                 "Mammals"),
                                     ordered = TRUE)

# w.marginality -----------------------------------------------------------

m.marg <- kruskal.test(w.marginality ~ className,
                       data = vert_enfa_unique) ## it is significant


dunn.test(vert_enfa_unique$w.marginality, vert_enfa_unique$className,
          method = "holm")


############ Boxplot for w. marginality####################

##defining colors

class_colors <- c(
  "Mammals" = "#E66101",
  "Birds" = "#E69F00",   
  
  "Reptiles" = "#018571", 
  "Amphibians" = "#7fbf7b"
)

#######PLOT###########

png("text/figures/ridge_marg_class.png",
    width = 13, height = 11, units = "cm", 
    pointsize = 8, res = 300)


ggplot(vert_enfa_unique, aes(x = w.marginality, y = className, fill = className)) +
  geom_density_ridges() +
  scale_fill_manual(values = class_colors)+
  xlab("Weighted marginality")+
  ylab(NULL)+
  theme_ridges() + 
  xlim(0,8)+
  mynamestheme+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(vjust = 0, hjust = 0.5,
                                    size = 14),
        axis.title.y = element_text(vjust = 1, hjust = 0.5,
                                    size = 14),
        axis.text = element_text(size = 14))

dev.off()


###boxplot

##changing the factors order
vert_enfa_unique$className <- factor(vert_enfa_unique$className, 
                                     
                                     levels = c( "Mammals",
                                                 "Birds",
                                                 "Reptiles",
                                                 "Amphibians"),
                                     ordered = TRUE)



png("text/figures/box_marg_class.png",
    width = 15, height = 13, units = "cm", 
    pointsize = 8, res = 300)

ggplot(vert_enfa_unique, aes(x = className, y = w.marginality,
                             fill = className)) +
  geom_boxplot(outlier.shape  = NA) +
  scale_fill_manual(values = class_colors)+
  ylim(0,6)+
  xlab(NULL)+
  ylab("Marginality")+
  theme_bw()+
  mynamestheme+
  theme(legend.position = "none",
        axis.title.y = element_text(margin = margin(r = 13)),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 17),
        axis.text.y = element_text(size = 16))

dev.off()

# w.specialization --------------------------------------------------------

m.spe <- kruskal.test(w.specialization ~ className,
                      data = vert_enfa_unique) ## it is significant


dunn.test(vert_enfa_unique$w.specialization, vert_enfa_unique$className,
          method = "holm")


#######plot#############
png("text/figures/specialization/weighted_enfa/ridge_spe_class.png",
    width = 14, height = 11, units = "cm", 
    pointsize = 8, res = 300)

ggplot(vert_enfa_unique, aes(x = w.specialization,
                             y = className, fill = className)) +
  geom_density_ridges() +
  theme_ridges() + 
  scale_fill_manual(values = class_colors)+
  xlab("Weighted specialization")+
  ylab(NULL)+
  xlim(0,40)+
  mynamestheme+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(vjust = 0, hjust = 0.5),
        axis.title.y = element_text(vjust = 1, hjust = 0.5))

dev.off()


########Boxplot
png("text/figures/box_spe_class.png",
    width = 15, height = 13, units = "cm", 
    pointsize = 8, res = 300)

ggplot(vert_enfa_unique, aes(x = className, y = w.specialization,
                             fill = className)) +
  geom_boxplot(outlier.shape  = NA) +
  scale_fill_manual(values = class_colors)+
  ylim(0,25)+
  xlab(NULL)+
  ylab("Specialization")+
  theme_bw()+
  mynamestheme+
  theme(legend.position = "none",
        axis.title.y = element_text(margin = margin(r = 13)),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 17),
        axis.text.y = element_text(size = 16))

dev.off()
