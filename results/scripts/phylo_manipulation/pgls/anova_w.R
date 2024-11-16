
# ANOVA for weighted measures ---------------------------------------------

##sourcing the libraries and the directories
source(file.path(getwd(), "/source.R"))

##reading vertebrate enfa unique 
vert_enfa_unique <- read_csv(file = "results/data/processed/vert_enfa_unique.csv")


class(vert_enfa_unique$className)
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

##fitting model
marg_anova <- aov(w.marginality ~ className, data = vert_enfa_unique)

summary(marg_anova)

TukeyHSD(marg_anova)

#######PLOT###########

png("text/figures/marginality/weighted_enfa/ridge_marg_class.png",
    width = 13, height = 11, units = "cm", 
    pointsize = 8, res = 300)


ggplot(vert_enfa_unique, aes(x = w.marginality, y = className, fill = className)) +
  geom_density_ridges() +
  scale_fill_manual(values = c( "#a6dba0",
                                "#008837",
                                "#fdae61",
                                "red"))+
  xlab("Weighted marginality")+
  ylab(NULL)+
  theme_ridges() + 
  xlim(0,8)+
  mynamestheme+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(vjust = 0, hjust = 0.5),
        axis.title.y = element_text(vjust = 1, hjust = 0.5))

dev.off()


###boxplot

##changing the factors order
vert_enfa_unique$className <- factor(vert_enfa_unique$className, 
                                    
                                     levels = c( "Mammals",
                                                 "Birds",
                                                 "Reptiles",
                                                 "Amphibians"),
                                     ordered = TRUE)

png("text/figures/marginality/weighted_enfa/box_marg_class.png",
    width = 13, height = 11, units = "cm", 
    pointsize = 8, res = 300)

ggplot(vert_enfa_unique, aes(x = className, y = w.marginality,
                             fill = className)) +
  geom_boxplot(outlier.shape  = NA) +
  scale_fill_manual(values = c( "red",
                                "#fdae61",
                                "#008837",
                                "#a6dba0"))+
  ylim(0,8)+
  xlab(NULL)+
  ylab("Weighted marginality")+
  theme_bw()+
  mynamestheme+
  theme(legend.position = "none")

dev.off()

# w.specialization --------------------------------------------------------

spe_anova <- aov(w.specialization ~ className, data = vert_enfa_unique)

summary(spe_anova)

TukeyHSD(spe_anova)


#######plot#############
png("text/figures/specialization/weighted_enfa/ridge_spe_class.png",
    width = 14, height = 11, units = "cm", 
    pointsize = 8, res = 300)

ggplot(vert_enfa_unique, aes(x = w.specialization,
                             y = className, fill = className)) +
  geom_density_ridges() +
  theme_ridges() + 
  scale_fill_manual(values = c( "#a6dba0",
                                "#008837",
                                "#fdae61",
                                "red"))+
  xlab("Weighted specialization")+
  ylab(NULL)+
  xlim(0,40)+
  mynamestheme+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(vjust = 0, hjust = 0.5),
        axis.title.y = element_text(vjust = 1, hjust = 0.5))

dev.off()


########Boxplot
png("text/figures/specialization/weighted_enfa/box_spe_class.png",
    width = 13, height = 11, units = "cm", 
    pointsize = 8, res = 300)

ggplot(vert_enfa_unique, aes(x = className, y = w.specialization,
                             fill = className)) +
  geom_boxplot(outlier.shape  = NA) +
  scale_fill_manual(values = c( "red",
                                "#fdae61",
                                "#008837",
                                "#a6dba0"))+
  ylim(0,25)+
  xlab(NULL)+
  ylab("Weighted specialization")+
  theme_bw()+
  mynamestheme+
  theme(legend.position = "none")

dev.off()
