library(tidyverse)
library(purrr)

# ----------------------------------------------------------
# 1. DATA
# ----------------------------------------------------------

df <- tribble(
  ~Metric, ~Class, ~Beta, ~Lower_CI, ~Upper_CI, ~p_value, ~Sig, ~R2, ~lambda, ~Richness,
  "Marginality", "Mammals",   -0.064, -0.094, -0.032, 0.009,     83, 0.005, 0.690, 1149,
  "Marginality", "Birds",     -0.078, -0.087, -0.067, 0.0001,   100, 0.012, 0.480, 2149,
  "Marginality", "Reptiles",  -0.019, -0.039,  0.001, 0.135,     29, 0.001, 0.414, 1826,
  "Marginality", "Amphibians",-0.019, -0.039,  0.005, 0.103,     32, 0.001, 0.633, 1435,
  
  "Specialization","Mammals",  -0.117, -0.183, -0.028, 0.027,    63, 0.003, 0.685, 1149,
  "Specialization","Birds",    -0.136, -0.156, -0.119, 0.0001,  100, 0.011, 0.274, 2149,
  "Specialization","Reptiles", -0.063, -0.120, -0.011, 0.055,    48, 0.001, 0.358, 1826,
  "Specialization","Amphibians",-0.050,-0.110, -0.006, 0.100,    38, 0.001, 0.352, 1149
)

df$p_value_label <- ifelse(df$p_value < 0.001, "< 0.001",
                           sprintf("%.3f", df$p_value))

######calling ENFA values#####
vert_enfa_unique <- read_csv("results/data/processed/vert_enfa_unique.csv")

##plotting log1p values of ENFA



vert <- vert_enfa_unique %>%
  mutate(
    Class = case_when(
      className == "MAMMALIA" ~ "Mammals",
      className == "AVES"     ~ "Birds",
      className == "REPTILIA" ~ "Reptiles",
      className == "AMPHIBIA" ~ "Amphibians",
      TRUE ~ className
    ),
    log_w_marg   = log1p(w.marginality),
    low_w_spe = log1p(w.specialization),
    log_high_age = log1p(high.age)
  )

##calling general results for extracting intercepts
general_results <- read_csv("results/data/processed/weighted_enfa/general_results.csv")

##only high extinction age and significant groups
gen_high <- general_results %>% filter(ext == "high",
                                       variable == "w.marginality",
                                       class %in% c("Mammals", "Birds"))

mean_intercepts <- gen_high %>%
  filter(term == "(Intercept)") %>%      # keep only intercept rows
  group_by(class) %>%                    # group by vertebrate class
  summarize(mean_intercept = mean(Estimate),
            sd_intercept   = sd(Estimate),
            n_models       = n(),
            .groups = "drop")


# Colors for all classes
class_colors <- c(
  "Mammals"    = "#E66101",
  "Birds"      = "#E69F00",
  "Reptiles"   = "#018571",
  "Amphibians" = "#7fbf7b"
)

# Regression slopes provided 
coefs <- tribble(
  ~Class,     ~Beta,    ~CI_low,  ~CI_high, ~Intercept,
  "Mammals",  -0.064,   -0.094,   -0.032,   1.45,
  "Birds",    -0.078,   -0.087,   -0.067,   1.29
)


##predicted lines and CI ribbon.
# Determine global x-range across the two classes
x_grid <- tibble(
  log_high_age = seq(
    min(vert$log_high_age, na.rm = TRUE),
    max(vert$log_high_age, na.rm = TRUE),
    length.out = 400
  )
)

# Build pred_lines directly from coefficients
pred_lines <- coefs %>%
  mutate(dummy = 1) %>%                      # cartesian join trick
 full_join(x_grid %>% mutate(dummy = 1), by = "dummy") %>%
  select(-dummy) %>%
  mutate(
    pred      = Intercept + Beta    * log_high_age,
    pred_low  = Intercept + CI_low  * log_high_age,
    pred_high = Intercept + CI_high * log_high_age
  )

#establishing a max and min for plotting
ranges <- vert %>%
  group_by(Class) %>%
  summarise(
    xmin = min(log_high_age, na.rm = TRUE),
    xmax = max(log_high_age, na.rm = TRUE)
  ) %>% 
  mutate(Class = as.character(Class))

#filtering regarding ranges
pred_lines <- pred_lines %>%
  left_join(ranges, by = "Class") %>%
  filter(log_high_age >= xmin,
         log_high_age <= xmax)



##ordering factors
vert$Class <- factor(vert$Class, levels = c("Mammals",
                                        "Birds",
                                        "Reptiles",
                                       "Amphibians"), 
                                      ordered = TRUE)
#predicting lines
pred_lines$Class <- factor(
  pred_lines$Class,
  levels = c("Mammals", "Birds", "Reptiles", "Amphibians")
)



# marginality plot --------------------------------------------------------


w_marg <- ggplot() +
  geom_point(
    data = vert,
    aes(x = log_high_age, y = log_w_marg, colour = Class),
    alpha = 0.3, size = 1
  ) +
  
  geom_ribbon(
    data = pred_lines,
    aes(x = log_high_age, ymin = pred_low, ymax = pred_high, fill = "gray"),
    alpha = 0.20
  ) +
  
  geom_line(
    data = pred_lines,
    aes(x = log_high_age, y = pred, colour = Class),
    linewidth = 1.1
  ) +
  
  scale_color_manual(values = class_colors) +
  #scale_fill_manual(values = class_colors) +
  
  facet_wrap(~ Class, ncol = 2) +
  coord_cartesian(xlim = c(0, 3)) + 
  theme_bw(15) +
  labs(
    x = "Species age",
    y = "Weighted marginality"
  ) +
  mynamestheme+
  theme(
    legend.position = "none",
    strip.text = element_blank()
    
  ) 

#saving
svg("figures/w_marg_vs_ages.svg",
    width = 10, height = 8)

w_marg

dev.off()


###### plotting mammals beta and CI #######
CI_marg_mam <- df %>% 
  filter(Class == "Mammals", Metric == "Marginality") %>% 
  ggplot(aes(x = Beta, y = Class)) +
  
 # CI line inside
  geom_segment(aes(x = Lower_CI, xend = Upper_CI, y = Class, yend = Class),
               color = "#E66101", linewidth = 3, alpha = 0.9) +
  
  # Point
  geom_point(color = "#E66101", size = 6, alpha = 0.9) +
  
  # Zero line
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 1.1) +
  
  labs(x = "Beta", y = NULL) +
  xlim(-0.20, 0.20) +
  
  theme_bw() +
  mynamestheme +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

##saving
svg("figures/CI_marg_mam.svg",
    width = 3, height = 3)

CI_marg_mam

dev.off()

##plotting birds

CI_marg_birds <-df %>% filter(Class == "Birds",
                            Metric == "Marginality") %>% 
  
  ggplot(aes(x = Beta, y = Class)) +
  # CI line
  geom_segment(aes(x = Lower_CI, xend = Upper_CI, y = Class, yend = Class),
               color = "#E69F00", linewidth = 3, alpha = 0.8) +
  # Point
  geom_point(color = "#E69F00", size = 6, alpha = 0.9) +
  # Zero line
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 1.1) +
  # Labels
  labs(x = "Beta", y = NULL) +
  # Limits for symmetry (optional, adjust as needed)
  xlim(-0.15, 0.15) +
  theme_bw() +
  mynamestheme+
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

##saving
svg("figures/CI_marg_birds.svg",
    width = 3, height = 3)

CI_marg_birds

dev.off()



#########mmammals and birds ##########


CI_marg_MB <- df %>%
  filter(Class %in% c("Mammals", "Birds"),
         Metric == "Marginality") %>%
  
  ggplot(aes(x = Beta, y = Class, colour = Class)) +
  
  # CI line
  geom_segment(aes(x = Lower_CI, xend = Upper_CI,
                   y = Class, yend = Class),
               linewidth = 3, alpha = 0.9) +
  
  # Point
  geom_point(size = 6, alpha = 0.95) +
  
  # Zero line
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "black", linewidth = 1.1) +
  
  # Colors for each class
  scale_color_manual(values = c(
    "Mammals" = "#E66101",
    "Birds"   = "#E69F00"
  )) +
  
  labs(x = "Beta", y = NULL) +
  
  xlim(-0.15, 0.15) +
  
  theme_bw() +
  mynamestheme +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

##saving
svg("figures/CI_marg.svg",
    width = 3, height = 4)

CI_marg_MB

dev.off()



# Specialization ----------------------------------------------------------

##only high extinction age and significant groups
gen_high_spe <- general_results %>% filter(ext == "high",
                                       variable == "w.specialization",
                                       class== "Birds")

mean_intercepts <- gen_high_spe %>%
  filter(term == "(Intercept)") %>%      # keep only intercept rows
  group_by(class) %>%                    # group by vertebrate class
  summarize(mean_intercept = mean(Estimate),
            sd_intercept   = sd(Estimate),
            n_models       = n(),
            .groups = "drop")


# Colors for all classes
class_colors <- c(
  "Mammals"    = "#E66101",
  "Birds"      = "#E69F00",
  "Reptiles"   = "#018571",
  "Amphibians" = "#7fbf7b"
)

# Regression slopes provided 
coefs <- tribble(
  ~Class,     ~Beta,    ~CI_low,  ~CI_high, ~Intercept,
  
  "Birds",    -0.136,  -0.156,   -0.119,   1.606
)


##predicted lines and CI ribbon.
# Determine global x-range across the two classes
x_grid <- tibble(
  log_high_age = seq(
    min(vert$log_high_age, na.rm = TRUE),
    max(vert$log_high_age, na.rm = TRUE),
    length.out = 400
  )
)

# Build pred_lines directly from coefficients
pred_lines <- coefs %>%
  mutate(dummy = 1) %>%                      # cartesian join trick
  full_join(x_grid %>% mutate(dummy = 1), by = "dummy") %>%
  select(-dummy) %>%
  mutate(
    pred      = Intercept + Beta    * log_high_age,
    pred_low  = Intercept + CI_low  * log_high_age,
    pred_high = Intercept + CI_high * log_high_age
  )

ranges <- vert %>%
  group_by(Class) %>%
  summarise(
    xmin = min(log_high_age, na.rm = TRUE),
    xmax = max(log_high_age, na.rm = TRUE)
  ) %>% 
  mutate(Class = as.character(Class))


pred_lines <- pred_lines %>%
  left_join(ranges, by = "Class") %>%
  filter(log_high_age >= xmin,
         log_high_age <= xmax)



##ordering factors
vert$Class <- factor(vert$Class, levels = c("Mammals",
                                            "Birds",
                                            "Reptiles",
                                            "Amphibians"), 
                     ordered = TRUE)
#predicting lines
pred_lines$Class <- factor(
  pred_lines$Class,
  levels = c("Mammals", "Birds", "Reptiles", "Amphibians")
)



# specialization plot --------------------------------------------------------


w_spe <- ggplot() +
  geom_point(
    data = vert,
    aes(x = log_high_age, y = low_w_spe, colour = Class),
    alpha = 0.3, size = 1
  ) +
  
  geom_ribbon(
    data = pred_lines,
    aes(x = log_high_age, ymin = pred_low, ymax = pred_high, fill = "gray"),
    alpha = 0.20
  ) +
  
  geom_line(
    data = pred_lines,
    aes(x = log_high_age, y = pred, colour = Class),
    linewidth = 1.1
  ) +
  
  scale_color_manual(values = class_colors) +
  #scale_fill_manual(values = class_colors) +
  
  facet_wrap(~ Class, ncol = 2,
             scales = "free") +
  #coord_cartesian(xlim = c(0, 3)) + 
  theme_bw(15) +
  theme(
    legend.position = "none",
    strip.text = element_blank()
  ) +
  labs(
    x = "Species age",
    y = "Weighted specialization"
  ) +
  mynamestheme+
  theme(
    legend.position = "none",
    strip.text = element_blank()
    
  ) 

#saving
svg("figures/w_spe_vs_ages.svg",
    width = 10, height = 8)

w_spe

dev.off()



##plotting birds
###### plotting mammals beta and CI #######
CI_spe_birds <-df %>% filter(Class == "Birds",
                              Metric == "Specialization") %>% 
  
  ggplot(aes(x = Beta, y = Class)) +
  # CI line
  geom_segment(aes(x = Lower_CI, xend = Upper_CI, y = Class, yend = Class),
               color = "#E69F00", linewidth = 3, alpha = 0.8) +
  # Point
  geom_point(color = "#E69F00", size = 6, alpha = 0.9) +
  # Zero line
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 1.1) +
  # Labels
  labs(x = "Beta", y = NULL) +
  # Limits for symmetry (optional, adjust as needed)
  xlim(-0.20, 0.20) +
  theme_bw() +
  mynamestheme+
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

##saving
svg("figures/CI_spe_birds.svg",
    width = 3, height = 3)

CI_spe_birds

dev.off()


