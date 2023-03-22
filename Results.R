### Global 
source("Models.R")

## Libs
library(ggplot2)
library(visreg)
library(data.table)
library(plotrix)
library(RColorBrewer)
library(ggpubr)
library(car)

### Descriptive data
## Field
mean(FOR_fld_OUT$animal_dens)

mean(LYR_fld_OUT$ani_den)
mean(LYR_fld_OUT$rot_count/LYR_fld_OUT$dry_weight)
mean(LYR_fld_OUT$tar_count/LYR_fld_OUT$dry_weight)

mean(LYR_fld_OUT$OM)
mean(FOR_fld_OUT$OM)

mean((LYR_fld_OUT$diameter_longitudinal_cm[1:40])[-c(22,30)])

LYR_fld_OUT$rot_den <- LYR_fld_OUT$rot_count/LYR_fld_OUT$dry_weight
LYR_fld_OUT$tar_den <- LYR_fld_OUT$tar_count/LYR_fld_OUT$dry_weight

summary(lm(rot_den ~ tar_den, data = LYR_fld_OUT))

### Models outputs --------------------------------------------
## Field
# Forni (density)
summary(m_for_fld_1)
Anova(m_for_fld_1, type = "II")

# Lyr sediment (density)
summary(m_lyr_fld_1.1)
Anova(m_lyr_fld_1.1, type = "II")

## Experiment
# Forni
summary(m_for_exp_1)
Anova(m_for_exp_1, type = "II")

# Lyr
summary(m_lyr_exp_2)
Anova(m_lyr_exp_2, type = "III")

### Raw data ---------------------------------------------------
## Forni field
# Forni_fld animal density
g1 <- ggplot(FOR_fld_OUT, aes(x = animal_dens)) + 
  geom_density(fill = "darkgreen", alpha = 0.3) + 
  theme_classic(base_size = 20) + ylab("Density") + 
  xlab(expression(paste("Animal density (ind. g" ^-1,")"))) +
  theme(); g1

ggsave("Graphs/For_animals_DEN.png", 
       dpi = 300, 
       width = 12,
       height = 8, 
       units = "in")

# Forni_fld organic matter
g2 <- ggplot(FOR_fld_OUT, aes(x = OM)) + 
  geom_density(fill = "black", alpha = 0.3) + 
  theme_classic(base_size = 20) + ylab("Density") + 
  xlab("Organic matter (%)") +
  theme(); g2

ggsave("Graphs/For_OM_DEN.png", 
       dpi = 300, 
       width = 12,
       height = 8, 
       units = "in")

# Forni_fld oxygen
g3 <- ggplot(FOR_fld_OUT, aes(x = mean_oxygen)) + 
geom_density(fill = "Darkblue", alpha = 0.3) + 
  theme_classic(base_size = 20) + ylab("Density") + 
  xlab(expression(paste("Oxygen concentration (",mu, "mol L" ^-1,")"))) +
  theme(); g3

ggsave("Graphs/For_oxygen_DEN.png", 
       dpi = 300, 
       width = 12,
       height = 8, 
       units = "in")

## Lyr field
# Lyr_fld animal density
LYR_fld_OUT_plot <- data.frame(ani_den = c(LYR_fld_OUT$rot_den, LYR_fld_OUT$tar_den),
                               ani_type = c(rep("Rotifera", 40), rep("Tardigrada", 40)))

g4 <- ggplot(LYR_fld_OUT_plot, aes(x = ani_den, fill = ani_type)) + 
  geom_density(alpha = 0.75) + 
  scale_fill_brewer(palette = "Greens", name = NULL) + 
  theme_classic(base_size = 20) + ylab("Density") + 
  xlab(expression(paste("Animal density (ind. g" ^-1,")"))) + 
  theme(legend.position = c(0.8, 0.7)); g4

ggsave("Graphs/Lyr_animals_DEN.png", 
       dpi = 300, 
       width = 12,
       height = 8, 
       units = "in")

# Lyr_fld organic matter
g5 <- ggplot(LYR_fld_OUT, aes(x = OM)) + 
  geom_density(fill = "black", alpha = 0.3) + 
  theme_classic(base_size = 20) + ylab("Density") + 
  xlab("Organic matter (%)") +
  theme(); g5

ggsave("Graphs/Lyr_OM_DEN.png", 
       dpi = 300, 
       width = 12,
       height = 8, 
       units = "in")

# Lyr_fld Oxygen
g6 <- ggplot(LYR_fld_OUT, aes(x = sed_oxygen)) + 
  geom_density(fill = "Darkblue", alpha = 0.3) + 
  theme_classic(base_size = 20) + ylab("Density") + 
  xlab(expression(paste("Oxygen concentration (",mu, "mol L" ^-1,")"))) +
  theme(); g6

ggsave("Graphs/Lyr_oxygen_DEN.png", 
       dpi = 300, 
       width = 12,
       height = 8, 
       units = "in")

# relation of animals
g7 <- ggplot(LYR_fld_OUT, aes(x = rot_den, y = tar_den)) + 
  geom_point(color = "darkgreen", alpha = 0.75) + 
  geom_smooth(method = "lm", color = "darkgreen", alpha = 0.3) + 
  theme_classic(base_size = 20) + 
  xlab("Rotifers density") + ylab("Tardigrades density") + 
  stat_regline_equation(label.y = 150, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 125, aes(label = ..rr.label..)); g7
rm(plot_dat)

ggsave("Graphs/Lyr_animals_LM.png", 
       dpi = 300, 
       width = 12,
       height = 8, 
       units = "in")

### Single plots of models -------------------------------------- 
## Field
# Forni densities 
p1.1 <- visreg(m_for_fld_1, "animal_dens",
               line = list(col = "darkgreen"),
               partial = TRUE, gg = TRUE)

p1.2 <- p1.1 + 
  theme_classic(base_size = 20) + 
  xlab(expression(paste("Animal density (ind. g" ^-1,")"))) + 
  ylab(expression(paste("Oxygen concentration ( " ,mu, "mol L" ^-1,")"))); p1.2

ggsave("Graphs/For_fld_LM.png", 
       dpi = 300, 
       width = 12,
       height = 8, 
       units = "in")

# Lyr sediment 
p2.1 <- visreg(m_lyr_fld_1.1, "ani_den", 
               line = list(col = "darkgreen"),
               partial = TRUE, gg = TRUE); p2.1

p2.2 <- p2.1 + theme_classic(base_size = 20) + 
  xlab(expression(paste("Animal density (ind. g" ^-1,")"))) + 
  ylab(expression(paste("Oxygen concentration ( " ,mu, "mol L" ^-1,")"))); p2.2

ggsave("Graphs/Lyr_fld_sediment_LM.png", 
       dpi = 300, 
       width = 12,
       height = 8, 
       units = "in")

## Experiment
# Forni
p4.1 <- visreg(m_for_exp_1, "Animals", 
               line = list(col = "darkgreen"),
               gg = TRUE, partial = TRUE)

p4.2 <- p4.1 + theme_classic(base_size = 20) + 
  xlab("") + ylab(expression(paste("Oxygen total amount ( ",mu,"mol)"))); p4.2

ggsave("Graphs/For_exp_LM.png", 
       dpi = 300, 
       width = 12,
       height = 8, 
       units = "in")


# Lyr
p5.1 <- visreg(m_lyr_exp_2, "Mixed", "Animals",
               line = list(col = "darkgreen"),
               fill = list(col = "grey"),
               plot = TRUE, gg = TRUE, overlay = TRUE, partial = TRUE)

p5.2 <- p5.1 + 
  theme_classic(base_size = 20) + 
  xlab("") + ylab(expression(paste("Oxygen total amount ( ",mu,"mol)"))) + 
  theme(legend.position = c(0.80, 0.90), legend.title = element_blank()) + ylim(c(1,7)); p5.2

ggsave("Graphs/Lyr_exp_LM.png", 
       dpi = 300, 
       width = 12,
       height = 8, 
       units = "px")

### Profiles -------------------------------------------------- 
## Field
# Forni
fld_prof_FOR <- ggplot(data = subset(df_prof_for_all, profile_ID %in% c("3","6","16","5")), 
                       aes(x = oxygen_concentration_ul_L, y = depth, fill = profile_ID)) +
  geom_point() + scale_y_reverse() + scale_x_reverse() + 
  facet_wrap(. ~ profile_ID, scale = "free", nrow = 2) + geom_line(orientation = "y") +
  xlab(expression(paste("Oxygen (",mu,"mol L"^-1, ")"))) + ylab(expression(paste("Depth (",mu,"m)"))) +
  theme_classic(base_size = 20) + 
  theme(legend.position = "none"); fld_prof_FOR

ggsave("Graphs/For_fld_PROFILES.png", 
       dpi = 300, 
       width = 16,
       height = 16, 
       units = "in")
# Lyr
fld_prof_lyr <- ggplot(data = subset(df_prof_lyr_all, profile_ID %in% c("Profile 1.1", "Profile 3.1", "Profile 4.1" , "Profile 5.1")), 
                       aes(x = oxygen_concentration_ul_L, y = depth)) +
  geom_point() + scale_y_reverse() + scale_x_reverse() + 
  facet_wrap(. ~ profile_ID, scales = "free", nrow = 2) + geom_line(orientation = "y") +
  xlab(expression(paste("Oxygen (",mu,"mol L"^-1, ")"))) + ylab(expression(paste("Depth (",mu,"m)"))) + 
  theme_classic(base_size = 20); fld_prof_lyr

ggsave("Graphs/Lyr_fld_PROFILES.png", 
       dpi = 300, 
       width = 16,
       height = 16, 
       units = "in")

## Experiment
# Forni
prof_FORexp <- data.table(subset(exp_SED, Glacier == "Forni"))

prof_out_FORexp <- prof_FORexp[, .("MeanOxygen" = mean(Oxygen), 
                            "se" = std.error(Oxygen)), 
                        by = c("Animals", "Depth")]; rm(prof_FORexp)

exp_prof_FOR <- ggplot(data = prof_out_FORexp, aes(x = MeanOxygen, y = Depth, color = Animals)) +
  geom_point() + scale_y_reverse() + scale_x_reverse() + 
  facet_grid(. ~ Animals) + geom_path(data = prof_out_FORexp[c(1:30),], aes(color = Animals)) +
  geom_hline(yintercept = 3000, linetype = "dashed", color = "black", size = 1.1) + 
  geom_errorbarh(aes(xmin = MeanOxygen - se, xmax = MeanOxygen + se),
                 position = position_dodge(.9)) + ylab(expression(paste("Depth (",mu,"m",")"))) + 
  xlab(expression(paste("Oxygen (",mu,"mol L"^-1, ")"))) +
  theme_classic() + theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold"), 
                          axis.title.x = element_text(size = 15),
                          axis.title.y = element_text(size = 14,vjust = 2),
                          axis.text.x = element_text(size = 13),
                          axis.text.y = element_text(size = 13),
                          strip.text.x = element_text(size = 12),
                          strip.text.y = element_text(size = 12),
                          plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines")) +
  ggtitle("Forni - oxygen profiles") + scale_color_manual(values = c("grey","darkgreen")) + 
  labs(color = "Treatment"); exp_prof_FOR

ggsave("Graphs/For_exp_PROFILES.png", 
       dpi = 300, 
       width = 16,
       height = 16, 
       units = "in")

# Lyr 
prof_LYRexp <- data.table(subset(exp_SED, Glacier == "Longyearbreen"))

prof_out_LYRexp <- prof_LYRexp[, .("MeanOxygen" = mean(Oxygen), 
                                   "se" = std.error(Oxygen)), 
                               by = c("Mixed", "Depth", "Animals")]; rm(prof_LYRexp)

exp_prof_LYR <-  ggplot(data = prof_out_LYRexp, aes(x = MeanOxygen, y = Depth, color = Animals)) +
  geom_point() + scale_y_reverse() + scale_x_reverse() + 
  facet_grid(Mixed ~ Animals) + geom_path(data = prof_out_LYRexp[c(1:60),], aes(color = Animals)) +
  geom_hline(yintercept = 3000, linetype = "dashed", color = "black", size = 1.1) + 
  geom_errorbarh(aes(xmin = MeanOxygen - se, xmax = MeanOxygen + se),
                 position = position_dodge(.9)) + ylab(expression(paste("Depth (",mu,"m",")"))) + 
  xlab(expression(paste("Oxygen (",mu,"mol L"^-1, ")"))) +
  theme_classic() + theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold"), 
                           axis.title.x = element_text(size = 15),
                           axis.title.y = element_text(size = 14,vjust = 2),
                           axis.text.x = element_text(size = 13),
                           axis.text.y = element_text(size = 13),
                           strip.text.x = element_text(size = 12),
                           strip.text.y = element_text(size = 12),
                           plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines")) +
  ggtitle("Longyearbreen - oxygen profiles") + scale_color_manual(values = c("grey","darkgreen")) + 
  labs(color = "Treatment"); exp_prof_LYR

ggsave("Graphs/Lyr_exp_PROFILES.png", 
       dpi = 300, 
       width = 16,
       height = 16, 
       units = "in")