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
mean(subset(LYR_fld_OUT, ani_type == "Rotifera")$ani_den)
mean(subset(LYR_fld_OUT, ani_type == "Tardigrada")$ani_den)

mean(LYR_fld_OUT$OM)
mean(FOR_fld_OUT$OM)

mean((LYR_fld_OUT$diameter_longitudinal_cm[1:40])[-c(22,30)])

summary(lm(LYR_fld_OUT$ani_den[1:40] ~ LYR_fld_OUT$ani_den[41:80]))

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

### Rawa data ---------------------------------------------------
## Forni field
# Forni_fld animal density
g1 <- ggplot(FOR_fld_OUT, aes(x = animal_dens)) + 
  geom_density(fill = "darkgreen", alpha = 0.3) + 
  theme_classic(base_size = 14) + ylab("Density") + 
  xlab(expression(paste("Animal density (ind. g" ^-1,")"))) +
  theme(); g1

ggsave("Graphs/For_animals_DEN.png", 
       dpi = 300, 
       width = 1200,
       height = 800, 
       units = "px")

# Forni_fld organic matter
g2 <- ggplot(FOR_fld_OUT, aes(x = OM)) + 
  geom_density(fill = "black", alpha = 0.3) + 
  theme_classic(base_size = 14) + ylab("Density") + 
  xlab("Organic matter (%)") +
  theme(); g2

ggsave("Graphs/For_OM_DEN.png", 
       dpi = 300, 
       width = 1200,
       height = 800, 
       units = "px")

# Forni_fld oxygen
g3 <- ggplot(FOR_fld_OUT, aes(x = mean_oxygen)) + 
geom_density(fill = "Darkblue", alpha = 0.3) + 
  theme_classic(base_size = 14) + ylab("Density") + 
  xlab(expression(paste("Oxygen concentration (",mu, "mol L" ^-1,")"))) +
  theme(); g3

ggsave("Graphs/For_oxygen_DEN.png", 
       dpi = 300, 
       width = 1200,
       height = 800, 
       units = "px")

## Lyr field
# Lyr_fld animal density
g4 <- ggplot(LYR_fld_OUT, aes(x = ani_den, fill = ani_type)) + 
  geom_density(alpha = 0.75) + 
  scale_fill_brewer(palette = "Greens", name = NULL) + 
  theme_classic(base_size = 14) + ylab("Density") + 
  xlab(expression(paste("Animal density (ind. g" ^-1,")"))) + 
  theme(legend.position = c(0.8, 0.7)); g4

ggsave("Graphs/Lyr_animals_DEN.png", 
       dpi = 300, 
       width = 1200,
       height = 800, 
       units = "px")

# Lyr_fld organic matter
g5 <- ggplot(LYR_fld_OUT[1:40,], aes(x = OM)) + 
  geom_density(fill = "black", alpha = 0.3) + 
  theme_classic(base_size = 14) + ylab("Density") + 
  xlab("Organic matter (%)") +
  theme(); g5

ggsave("Graphs/Lyr_OM_DEN.png", 
       dpi = 300, 
       width = 1200,
       height = 800, 
       units = "px")

# Lyr_fld Oxygen
g6 <- ggplot(LYR_fld_OUT[1:40,], aes(x = sed_oxygen)) + 
  geom_density(fill = "Darkblue", alpha = 0.3) + 
  theme_classic(base_size = 14) + ylab("Density") + 
  xlab(expression(paste("Oxygen concentration (",mu, "mol L" ^-1,")"))) +
  theme(); g6

ggsave("Graphs/Lyr_oxygen_DEN.png", 
       dpi = 300, 
       width = 1200,
       height = 800, 
       units = "px")

# relation of animals
plot_dat <- data.frame(Rot = LYR_fld_OUT$ani_den[1:40], 
                       Tar = LYR_fld_OUT$ani_den[41:80])

g7 <- ggplot(plot_dat, aes(x = Rot, y = Tar)) + 
  geom_point(color = "darkgreen", alpha = 0.75) + 
  geom_smooth(method = "lm", color = "darkgreen", alpha = 0.3) + 
  theme_classic(base_size = 14) + 
  xlab("Rotifers density") + ylab("Tardigrades density") + 
  stat_regline_equation(label.y = 150, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 125, aes(label = ..rr.label..)); g7
rm(plot_dat)

ggsave("Graphs/Lyr_animals_LM.png", 
       dpi = 300, 
       width = 1200,
       height = 800, 
       units = "px")

### Single plots of models --------------------------------------
## Field
# Forni densities
p1.1 <- visreg(m_for_fld_1, "animal_dens",
             plot = FALSE, partial = TRUE)

p1.2 <- ggplot(p1.1$fit, aes(animal_dens, visregFit)) + 
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.2, 
              colour = "darkgreen", linetype = 1, size = 0.5) +
  geom_line(size = 1, color = "darkgreen") + 
  theme_classic(base_size = 14) + 
  xlab(expression(paste("Animal density (ind. g" ^-1,")"))) + 
  ylab(expression(paste("Oxygen concentration ( " ,mu, "mol L" ^-1,")"))); p1.2

ggsave("Graphs/For_fld_LM.png", 
       dpi = 300, 
       width = 2400,
       height = 1600, 
       units = "px")

# Lyr sediment
p2.1 <- visreg(m_lyr_fld_1.1, "ani_den", "Animal", 
             plot = FALSE, overlay = TRUE, partial = TRUE)

p2.2 <- ggplot(p2.1$fit, aes(ani_den, visregFit, linetype = factor(Animal), fill = factor(Animal))) +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.5, 
              colour = "grey50", linetype = 1, size = 0.3) +
  geom_line(size = 1) + theme_classic(base_size = 14) + 
  xlab(expression(paste("Animal density (ind. g"^-1, ")"))) + 
  ylab(expression(paste("Oxygen concentration ( ",mu,"mol L"^-1, ")"))) + 
  geom_point(data = p2.1$res, aes(ani_den, visregRes, color = factor(Animal)), size = 2, alpha = 0.5) + 
  scale_fill_grey(start = 0.2, end = 0.8) +
  scale_color_manual(values = c("Darkblue","Darkred")) +
  labs(linetype = "Animal type", fill = "Animal type", color = "Animal type") + ylim(c(300,450)) +
  theme(legend.position = c(0.80,0.90), legend.title = element_blank()); p2.2

ggsave("Graphs/Lyr_fld_sediment_LM.png", 
       dpi = 300, 
       width = 2400,
       height = 1600, 
       units = "px")

# Lyr water
p3.1 <- visreg(m_lyr_fld_2.1, "ani_den", "Animal", 
               plot = FALSE, overlay = TRUE, partial = TRUE)

p3.2 <- ggplot(p3.1$fit, aes(ani_den, visregFit, linetype = factor(Animal), fill = factor(Animal))) +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.5, 
              colour = "grey50", linetype = 1, size = 0.3) +
  geom_line(size = 1) + theme_classic() + xlab(expression(paste("Animals density (ind. ml"^-1, ")"))) + 
  ylab(expression(paste("Oxygen concentration ( ",mu,"mol L"^-1, ")"))) + 
  geom_point(data = p3.1$res, aes(ani_den, visregRes, color = factor(Animal)), size = 2, alpha = 0.5) + 
  scale_fill_grey(start = 0.2, end = 0.8) + 
  scale_color_manual(values = c("Darkblue","darkred")) + ggtitle("Water above the sediment") + 
  labs(linetype = "Animal type", fill = "Animal type", color = "Animal type") + ylim(c(340,450)) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none"); p3.2

## Experiment
# Forni
p4.1 <- visreg(m_for_exp_1, "Animals", 
               line = list(col = "darkgreen"),
               gg = TRUE, partial = TRUE)

p4.2 <- p4.1 + theme_classic(base_size = 14) + 
  xlab("") + ylab(expression(paste("Oxygen total amount ( ",mu,"mol)"))); p4.2

ggsave("Graphs/For_exp_LM.png", 
       dpi = 300, 
       width = 2400,
       height = 1600, 
       units = "px")


# Lyr
p5.1 <- visreg(m_lyr_exp_2, "Mixed", "Animals",
               line = list(col = "darkgreen"),
               fill = list(col = "grey"),
               plot = TRUE, gg = TRUE, overlay = TRUE, partial = TRUE)

p5.2 <- p5.1 + 
  theme_classic(base_size = 14) + 
  xlab("") + ylab(expression(paste("Oxygen total amount ( ",mu,"mol)"))) + 
  theme(legend.position = c(0.80, 0.90), legend.title = element_blank()) + ylim(c(1,7)); p5.2

ggsave("Graphs/Lyr_exp_LM.png", 
       dpi = 300, 
       width = 2400,
       height = 1600, 
       units = "px")


# dfbox <- data.frame()
# 
# p5.2 <- ggplot(data = p5.1$fit, aes(Animals, visregFit, fill = Mixed)) + 
#   geom_boxplot() + 
#   geom_rect(alpha = 0.4, data = p5.1$fit, aes(xmin = c(0.5,1,1.5,2), xmax = c(1,1.5,2,2.5), ymin = visregLwr, ymax = visregUpr, fill = Animals)) + 
#   geom_point(data = p5.1$res, aes(Animals, visregRes, fill = factor(Mixed)), size = 2, alpha = 0.5); p5.2



### Profiles -------------------------------------------------- 
## Field
# Forni
fld_prof_FOR <- ggplot(data = subset(df_prof_for_all, profile_ID %in% c("3","6","16","5")), 
                       aes(x = oxygen_concentration_ul_L, y = depth, fill = profile_ID)) +
  geom_point() + scale_y_reverse() + scale_x_reverse() + 
  facet_wrap(. ~ profile_ID, scale = "free", nrow = 2) + geom_line(orientation = "y") +
  xlab(expression(paste("Oxygen (",mu,"mol L"^-1, ")"))) + ylab(expression(paste("Depth (",mu,"m)"))) +
  theme_classic(base_size = 14) + 
  theme(legend.position = "none"); fld_prof_FOR

ggsave("Graphs/For_fld_PROFILES.png", 
       dpi = 300, 
       width = 1600,
       height = 1600, 
       units = "px")
# Lyr
fld_prof_lyr <- ggplot(data = subset(df_prof_lyr_all, profile_ID %in% c("Profile 1.1", "Profile 3.1", "Profile 4.1" , "Profile 5.1")), 
                       aes(x = oxygen_concentration_ul_L, y = depth)) +
  geom_point() + scale_y_reverse() + scale_x_reverse() + 
  facet_wrap(. ~ profile_ID, scales = "free", nrow = 2) + geom_line(orientation = "y") +
  xlab(expression(paste("Oxygen (",mu,"mol L"^-1, ")"))) + ylab(expression(paste("Depth (",mu,"m)"))) + 
  theme_classic(base_size = 14); fld_prof_lyr

ggsave("Graphs/Lyr_fld_PROFILES.png", 
       dpi = 300, 
       width = 1600,
       height = 1600, 
       units = "px")

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


### Arranged plots --------------------------------------------
