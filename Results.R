### Global 
source("Models.R")

## Libs
library(ggplot2)
library(visreg)
library(data.table)
library(plotrix)

### Descriptive data
## Field
mean(FOR_fld_OUT$animal_dens)

mean(LYR_fld_OUT$ani_den)
mean(subset(LYR_fld_OUT, ani_type == "rotifera")$ani_den)
mean(subset(LYR_fld_OUT, ani_type == "tardigrada")$ani_den)

mean(LYR_fld_OUT$OM)
mean(FOR_fld_OUT$OM)

mean((LYR_fld_OUT$diameter_longitudinal_cm[1:40])[-c(22,30)])

### Models outputs --------------------------------------------
## Field
# Forni (density)
summary(m_for_fld_1)

# Lyr sediment (density)
summary(m_lyr_fld_1.1)

# Lyr water (density)
summary(m_lyr_fld_2.1)

## Experiment
# Forni
summary(m_for_exp_1)

# Lyr
summary(m_lyr_exp_1)

summary(m_lyr_exp_2)

### Single plots of model --------------------------------------
## Field
# Forni densities
p1 <- visreg(m_for_fld_1, "animal_dens" ,
             line.par = list(col = 'black'), gg = TRUE)
p1 + theme_classic() + geom_point(color = "black")

# Lyr sediment
p3.1 <- visreg(m_lyr_fld_1.1, "ani_den", "Animal", 
             plot = FALSE, overlay = TRUE, partial = TRUE)

p3.2 <- ggplot(p3.1$fit, aes(ani_den, visregFit, linetype = factor(Animal), fill = factor(Animal))) +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.5, 
              colour = "grey50", linetype = 1, size = 0.3) +
  geom_line(size = 1) + theme_classic() + xlab(expression(paste("Animals density (ind. ml"^-1, ")"))) + 
  ylab(expression(paste("Oxygen ( ",mu,"mol L"^-1, ")"))) + 
  geom_point(data = p3.1$res, aes(ani_den, visregRes, color = factor(Animal)), size = 2, alpha = 0.5) + 
  scale_fill_grey(start = 0.2, end = 0.8) + ggtitle("Sediment surface") + 
  scale_color_manual(values = c("Darkblue","darkred")) +
  labs(linetype = "Animal type", fill = "Animal type", color = "Animal type") + ylim(c(300,450)) + 
  theme(plot.title = element_text(hjust = 0.5)); p3.2

# Lyr water
p4.1 <- visreg(m_lyr_fld_2.1, "ani_den", "Animal", 
               plot = FALSE, overlay = TRUE, partial = TRUE)

p4.2 <- ggplot(p4.1$fit, aes(ani_den, visregFit, linetype = factor(Animal), fill = factor(Animal))) +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.5, 
              colour = "grey50", linetype = 1, size = 0.3) +
  geom_line(size = 1) + theme_classic() + xlab(expression(paste("Animals density (ind. ml"^-1, ")"))) + 
  ylab(expression(paste("Oxygen ( ",mu,"mol L"^-1, ")"))) + 
  geom_point(data = p4.1$res, aes(ani_den, visregRes, color = factor(Animal)), size = 2, alpha = 0.5) + 
  scale_fill_grey(start = 0.2, end = 0.8) + 
  scale_color_manual(values = c("Darkblue","darkred")) + ggtitle("Water above the sediment") + 
  labs(linetype = "Animal type", fill = "Animal type", color = "Animal type") + ylim(c(340,450)) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none"); p4.2

## Experiment
# Forni
p5.1 <- visreg(m_for_exp_1, "Animals", 
               plot = TRUE, overlay = TRUE, partial = TRUE)
# Lyr
p6.1 <- visreg(m_lyr_exp_2, "Mixed", "Animals" ,  
               plot = TRUE, overlay = TRUE, partial = TRUE)

### Profiles -------------------------------------------------- 
## Field
# Forni
fld_prof_FOR <- ggplot(data = df_prof_for_all, 
                       aes(x = oxygen_concentration_ul_L, y = depth)) +
  geom_point() + scale_y_reverse() + scale_x_reverse() + 
  facet_wrap(. ~ profile_ID, scale = "free") + geom_line() +
  xlab(expression(paste("Oxygen (",mu,"mol L"^-1, ")"))) +
  theme_classic() + theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold"), 
                          axis.title.x = element_text(size = 15),
                          axis.title.y = element_text(size = 14,vjust = 2),
                          axis.text.x = element_text(size = 13),
                          axis.text.y = element_text(size = 13),
                          strip.text.x = element_text(size = 12),
                          strip.text.y = element_text(size = 12),
                          plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines")) +
  ggtitle("Forni - oxygen profiles field"); fld_prof_FOR

# Lyr
fld_prof_lyr <- ggplot(data = df_prof_lyr_all, 
                       aes(x = oxygen_concentration_ul_L, y = depth)) +
  geom_point() + scale_y_reverse() + scale_x_reverse() + 
  facet_wrap(. ~ profile_ID, scales = "free_y") + geom_line() +
  xlab(expression(paste("Oxygen (",mu,"mol L"^-1, ")"))) +
  theme_classic() + theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold"), 
                          axis.title.x = element_text(size = 15),
                          axis.title.y = element_text(size = 14,vjust = 2),
                          axis.text.x = element_text(size = 13),
                          axis.text.y = element_text(size = 13),
                          strip.text.x = element_text(size = 12),
                          strip.text.y = element_text(size = 12),
                          plot.margin = unit(c(1.5,1.5,1.5,1.5), "lines")) +
  ggtitle("Longyear - oxygen profiles field"); fld_prof_lyr

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