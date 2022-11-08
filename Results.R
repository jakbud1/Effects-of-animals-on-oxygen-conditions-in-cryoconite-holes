### Global 
source("Models.R")

## Libs
library(ggplot2)
library(visreg)
library(data.table)

### Models outputs --------------------------------------------
## Field
# Forni counts
summary(m_for_fld_1)
anova(m_for_fld_0, m_for_fld_1)
# Forni biomass
summary(m_for_fld_2)
anova(m_for_fld_0, m_for_fld_2)

# Lyr sediment
summary(m_lyr_fld_1.1)
anova(m_lyr_fld_1.0, m_lyr_fld_1.1)

# Lyr water -> Skoro także w wodzie tzn. ze nie jest to efekt bioturbacji.
summary(m_lyr_fld_1.1)
anova(m_lyr_fld_1.0, m_lyr_fld_1.1)

## Experiment
# Forni
summary(m_for_exp_1)
anova(m_for_exp_0, m_for_exp_1)
# Lyr
summary(m_lyr_exp_1)
anova(m_lyr_exp_0, m_lyr_exp_1, test = "Chisq")

### Single plots of model --------------------------------------
## Field
# Forni
# Lyr
## Experiment
# Forni
# Lyr

### Profiles --------------------------------------------------

### Arranged plots --------------------------------------------


## Wizualizacja RAW - LYR in field 
ox1.s <- ggplot(ox, aes(x = ani_den, y = sed_oxygen)) + 
  geom_point() + geom_smooth(method = "lm") + facet_wrap(.~ ani_type, scales = "free"); ox1.s

ox1.w <- ggplot(ox, aes(x = ani_den, y = water_oxygen)) + 
  geom_point() + geom_smooth(method = "lm") + facet_wrap(.~ ani_type, scales = "free"); ox1.w

## LYR - modele field (osad)
vv1 <- visreg(m_lyr_fld_1.1, "ani_den", by = "Animal",overlay = TRUE, gg = TRUE) + 
  xlab(expression(paste("Density of animals [ind. ml"^"-1","]"))) +  ylab(expression(paste("Oxygen (",mu,"mol L"^"-1",")"))) +
  ggtitle("Surface of sediment") + theme_bw(); vv1
anova(m_lyr_fld_1.0, m_lyr_fld_1.1, test = "Chisq")

png("Graphs/Tlen_a_zwierzeta_teren_osad.png", width = 1200, height = 800, units = "px", res = 200)
vv1
dev.off()

## LYR - modele field (woda)
vv2 <- visreg(mw2.1, "ani_den", by = "Animal", overlay = TRUE, gg = TRUE) + 
  xlab(expression(paste("Density of animals [ind. ml"^"-1","]"))) +  ylab(expression(paste("Oxygen (",mu,"mol L"^"-1",")"))) + 
  ggtitle("Above water") + theme_bw() ; vv2
anova(mw2.0, mw2.1, test = "Chisq")

png("Graphs/Tlen_a_zwierzeta_teren_woda.png", width = 1200, height = 800, units = "px", res = 200)
vv2
dev.off()

### Experiment Visualization
## Forni experiment
AUCexp_FOR$Animals <- revalue(AUCexp_FOR$Animals, c("TZ" = "Animals", "BZ" = "Without animals"))

## Visualisation of profiles
EKS_AN_FOR <- ggplot(data = exp_SED, aes(y = Oxygen, x = Depth, color = Animals)) + 
  geom_smooth() + 
  scale_color_discrete(name = "", labels = c("Animals", "Without animals")) + 
  xlab(expression(paste("Depth (",mu,"m",")"))) + 
  ylab(expression(paste("Oxygen (",mu,"mol L"^"-1",")"))) +
  theme_bw() + theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"), 
                     axis.title.x = element_text(size = 15),
                     axis.title.y = element_text(size = 15,vjust = 2),
                     axis.text.x = element_text(size = 13),
                     axis.text.y = element_text(size = 13),
                     strip.text.x = element_text(size = 12),
                     strip.text.y = element_text(size = 12)) +  
  ggtitle("Forni_mixed_lab_oxygenProfiles"); EKS_AN_FOR

## Lyr experiment 
dfSEDIMENTexp2LYR <- subset(dfSEDIMENTexp2, Glacier != "Forni")

dfSEDIMENTexp2LYR$Animals <- revalue(dfSEDIMENTexp2LYR$Animals, c("TZ" = "Animals", "BZ" = "Without animals"))
dfSEDIMENTexp2LYR$Mixed <- revalue(dfSEDIMENTexp2LYR$Mixed, c("TM" = "Mixed", "NM" = "Not mixed"))



## Visualisation raw data
EKS_AN <- ggplot(data = dfSEDIMENTexp2LYR, aes(y = Oxygen, x = Depth, color = Animals)) + 
  geom_smooth() + 
  facet_grid(. ~ Mixed) + 
  scale_color_discrete(name = "", labels = c("Animals", "Without animals")) + 
  xlab(expression(paste("Depth (",mu,"m",")"))) + 
  ylab(expression(paste("Oxygen (",mu,"mol L"^"-1",")"))) +
  theme_bw() + theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"), 
                     axis.title.x = element_text(size = 15),
                     axis.title.y = element_text(size = 15,vjust = 2),
                     axis.text.x = element_text(size = 13),
                     axis.text.y = element_text(size = 13),
                     strip.text.x = element_text(size = 12),
                     strip.text.y = element_text(size = 12)) + geom_point(alpha = 0.3); EKS_AN

png("Graphs/Tlen_a_zwierzeta.png", width = 1200, height = 800, units = "px", res = 200)
EKS_AN
dev.off()

## visualisation models 
# Model visualisation
AUCVIS <- visreg(m_lyr_exp_1, "Animals", "Mixed", gg = TRUE) + 
  ylab(expression(paste("Oxygen (",mu,"mol)"))) +
  xlab("") + 
  theme_bw() + theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"), 
                     axis.title.x = element_text(size = 15),
                     axis.title.y = element_text(size = 15,vjust = 2),
                     axis.text.x = element_text(size = 13),
                     axis.text.y = element_text(size = 13),
                     strip.text.x = element_text(size = 12),
                     strip.text.y = element_text(size = 12)); AUCVIS 

png("Graphs/Tlen_a_zwierzeta2.png", width = 1200, height = 800, units = "px", res = 200)
AUCVIS
dev.off()