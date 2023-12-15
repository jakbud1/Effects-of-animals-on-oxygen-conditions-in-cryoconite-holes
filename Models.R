### Global 
source("Data_prep.R")

## Libs
library(glmmTMB)
library(visreg)
library(data.table)
library(plyr)

# ### Field - Forni ----------------------------------------------------
# # density (gaussian) 
# m_for_fld_1 <- glmmTMB(mean_oxygen ~ animal_dens + OM + (1|cryo_ID),
#                 REML = TRUE, data = FOR_fld_OUT); summary(m_for_fld_1)
# plot(residuals(m_for_fld_1, type = "pearson") ~ fitted(m_for_fld_1))
# qqnorm(residuals(m_for_fld_1, type = "pearson")); qqline(residuals(m_for_fld_1, type = "pearson"))
# 
# ### Field - Longyearbreen --------------------------------------------
# # sediment with ratio of animals; density (gaussian)
# m_lyr_fld_1.1 <- glmmTMB(sed_oxygen ~ OM + ani_den * rot_to_tar_ratio, 
#                          REML = TRUE, data = LYR_fld_OUT); summary(m_lyr_fld_1.1)
# 
# plot(residuals(m_lyr_fld_1.1, type = "pearson") ~ fitted(m_lyr_fld_1.1))
# qqnorm(residuals(m_lyr_fld_1.1, type = "pearson")); qqline(residuals(m_lyr_fld_1.1, type = "pearson"))

### Experiment - Forni -----------------------------------------------
AUCexp_FOR$AUC <- as.numeric(as.character(AUCexp_FOR$AUC))

## Model - oxygen
m_for_exp_1 <- glmmTMB(AUC ~ Animals, 
                REML = TRUE, data = AUCexp_FOR); summary(m_for_exp_1)

qqnorm(residuals(m_for_exp_1, type = "pearson")); qqline(residuals(m_for_exp_1, type = "pearson"))
plot(residuals(m_for_exp_1) ~ fitted(m_for_exp_1))

## Model - OM
m1_exp_FOR <- glmmTMB(OM_diff_scaled ~ Anim_treatment, 
                      REML = TRUE, data = df_exp_FOR); summary(m1_exp_FOR)

plot(residuals(m1_exp_FOR), fitted(m1_exp_FOR))
qqnorm(residuals(m1_exp_FOR)); qqline(residuals(m1_exp_FOR))
visreg(m1_exp_FOR)

### Experiment - Longyearbreen ---------------------------------------
# Model - OM
m1_exp_LYR <- glmmTMB(OM_diff_scaled ~ Anim_treatment * Mixing_treatment, 
                      REML = TRUE, data = df_exp_LYR); summary(m1_exp_LYR)

plot(residuals(m1_exp_LYR), fitted(m1_exp_LYR))
qqnorm(residuals(m1_exp_LYR)); qqline(residuals(m1_exp_LYR))
visreg(m1_exp_LYR, "Anim_treatment", "Mixing_treatment")

### Experiment - Ebenferner -----------------------------------------
# Model - OM 
m1_exp_STE <- glmmTMB(OM_diff_scaled ~ Anim_treatment * Mixing_treatment, 
                      REML = TRUE, data = df_exp_STE); summary(m1_exp_STE)

plot(residuals(m1_exp_STE), fitted(m1_exp_STE))
qqnorm(residuals(m1_exp_STE)); qqline(residuals(m1_exp_STE))
visreg(m1_exp_STE,"Anim_treatment", "Mixing_treatment")
