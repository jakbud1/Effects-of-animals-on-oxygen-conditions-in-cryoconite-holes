### Global 
source("Data_prep.R")

## Libs
library(glmmTMB)
library(visreg)
library(data.table)
library(plyr)

### Field - Forni ----------------------------------------------------
# density (gaussian) 
m_for_fld_1 <- glmmTMB(mean_oxygen ~ animal_dens + OM + (1|cryo_ID),
                REML = TRUE, data = FOR_fld_OUT); summary(m_for_fld_1)
plot(residuals(m_for_fld_1, type = "pearson") ~ fitted(m_for_fld_1))
qqnorm(residuals(m_for_fld_1, type = "pearson")); qqline(residuals(m_for_fld_1, type = "pearson"))

### Field - Longyearbreen --------------------------------------------
# sediment with ratio of animals; density (gaussian)
m_lyr_fld_1.1 <- glmmTMB(sed_oxygen ~ OM + ani_den * rot_to_tar_ratio, 
                         REML = TRUE, data = LYR_fld_OUT); summary(m_lyr_fld_1.1)

plot(residuals(m_lyr_fld_1.1, type = "pearson") ~ fitted(m_lyr_fld_1.1))
qqnorm(residuals(m_lyr_fld_1.1, type = "pearson")); qqline(residuals(m_lyr_fld_1.1, type = "pearson"))

### Experiment - Forni -----------------------------------------------
AUCexp_FOR <- subset(AUCexp_SED, Glacier == "Forni")
AUCexp_FOR$AUC <- as.numeric(as.character(AUCexp_FOR$AUC))

## Models
m_for_exp_1 <- glmmTMB(AUC ~ Animals, 
                REML = TRUE, data = AUCexp_FOR); summary(m_for_exp_1)

qqnorm(residuals(m_for_exp_1, type = "pearson")); qqline(residuals(m_for_exp_1, type = "pearson"))
plot(residuals(m_for_exp_1) ~ fitted(m_for_exp_1))

### Experiment - Longyearbreen ---------------------------------------
AUCexp_LYR <- subset(AUCexp_SED, Glacier == "Longyearbreen")
AUCexp_LYR$AUC <- as.numeric(as.character(AUCexp_LYR$AUC))

# Modeling
m_lyr_exp_1 <- glmmTMB(AUC ~ Animals * Mixed, 
                       REML = TRUE, data = AUCexp_LYR); summary(m_lyr_exp_1)

qqnorm(residuals(m_lyr_exp_1, type = "pearson")); qqline(residuals(m_lyr_exp_1, type = "pearson"))
plot(residuals(m_lyr_exp_1, type = "pearson") ~ fitted(m_lyr_exp_1))
