### Global 
source("Data_prep.R")

## Libs
library(glmmTMB)
library(visreg)
library(data.table)
library(plyr)

### Field - Forni ----------------------------------------------------
# density (gaussian) 
m_for_fld_0 <- glmmTMB(mean_oxygen ~ 1 + (1|krio_ID),
                data = FOR_fld_OUT); summary(m_for_fld_0)

m_for_fld_1 <- glmmTMB(mean_oxygen ~ animal_dens + OM + (1|krio_ID),
                data = FOR_fld_OUT); summary(m_for_fld_1)
plot(residuals(m_for_fld_1, type = "pearson") ~ fitted(m_for_fld_1))
qqnorm(residuals(m_for_fld_1, type = "pearson")); qqline(residuals(m_for_fld_1, type = "pearson"))

### Field - Longyearbreen --------------------------------------------
## Models 
LYR_fld_OUT$Animal <- LYR_fld_OUT$ani_type

# sediment (gaussian)
m_lyr_fld_1.0 <- glmmTMB(sed_oxygen ~ 1, 
             data = LYR_fld_OUT); summary(m_lyr_fld_1.0)
m_lyr_fld_1.1 <- glmmTMB(sed_oxygen ~ Animal * ani_den + OM, 
             data = LYR_fld_OUT); summary(m_lyr_fld_1.1)

plot(residuals(m_lyr_fld_1.1, type = "pearson") ~ fitted(m_lyr_fld_1.1))
qqnorm(residuals(m_lyr_fld_1.1, type = "pearson")); qqline(residuals(m_lyr_fld_1.1, type = "pearson"))

# water (gaussian)
m_lyr_fld_2.0 <- glmmTMB(water_oxygen ~ 1, 
             data = LYR_fld_OUT); summary(m_lyr_fld_2.0)
m_lyr_fld_2.1 <- glmmTMB(water_oxygen ~ Animal * ani_den + OM, 
             data = LYR_fld_OUT); summary(m_lyr_fld_2.1)

plot(residuals(m_lyr_fld_2.1, type = "pearson") ~ fitted(m_lyr_fld_2.1))
qqnorm(residuals(m_lyr_fld_2.1, type = "pearson")); qqline(residuals(m_lyr_fld_2.1, type = "pearson"))
hist(residuals(m_lyr_fld_2.1, type = "pearson"))

### Experiment - Forni -----------------------------------------------
AUCexp_FOR <- subset(AUCexp_SED, Glacier == "Forni")

AUCexp_FOR$AUC <- as.numeric(as.character(AUCexp_FOR$AUC))

## Models
m_for_exp_0 <- glmmTMB(AUC ~ 1, 
                       data = AUCexp_FOR); summary(m_for_exp_0)
m_for_exp_1 <- glmmTMB(AUC ~ Animals, 
                data = AUCexp_FOR); summary(m_for_exp_1)

qqnorm(residuals(m_for_exp_1, type = "pearson")); qqline(residuals(m_for_exp_1, type = "pearson"))
plot(residuals(m_for_exp_1) ~ fitted(m_for_exp_1))


### Experiment - Longyearbreen ---------------------------------------
AUCexp_LYR <- subset(AUCexp_SED, Glacier == "Longyearbreen")

AUCexp_LYR$AUC <- as.numeric(as.character(AUCexp_LYR$AUC))

# Modelling
m_lyr_exp_0 <- glmmTMB(AUC ~ 1, data = AUCexp_LYR); summary(m_lyr_exp_0)
m_lyr_exp_1 <- glmmTMB(AUC ~ Mixed + Animals, 
                data = AUCexp_LYR); summary(m_lyr_exp_1)
m_lyr_exp_2 <- glmmTMB(AUC ~ Mixed * Animals, 
                       data = AUCexp_LYR); summary(m_lyr_exp_2)
AIC(m_lyr_exp_0, m_lyr_exp_1, m_lyr_exp_2)

qqnorm(residuals(m_lyr_exp_1, type = "pearson")); qqline(residuals(m_lyr_exp_1, type = "pearson"))
plot(residuals(m_lyr_exp_1, type = "pearson") ~ fitted(m_lyr_exp_1))

qqnorm(residuals(m_lyr_exp_2, type = "pearson")); qqline(residuals(m_lyr_exp_2, type = "pearson"))
plot(residuals(m_lyr_exp_2, type = "pearson") ~ fitted(m_lyr_exp_2))