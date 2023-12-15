### Global 
## Libs
library(readxl)
library(dplyr)
library(stats)
library(plyr)
library(ggplot2)

## Merge comments function
merge_comments <- function(dataInput, dataNames){
  for (z in 1:length(dataNames)) {
    for (i in 1:length(dataInput[[1]])) {
      if (dataInput[i,1] == substr(dataNames[z], 1, 12)) {
        dataInput[i,7] <- dataNames[z]
        dataInput[i,8] <- substr(dataNames[z], 20, 21)
        dataInput[i,9] <- substr(dataNames[z], 23, 24)
        dataInput[i,10] <- substr(dataNames[z], 26, 26)
      }
    }
  } 
  .GlobalEnv$newDF <- dataInput
}


### Field - Forni ---------------------------------------------------
wd <- getwd()
setwd("Input/Forni/profiles/")
temp <- list.files(pattern = "*.xlsx")
myfiles <- lapply(temp, read_xlsx)
df_for_fld <- do.call(rbind.data.frame, myfiles); rm(myfiles, temp)

setwd(wd)

## select variable
df_for_fld <- df_for_fld[,c(1,2,8)]
colnames(df_for_fld) <- c("profile_ID", "depth", "oxygen_concentration_ul_L")

## aggregate
df_mean <- aggregate(df_for_fld$oxygen_concentration_ul_L ~ df_for_fld$profile_ID + df_for_fld$depth, 
                     FUN = mean)
colnames(df_mean) <- c("profile_ID", "depth", "oxygen_concentration_ul_L")

## Subset negative values to 0
for (i in 1:nrow(df_mean)){
  if(df_mean$oxygen_concentration_ul_L[i] < 0){
    df_mean$oxygen_concentration_ul_L[i] <- 0
  }
}; rm(i)

# Check observations
# ggplot(df_mean, aes(x = depth, y = oxygen_concentration_ul_L)) +  
#   facet_wrap(. ~ profile_ID) + geom_line() + geom_jitter()

df_mean <- df_mean[-217,]# measurement error, after lack of oxygen, sudden pick.

## Cut measurements higher than 350um - it lets to remove effects of different length of profile on the results
OUT_for <- subset(df_mean, oxygen_concentration_ul_L < 350)

# Check observations
ggplot(OUT_for, aes(x = depth, y = oxygen_concentration_ul_L)) +  
  facet_wrap(. ~ profile_ID) + geom_line() + geom_jitter()

## Cut tail of measurements, leave only 3 top measurements and calculate the mean
df_mean$profile_ID <- as.factor(df_mean$profile_ID)

OUT_for2 <- OUT_for %>% 
  group_by(profile_ID) %>% 
  top_n(-3, depth) ; rm(OUT_for)

OUT_mean <- aggregate(OUT_for2$oxygen_concentration_ul_L ~ OUT_for2$profile_ID, FUN = mean)
colnames(OUT_mean) <- c("prof_ID", "mean_oxygen")

## calculate standard error at each depth 
df_prof_for_mean <- aggregate(df_mean$oxygen_concentration_ul_L ~ df_mean$depth, FUN = mean)
df_prof_for_mean$SE <- unlist(aggregate(df_mean$oxygen_concentration_ul_L ~ df_mean$depth, FUN = sd)[2]/
  sqrt(aggregate(df_mean$oxygen_concentration_ul_L ~ df_mean$depth, FUN = length)[2]))
colnames(df_prof_for_mean) <- c("depth", "mean_oxygen", "SE")

## Add tardigrada data
df_tar <- read_xlsx("Input/Forni/FOR_21_tar.xlsx")
df_tar$cryo_ID <- as.factor(df_tar$cryo_ID)

## merge both dfs
FOR_fld_OUT <- merge(df_tar, OUT_mean, by = "prof_ID")

df_prof_for_all <- df_mean
rm(df_for_fld, df_mean, df_tar, OUT_mean, OUT_for2)

write.csv(FOR_fld_OUT, "Output_data/Forni_field.csv")

### Field - Longyearbreen ---------------------------------------
## Double measurements
LYR_fld_OUT <- readxl::read_xlsx("Input/Lyr/LYR_21_animals.xlsx")

write.csv(LYR_fld_OUT, "Output_data/Longyear_field.csv")
## Profiles
setwd("Input/Lyr/profile/")
temp <- list.files(pattern = "*.xlsx")
myfiles <- lapply(temp, read_xlsx)
df_lyr_fld <- do.call(rbind.data.frame, myfiles); rm(myfiles, temp)

## select variable
df_lyr_fld <- df_lyr_fld[,c(1,2,8)]
colnames(df_lyr_fld) <- c("profile_ID", "depth", "oxygen_concentration_ul_L")

## aggregate
df_mean <- aggregate(df_lyr_fld$oxygen_concentration_ul_L ~ df_lyr_fld$profile_ID + df_lyr_fld$depth, 
                     FUN = mean)
colnames(df_mean) <- c("profile_ID", "depth", "oxygen_concentration_ul_L")

## Subset negative values to 0
for (i in 1:nrow(df_mean)){
  if (df_mean$oxygen_concentration_ul_L[i] < 0){
    df_mean$oxygen_concentration_ul_L[i] <- 0
  }
}; rm(i)

## Check observations
# ggplot(df_mean, aes(x = depth, y = oxygen_concentration_ul_L)) +  
#   facet_wrap(. ~ profile_ID) + geom_line() + geom_jitter()

## calculate standard error at each depth 
df_prof_lyr_mean <- aggregate(df_mean$oxygen_concentration_ul_L ~ df_mean$depth, FUN = mean)
df_prof_lyr_mean$SE <- unlist(aggregate(df_mean$oxygen_concentration_ul_L ~ df_mean$depth, FUN = sd)[2]/
  sqrt(aggregate(df_mean$oxygen_concentration_ul_L ~ df_mean$depth, FUN = length)[2]))
colnames(df_prof_lyr_mean) <- c("depth", "mean_oxygen", "SE")

df_prof_lyr_all <- df_mean; rm(df_mean)

### Experiment - Forni (Sediment; Oxygen)  -------------------------------------
setwd(wd)

FOR_OS <- lapply(excel_sheets("Input/Experiment/FOR_exp.xlsx"), 
                 read_excel, path = "Input/Experiment/FOR_exp.xlsx")
FOR_OS2 <- bind_rows(FOR_OS[1:10])
FOR_OS_NAMES <- as.data.frame(FOR_OS[13])[,3]; rm(FOR_OS)

FOR_OS2 <- FOR_OS2[,c(1,2,7,8,6)]
FOR_OS2$glacier <- rep("Forni", length(FOR_OS2$`Profile name`))

FOR_OS2$abv_name <- as.character(NA)
FOR_OS2$mixed <- as.character(NA)
FOR_OS2$animals <- as.character(NA)
FOR_OS2$beakerNum <- as.character(NA)

## merge comments
merge_comments(FOR_OS2, FOR_OS_NAMES)
rm(FOR_OS2, FOR_OS_NAMES)
FOR_exp_SED <- newDF; rm(newDF) 

## select variables
exp_SED <- FOR_exp_SED[,c(2,4,6,7,9,10)]
names(exp_SED) <- c("Depth", "Oxygen", "Glacier", "Name", "Animals", "BeakerNum")

## Aggregate data by double measurements
exp_SED <- aggregate(exp_SED$Oxygen ~ exp_SED$Depth + exp_SED$Glacier + 
                       exp_SED$BeakerNum + exp_SED$Animals +  
                       exp_SED$Name, 
                     FUN = mean)
colnames(exp_SED) <- c("Depth","Glacier","BeakerNum","Animals","Name","Oxygen")

exp_SED$Animals <- as.factor(exp_SED$Animals)
levels(exp_SED$Animals) <- c("Without animals", "Animals")

## calculate the calculus for each profile 
for (e in 1:length(exp_SED$Name)){
    if (exp_SED[e, 6] < 0) {
      exp_SED[e, 6] <- 0
    }
}; rm(e)

# change concentration to amount per particular depth (0.001 cm3 = 1L); Such approach let us only to compare oxygen amount within the experiments, because we used the same beakers and amount of sediment.
r_zlew <- 1 
h_pom <- 0.001 # Assumed that one measurement was done for 10um
v_pom <- pi * sqrt(r_zlew) * h_pom; rm(r_zlew, h_pom) # w cm3 

exp_SED$Oxygen_amount <- (v_pom * exp_SED$Oxygen)/1000; rm(v_pom)

exp_SEDL <- split(exp_SED, exp_SED$Name)
Int_exp_SED <- lapply(exp_SEDL, 
                      function(z) integrate(approxfun(z[,1],z[,7]), 
                                            range(z[,1])[1], range(z[,1])[2]))

# extracting the data from the list
AUCl <- numeric(length(exp_SEDL))
leng <- length(exp_SEDL)

for (i in 1:leng) {
  AUCl[i] <- as.numeric(Int_exp_SED[[i]][1])
};  rm(i, leng)

AUCexp_SED <- cbind(AUCl, names(Int_exp_SED))
colnames(AUCexp_SED) <- c("AUC","Name")
AUCexp_SED <- as.data.frame(AUCexp_SED); rm(exp_SEDL, Int_exp_SED, AUCl)

# Merging the other variables with new df <- tutaj jest cos nie tak. 
for (z in 1:length(exp_SED$Depth)) {
  for (i in 1:length(AUCexp_SED$AUC)) {
    if (AUCexp_SED[i,2] == exp_SED[z,5]) {
      AUCexp_SED$Glacier[i] <- exp_SED[z,2] 
      AUCexp_SED$BeakerNum[i] <- exp_SED[z,3]
      AUCexp_SED$Animals[i] <- as.character(exp_SED[z,4])
    }
  } 
}; rm(i, z)

## change structures of variables 
AUCexp_SED$AUC <- as.factor(AUCexp_SED$AUC)

AUCexp_FOR <- AUCexp_SED; rm(AUCexp_SED)

### Experiment - Forni, Longyear, Ebenferner (Sediment; OM)  -------------------
df_exp <- read_xlsx("Input/Exp_OM.xlsx")
df_exp <- df_exp[-c(1:3),]

## Remover records where animals haven't survived until the end of the experiment but also drop the non-mixed for Forni (experiment fail before end)
df_exp <- subset(df_exp, is.na(df_exp$Remove) == TRUE)[,-10]

## Split the dfs
df_exp_FOR <- subset(df_exp, Glacier == "FOR") 
df_exp_STE <- subset(df_exp, Glacier == "EBE")
df_exp_LYR <- subset(df_exp, Glacier == "LYR")


### Tlen: 
# - Teren (FOR i LYR)
# - experyment (FOR)

### OM:
# - Teren (FOR i LYR)
# - experyment (FOR, EBE, LYR)