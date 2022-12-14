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
setwd("Input/Forni/profile/")
temp <- list.files(pattern = "*.xlsx")
myfiles <- lapply(temp, read_xlsx)
df_for_fld <- do.call(rbind.data.frame, myfiles); rm(myfiles, temp)

setwd(wd); rm(wd)

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
ggplot(df_mean, aes(x = depth, y = oxygen_concentration_ul_L)) +  
  facet_wrap(. ~ profile_ID) + geom_line() + geom_jitter()

df_mean <- df_mean[-217,]# measurement error, after lack of oxygen, sudden pick.

## Cut measurements higher than 350um - it lets to remove effects of different length of profile on the results
OUT_for <- subset(df_mean, oxygen_concentration_ul_L < 350)

# Check observations
ggplot(OUT_for, aes(x = depth, y = oxygen_concentration_ul_L)) +  
  facet_wrap(. ~ profile_ID) + geom_line() + geom_jitter()

## Cut tail of measurements, leave only 3 top measurements and calculate the mean.
df_mean$profile_ID <- as.factor(df_mean$profile_ID)

OUT_for2 <- OUT_for %>% 
  group_by(profile_ID) %>% 
  top_n(-3, depth) ; rm(OUT_for)

OUT_mean <- aggregate(OUT_for2$oxygen_concentration_ul_L ~ OUT_for2$profile_ID, FUN = mean)
colnames(OUT_mean) <- c("prof_ID", "mean_oxygen")

## calculate standard error at each depth 
df_prof_for_mean <- aggregate(df_mean$oxygen_concentration_ul_L ~ df_mean$depth, FUN = mean)
df_prof_for_mean$SE <- aggregate(df_mean$oxygen_concentration_ul_L ~ df_mean$depth, FUN = sd)[2]/
  sqrt(aggregate(df_mean$oxygen_concentration_ul_L ~ df_mean$depth, FUN = length)[2])
colnames(df_prof_for_mean) <- c("depth", "mean_oxygen", "SE")

## Add tardigrada data
df_tar <- read_xlsx("Input/Forni/FOR_21_Oxygen.xlsx")
df_tar$krio_ID <- as.factor(df_tar$krio_ID)

## merge both dfs
FOR_fld_OUT <- merge(df_tar, OUT_mean, by = "prof_ID")
FOR_fld_OUT$tar_count <- FOR_fld_OUT$deformed_specimens + FOR_fld_OUT$accurate_specimens

df_prof_forni_all <- df_mean
rm(df_for_fld, df_mean, df_tar, OUT_mean, OUT_for2)

### Field - Longyearbreen ---------------------------------------
LYR_fld_OUT <- readxl::read_xlsx("Input/Lyr/LYR_21_oxygen.xlsx")

### Experiment - Forni (Sediment)  -----------------------------------------------
### Sediment data
FOR_OS <- lapply(excel_sheets("Input/Experiment/Tlen_FOR_OSAD.xlsx"), 
                 read_excel, path = "Input/Experiment/Tlen_FOR_OSAD.xlsx")
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

### Experiment - Forni (Water)  -----------------------------------------------
FOR_WO <- lapply(excel_sheets("Input/Experiment/Tlen_FOR_WODA.xlsx"), 
                 read_excel, path = "Input/Experiment/Tlen_FOR_WODA.xlsx")
FOR_WO2 <- bind_rows(FOR_WO[1:10])
FOR_WO_NAMES <- as.data.frame(FOR_WO[13])[-1,3]; rm(FOR_WO)

FOR_WO2 <- FOR_WO2[,c(1,2,7,8,6)]
FOR_WO2$glacier <- rep("Forni", length(FOR_WO2$`Profile name`))

FOR_WO2$abv_name <- as.character(NA)
FOR_WO2$mixed <- as.character(NA)
FOR_WO2$animals <- as.character(NA)
FOR_WO2$beakerNum <- as.character(NA)

merge_comments(FOR_WO2, FOR_WO_NAMES); rm(FOR_WO2, FOR_WO_NAMES)
FOR_exp_WAT <- newDF; rm(newDF)

### Experiment - Longyearbreen (Sediment)  -----------------------------------------------
LYR_OS <- lapply(excel_sheets("Input/Experiment/Tlen_LYR_OSAD.xlsx"), 
                 read_excel, path = "Input/Experiment/tlen_LYR_OSAD.xlsx")
LYR_OS2 <- bind_rows(LYR_OS[1:20])
LYR_OS_NAMES <- as.data.frame(LYR_OS[23])[-1,3]; rm(LYR_OS)

LYR_OS2 <- LYR_OS2[,c(1,2,7,8,6)]
LYR_OS2$glacier <- rep("Longyearbreen", length(LYR_OS2$`Profile name`))

LYR_OS2$abv_name <- as.character(NA)
LYR_OS2$mixed <- as.character(NA)
LYR_OS2$animals <- as.character(NA)
LYR_OS2$beakerNum <- as.character(NA)

merge_comments(LYR_OS2, LYR_OS_NAMES); rm(LYR_OS2, LYR_OS_NAMES)
LYR_exp_SED <- newDF; rm(newDF)

### Experiment - Longyearbreen (Water)  -----------------------------------------------
LYR_WO <- lapply(excel_sheets("Input/Experiment/Tlen_LYR_WODA.xlsx"), 
                 read_excel, path = "Input/Experiment/Tlen_LYR_WODA.xlsx")
LYR_WO2 <- bind_rows(LYR_WO[1:20])
LYR_WO_NAMES <- as.data.frame(LYR_WO[23])[-1,3]; rm(LYR_WO)

LYR_WO2 <- LYR_WO2[,c(1,2,7,8,6)]
LYR_WO2$glacier <- rep("Longyearbreen", length(LYR_WO2$`Profile name`))

LYR_WO2$abv_name <- as.character(NA)
LYR_WO2$mixed <- as.character(NA)
LYR_WO2$animals <- as.character(NA)
LYR_WO2$beakerNum <- as.character(NA)

merge_comments(LYR_WO2, LYR_WO_NAMES); rm(LYR_WO2, LYR_WO_NAMES)
LYR_exp_WAT <- newDF; rm(newDF)

### Experiment - process experimental data ---------------------------------------------------
## Merge dfs
# water dfs
exp_WAT <- rbind(FOR_exp_WAT, LYR_exp_WAT); rm(FOR_exp_WAT, LYR_exp_WAT) 
exp_WAT <- exp_WAT[,c(2,4,6:10)]
names(exp_WAT) <- c("Depth", "Oxygen", "Glacier", "Name", "Mixed", "Animals", "BeakerNum")

# sediment dfs 
exp_SED <- rbind(FOR_exp_SED, LYR_exp_SED); rm(FOR_exp_SED, LYR_exp_SED)
exp_SED <- exp_SED[,c(2,4,6:10)]
names(exp_SED) <- c("Depth", "Oxygen", "Glacier", "Name", "Mixed", "Animals", "BeakerNum")

## Aggregate data by double measurements
# sediment
exp_SED <- aggregate(exp_SED$Oxygen ~ exp_SED$Depth + exp_SED$Glacier + 
                              exp_SED$BeakerNum + exp_SED$Animals + exp_SED$Mixed + 
                              exp_SED$Name, 
                            FUN = mean)
colnames(exp_SED) <- c("Depth","Glacier","BeakerNum","Animals","Mixed","Name","Oxygen")

exp_SED$Animals <- as.factor(exp_SED$Animals)
levels(exp_SED$Animals) <- c("Without animals", "Animals")

exp_SED$Mixed <- as.factor(exp_SED$Mixed)
levels(exp_SED$Mixed) <- c("Not mixed", "Mixed")

# water
exp_WAT <- aggregate(exp_WAT$Oxygen ~ exp_WAT$Depth + exp_WAT$Glacier + 
                          exp_WAT$BeakerNum + exp_WAT$Animals + exp_WAT$Mixed + 
                          exp_WAT$Name, 
                        FUN = mean)
colnames(exp_WAT) <- c("Depth","Glacier","BeakerNum","Animals","Mixed","Name","Oxygen")

exp_WAT$Animals <- as.factor(exp_WAT$Animals)
levels(exp_WAT$Animals) <- c("Without animals", "Animals")
exp_WAT$Mixed <- as.factor(exp_WAT$Mixed)
levels(exp_WAT$Mixed) <- c("Not mixed", "Mixed")

### Experiment - calculate the calculus for each profile ------------------------
## Sediment
# set negative value of oxygen as 0 to avoid negative integration. 
for (e in 1:length(exp_SED$Name)) {
  if (exp_SED[e, 7] < 0) {
    exp_SED[e, 7] <- 0
  }
}; rm(e)

# change concentration to amount per particular depth (0.001 cm3 = 1L); Such approach let us only to compare oxygen amount within the experiments, because we used the same beakers and amount of sediment.
r_zlew <- 1 
h_pom <- 0.001 # Assumed that one measurement was done for 10um
v_pom <- pi * sqrt(r_zlew) * h_pom; rm(r_zlew, h_pom) # w cm3 

exp_SED$Oxygen_amount <- (v_pom * exp_SED$Oxygen)/1000; rm(v_pom)

exp_SEDL <- split(exp_SED, exp_SED$Name)
Int_exp_SED <- lapply(exp_SEDL, 
                             function(z) integrate(approxfun(z[,1],z[,8]), 
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

# Merging the other variables with new df
for (z in 1:length(exp_SED$Depth)) {
  for (i in 1:length(AUCexp_SED$AUC)) {
    if (AUCexp_SED[i,2] == exp_SED[z,6]) {
      AUCexp_SED$Glacier[i] <- exp_SED[z,2] 
      AUCexp_SED$BeakerNum[i] <- exp_SED[z,3]
      AUCexp_SED$Animals[i] <- as.character(exp_SED[z,4])
      AUCexp_SED$Mixed[i] <- as.character(exp_SED[z,5])
    }
  } 
}; rm(i, z)

## change structures of variables 
AUCexp_SED$AUC <- as.factor(AUCexp_SED$AUC)




