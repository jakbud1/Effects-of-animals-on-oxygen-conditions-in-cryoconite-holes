### Global 
library(readxl)
library(dplyr)

### Field approach ------------------------------------------------
### Forni ---------------------------------------------------------
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

## Add tardigrada data
df_tar <- read_xlsx("Input/Forni/FOR_21_Oxygen.xlsx")
df_tar$krio_ID <- as.factor(df_tar$krio_ID)

## merge both dfs
FOR_fld_OUT <- merge(df_tar, OUT_mean, by = "prof_ID")
FOR_fld_OUT$tar_count <- FOR_fld_OUT$deformed_specimens + FOR_fld_OUT$accurate_specimens

rm(df_for_fld, df_mean, df_tar, OUT_mean, OUT_for2)

### Longyearbreen ------------------------------------------------