library(plyr)
library(dplyr)
library(stringr)

#==============================================================================#
# Data Pre-processing
#==============================================================================#

# Load original dataset
data = read.csv('sph6004_assignment1_data.csv')

# Remove those columns with NA's percentage over 30%
data = data[, colMeans(is.na(data)) < 0.3]
data = na.omit(data)

# Since each individual's body status is dynamic, simply looking at minimum or maximum
# may not be so meaningful. Therefore, I will calculate the average of each individual's
# min and max
basic_cols = c('id','hospital_mortality','aki_stage','gender','admission_age','race','height','weight_admit')
mean_cols = grep("_mean$", colnames(data), value = TRUE)
prefix_mean = unique(gsub("(_mean)$", "", mean_cols))
remaining_cols = setdiff(colnames(data), c(basic_cols, mean_cols))

# We take those varibales with 'gcs' as prefix out as unlike all other variables,
# these variables are linearly related, 
# gcs_min = (1-gcs_unable)*(gcs_motor + gcs_verbal + gcs_eyes)

data %>% select(contains('gcs')) %>% filter(gcs_verbal == 0) %>% group_by(gcs_unable) %>% reframe(n = n())
# # A tibble: 2 × 2
# gcs_unable       n
# <int>    <int>
# 1          0    22
# 2          1  7573

# And we have also checked that when gcs_unable == 1, which means we are unable to 
# conduct the test, gcs_verbal are always 0, but when gcs_verbal == 0, 22 of gcs_unable
# are not 1, which means the test is conducted. Therefore, we can see that gcs_unable
# is not a good indicator of whether the test is conducted or not. Therefore, we will
# take gcs_unable out of the variables list but focusing on the other three variables
# gcs_motor, gcs_verbal and gcs_eyes

gcs_cols = grep("gcs",remaining_cols,value=T)
remaining_cols = setdiff(remaining_cols, gcs_cols)

# Extract unique prefixes
prefixes = unique(gsub("(_min|_max)$", "", remaining_cols))

data = data %>% 
  rowwise %>% 
  mutate(hematocrit_lab_mean = mean(hemoglobin_lab_max,hemoglobin_lab_min,na.rm = T),
         hemoglobin_lab_mean = mean(hematocrit_lab_max,hematocrit_lab_min,na.rm = T),
         platelets_mean = mean(platelets_max,platelets_min,na.rm = T),
         wbc_mean = mean(wbc_max,wbc_min,na.rm = T),
         aniongap_mean = mean(aniongap_max,aniongap_min,na.rm = T),
         bicarbonate_lab_mean = mean(bicarbonate_lab_max,bicarbonate_lab_min,na.rm = T),
         creatinine_mean = mean(creatinine_max,creatinine_min,na.rm = T),
         glucose_lab_mean = mean(glucose_lab_max,glucose_lab_min,na.rm = T),
         sodium_lab_mean = mean(sodium_lab_max,sodium_lab_min,na.rm = T),
         potassium_lab_mean = mean(potassium_lab_max,potassium_lab_min,na.rm = T),
         inr_mean = mean(inr_max,inr_min,na.rm = T),
         pt_mean = mean(pt_max,pt_min,na.rm = T),
         ptt_mean = mean(ptt_max,ptt_min,na.rm = T)) %>% 
  select(aki_stage, gender, admission_age, race, weight_admit,
         hematocrit_lab_mean,hemoglobin_lab_mean,platelets_mean,wbc_mean,aniongap_mean,
         bicarbonate_lab_mean,creatinine_mean,glucose_lab_mean,sodium_lab_mean,potassium_lab_mean,inr_mean,
         pt_mean,ptt_mean,
         heart_rate_mean,sbp_mean,dbp_mean,mbp_mean,resp_rate_mean,temperature_vital_mean,spo2_mean,glucose_vital_mean,
         gcs_motor,gcs_verbal,gcs_eyes)

# Dictionary
old_race <- c("WHITE", "WHITE - RUSSIAN", "WHITE - OTHER EUROPEAN", "WHITE - EASTERN EUROPEAN",
              "BLACK/AFRICAN AMERICAN", "BLACK/AFRICAN", "BLACK/CAPE VERDEAN", "BLACK/CARIBBEAN ISLAND",
              "ASIAN", "ASIAN - CHINESE", "ASIAN - KOREAN", "ASIAN - SOUTH EAST ASIAN", "ASIAN - ASIAN INDIAN",
              "HISPANIC OR LATINO", "HISPANIC/LATINO - SALVADORAN", "HISPANIC/LATINO - PUERTO RICAN",
              "HISPANIC/LATINO - CUBAN", "HISPANIC/LATINO - DOMINICAN", "HISPANIC/LATINO - GUATEMALAN",
              "HISPANIC/LATINO - HONDURAN", "HISPANIC/LATINO - COLUMBIAN", "HISPANIC/LATINO - CENTRAL AMERICAN",
              "HISPANIC/LATINO - MEXICAN", "PORTUGUESE", "SOUTH AMERICAN",
              "AMERICAN INDIAN/ALASKA NATIVE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
              "MULTIPLE RACE/ETHNICITY", "UNKNOWN", "UNABLE TO OBTAIN", "PATIENT DECLINED TO ANSWER", "OTHER")

new_race <- c("White", "White", "White", "White",
              "Black", "Black", "Black", "Black",
              "Asian", "Asian", "Asian", "Asian", "Asian",
              "Hispanic/Latino", "Hispanic/Latino", "Hispanic/Latino",
              "Hispanic/Latino", "Hispanic/Latino", "Hispanic/Latino",
              "Hispanic/Latino", "Hispanic/Latino", "Hispanic/Latino",
              "Hispanic/Latino", "Hispanic/Latino", "Hispanic/Latino",
              "Native American", "Pacific Islander",
              "Multiple", "Unknown", "Unknown", "Unknown", "Other")

# Mapping
data$race <- mapvalues(data$race, from = old_race, to = new_race, warn_missing = FALSE)

# So for those with min and max in the variables list, we just take the mean of them
# and for those who have already had the mean value, we just keep them and remove the 
# max and min columns. So finally, our variables list will be:
# basic_cols + mean_cols + remaining_cols
write.csv(data,'sph6004_assignment1_data_processed.csv',row.names = F)

#==============================================================================#
# Re-code all variables
#==============================================================================#
# Recode gender, race and hospital_mortality into nominal variables and aki_stage into ordinal variable
# Moreoever, since SVM and logistic regression are both very sensitive to the scale of the variables,
# So we need to normalize all continuous variables
# This part will be done in python