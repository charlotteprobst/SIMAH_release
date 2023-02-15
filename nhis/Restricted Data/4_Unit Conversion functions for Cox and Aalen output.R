library(tidyverse)


##################### R function to convert output in the unit of grams per day into standard drinks (14 grams) per day #######################

transform_14g <- function(data_g, output_name){
  
  data_14g <- data_g %>%
    separate(col = CI, into = c("CI_low", "CI_high"), sep = ", ", remove = TRUE) %>%
    separate(col = CI_unwt, into = c("CI_unwt_low", "CI_unwt_high"), sep = ", ", remove = TRUE) %>%
    separate(col = Deaths_CI_100000py, into = c("Deaths_CI_100000py_low", "Deaths_CI_100000py_high"), sep = ", ", remove = TRUE) %>%
    mutate(CI_low = str_replace_all(CI_low, pattern = "[(]", replacement = ""),
           CI_high = str_replace_all(CI_high, pattern = "[)]", replacement = ""),
           CI_unwt_low = str_replace_all(CI_unwt_low, pattern = "[(]", replacement = "" ),
           CI_unwt_high = str_replace_all(CI_unwt_high, pattern = "[)]", replacement = ""),
           Deaths_CI_100000py_low = str_replace_all(Deaths_CI_100000py_low, pattern = "[(]", replacement = "" ),
           Deaths_CI_100000py_high = str_replace_all(Deaths_CI_100000py_high, pattern = "[)]", replacement = "") ) %>%
    mutate_at(c("CI_low", "CI_high", "CI_unwt_low", "CI_unwt_high", "Deaths_CI_100000py_low", "Deaths_CI_100000py_high"), as.numeric) 
  
  
  data_14g[5:7, c("HR", "CI_low", "CI_high", "HR_unwt", "CI_unwt_low", "CI_unwt_high")] <- 
    exp(log(data_14g[5:7, c("HR", "CI_low", "CI_high", "HR_unwt", "CI_unwt_low", "CI_unwt_high")]) * 14) 
  
  
  data_14g[5:7, c("estimate_100000py", "Deaths_CI_100000py_low", "Deaths_CI_100000py_high")] <- 
    data_14g[5:7, c("estimate_100000py", "Deaths_CI_100000py_low", "Deaths_CI_100000py_high")] * 14
  
  
  data_14g <- data_14g %>%
    mutate_at(c("HR", "HR_unwt", "CI_low", "CI_high", "CI_unwt_low", "CI_unwt_high", "Deaths_CI_100000py_low", "Deaths_CI_100000py_high"), funs(round(., 4))) %>%
    mutate(CI = paste0("(",CI_low,", ", CI_high, ")"), 
           CI_unwt = paste0("(", CI_unwt_low, ", ", CI_unwt_high, ")"),
           Deaths_CI_100000py = paste0("(",Deaths_CI_100000py_low,", ", Deaths_CI_100000py_high, ")") ) %>%
    select(variable, HR, CI, p.value_HR, HR_unwt, CI_unwt, p.value_HR_unwt, estimate_100000py, Deaths_CI_100000py, p.value_Deaths)
  
  
  write_csv(data_14g, paste0(output_tables, output_name, ".csv"), na="")
  
}

output_tables <- "C:/Users/yzhu/Desktop/SIMAH project/..."


transform_14g(data_g = table3_Alcohol_edu3_alc_daily_g_all_sex, output_name = "table3_Alcohol_edu3_alc_daily_14g_all_sex")
transform_14g(data_g = table3_Alcohol_edu3_alc_daily_g_female_sex, output_name = "table3_Alcohol_edu3_alc_daily_14g_female_sex")
transform_14g(data_g = table3_Alcohol_edu3_alc_daily_g_male_sex, output_name = "table3_Alcohol_edu3_alc_daily_14g_male_sex")


transform_14g(data_g = table3_LDAC_edu3_alc_daily_g_all_sex, output_name = "table3_LDAC_edu3_alc_daily_14g_all_sex")
transform_14g(data_g = table3_LDAC_edu3_alc_daily_g_female_sex, output_name = "table3_LDAC_edu3_alc_daily_14g_female_sex")
transform_14g(data_g = table3_LDAC_edu3_alc_daily_g_male_sex, output_name = "table3_LDAC_edu3_alc_daily_14g_male_sex")



############################## R function to convert output in the unit of grams per day into 20 grams per day ##################################

transform_20g <- function(data_g, output_name){
  
  data_20g <- data_g %>%
    separate(col = CI, into = c("CI_low", "CI_high"), sep = ", ", remove = TRUE) %>%
    separate(col = CI_unwt, into = c("CI_unwt_low", "CI_unwt_high"), sep = ", ", remove = TRUE) %>%
    separate(col = Deaths_CI_100000py, into = c("Deaths_CI_100000py_low", "Deaths_CI_100000py_high"), sep = ", ", remove = TRUE) %>%
    mutate(CI_low = str_replace_all(CI_low, pattern = "[(]", replacement = ""),
           CI_high = str_replace_all(CI_high, pattern = "[)]", replacement = ""),
           CI_unwt_low = str_replace_all(CI_unwt_low, pattern = "[(]", replacement = "" ),
           CI_unwt_high = str_replace_all(CI_unwt_high, pattern = "[)]", replacement = ""),
           Deaths_CI_100000py_low = str_replace_all(Deaths_CI_100000py_low, pattern = "[(]", replacement = "" ),
           Deaths_CI_100000py_high = str_replace_all(Deaths_CI_100000py_high, pattern = "[)]", replacement = "") ) %>%
    mutate_at(c("CI_low", "CI_high", "CI_unwt_low", "CI_unwt_high", "Deaths_CI_100000py_low", "Deaths_CI_100000py_high"), as.numeric) 
  
  
  data_20g[5:7, c("HR", "CI_low", "CI_high", "HR_unwt", "CI_unwt_low", "CI_unwt_high")] <- 
    exp(log(data_20g[5:7, c("HR", "CI_low", "CI_high", "HR_unwt", "CI_unwt_low", "CI_unwt_high")]) * 20) 
  
  
  data_20g[5:7, c("estimate_100000py", "Deaths_CI_100000py_low", "Deaths_CI_100000py_high")] <- 
    data_20g[5:7, c("estimate_100000py", "Deaths_CI_100000py_low", "Deaths_CI_100000py_high")] * 20
  
  
  data_20g <- data_20g %>%
    mutate_at(c("HR", "HR_unwt", "CI_low", "CI_high", "CI_unwt_low", "CI_unwt_high", "Deaths_CI_100000py_low", "Deaths_CI_100000py_high"), funs(round(., 4))) %>%
    mutate(CI = paste0("(",CI_low,", ", CI_high, ")"), 
           CI_unwt = paste0("(", CI_unwt_low, ", ", CI_unwt_high, ")"),
           Deaths_CI_100000py = paste0("(",Deaths_CI_100000py_low,", ", Deaths_CI_100000py_high, ")") ) %>%
    select(variable, HR, CI, p.value_HR, HR_unwt, CI_unwt, p.value_HR_unwt, estimate_100000py, Deaths_CI_100000py, p.value_Deaths)
  
  
  write_csv(data_20g, paste0(output_tables, output_name, ".csv"), na="")
  
}



transform_20g(data_g = table3_LDAC_edu3_alc_daily_g_all_sex, output_name = "table3_LDAC_edu3_alc_daily_20g_all_sex")
transform_20g(data_g = table3_LDAC_edu3_alc_daily_g_female_sex, output_name = "table3_LDAC_edu3_alc_daily_20g_female_sex")
transform_20g(data_g = table3_LDAC_edu3_alc_daily_g_male_sex, output_name = "table3_LDAC_edu3_alc_daily_20g_male_sex")
