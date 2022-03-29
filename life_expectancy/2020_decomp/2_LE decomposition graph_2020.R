# Plot LE decomposition results for 2018 to 2020 
# by SES and race and ethnicity 
# Project: SIMAH

# libraries required:
library("tidyverse")
library("dplyr")
library("RColorBrewer")

## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")

dResults_contrib <- read.csv(
  "SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/dResults_contrib_2018_2020ACS.csv")
dResults_contrib_race <- read.csv(
  "SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/dResults_contrib_2018_2020race_ACS.csv")
# Graph results by sex and SES
dResults_contrib <- as.data.frame(dResults_contrib[, -1])
group1gathered <- gather(data = dResults_contrib, key = "mort", value = "value", 
                         -sex , -edclass, -start_year, -end_year, -LE1, -LE2)

# have to be factor variables
group1gathered$sex <- as.factor(group1gathered$sex)
group1gathered$SES <- as.factor(group1gathered$edclass)
class(group1gathered$SES)
group1gathered$SES <- as.factor(group1gathered$SES)
group1gathered$mort = gsub(pattern = "mort", replacement = "", x = group1gathered$mort)
group1gathered$mort <- as.factor(group1gathered$mort)

names(group1gathered) <- c("Sex", "SES", "start_year", "end_year", "LE1", "LE2", "Cause_of_death", "Contribution")     

levels(group1gathered$SES) <- list("High" = "College", "Middle" = "SomeC", "Low" = "LEHS")
levels(group1gathered$Sex) <- list(Men = "1", Women = "2")
levels(group1gathered$Cause_of_death) <- list("Suicide" = "SIJ",
                                              "Motor vehicle accident" = "MVACC", 
                                              "Opioid poisoning" = "OPDPOI", 
                                              "Alcohol poisoning" = "ALCPOI", 
                                              "Unintentional injury*" = "UIJ",   
                                              "Other injury" = "OTHJ",   
                                              
                                              "Alcohol use disorder" = "AUD",
                                              "Liver disease & cirrhosis" = "LIVER", 
                                              "Kidney disease" = "KIDNEY",
                                              "Diabetes Mellitus" = "DM",
                                              "Dementia" = "ALZH",
                                              "Cerebrovascular diseases" = "HEART", 
                                              "Diseases of the heart" = "STROKE", 
                                              "Cancer" = "CANCER", 
                                              "Chronic lower respiratory diseases" = "RESP", 
                                              "Other NCDs" = "OTHNCD", 
                                              
                                              "Covid 19" = "COVID",
                                              "Influenza and pneumonia" = "FLU", 
                                              "Other infectious diseases" = "OTHINF", 
                                              "Rest" = "REST")
	

ncd1 <- 5
ncd2 <- 5
injury <- 6
infectious <- 3
other <- 1
color.vec <- c(rev(brewer.pal(injury,"RdPu")),
               rev(brewer.pal(ncd1,"YlOrRd")),
               rev(brewer.pal(ncd2,"Greens")),
               rev(brewer.pal(infectious,"Blues")),
               c("grey"))

write.csv(group1gathered, "SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/decomp_results_2020.csv")
group1gathered <- group1gathered[group1gathered$start_year == 2019,] 
## Plot showing changes in every year (will not be included in publication)
ggplot(data = group1gathered, aes(y = Contribution, x = SES, fill = Cause_of_death)) +
  geom_bar(position = position_stack(reverse = T), stat = "identity") +
  scale_fill_manual("Cause of death", values = color.vec)+ 
  facet_grid( rows = vars(Sex),  scales = "free") 
ggsave("SIMAH_workplace/life_expectancy/3_graphs/2020_decomp/2_LE decomp_2020.jpeg", dpi=600, width=30, height=15, units="cm")

#### Graph results by sex, SES and race
# Graph results by sex and SES
dResults_contrib <- as.data.frame(dResults_contrib_race[, -1])
dResults_contrib <- filter(dResults_contrib, start_year == 2019)
group1gathered <- gather(data = dResults_contrib, key = "mort", value = "value", 
                         -sex , -edclass, -race, -start_year, -end_year, -LE1, -LE2)

# have to be factor variables
group1gathered$sex <- as.factor(group1gathered$sex)
group1gathered$SES <- as.factor(group1gathered$edclass)
group1gathered$race <- as.factor(group1gathered$race)
class(group1gathered$SES)
group1gathered$SES <- as.factor(group1gathered$SES)
group1gathered$mort = gsub(pattern = "mort", replacement = "", x = group1gathered$mort)
group1gathered$mort <- as.factor(group1gathered$mort)

names(group1gathered) <- c("Sex", "SES", "Race", "start_year", "end_year", "LE1", "LE2", "Cause_of_death", "Contribution")     

levels(group1gathered$SES) <- list(High = "College", Middle = "SomeC", Low = "LEHS")
levels(group1gathered$Sex) <- list(Men = "1", Women = "2")
levels(group1gathered$Cause_of_death) <- list("Suicide" = "SIJ",
                                              "Motor vehicle accident" = "MVACC", 
                                              "Opioid poisoning" = "OPDPOI", 
                                              "Alcohol poisoning" = "ALCPOI", 
                                              "Unintentional injury*" = "UIJ",   
                                              "Other injury" = "OTHJ",   
                                              
                                              "Alcohol use disorder" = "AUD",
                                              "Liver disease & cirrhosis" = "LIVER", 
                                              "Kidney disease" = "KIDNEY",
                                              "Diabetes Mellitus" = "DM",
                                              "Dementia" = "ALZH",
                                              "Cerebrovascular diseases" = "HEART", 
                                              "Diseases of the heart" = "STROKE", 
                                              "Cancer" = "CANCER", 
                                              "Chronic lower respiratory diseases" = "RESP", 
                                              "Other NCDs" = "OTHNCD", 
                                              
                                              "Covid 19" = "COVID",
                                              "Influenza and pneumonia" = "FLU", 
                                              "Other infectious diseases" = "OTHINF", 
                                              "Rest" = "REST")
write.csv(group1gathered, "SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/decomp_results_2020_race.csv")
group1gathered <- group1gathered[group1gathered$start_year == 2019,] 
ggplot(data = group1gathered, aes(y = Contribution, x = SES, fill = Cause_of_death)) +
  geom_bar(position = position_stack(reverse = T), stat = "identity") +
  scale_fill_manual("Cause of death", values = color.vec)+ 
  facet_grid(cols = vars(Race), rows = vars(Sex)) 
ggsave("SIMAH_workplace/life_expectancy/3_graphs/2020_decomp/2_LE decomp_race_2020.jpeg", dpi=600, width=30, height=15, units="cm")
