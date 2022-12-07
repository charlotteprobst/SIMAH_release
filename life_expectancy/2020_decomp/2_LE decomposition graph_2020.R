# Plot LE decomposition results for 2018 to 2020 
# by SES and race and ethnicity 
# Project: SIMAH

# libraries required:
library("dplyr")
library("RColorBrewer")
library("tidyverse")

## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")

dResults_contrib_SES <- read.csv(
  "SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/dResults_contrib_2018_2020_SES_ACS.csv")
dResults_contrib_race <- read.csv(
  "SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/dResults_contrib_2018_2020race_ACS.csv")

# Graph results by sex and SES
dResults_contrib <- as.data.frame(dResults_contrib_SES[, -1])
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
levels(group1gathered$Cause_of_death) <- list("Covid 19" = "cov",
                                              "Influenza and pneumonia" = "flu", 
                                              "Other infectious diseases" = "othinf", 
                                              
                                              "Alcohol poisoning" = "alcpoi",
                                              "Opioid poisoning" = "opioid",
                                              "Suicide" = "sij",
                                              "Motor vehicle accident" = "mvacc", 
                                              "Unintentional injury*" = "uij",   
                                              "Other injury" = "othj",   
                                              
                                              "Alcohol use disorder" = "aud",
                                              "Liver disease & cirrhosis" = "liver", 
                                              "Kidney disease" = "kidney",
                                              "Diabetes Mellitus" = "dm",
                                              "Dementia" = "dementia",
                                              "Cerebrovascular diseases" = "heart", 
                                              "Diseases of the heart" = "stroke", 
                                              "Cancer" = "cancer", 
                                              "Chronic lower respiratory diseases" = "resp", 
                                              "Other NCDs" = "othncd",
                                              
                                              "Rest" = "rest"
                                              )
	

infectious <- 5
injury1 <- 3
injury2 <- 3
ncd1 <- 3
ncd2 <- 7

other <- 1
color.vec <- c(rev(brewer.pal(infectious,"Blues"))[1:3],
               rev(brewer.pal(injury1,"Reds")),
               rev(brewer.pal(injury2,"Greens")), #Oranges
               rev(brewer.pal(ncd1,"RdPu")), #YlOrRd
               rev(brewer.pal(ncd2,"YlOrRd")),
               
               c("grey"))

write.csv(group1gathered, "SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/decomp_results_2020.csv")
group1gathered <- group1gathered[group1gathered$start_year == 2019,] 
## Plot showing changes in every year (will not be included in publication)
ggplot(data = group1gathered, aes(y = Contribution, x = SES, fill = Cause_of_death)) +
  geom_bar(position = position_stack(reverse = T), stat = "identity") +
  scale_fill_manual("Cause of death", values = color.vec)+ 
  facet_grid( rows = vars(Sex),  scales = "free")  + 
  coord_flip() +
  xlab("Contribution in years of life expectancy")
ggsave("SIMAH_workplace/life_expectancy/3_graphs/2020_decomp/2_LE decomp_SES_2020.jpeg", dpi=600, width=30, height=15, units="cm")

#### Graph results by sex, SES and race
# Graph results by sex and SES
dResults_contrib <- as.data.frame(dResults_contrib_race[, -1])
dResults_contrib <- filter(dResults_contrib, start_year == 2019)
group1gathered <- gather(data = dResults_contrib, key = "mort", value = "value", 
                         -sex , -edclass, -race, -start_year, -end_year, -LE1, -LE2)

# have to be factor variables
group1gathered$mort = gsub(pattern = "mort", replacement = "", x = group1gathered$mort)

group1gathered <- group1gathered %>% 
  mutate_at(vars(sex, race, edclass, mort), as.factor)

levels(group1gathered$sex) <- list(Men = "1", Women = "2")
levels(group1gathered$edclass) <- list("Low" = "LEHS", "Middle" = "SomeC", "High" = "College")
levels(group1gathered$mort) <- list("Covid 19" = "cov",
                                              "Influenza and pneumonia" = "flu", 
                                              "Other infectious diseases" = "othinf", 
                                              
                                              "Alcohol poisoning" = "alcpoi",
                                              "Opioid poisoning" = "opioid",
                                              "Suicide" = "sij",
                                              "Motor vehicle accident" = "mvacc", 
                                              "Unintentional injury*" = "uij",   
                                              "Other injury" = "othj",   
                                              
                                              "Alcohol use disorder" = "aud",
                                              "Liver disease & cirrhosis" = "liver", 
                                              "Kidney disease" = "kidney",
                                              "Diabetes Mellitus" = "dm",
                                              "Dementia" = "dementia",
                                              "Cerebrovascular diseases" = "heart", 
                                              "Diseases of the heart" = "stroke", 
                                              "Cancer" = "cancer", 
                                              "Chronic lower respiratory diseases" = "resp", 
                                              "Other NCDs" = "othncd",
                                              
                                              "Rest" = "rest")

group1gathered <- rename(group1gathered, Sex = sex,
       "Race" = "race", 
       "Cause_of_death" = "mort", 
       "Contribution"= "value", 
       "Education" = "edclass")


write.csv(group1gathered, "SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/decomp_results_2020_race.csv")
group1gathered <- filter(group1gathered, Race != "Other", start_year == 2019)
ggplot(data = group1gathered, aes(x = Education, y = Contribution, fill = Cause_of_death)) +
  geom_bar(position = position_stack(reverse = T), stat = "identity") +
  scale_fill_manual("Cause of death", values = color.vec)+ 
  facet_grid(rows = vars(Sex, Race)) + 
  coord_flip() +
  theme_bw() +
  ylab("Contribution to changes in life expectancy in years")
ggsave("SIMAH_workplace/life_expectancy/3_graphs/2020_decomp/2_LE decomp_race_2020.jpeg", dpi=600, width=30, height=15, units="cm")
