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
  "SIMAH_workplace/life_expectancy/2_out_data/dResults_contrib_2018_2020race_ACS.csv")

# Graph results by sex and SES
dResults_contrib <- as.data.frame(dResults_contrib[, -1])
names(dResults_contrib) = gsub(pattern = "mort", replacement = "", x = names(dResults_contrib))
group1gathered <- gather(data = dResults_contrib, key = "mort", value = "value", 
                         -sex , -edclass, -start_year, -end_year, -LE1, -LE2)

# have to be factor variables
group1gathered$mort <- as.factor(group1gathered$mort)
group1gathered$sex <- as.factor(group1gathered$sex)
group1gathered$SES <- as.factor(group1gathered$edclass)
class(group1gathered$SES)
group1gathered$SES <- as.factor(group1gathered$SES)

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
	

color.vec <- c("#E2C244", "#ed7a6d", "#d72c40", "#cf82a6", "#a14d72", "#732946" ,"#93AEBF", "#447a9e", "#132268", "#69AA9E")
ncd1 <- 5
ncd2 <- 5
injury <- 6
infectious <- 3
other <- 1
qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
color.vec <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

color.vec <- c(rev(brewer.pal(injury,"Oranges")),
               rev(brewer.pal(ncd1,"YlOrRd")),
               rev(brewer.pal(ncd2,"Greens")),
               rev(brewer.pal(infectious,"Blues")),
               c("grey"))
## Plot showing changes in every year (will not be included in publication)
ggplot(data = group1gathered, aes(y = Contribution, x = end_year, fill = Cause_of_death)) +
  geom_bar(position = position_stack(reverse = T), stat = "identity") +
  scale_fill_manual("Cause of death", values = color.vec)+ 
  facet_grid(cols = vars(SES), rows = vars(Sex),  scales = "free") 
ggsave("SIMAH_workplace/life_expectancy/3_graphs/2020_decomp/2_LE decomp_2018_2020_rest.jpeg", dpi=600, width=30, height=15, units="cm")

dGraph <- group1gathered[which(group1gathered$Cause_of_death != "Rest" & 
                                 group1gathered$Cause_of_death != "Covid 19"),]
dGraph$Year <- as.factor(dGraph$end_year) 
color.vec <- c("#E2C244", "#ed7a6d", "#d72c40", "#cf82a6", "#a14d72", "#732946" ,"#93AEBF", "#447a9e", "#69AA9E")
color.vec <- c("#ed7a6d", "#d72c40", "#cf82a6", "#a14d72", "#732946" ,"#93AEBF", "#447a9e", "#69AA9E")

## Plot contributions
ggplot(data = dGraph, aes(y = Contribution, x = Year, fill = Cause_of_death)) +
  geom_bar(position = "stack", stat = "identity", width = 0.8, ) +
  #facet_grid(cols = vars(Sex), rows = vars(SES),  scales = "free") +
  facet_grid(cols = vars(SES), rows = vars(Sex),  scales = "free") +
  theme_light() +
  theme(strip.background = element_rect(fill = "white")) +
  theme(strip.text = element_text(colour = 'black'), text = element_text(size = 14)) +
  #theme(legend.position = "right") +
  scale_fill_manual("Cause of death", values = color.vec)+ 
  ylab("Contribution to changes in life expectancy (years)") +
  #xlab("Years") +
  geom_hline(aes(yintercept=0)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#ggsave("2_LE decomposition_5year_v1.jpeg", dpi = 600, width = 23, height = 15, units = "cm")
ggsave("SIMAH_workplace/life_expectancy/3_graphs/2_LE decomp_2018_2020.jpeg", dpi=600, width=30, height=15, units="cm")

#### Graph results by sex, SES and race
# Graph results by sex and SES
dResults_contrib_race <- as.data.frame(dResults_contrib_race[, -1])
group1gathered <- gather(data = dResults_contrib, key = "mort", value = "value", 
                         -sex , -edclass, -race, -start_year, -end_year, -LE1, -LE2)

# have to be factor variables
group1gathered$mort <- as.factor(group1gathered$mort)
group1gathered$sex <- as.factor(group1gathered$sex)
group1gathered$SES <- as.factor(group1gathered$edclass)
group1gathered$race <- as.factor(group1gathered$race)
class(group1gathered$SES)
group1gathered$SES <- as.factor(group1gathered$SES)

names(group1gathered) <- c("Sex", "SES", "Race", "start_year", "end_year", "LE1", "LE2", "Cause_of_death", "Contribution")     

levels(group1gathered$SES) <- list(High = "College", Middle = "SomeC", Low = "LEHS")
levels(group1gathered$Sex) <- list(Men = "1", Women = "2")
levels(group1gathered$Cause_of_death) <- list("Covid 19" = "COVmort",
                                              "Alcohol use disorder" = "AUDmort", 
                                              "Liver disease & cirrhosis" = "LVDCmort", 
                                              "Suicide" = "IJmort",
                                              "Motor vehicle accident" = "MVACCmort", 
                                              "Unintentional injury*" = "UIJmort",   
                                              "IHD & ischemic stroke" = "IHDmort", 
                                              "Hypertensive heart disease" = "HYPHDmort", 
                                              "Diabetes mellitus"= "DMmort",
                                              "Rest" = "RESTmort")

color.vec <- c("#E2C244", "#ed7a6d", "#d72c40", "#cf82a6", "#a14d72", "#732946" ,"#93AEBF", "#447a9e", "#132268", "#69AA9E")

## Plot showing changes in every year (will not be included in publication)
ggplot(data = group1gathered, aes(y = Contribution, x = end_year, fill = Cause_of_death)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual("Cause of death", values = color.vec)+ 
  facet_grid(cols = vars(Sex, SES), rows = vars(Race)) 
ggsave("SIMAH_workplace/life_expectancy/3_graphs/2_LE decomp_race_2018_2020_rest.jpeg", dpi=600, width=30, height=15, units="cm")

dGraph <- group1gathered[which(group1gathered$Cause_of_death != "Rest" & 
                                 group1gathered$Cause_of_death != "Covid 19"),]
dGraph$Year <- as.factor(dGraph$end_year) 
color.vec <- c("#E2C244", "#ed7a6d", "#d72c40", "#cf82a6", "#a14d72", "#732946" ,"#93AEBF", "#447a9e", "#69AA9E")
color.vec <- c("#ed7a6d", "#d72c40", "#cf82a6", "#a14d72", "#732946" ,"#93AEBF", "#447a9e", "#69AA9E")

## Plot contributions
ggplot(data = dGraph, aes(y = Contribution, x = Year, fill = Cause_of_death)) +
  geom_bar(position = "stack", stat = "identity", width = 0.8) +
  #facet_grid(cols = vars(Sex), rows = vars(SES),  scales = "free") +
  facet_grid(cols = vars(Sex, SES), rows = vars(Race)) +
  theme_light() +
  theme(strip.background = element_rect(fill = "white")) +
  theme(strip.text = element_text(colour = 'black'), text = element_text(size = 14)) +
  #theme(legend.position = "right") +
  scale_fill_manual("Cause of death", values = color.vec)+ 
  ylab("Contribution to changes in life expectancy (years)") +
  #xlab("Years") +
  geom_hline(aes(yintercept=0)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#ggsave("2_LE decomposition_5year_v1.jpeg", dpi = 600, width = 23, height = 15, units = "cm")
ggsave("SIMAH_workplace/life_expectancy/3_graphs/2_LE decomp_race_2018_2020.jpeg", dpi=600, width=30, height=15, units="cm")
