# create preliminary file for LE decomposition with aggregated US mortality data (2000 & 2015)

# libraries required:
library("tidyverse")
library("dplyr")
library("reshape")
library("data.table")

## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")

dResults_contrib <- read.csv(
  "SIMAH_workplace/life_expectancy/2_out_data/dResults_contrib_2018_2020ACS.csv")

# Graph results
dResults_contrib <- as.data.frame(dResults_contrib[, -1])
group1gathered <- gather(data = dResults_contrib, key = "mort", value = "value", 
                         -sex , -edclass, -start_year, -end_year, -LE1, -LE2)

# have to be factor variables
group1gathered$mort <- as.factor(group1gathered$mort)
group1gathered$sex <- as.factor(group1gathered$sex)
group1gathered$SES <- as.factor(group1gathered$edclass)
class(group1gathered$SES)
group1gathered$SES <- as.factor(group1gathered$SES)

names(group1gathered) <- c("Sex", "SES", "start_year", "end_year", "LE1", "LE2", "Cause_of_death", "Contribution")     

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
  facet_grid(cols = vars(SES), rows = vars(Sex),  scales = "free") 
ggsave("SIMAH_workplace/life_expectancy/3_graphs/2_LE decomp_2018_2020_rest.jpeg", dpi=600, width=20, height=30, units="cm")

dGraph <- group1gathered[which(group1gathered$Cause_of_death != "Rest"),]
dGraph$Year <- as.factor(dGraph$end_year) 
color.vec <- c("#E2C244", "#ed7a6d", "#d72c40", "#cf82a6", "#a14d72", "#732946" ,"#93AEBF", "#447a9e", "#69AA9E")

## Plot contributions
ggplot(data = dGraph, aes(y = Contribution, x = Year, fill = Cause_of_death)) +
  geom_bar(position = "stack", stat = "identity", width = 0.8) +
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
ggsave("SIMAH_workplace/life_expectancy/3_graphs/2_LE decomp_2018_2020.jpeg", dpi=600, width=23, height=15, units="cm")

