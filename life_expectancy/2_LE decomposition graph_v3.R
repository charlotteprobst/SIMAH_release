# create preliminary file for LE decomposition with aggregated US mortality data (2000 & 2015)

# libraries required:
library("tidyverse")
library("dplyr")
library("reshape")
library("data.table")

## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")

dResults_contrib <- read.csv("SIMAH_workplace/life_expectancy/2_out_data/dResults_contrib_2000_2018CPS_v4.csv")


# Graph results
dResults_contrib <- as.data.frame(dResults_contrib[, -1])
group1gathered <- gather(data = dResults_contrib, key = "mort", value = "value", 
                         -sex , -edclass, -start_year, -end_year, -LE1, -LE2)

# have to be factor variables
group1gathered$mort <- as.factor(group1gathered$mort)
#group1gathered <- group1gathered[order(group1gathered$start_year, group1gathered$edclass), ]
group1gathered$sex <- as.factor(group1gathered$sex)
group1gathered$edclass <- as.factor(group1gathered$edclass)

names(group1gathered) <- c("Sex", "SES", "start_year", "end_year", "LE1", "LE2", "Cause_of_death", "Contribution")     

levels(group1gathered$SES) <- list("High" = "College", "Middle" = "SomeC", "Low" = "LEHS")
levels(group1gathered$Sex) <- list(Men = "1", Women = "2")
levels(group1gathered$Cause_of_death) <- list("Alcohol use disorder" = "AUDmort", 
                                              "Liver disease & cirrhosis" = "LVDCmort", 
                                              "Pancreatitis" = "PANCmort",
                                              "Suicide" = "IJmort",
                                              "Motor vehicle accident" = "MVACCmort", 
                                              "Unintentional injury*" = "UIJmort",   
                                              "IHD & ischemic stroke" = "IHDmort", 
                                              "Hemorrhagic Stroke" = "HSTRmort",
                                              "Hypertensive heart disease" = "HYPHDmort", 
                                              "Diabetes mellitus"= "DMmort",
                                              "Cancer" = "CANmort", 
                                              "LRI" = "LRImort",
                                              "Rest" = "RESTmort")
graph_detailed <- group1gathered[which(group1gathered$Cause_of_death != "RESTmort"),]


## Calculat average total gains and losses across five year intervals
keyDat <- data.frame(start_year = c(2000:2017), 
                     range = c(rep(1, 5), rep(2, 5), rep(3, 5), rep(4, 3)))
dat5year <- group1gathered %>%
  left_join(., keyDat) %>% 
  group_by(Sex, SES, Cause_of_death, Range = range) %>%
  summarise(Contribution = sum(Contribution))

dat5year <- dat5year %>%
  mutate(Years = recode(Range, 
                        `1` = "2000-2005", 
                        `2` = "2005-2010",
                        `3` = "2010-2015", 
                        `4` = "2015-2018"))
#write.csv(dat5year, "2_LE decomposition_5year.csv")
write.csv(dat5year, "SIMAH_workplace/life_expectancy/2_out_data/2_LE decomposition_5year.csv")
dat5year_wide <- spread(data = dat5year, key = Cause_of_death, value = Contribution)
#write.csv(dat5year_wide, "2_LE decomposition_5year_wide.csv")
write.csv(dat5year_wide, "SIMAH_workplace/life_expectancy/2_out_data/2_LE decomposition_5year_wide.csv")

dat5year <- dat5year[which(dat5year$Cause_of_death != "Rest"),]

color.vec <- c("#22577a", "#00b4d8", "#450920", "#a53860", "#da627d" ,"#1b4332", "#1a936f", "#88d498", "#FFCA09")
color.vec <- c("#fa8a8d", "#da315e", "#cf82a6", "#a14d72", "#732946" ,"#80a6bd", "#447a9e", "#132268", "#69AA9E")
color.vec <- c("#ed7a6d", "#d72c40", "#cf82a6", "#a14d72", "#732946" ,"#93AEBF", "#447a9e", "#132268", "#69AA9E")
color.vec <- c("#ed7a6d", "#d72c40", "#cf82a6", "#a14d72", "#732946" ,"#93AEBF", "#447a9e", "#69AA9E", "blue", "black", "yellow", "red")

## version 1 (dark green) Celine
ggplot(data = dat5year, aes(y = Contribution, x = Years, fill = Cause_of_death)) +
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
ggsave("SIMAH_workplace/life_expectancy/3_graphs/2_LE decomposition_5year.jpeg", dpi=600, width=23, height=15, units="cm")
