library(dplyr)
library(tidyr)
library(ggplot2)

k.wd <- c("C:/Users/Marie/Dropbox/NIH2020/Protocol_paper")
setwd(k.wd)

# GRAPH SPLIT BY SEX * SES

summary <- read.csv("1_mortality_rates_data.csv")

data_graph <- subset(summary, cause!="REST" )
Data <- c("dashed", "solid")
Data <- Data[as.numeric(as.factor(data_graph$datatype))]
symbol <- c(16, 21)
symbol <- symbol[as.numeric(as.factor(data_graph$datatype))]
levels(data_graph$edclass) <- list("High school degree or less" = "LEHS", "Some college" = "SomeC", "College degree or more" = "College")
levels(data_graph$Sex) <- list(Males = "m", Females = "f")
levels(data_graph$Sex) <- list(Males = "m", Females = "f")
#levels(data_graph$cause) <- list("Alcohol use disorder" = "AUD", 
#                                 "Liver Disease and Cirrhosis" = "LVDC",  
#                                 "Intentional self-harm" = "IJ",
#                                 "Motor vehicle accident" = "MVACC", 
#                                 "Other unintent. injury" = "UIJ",   
#                                 "Ischemic Heart Disease" = "IHD", 
#                                 "Hypertensive Heart Disease" = "HYPHD",
#                                 "Stroke" = "STR", 
#                                 "Diabetes mellitus"= "DM")

levels(data_graph$cause) <- list("AUD" = "AUD",
                                 "Liver C." = "LVDC",
                                 "Suicide" = "IJ",
                                 "MVA" = "MVACC", 
                                 "Other UI" = "UIJ",
                                 "IHD" = "IHD", "HHD" = "HYPHD",
                                 "Stroke" = "STR", 
                                 "Diabetes"= "DM")
data_graph$datatype <- recode(data_graph$datatype, "microsim" = "Microsimulation", "target"= "Observed")

color.vec <-  c('#bce784', "#5dd39e",'#348aa7')
color.vec <- c("#132268", "#447a9e", "#93AEBF")
ggplot(data=data_graph, aes(x=year, y=rate, colour=edclass)) + 
  facet_grid(rows = vars(cause), cols = vars(Sex), scales = "free") +
  theme_light() +
  theme(strip.background = element_rect(fill = "white")) +
  theme(strip.text = element_text(colour = 'black'), text = element_text(size = 16)) +
  ylab("Age standardized mortality rate per 100,000") + xlab("Year") +
  ylim(0, NA) +
  scale_color_manual(values = color.vec) +
  geom_line(aes(linetype=datatype, size = datatype), alpha= .65) + 
  #geom_line(aes(linetype=datatype), size = 0.7, alpha= .7) + 
  scale_linetype_manual(values = c(1, 3, 3, 3)) +
  scale_size_manual(breaks=c("Microsimulation", "Observed"), values=c(1, 0.6, 0.6, 0.6)) +
  labs(color="Education", linetype = "Data type", size = "Data type")

ggsave("1_mortality_rates_graph_v1.jpeg", dpi=600, width=25, height=28, units="cm")

# GRAPH SPLIT BY RACE/SES

summary <- read.csv("output_data/1_mortality_rates_data_race.csv")

data_graph <- subset(summary, cause!="REST" )
Data <- c("dashed", "solid")
Data <- Data[as.numeric(as.factor(data_graph$datatype))]
symbol <- c(16, 21)
symbol <- symbol[as.numeric(as.factor(data_graph$datatype))]
levels(data_graph$edclass) <- list("High school degree or less" = "LEHS", "Some college" = "SomeC", "College degree or more" = "College")
levels(data_graph$Sex) <- list(Males = "m", Females = "f")
#levels(data_graph$cause) <- list("Alcohol use disorder" = "AUD", 
#                                 "Liver Disease and Cirrhosis" = "LVDC",  
#                                 "Intentional self-harm" = "IJ",
#                                 "Motor vehicle accident" = "MVACC", 
#                                 "Other unintent. injury" = "UIJ",   
#                                 "Ischemic Heart Disease" = "IHD", 
#                                 "Hypertensive Heart Disease" = "HYPHD",
#                                 "Stroke" = "STR", 
#                                 "Diabetes mellitus"= "DM")

levels(data_graph$cause) <- list("AUD" = "AUD",
                                 "Liver C." = "LVDC",
                                 "Suicide" = "IJ",
                                 "MVA" = "MVACC", 
                                 "Other UI" = "UIJ",
                                 "IHD" = "IHD", "HHD" = "HYPHD",
                                 "Stroke" = "STR", 
                                 "Diabetes"= "DM")
data_graph$datatype <- recode(data_graph$datatype, "microsim" = "Microsimulation", "target"= "Observed")

color.vec <-  c('#bce784', "#5dd39e",'#348aa7')
color.vec <- c("#132268", "#447a9e", "#93AEBF")
ggplot(data=data_graph, aes(x=year, y=rate, colour=edclass)) + 
  facet_grid(rows = vars(cause), cols = vars(raceeth), scales = "free") +
  theme_light() +
  theme(strip.background = element_rect(fill = "white")) +
  theme(strip.text = element_text(colour = 'black'), text = element_text(size = 16)) +
  ylab("Age standardized mortality rate per 100,000") + xlab("Year") +
  ylim(0, NA) +
  scale_color_manual(values = color.vec) +
  geom_line(aes(linetype=datatype, size = datatype), alpha= .65) + 
  #geom_line(aes(linetype=datatype), size = 0.7, alpha= .7) + 
  scale_linetype_manual(values = c(1, 3, 3, 3)) +
  scale_size_manual(breaks=c("Microsimulation", "Observed"), values=c(1, 0.6, 0.6, 0.6)) +
  labs(color="Education", linetype = "Data type", size = "Data type")

ggsave("1_mortality_rates_graph_race_v1.jpeg", dpi=600, width=25, height=28, units="cm")

# data for correlations table 
correlations <- data_graph %>% select(-X) %>% pivot_wider(names_from=c(datatype), values_from=rate) 
correlation <- list()
# loop through causes and save the correlation to a .csv file
for(i in levels(as.factor(correlations$cause))){
  for(j in levels(as.factor(correlations$edclass))){
  tocorrelate <- correlations %>% filter(cause==i) %>% filter(edclass==j)
  cor <- cor.test(tocorrelate$Microsimulation, tocorrelate$Observed)
  correlation[[paste(i,j)]]$R <- as.numeric(cor$estimate)
  correlation[[paste(i,j)]]$lower <- cor$conf.int[1]
  correlation[[paste(i,j)]]$upper <- cor$conf.int[2]
  correlation[[paste(i,j)]]$cause <- i
  correlation[[paste(i,j)]]$edclass <- j
  correlation[[paste(i,j)]] <- data.frame(correlation[[paste(i,j)]])
}
}

correlation <- do.call(rbind,correlation)
correlation <- correlation %>% pivot_wider(names_from=edclass, values_from=c(R,lower,upper)) %>% 
  select(cause, R_LEHS, lower_LEHS, upper_LEHS,
         R_SomeC, lower_SomeC, upper_SomeC,
         R_College, lower_College, upper_College)
write.csv(correlation, "correlation_deaths.csv", row.names=F)

