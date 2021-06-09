# SIMAH - protocol paper. June 2021
# This code reads in population weights and mortality output over time to generate
# graphs for the manuscript (main body and appendix) for mortality rates over time
# by SES and sex

library(dplyr)
library(tidyr)
library(ggplot2)

k.wd <- c("C:/Users/Marie/Dropbox/NIH2020/")
k.wd <- c("~/Google Drive/SIMAH Sheffield/")
setwd(k.wd)

weights <- read.csv("SIMAH_workplace/protocol/output_data/1_population_weights_2018.csv")
df <- read.csv("SIMAH_workplace/protocol/output_data/1_microsim_deaths_summary.csv")

## General specifications for graphs
weights$datatype <- recode(weights$datatype, "microsim" = "Microsimulation", "target"= "Observed")

Data <- c("dashed", "solid")
Data <- Data[as.numeric(as.factor(df$datatype))]
symbol <- c(16, 21)
symbol <- symbol[as.numeric(as.factor(df$datatype))]

df$edclass <- recode(df$edclass,
                             "LEHS" = "High school degree or less",
                             "SomeC" = "Some college",
                             "College" = "College degree or more")
df$edclass <- factor(df$edclass,
                             levels = c("High school degree or less",
                                        "Some college",
                                        "College degree or more"))

df$sex <- recode(df$sex, "m" = "Men", "f" = "Women")

df$cause <- recode(df$cause, 
                           "REST" = "Rest",        
                           "AUD" = "AUD",
                           "LVDC" = "Liver C.",
                           "IJ" = "Suicide",
                           "MVACC" = "MVA", 
                           "UIJ" = "Other UI",
                           "IHD" = "IHD", 
                           "HYPHD" = "HHD",
                           "ISTR" = "Stroke",  
                           "DM" = "Diabetes")
df$cause <- factor(df$cause, levels = c("AUD", "Liver C.", 
                                                        "Suicide", "MVA", 
                                                        "Other UI", "IHD", "HHD", 
                                                        "Stroke", "Diabetes", "Rest"))
df$datatype <- recode(df$datatype, "microsim" = "Microsimulation", "target"= "Observed")


## Prepare for SES graph
sum <- df %>% ungroup() %>% group_by(year, datatype, cause, agecat, sex, edclass, .drop=FALSE) %>% 
  summarise(deaths=sum(totaldeaths), population=sum(n))

# join 2018 age percentages with data for all years - to only use age splits for 2018
sum <- left_join(sum, weights)

# calculate crude death rate for each row (year, sex, edclass, agecat, cause) per 100,000
sum$rate <- (sum$deaths/sum$population)*100000

# calculate weighted rate
sum$weighted_rate <- sum$rate*sum$percent

# calculate age-standardised rates - sum of age-weighted rates (i.e. weighted sum)
sum <- sum %>% group_by(year, sex, edclass, cause, datatype) %>% 
  summarise(rate = sum(weighted_rate))

# Specifications for the SES graph
data_graph <- subset(sum, cause!="Rest")

color.vec <- c("#132268", "#447a9e", "#93AEBF")
ggplot(data=data_graph, aes(x=year, y=rate, colour=edclass)) + 
  facet_grid(rows = vars(cause), cols = vars(sex), scales = "free") +
  theme_light() +
  theme(strip.background = element_rect(fill = "white"), legend.position = "bottom",  legend.box = "horizontal", legend.box.just = "top") +
  theme(strip.text = element_text(colour = 'black'), text = element_text(size = 14)) +
  ylab("Age standardized mortality rate per 100,000") + xlab("Year") +
  ylim(0, NA) +
  scale_color_manual(values = color.vec) +
  geom_line(aes(linetype=datatype, size = datatype), alpha= .75) + 
  #geom_line(aes(linetype=datatype), size = 0.7, alpha= .7) + 
  scale_linetype_manual(values = c(1, 3, 3, 3)) +
  scale_size_manual(breaks=c("Microsimulation", "Observed"), values=c(1, 0.7, 0.7, 0.7)) +
  labs(color="Education", linetype = "Data type", size = "Data type") +
  guides(color = guide_legend(nrow = 3), linetype = guide_legend(nrow = 2), size = guide_legend(nrow = 2))
ggsave("SIMAH_workplace/protocol/graphs/1_mortality_rates_ses.jpeg", dpi=600, width=18, height=25, units="cm")


