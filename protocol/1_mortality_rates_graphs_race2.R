library(dplyr)
library(tidyr)
library(ggplot2)

k.wd <- c("C:/Users/Marie/Dropbox/NIH2020/")
k.wd <- c("~/Google Drive/SIMAH Sheffield/")
setwd(k.wd)

weights <- read.csv("SIMAH_workplace/protocol/output_data/1_population_weights_2018_2.csv")
df <- read.csv("SIMAH_workplace/protocol/output_data/1_microsim_deaths_summary2.csv")

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
sum <- df %>% ungroup() %>% group_by(year, datatype, cause, agecat, sex, raceeth, .drop=FALSE) %>% 
  summarise(deaths=sum(totaldeaths), population=sum(n))

# join 2018 age percentages with data for all years - to only use age splits for 2018
sum <- left_join(sum, weights)

# calculate crude death rate for each row (year, sex, edclass, agecat, cause) per 100,000
sum$rate <- (sum$deaths/sum$population)*100000

# calculate weighted rate
sum$weighted_rate <- sum$rate*sum$percent

# calculate age-standardised rates - sum of age-weighted rates (i.e. weighted sum)
sum <- sum %>% mutate(sex=as.factor(sex),raceeth=as.factor(raceeth),cause=as.factor(cause),
                      year=as.factor(year), datatype=as.factor(datatype)) %>% 
  group_by(year, sex, raceeth, cause, datatype, .drop=FALSE) %>% 
  summarise(rate = sum(weighted_rate))

# Specifications for the SES graph
data_graph <- subset(sum, cause!="Rest")

# recode the causes of death to change order on plot 
data_graph <- data_graph %>% 
  mutate(cause = factor(cause, levels=c("AUD","Stroke","HHD",
                                        "Liver C.","Suicide","Other UI", "MVA",
                                        "Diabetes",
                                        "IHD")),
         raceeth = recode(raceeth, "WHI"="Non-Hispanic White",
                          "BLA"="Non-Hispanic Black", "SPA"="Hispanic",
                          "OTH"="Non-Hispanic Others"),
         raceeth = factor(raceeth, levels=c("Non-Hispanic White","Non-Hispanic Black",
                                            "Hispanic","Non-Hispanic Others")),
         year = as.numeric(as.character(year)))
  
# create dummy data for custom scale 
cause <- unique(data_graph$cause)
sex <- unique(data_graph$sex)
raceeth <- unique(data_graph$raceeth)
facet_bounds <- expand.grid(cause,sex,raceeth)
facet_bounds$ymin <- 0
facet_bounds$ymax <- ifelse(facet_bounds$Var1=="IHD", 300,
                            ifelse(facet_bounds$Var1=="AUD" |
                                     facet_bounds$Var1=="Stroke", 25,
                                   60))
names(facet_bounds) <- c("cause","sex","raceeth","ymin","ymax")

ff <- with(facet_bounds,
           data.frame(rate=c(ymin,ymax),
                      cause=c(cause,cause),
                      sex =c(sex,sex),
                      raceeth=c(raceeth,raceeth)))

color.vec <- c("#132268", "#447a9e", "#93AEBF")
color.vec <- c("#132268", "#447a9e")
color.vec <- c("#062D59", "#76868D", "#E6DFC0")

ggplot(data=data_graph, aes(x=year, y=rate, colour=sex)) + 
  facet_grid(rows = vars(cause), cols = vars(raceeth), scales = "free") +
  theme_light() + 
  theme(strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = 'black'), 
        text = element_text(size = 14),
        axis.text = element_text(size = 12), legend.position="bottom", 
        legend.title = element_text(size = 12),
        strip.placement = "outside") +
  ylab("Age standardized mortality rate per 100,000") + xlab("Year") +
  ylim(0, NA) +
  # scale_color_brewer(palette="Set2") + 
  scale_color_manual(values = color.vec) +
  geom_line(aes(linetype=datatype, size = datatype), alpha= .65) + 
  #geom_line(aes(linetype=datatype), size = 0.7, alpha= .7) + 
  scale_linetype_manual(values = c(1, 3, 3, 3)) +
  scale_size_manual(breaks=c("Microsimulation", "Observed"), values=c(1, 0.7, 0.7, 0.7)) +
  labs(color="Sex", linetype = "Data type", size = "Data type") +
  guides(color = guide_legend(nrow = 3), linetype = guide_legend(nrow = 2), size = guide_legend(nrow = 2)) + 
  geom_point(data=ff,x=NA, colour=NA)

ggsave("SIMAH_workplace/protocol/graphs/1_mortality_rates_race.jpeg", dpi=600, width=18, height=25, units="cm")


