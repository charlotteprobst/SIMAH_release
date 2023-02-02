# SIMAH - protocol paper. June 2021
# This code reads in population weights and mortality output over time to generate
# graphs for the manuscript (main body and appendix) for mortality rates over time
# by SES and sex

library(dplyr)
library(tidyr)
library(ggplot2)
library(lemon)

k.wd <- c("C:/Users/Marie/Dropbox/NIH2020/")
#k.wd <- c("~/Google Drive/SIMAH Sheffield/")
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
                             "LEHS" = "High School Degree or Less",
                             "SomeC" = "Some College",
                             "College" = "College Degree or More")
df$edclass <- factor(df$edclass,
                             levels = c("High School Degree or Less",
                                        "Some College",
                                        "College Degree or More"))

df$sex <- recode(df$sex, "m" = "Men", "f" = "Women")

df$cause <- recode(df$cause, 
                           "REST" = "Rest",        
                           "AUD" = "AUD",
                           "LVDC" = "Liver cirrhosis",
                           "IJ" = "Suicide",
                           "MVACC" = "MVA", 
                           "UIJ" = "Other UI",
                           "IHD" = "IHD", 
                           "HYPHD" = "HHD",
                           "ISTR" = "Stroke",  
                           "DM" = "Diabetes")
df$cause <- factor(df$cause, levels = c("AUD", "Liver cirrhosis", 
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

# recode the causes of death to change order on plot 
data_graph <- data_graph %>% 
  mutate(cause = factor(cause, levels=c("AUD","HHD","Stroke",
                                        "Liver cirrhosis","Suicide","Other UI", "MVA",
                                        "Diabetes",
                                        "IHD")))

# create dummy data for custom scale 
cause <- unique(data_graph$cause)
sex <- unique(data_graph$sex)
edclass <- unique(data_graph$edclass)
facet_bounds <- expand.grid(cause,sex,edclass)
facet_bounds$ymin <- 0
facet_bounds$ymax <- ifelse(facet_bounds$Var1=="IHD", 300,
                            ifelse(facet_bounds$Var1=="AUD" | facet_bounds$Var1=="HHD" |
                                     facet_bounds$Var1=="Stroke", 25,
                                   50))
names(facet_bounds) <- c("cause","sex","edclass","ymin","ymax")

ff <- with(facet_bounds,
           data.frame(rate=c(ymin,ymax),
                      cause=c(cause,cause),
                      sex =c(sex,sex),
                      edclass=c(edclass,edclass)))

v.ylim <- c(rep(25, 9), rep(50, 15), rep(300, 3))
  
col.vec <- c("#062D59", "#76868D", "#E6DFC0")
col.vec <- c("black",  "gray75", "black",  "gray75")


ggplot(data=data_graph, aes(x=year, y=(rate), colour=sex)) + 
  facet_rep_grid(rows = vars(cause), cols = vars(edclass), scales = "free", repeat.tick.labels = TRUE) +
  #facet_wrap(c("edclass", "cause"), scales = "free", nrow = 3) +
  theme_light() +
  theme(strip.background = element_rect(fill = "white"), 
        panel.grid = element_blank(),
        panel.border = element_blank(), 
        axis.ticks = element_line(colour="black", size = 0.352), 
        axis.line = element_line(colour="black", size = 0.352),
        strip.text = element_text(size = 9, colour = 'black'), 
        axis.text = element_text(size = 9, colour="black"), 
        text = element_text(size = 9, colour="black", family="sans"), 
        strip.placement = "outside",
        legend.position="none") +
  ylim(0, NA) +
  xlim(2000, 2020) +
  scale_color_manual(values = col.vec) +
  geom_line(aes(linetype=datatype), size = 0.352) + 
  geom_point(data=ff,x=NA, colour=NA) +
  
  scale_linetype_manual(values = c(1, 3, 3, 3)) +

  labs(y = "Mortality Rate per 100,000", 
       x = "Year") 

ggsave("SIMAH_workplace/protocol/graphs/AJE-00063-2022 Probst Figure 4.pdf", width=8, height=11, units="in")

k <- 0

for (i in levels(data_graph$cause)) {
  for (j in levels(data_graph$edclass)) {
    k <- k+1
    ggplot(data=data_graph[which(data_graph$cause == i & data_graph$edclass == j),], aes(x=year, y=(rate), colour=sex)) + 
      ggtitle(paste0(j,", ",sep="\n", i)) +
      theme_light() +
      theme(strip.background = element_rect(fill = "white"), 
            panel.grid = element_blank(),
            panel.border = element_blank(), 
            axis.ticks = element_line(colour="black", size = 0.352),
            axis.line = element_line(colour="black", size = 0.352),
            strip.text = element_text(size = 9, colour = 'black'), 
            axis.text = element_text(size = 9, colour="black"), 
            text = element_text(size = 9, colour="black"), 
            strip.placement = "outside",
            legend.position="none",
            plot.title = element_text(size=9)) +
      ylim(0, v.ylim[k]) +
      xlim(2000, 2020) +
      scale_color_manual(values = col.vec) +
      geom_line(aes(linetype=datatype), size = 0.352) + 
      geom_point(data=ff,x=NA, colour=NA) +
      
      scale_linetype_manual(values = c(1, 3, 3, 3)) +
      
      labs(y = "Mortality Rate per 100,000", 
           x = "Year")
    
    ggsave(paste0("SIMAH_workplace/protocol/graphs/AJE-00063-2022 Probst Figure 4", LETTERS[k], ".eps"), 
           width=2.7, height=2.5, units="in", device = "eps")
    
  }
}


LiverC <- data_graph %>% filter(datatype=="Observed") %>% filter(cause=="Liver Cirrhosis")

ggplot(data=LiverC, aes(x=year, y=rate, colour=edclass)) + geom_line(size=1.5) + 
  facet_grid(cols=vars(sex)) + theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position="bottom",
        strip.background = element_rect(fill="white"),
        text = element_text(size=20)) +
  ylab("Age-standardised mortality rate per 100,000 population") + xlab("Year") + ylim(0,NA)
ggsave("SIMAH_workplace/protocol/graphs/mortality_LC.png", dpi=300, width=33.87, height=19.05, units="cm")
