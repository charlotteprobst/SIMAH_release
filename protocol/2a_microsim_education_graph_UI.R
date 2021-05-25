library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)

k.wd <- c("C:/Users/Marie/Dropbox/NIH2020/Protocol_paper")
setwd(k.wd)

######## plot for uncertainty BY SEX 
col.vec <- c('#d72c40', '#132268','#447a9e','#93aebf')
uncertainty <- read.csv("output_data/uncertainty_estimates100sex.csv") %>% rename(sex = microsim.init.sex,
                                                                            edclass = microsim.init.education) %>% 
  mutate(sex = recode(sex, "Male"="Men","Female"="Women"),
         sex = factor(sex, levels=c("Men","Women")),
         edclass = recode(edclass, "High school diploma or less" = "High school degree or less",
                          "Some college"="Some college",
                          "College degree or more" = "College degree or more"),
         edclass = factor(edclass, levels=c("High school degree or less","Some college",
                                  "College degree or more")))

pe <- read.csv("output_data/microsim_education_summary_10000.csv") %>% filter(datatype=="Microsimulation") %>% 
  mutate(sex = recode(sex, "Female"="Women","Male"="Men"),
                  sex = factor(sex, levels=c("Men","Women")),
                   edclass = recode(edclass,"LEHS"="High school degree or less",
                                           "SomeC" = "Some college",
                                            "College"="College degree or more"),
                  edclass = factor(edclass, levels=c("High school degree or less","Some college",
                                                     "College degree or more")),
                  datatype = "Microsimulation") %>% select(-n) %>% rename(PE=percent)

uncertainty <- left_join(uncertainty, pe)
uncertainty$percent <- ifelse(uncertainty$datatype=="Microsimulation", uncertainty$PE,
                              uncertainty$percent)

uncertainty$PE <- NULL
uncertainty$datatype <- factor(uncertainty$datatype, levels = c("Microsimulation", "Census", "ACS", "PSID"))
levels(uncertainty$sex) <-  list("Men" =  "Males", 
                                 "Women" = "Females")
levels(uncertainty$edclass) <- list("High school degree or less" =  "High school diploma or less", 
                                    "Some college" = "Some college", 
                                    "College degree or more" = "College degree or more")
#uncertainty$edclass <- factor(uncertainty$edclass, levels = c("High school degree or less", "Some college", "College degree or more"))

ggplot(data = uncertainty, aes(x = year, y = percent, color=datatype, shape=datatype, fill=datatype, size=datatype)) +
  geom_ribbon(aes(ymin=min, ymax=max), alpha=0.25, colour=NA) + 
  geom_line(aes(color=datatype, size=datatype), alpha= .75) +
  facet_grid(cols = vars(sex), rows = vars(edclass), scales = "free") +
  geom_point(size = 1.3, alpha= .9) +
  #scale_size_manual(breaks=c("Microsimulation", "Census","ACS", "PSID"), values=c(1.1, 0.6, 0.6, 0.6)) +

  scale_shape_manual(name="Data Type", values = c(16, 18, 18, 18))  + 
  scale_color_manual(name = "Data Type", values = col.vec)  + 
  scale_fill_manual(name = "Data Type", values = c("grey40","white","white","white")) + 
  scale_size_manual(name="Data Type", values=c(1.1, 0.6, 0.6, 0.6)) +
  ylim(0,NA) + 
  theme_light() + 
  theme(strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = 'black'), 
        text = element_text(size = 14),
        axis.text = element_text(size = 12), legend.position="bottom", 
        legend.title = element_text(size = 12)) +
  labs(x = "Year ", y = "Proportion (%)", color = "Data Type", size = "Data Type", shape = "Data Type") +
  theme(panel.spacing = unit(1.2, "lines"))

ggsave("graphs/2_microsim_education_graph_uncertainty100sex.jpeg", dpi = 600, width = 22, height = 20, units = "cm")

##### plot for uncertainty BY RACE 
uncertainty <- read.csv("output_data/uncertainty_estimates100race.csv") %>% rename(race=microsim.init.race,
                                                                               edclass = microsim.init.education) %>% 
  mutate(edclass = recode(edclass, "High school diploma or less" = "High school degree or less",
                          "Some college"="Some college",
                          "College degree or more" = "College degree or more"),
         edclass = factor(edclass, levels=c("High school degree or less","Some college",
                                            "College degree or more")),
         race = recode(race, "BLA"="Non-Hispanic Black",
                       "WHI"="Non-Hispanic White",
                       "OTH"="Non-Hispanic Other",
                       "SPA"="Hispanic"))

pe <- read.csv("output_data/microsim_education_summary_10000race.csv") %>% filter(datatype=="Microsimulation") %>% 
  rename(race = microsim.init.race,
         edclass = microsim.init.education) %>% 
  mutate(edclass = recode(edclass,"LEHS"="High school degree or less",
                          "SomeC" = "Some college",
                          "College"="College degree or more"),
         edclass = factor(edclass, levels=c("High school degree or less","Some college",
                                            "College degree or more")),
         datatype = "Microsimulation") %>% rename(PE=percent)

uncertainty <- left_join(uncertainty, pe)
uncertainty$percent <- ifelse(uncertainty$datatype=="Microsimulation", uncertainty$PE,
                              uncertainty$percent)

uncertainty$PE <- NULL
uncertainty$datatype <- factor(uncertainty$datatype, levels = c("Microsimulation", "Census", "ACS", "PSID"))

levels(uncertainty$edclass) <- list("High school degree or less" =  "High school diploma or less", 
                                    "Some college" = "Some college", 
                                    "College degree or more" = "College degree or more")
uncertainty <- uncertainty %>% filter(year!=2010 & datatype!="Census")
#uncertainty$edclass <- factor(uncertainty$edclass, levels = c("High school degree or less", "Some college", "College degree or more"))
options(digits=2)
ggplot(data = uncertainty, aes(x = year, y = percent, color=datatype, shape=datatype, fill=datatype, size=datatype)) +
  geom_ribbon(aes(ymin=min, ymax=max), alpha=0.25, colour=NA) + 
  geom_line(aes(color=datatype, size=datatype), alpha= .75) +
  facet_grid(cols = vars(race), rows = vars(edclass), scales = "free") +
  geom_point(size = 1.3, alpha= .9) +
  #scale_size_manual(breaks=c("Microsimulation", "Census","ACS", "PSID"), values=c(1.1, 0.6, 0.6, 0.6)) +
  
  scale_shape_manual(name="Data Type", values = c(16, 18, 18, 18))  + 
  scale_color_manual(name = "Data Type", values = col.vec)  + 
  scale_fill_manual(name = "Data Type", values = c("grey40","white","white","white")) + 
  scale_size_manual(name="Data Type", values=c(1.1, 0.6, 0.6, 0.6)) +
  ylim(0,NA) + 
  theme_light() + 
  theme(strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = 'black'), 
        text = element_text(size = 14),
        axis.text = element_text(size = 12), legend.position="bottom", 
        legend.title = element_text(size = 12)) +
  labs(x = "Year ", y = "Proportion (%)", color = "Data Type", size = "Data Type", shape = "Data Type") +
  theme(panel.spacing = unit(1.2, "lines"))

ggsave("graphs/2_microsim_education_graph_uncertainty100race.jpeg", dpi = 600, width = 22, height = 20, units = "cm")



