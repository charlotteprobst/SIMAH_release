# SIMAH - protocol paper. June 2021
# This code reads in data on the microsimulation and respective uncertainty estimates
# to plot educational groups over time for the microsimulation and three data sources

library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)

k.wd <- c("C:/Users/Marie/Dropbox/NIH2020/")
k.wd <- c("~/Google Drive/SIMAH Sheffield")
setwd(k.wd)

######## plot for uncertainty BY SEX 
col.vec <- c('#062D59', '#576F81','#A8B0AA', '#EBE0B0') #Microsim first

uncertainty <- read.csv("SIMAH_workplace/protocol/output_data/2_uncertainty_estimatessex.csv") %>% rename(sex = microsim.init.sex,
                                                                            edclass = microsim.init.education) %>% 
  mutate(sex = recode(sex, "Male"="Men","Female"="Women"),
         sex = factor(sex, levels=c("Men","Women")),
         edclass = recode(edclass, "High school diploma or less" = "High school degree or less",
                          "Some college"="Some college",
                          "College degree or more" = "College degree or more"),
         edclass = factor(edclass, levels=c("High school degree or less","Some college",
                                  "College degree or more")))

pe <- read.csv("SIMAH_workplace/protocol/output_data/2_microsim_education_summary.csv") %>%
  mutate(datatype = "Microsimulation") %>% group_by(year, sex, edclass) %>% summarise(n=sum(n)) %>% 
  ungroup() %>% group_by(year, sex) %>% mutate(PE=n/sum(n)) %>% select(-n)


uncertainty <- left_join(uncertainty, pe)
uncertainty$percent <- ifelse(uncertainty$datatype=="Microsimulation", uncertainty$PE,
                              uncertainty$percent)

uncertainty$PE <- NULL
uncertainty$datatype <- factor(uncertainty$datatype, levels = c("Microsimulation", "Census", "ACS", "PSID"))
uncertainty$edclass <- factor(uncertainty$edclass, levels=c("High school degree or less",
                                                            "Some college",
                                                            "College degree or more"))
#uncertainty$edclass <- factor(uncertainty$edclass, levels = c("High school degree or less", "Some college", "College degree or more"))
# warning because we do not have a ribbon around the observed data
ggplot(data = uncertainty, aes(x = year, y = percent*100, color=datatype, shape=datatype, fill=datatype, size=datatype)) +
  geom_ribbon(aes(ymin=min*100, ymax=max*100), alpha=0.2,  fill = "grey",
              colour= "grey", linetype = 2, size=0.5) + 
  geom_line(aes(color=datatype, size=datatype), alpha= .7) +
  facet_grid(cols = vars(edclass), rows = vars(sex), scales = "free") +
  geom_point(size = 1.4, alpha= .9) +
  #scale_size_manual(breaks=c("Microsimulation", "Census","ACS", "PSID"), values=c(1.1, 0.6, 0.6, 0.6)) +

  scale_shape_manual(name="Data Type", values = c(16, 17, 18, 15))  + 
  scale_color_manual(name = "Data Type", values = col.vec)  + 
  scale_fill_manual(name = "Data Type", values = c("grey30","white","white","white")) + 
  scale_size_manual(name="Data Type", values=c(1.1, 1.1, 1.1, 1.1)) +
  ylim(0,NA) + 
  theme_light() + 
  theme(strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = 'black'), 
        text = element_text(size = 14),
        axis.text = element_text(size = 12), legend.position="bottom", 
        legend.title = element_text(size = 12),
        strip.placement = "outside") +
  labs(x = "Year ", y = "Proportion (%)", color = "Data Type", size = "Data Type", shape = "Data Type") +
  theme(panel.spacing = unit(1.2, "lines"))

ggsave("SIMAH_workplace/protocol/graphs/AJE-00063-2022 Probst Figure 3.pdf", 
       width = 20, height = 16, units = "cm")

for (i in unique(uncertainty$sex)) {
  for (j in unique(uncertainty$edclass)) {
    ggplot(data = uncertainty[which(uncertainty$sex == i & uncertainty$edclass == j),], aes(x = year, y = percent*100, color=datatype, shape=datatype, fill=datatype, size=datatype)) +
      geom_ribbon(aes(ymin=min*100, ymax=max*100),   fill = "white",
                  colour= "grey", linetype = 2, size=0.5) + 
      geom_line(aes(color=datatype, size=datatype)) +
      geom_point(size = 1.4) +
      #scale_size_manual(breaks=c("Microsimulation", "Census","ACS", "PSID"), values=c(1.1, 0.6, 0.6, 0.6)) +
      
      scale_shape_manual(name="Data Type", values = c(16, 17, 18, 15))  + 
      scale_color_manual(name = "Data Type", values = col.vec)  + 
      scale_fill_manual(name = "Data Type", values = c("grey30","white","white","white")) + 
      scale_size_manual(name="Data Type", values=c(1.1, 1.1, 1.1, 1.1)) +
      ylim(0,NA) + 
      theme_light() + 
      theme(strip.background = element_rect(fill = "white"), 
            strip.text = element_text(colour = 'black'), 
            text = element_text(size = 14),
            axis.text = element_text(size = 12), legend.position="bottom", 
            legend.title = element_text(size = 12),
            strip.placement = "outside") +
      labs(x = "Year ", y = "Proportion (%)", color = "Data Type", size = "Data Type", shape = "Data Type") +
      theme(panel.spacing = unit(1.2, "lines"))
    
    ggsave(paste0("SIMAH_workplace/protocol/graphs/AJE-00063-2022 Probst Figure 3",i, j, ".eps"), 
           width = 14, height = 8, units = "cm",  device="eps")
    
  }
}

##### plot for uncertainty BY RACE 
uncertainty <- read.csv("SIMAH_workplace/protocol/output_data/2_uncertainty_estimatesrace.csv") %>% rename(race=microsim.init.race,
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

pe <- read.csv("SIMAH_workplace/protocol/output_data/2_microsim_education_summary.csv") %>%
  mutate(datatype = "Microsimulation") %>% group_by(year, race, edclass) %>% summarise(n=sum(n)) %>% 
  ungroup() %>% group_by(year, race) %>% mutate(PE=n/sum(n)) %>% select(-n)

uncertainty <- left_join(uncertainty, pe)
uncertainty$percent <- ifelse(uncertainty$datatype=="Microsimulation", uncertainty$PE,
                              uncertainty$percent)
uncertainty$PE <- NULL
uncertainty$datatype <- factor(uncertainty$datatype, levels = c("Microsimulation", "Census", "ACS", "PSID"))
uncertainty$edclass <- factor(uncertainty$edclass, levels=c("High school degree or less","Some college",
                                                            "College degree or more"))
uncertainty <- uncertainty %>% filter(year!=2010 & datatype!="Census")
col.vec <- c('#062D59', '#A8B0AA', '#EBE0B0') #Microsim first

ggplot(data = uncertainty, aes(x = year, y = percent*100, color=datatype, shape=datatype, fill=datatype, size=datatype)) +
  geom_ribbon(aes(ymin=min*100, ymax=max*100), alpha=0.2,  fill = "grey",
              colour= "grey", linetype = 2, size=0.5) + 
  geom_line(aes(color=datatype, size=datatype), alpha= .7) +
  facet_grid(cols = vars(race), rows = vars(edclass), scales = "free") +
  geom_point(size = 1.4, alpha= .9) +
  #scale_size_manual(breaks=c("Microsimulation", "Census","ACS", "PSID"), values=c(1.1, 0.6, 0.6, 0.6)) +
  
  scale_shape_manual(name="Data Type", values = c(16, 17, 18, 15))  + 
  scale_color_manual(name = "Data Type", values = col.vec)  + 
  scale_fill_manual(name = "Data Type", values = c("grey30","white","white","white")) + 
  scale_size_manual(name="Data Type", values=c(1.1, 1.1, 1.1, 1.1)) +
  ylim(0,NA) + 
  theme_light() + 
  theme(strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = 'black'), 
        text = element_text(size = 14),
        axis.text = element_text(size = 12), legend.position="bottom", 
        legend.title = element_text(size = 12),
        strip.placement = "outside") +
  labs(x = "Year ", y = "Proportion (%)", color = "Data Type", size = "Data Type", shape = "Data Type") +
  theme(panel.spacing = unit(1.2, "lines"))

ggsave("SIMAH_workplace/protocol/graphs/2_microsim_education_graph_uncertaintyrace_revised.pdf", width = 22, height = 20, units = "cm")



