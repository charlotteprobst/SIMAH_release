# SIMAH - protocol paper. June 2021
# Figure 3
# This code reads in data on the microsimulation and respective uncertainty estimates
# to plot educational groups over time for the microsimulation and three data sources

library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)

k.wd <- c("C:/Users/Marie/Dropbox/NIH2020/")
#k.wd <- c("~/Google Drive/SIMAH Sheffield")
setwd(k.wd)

######## plot for uncertainty BY SEX 
col.vec <- c('#062D59', '#576F81','#A8B0AA', '#EBE0B0') #Microsim first
col.vec <- c("black", "#2B2B2B", "#808080", "#D3D3D3")

uncertainty <- read.csv("SIMAH_workplace/protocol/output_data/2_uncertainty_estimatessex.csv") %>% rename(sex = microsim.init.sex,
                                                                            edclass = microsim.init.education) %>% 
  mutate(sex = recode(sex, "Male"="Men","Female"="Women"),
         sex = factor(sex, levels=c("Men","Women")),
         edclass = recode(edclass, "High school diploma or less" = "High School Degree or Less",
                          "Some college"="Some College",
                          "College degree or more" = "College Degree or More"),
         edclass = factor(edclass, levels=c("High School Degree or Less","Some College",
                                  "College Degree or More")))

pe <- read.csv("SIMAH_workplace/protocol/output_data/2_microsim_education_summary.csv") %>%
  mutate(datatype = "Microsimulation", 
         edclass = recode(edclass, "High school degree or less" = "High School Degree or Less",
                                                        "Some college"="Some College",
                                                        "College degree or more" = "College Degree or More"),
         edclass = factor(edclass, levels=c("High School Degree or Less","Some College",
                                            "College Degree or More"))) %>%
  group_by(year, sex, edclass) %>% summarise(n=sum(n)) %>% 
  ungroup() %>% group_by(year, sex) %>% mutate(PE=n/sum(n)) %>% select(-n)


uncertainty <- left_join(uncertainty, pe)
uncertainty$percent <- ifelse(uncertainty$datatype=="Microsimulation", uncertainty$PE,
                              uncertainty$percent)

uncertainty$PE <- NULL
uncertainty$datatype <- factor(uncertainty$datatype, levels = c("Microsimulation", "Census", "ACS", "PSID"))
#uncertainty$edclass <- factor(uncertainty$edclass, levels = c("High school degree or less", "Some college", "College degree or more"))
# warning because we do not have a ribbon around the observed data
ggplot(data = uncertainty, aes(x = year, y = percent*100, shape=datatype)) +
  geom_ribbon(aes(ymin=min*100, ymax=max*100), alpha=0.2,  fill = "grey",
              colour= "grey", linetype = 2, size=0.352) + 
  geom_line(aes(), alpha= .7,size=0.352, colour = 'black') +
  facet_wrap(c("sex", "edclass"), scales = "free") +
  geom_point(size = 1.4, alpha= .9) +
  
  scale_shape_manual(values = c(16, 17, 18, 15))  + 
  scale_fill_manual(values = c("grey30","white","white","white")) + 
  ylim(0,60) + 
  xlim(2000,2020) + 
  theme_light() + 
  theme(strip.background = element_rect(fill = "white"), 
        panel.grid = element_blank(),
        panel.border = element_blank(), 
        axis.ticks = element_line(colour="black", size = 0.352), 
        axis.line = element_line(colour="black", size = 0.352),
        strip.text = element_text(size = 8, colour = 'black'), 
        axis.text = element_text(size = 8, colour="black"), 
        text = element_text(size = 8, colour="black", family="sans"), 
        legend.background = element_rect(size=0.352, 
                                         linetype = "solid", 
                                         color = "black", 
                                         fill=NA),
        legend.position= "right",
        legend.justification = "top",
        legend.title.align = 0.5,
        legend.title = element_text(size = 8, colour="black", family="sans"),
        legend.text = element_text(size = 8, colour="black", family="sans"),
        strip.placement = "outside") +
  labs(x = "Year ", y = "Proportion, %", shape=expression(underline("Data Type"))) +
  theme(panel.spacing = unit(1.2, "lines"))


ggsave("SIMAH_workplace/protocol/graphs/AJE-00063-2022 Probst Figure 3.pdf", 
       width = 7, height = 5, units = "in")

k <- 0

for (i in levels(as.factor(uncertainty$sex))) {
  for (j in levels(uncertainty$edclass)) {
    k <- k + 1
    ggplot(data = uncertainty[which(uncertainty$sex == i & uncertainty$edclass == j),], 
           aes(x = year, y = percent*100, shape=datatype)) +
      geom_ribbon(aes(ymin=min*100, ymax=max*100), fill = "grey90",
                  colour= "grey", linetype = 2, size=0.352) + 
      geom_line(aes(), size=0.352, colour = 'black') +
      geom_point(size = 1.4) +
      
      scale_shape_manual(values = c(16, 17, 18, 15))  + 
      scale_fill_manual(values = c("grey30","white","white","white")) + 
      ylim(0,60) + 
      xlim(2000,2020) + 
      theme_light() + 
      theme(strip.background = element_rect(fill = "white"), 
            panel.grid = element_blank(),
            panel.border = element_blank(), 
            axis.ticks = element_line(colour="black", size = 0.352), 
            axis.line = element_line(colour="black", size = 0.352),
            strip.text = element_text(size = 8, colour = 'black'), 
            axis.text = element_text(size = 8, colour="black"), 
            text = element_text(size = 8, colour="black"), 
            strip.placement = "outside", 
            legend.position="none") +
      labs(x = "Year ", y = "Proportion, %") +
      theme(panel.spacing = unit(1.2, "lines")) 
      #labs(tag = LETTERS[k])
      
      
    ggsave(paste0("SIMAH_workplace/protocol/graphs/AJE-00063-2022 Probst Figure 3", LETTERS[k], ".eps"), 
           width = 2, height = 2.2, units = "in",  
           device = "eps")
    
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

  scale_shape_manual(name="Data Type", values = c(16, 17, 18, 15))  + 
  scale_color_manual(name = "Data Type", values = col.vec)  + 
  scale_fill_manual(name = "Data Type", values = c("grey30","white","white","white")) + 
  scale_size_manual(name="Data Type", values=c(0.352, 0.352, 0.352, 0.352)) +
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



