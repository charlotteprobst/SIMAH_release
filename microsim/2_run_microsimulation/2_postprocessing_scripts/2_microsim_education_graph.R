library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)

k.wd <- c("C:/Users/Marie/Dropbox/NIH2020/Protocol_paper")
setwd(k.wd)

# read in the summarised data - comparison of microsimulation and death rates data
df <- read.csv("3_output_data/microsim_education_summary.csv") %>% 
  mutate(edclass = factor(microsim.init.education, levels=c("LEHS","SomeC","College")),
         race = recode(microsim.init.race, "BLA"="Black","WHI"="White","SPA"="Hispanic","OTH"="Other"))

uncertainty <- read.csv("output_data/uncertainty_estimates.csv")

# first without race breakdown 
summary <- df %>% group_by(microsim.init.sex, edclass, datatype, year) %>% 
  summarise(n=sum(n)) %>% ungroup() %>% 
  group_by(microsim.init.sex, datatype, year) %>% 
  mutate(percent=n/sum(n))

# calculate percentage by education for each data type in each year - split by race 
summary <- df %>% group_by(sex, edclass, race, datatype, year) %>% 
  summarise(n=sum(n)) %>% ungroup() %>%
  group_by(sex, race, datatype, year) %>% 
  mutate(percent=n/sum(n))

difference <- summary %>% select(sex, race, edclass, datatype, year, percent)
difference <- difference %>% pivot_wider(names_from=datatype, values_from=percent) %>% 
  mutate(censusdiff = (microsim-Census)^2,
         ACSdiff = (microsim-ACS)^2,
         PSIDdiff = (microsim-PSID)^2)

cor.test(difference$microsim, difference$Census)
cor.test(difference$microsim, difference$ACS)
cor.test(difference$microsim, difference$PSID)

## Graph for SES
summary <- df %>% group_by(sex, edclass, datatype, year) %>% summarise(n=sum(n)) %>% ungroup() %>% 
  group_by(sex, datatype, year) %>% mutate(percent=n/sum(n))

summary <- summary[which(summary$year < 2019), ]
summary$percent <- summary$percent * 100 

levels(summary$datatype) <- list( "Microsimulation" = "microsim", 
                                  "Census" = "Census",
                                  "ACS"="ACS", 
                                  "PSID"="PSID")
levels(summary$sex) <- list("Males" = "Male", "Females" = "Female")
levels(summary$edclass) <- list("High school degree or less" = "LEHS", "Some college" = "SomeC", "College degree or more" = "College")

col.vec <- c('#d72c40', '#132268','#447a9e','#93aebf')
ggplot(data = summary, aes(x = year, y = percent, colour = datatype)) +
  geom_line(aes(color = datatype, size = datatype), alpha= .55) +
  scale_size_manual(breaks=c("Microsimulation", "Census","ACS", "PSID"), values=c(1.1, 0.7, 0.7, 0.7)) +
  geom_point(aes(shape = datatype), size = 1.35, alpha= .7) +
  scale_shape_manual(values = c(16, 18, 18, 18)) +
  facet_grid(cols = vars(sex), rows = vars(edclass), scales = "free") +
  ylim(0,60) + 
  theme_light() + 
  theme(strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = 'black'), 
        text = element_text(size = 14),
        axis.text = element_text(size = 12), legend.position="bottom", 
        legend.title = element_text(size = 12)) +
  scale_color_manual(values = col.vec)  + 
  labs(x = "Year ", y = "Proportion (%)", color = "Data Type", size = "Data Type", shape = "Data Type") +
  theme(panel.spacing = unit(1.2, "lines"))

ggsave("2_microsim_education_graph_sex.jpeg", dpi = 600, width = 16, height = 20, units = "cm")

## Graph for SES by race
summary <- df %>% filter(!is.na(race)) %>% group_by(edclass, race, datatype, year) %>% 
  summarise(n=sum(n)) %>% ungroup() %>% 
  group_by(datatype, year, race) %>% mutate(percent=(n/sum(n))*100) %>% ungroup() %>%  
  mutate(datatype = recode(datatype, "microsim"="Microsimulation",
                           "Census"="Census", 
                           "PSID"="PSID",
                           "ACS"="ACS"),
         edclass = recode(edclass, "LEHS"="High school degree or less",
                          "SomeC"="Some college",
                          "College"="College degree or more")) %>% filter(year<2019)

summary$datatype <- factor(summary$datatype, levels=c("Microsimulation","Census","ACS","PSID"))
levels(summary$race) <- list( "Non-Hispanic White" = "White", 
                                  "Non-Hispanic Black" = "Black",
                                  "Hispanic"="Hispanic", 
                                  "Other"="Other")

ggplot(data = summary, aes(x = year, y = percent, colour = datatype)) +
  geom_line(aes(color = datatype, size = datatype), alpha= .55) +
  scale_size_manual(breaks=c("Microsimulation", "Census","ACS", "PSID"), values=c(1.1, 0.6, 0.6, 0.6)) +
  geom_point(aes(shape = datatype), size = 1.3, alpha= .7) +
  scale_shape_manual(values = c(16, 18, 18, 18)) +
  facet_grid(cols = vars(race), rows = vars(edclass), scales = "free") +
  ylim(0,NA) + 
  theme_light() + 
  theme(strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = 'black'), 
        text = element_text(size = 14),
        axis.text = element_text(size = 12), legend.position="bottom", 
        legend.title = element_text(size = 12)) +
  scale_color_manual(values = col.vec)  + 
  labs(x = "Year ", y = "Proportion (%)", color = "Data Type", size = "Data Type", shape = "Data Type") +
  theme(panel.spacing = unit(1.2, "lines"))
ggsave("2_microsim_education_graph_race.jpeg", dpi = 600, width = 22, height = 20, units = "cm")


# plot for uncertainty
uncertainty <- read.csv("output_data/uncertainty_estimates.csv") %>% rename(sex = microsim.init.sex,
                                                                            edclass = microsim.init.education)
col.vec <- c('#d72c40', '#132268','#447a9e','#93aebf')


ggplot(data = uncertainty, aes(x = year, y = percent, color=datatype, shape=datatype, fill=datatype, size=datatype)) +
  geom_ribbon(aes(ymin=min, ymax=max), alpha=0.7, colour=NA) + 
  geom_line(aes(color=datatype, size=datatype), alpha= .55) +
  geom_point(size = 1.3, alpha= .7) +
  scale_shape_manual(name="Data Type", values = c(16, 18, 18, 18))  + 
  scale_color_manual(name = "Data Type", values = col.vec)  + 
  scale_fill_manual(name = "Data Type", values = c("white","white","grey40","white")) + 
  scale_size_manual(breaks=c("Microsimulation", "Census","ACS", "PSID"), name="Data Type", values=c(1.1, 0.6, 0.6, 0.6)) +
  facet_grid(cols = vars(sex), rows = vars(edclass), scales = "free") +
  ylim(0,NA) + 
  theme_light() + 
  theme(strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = 'black'), 
        text = element_text(size = 14),
        axis.text = element_text(size = 12), legend.position="bottom", 
        legend.title = element_text(size = 12)) +
  labs(x = "Year ", y = "Proportion (%)", color = "Data Type", size = "Data Type", shape = "Data Type") +
  theme(panel.spacing = unit(1.2, "lines"))

ggsave("2_microsim_education_graph_uncertainty.jpeg", dpi = 600, width = 22, height = 20, units = "cm")





# calculate correlations by Sex and SES 
difference <- summary %>% select(sex, edclass, datatype, year, percent) %>% pivot_wider(names_from=datatype, values_from=percent)
cor.test(difference$microsim, difference$Census)
cor.test(difference$microsim, difference$ACS)
cor.test(difference$microsim, difference$PSID)
cor.test(subset(difference, sex=="Male")$microsim, subset(difference, sex=="Male")$Census)
cor.test(subset(difference, sex=="Female")$microsim, subset(difference, sex=="Female")$Census)
cor.test(subset(difference, sex=="Male")$microsim, subset(difference, sex=="Male")$ACS)
cor.test(subset(difference, sex=="Male")$microsim, subset(difference, sex=="Male")$PSID)
cor.test(subset(difference, sex=="Female")$microsim, subset(difference, sex=="Female")$Census)
cor.test(subset(difference, sex=="Female")$microsim, subset(difference, sex=="Female")$ACS)
cor.test(subset(difference, sex=="Female")$microsim, subset(difference, sex=="Female")$PSID)
# calculate correlations by Race and SES 
difference <- summary %>% select(race, edclass, datatype, year, percent) %>% pivot_wider(names_from=datatype, values_from=percent)
cor.test(difference$Microsimulation, difference$Census)
cor.test(difference$Microsimulation, difference$ACS)
cor.test(difference$Microsimulation, difference$PSID)
cor.test(subset(difference, race=="Black")$Microsimulation, subset(difference, race=="Black")$Census)
cor.test(subset(difference, race=="White")$Microsimulation, subset(difference, race=="White")$Census)
cor.test(subset(difference, race=="Other")$Microsimulation, subset(difference, race=="Other")$Census)
cor.test(subset(difference, race=="Hispanic")$Microsimulation, subset(difference, race=="Hispanic")$Census)
cor.test(subset(difference, race=="Black")$Microsimulation, subset(difference, race=="Black")$ACS)
cor.test(subset(difference, race=="White")$Microsimulation, subset(difference, race=="White")$ACS)
cor.test(subset(difference, race=="Others")$Microsimulation, subset(difference, race=="Other")$ACS)
cor.test(subset(difference, race=="Hispanic")$Microsimulation, subset(difference, race=="Hispanic")$ACS)
cor.test(subset(difference, race=="Black")$Microsimulation, subset(difference, race=="Black")$PSID)
cor.test(subset(difference, race=="White")$Microsimulation, subset(difference, race=="White")$PSID)
cor.test(subset(difference, race=="Other")$Microsimulation, subset(difference, race=="Other")$PSID)
cor.test(subset(difference, race=="Hispanic")$Microsimulation, subset(difference, race=="Hispanic")$PSID)




