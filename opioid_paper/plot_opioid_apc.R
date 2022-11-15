setwd("~/Google Drive/SIMAH Sheffield/SIMAH_workplace/opioid_paper/apcdata")

library(foreign)
library(haven)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)

DATE <- "02Nov2022"

data1 <- read_dta("temp2000-20poi-age-aggregate-population.dta") %>% 
  zap_labels() %>% zap_formats() %>% 
  mutate(birth_year = year - age,
         birth_gp = cut(birth_year,
                        breaks=c(0,1930,1935,1940,1945,1950, 1955, 1960, 1965,
                                 1970, 1975, 1980, 1985, 1990, 1995, 2002),
                        labels=c("1930","1931","1936", "1941","1946","1951","1956",
                                 "1961","1966","1971","1976","1981","1986","1991","1996")),
         age_gp = cut(age, breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,79,84,100),
                      labels=c("18","25","30","35","40","45","50","55","60","65",
                               "70","75","80","85"))) %>% 
  pivot_longer(alc_only:alc_opioid) %>% 
  group_by(age_gp,year,birth_gp, race, sex,name) %>% 
  summarise(value = sum(value),
            pop = sum(pop),
            rate = (value/ pop) * 100000,
            birth_gp = as.numeric(as.character(birth_gp)),
            age_gp = as.numeric(as.character(age_gp))) %>% 
  mutate(         name = recode(name, "alc_only"="Alcohol only","opioid_only"="Opioid only",
                                "alc_opioid"="Alcohol and Opioid"),
                  name = factor(name, levels=c("Alcohol only","Alcohol and Opioid","Opioid only")),
                  sex = ifelse(sex==1,"Men","Women"),
                  race = recode(race,
                                "1"="White","2"="Black","3"="Hispanic","4"="Others"),
                  race = factor(race, levels=c("White","Black","Hispanic","Others"))
                  )

overall <- data1 %>% group_by(sex, race, year, name) %>% 
  summarise(value = sum(value),
            pop = sum(pop),
            rate = (value/pop)*100000)

plot1 <- ggplot(overall, aes(x=year, y=rate, colour=as.factor(sex))) + 
  facet_grid(cols=vars(race),
             rows=vars(name), scales="free") + geom_line(size=1) + 
  theme_bw() + ylim(0,NA) +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        strip.background = element_rect(fill="white")) + 
  ylab("Mortality rate per 100,000 population") + xlab("Year") +
  # scale_colour_manual(values=mycolors) + 
  scale_colour_brewer(palette="Set1")

plot1
ggsave(paste0("Fig1_overall_trend", DATE, ".png"), plot1, dpi=300, width=33, height=19, units="cm")


agebycohort <- data1 %>% group_by(age_gp, birth_gp, name, sex) %>% 
  summarise(value = sum(value),
            pop = sum(pop),
            rate = (value/pop)*100000)
nb.cols <- 15

mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)

plot1 <- ggplot(agebycohort, aes(x=age_gp, y=rate, colour=as.factor(birth_gp))) + 
  facet_grid(cols=vars(sex),
             rows=vars(name), scales="free") + geom_line(size=1) + 
  theme_bw() + ylim(0,NA) +
  theme(legend.title=element_blank(),
        legend.position="right",
        strip.background = element_rect(fill="white")) + 
  ylab("Mortality rate per 100,000 population") + xlab("Age") +
  scale_colour_manual(values=mycolors)
plot1
ggsave(paste0("AgebyCohort_Sex", DATE, ".png"), plot1, dpi=300, width=33, height=19, units="cm")

#plot results from APC models - IRRs

IRR <- read_dta("APC-results-by-gender-race.dta") %>% 
  zap_labels() %>% zap_formats() %>% 
  mutate(sex = ifelse(sex==1, "Men","Women"),
         race = recode(race, "0"="all","1"="White","2"="Black","3"="Hispanic","4"="Others"),
         apc = recode(apc, "A"="Age","P"="Period","C"="Cohort")) %>% 
  pivot_longer(cols=c(alc_irr:both_hi), values_to="IRR") %>% 
  mutate(substance = ifelse(grepl("opioid", name), "Opioid only",
                            ifelse(grepl("alc",name), "Alcohol only",
                                   "Alcohol and Opioid")),
         type = ifelse(grepl("irr",name),"IRR",
                       ifelse(grepl("hi", name), "upper_CI",
                              ifelse(grepl("lo", name), "lower_CI", "p value")))) %>%
  dplyr::select(category, sex, race, apc, IRR, substance,type) %>% 
  pivot_wider(names_from=type, values_from=IRR) %>% 
  mutate(category = ifelse(apc=="Period", category,
                           ifelse(apc=="Age",substr(category, 1,2),
                                  ifelse(apc=="Cohort",substr(category, 1,4), NA))),
         category = ifelse(category=="<=19","1930",
                           ifelse(category==">=19","1996",category)),
         category = as.numeric(category),
         substance = factor(substance, levels=c("Alcohol only","Alcohol and Opioid","Opioid only")))
unique(IRR$category)
# plot total effects first 
total <- IRR %>% filter(race=="all")

age <- total %>% filter(apc=="Period") %>% filter(substance=="Alcohol and Opioid") %>% filter(sex=="Women")

summary(IRR$category)

plot <- ggplot(total, aes(x=category, y=IRR, colour=sex, fill=sex)) + 
  facet_grid(cols=vars(apc),
             rows=vars(substance), scales="free") + geom_line(size=1) + 
  theme_bw() + ylim(0,4.1) +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        strip.background = element_rect(fill="white")) + 
  ylab("IRR") + xlab("") + 
  geom_ribbon(aes(ymin=lower_CI, ymax=upper_CI, fill=sex), colour=NA, alpha=0.3) + 
  scale_colour_brewer(palette="Set1") + 
  scale_fill_brewer(palette="Set1") + 
  geom_hline(yintercept=1, linetype="dashed")
plot

ggsave(paste0("Fig2_APC_sex", DATE, ".png"), plot, dpi=300, width=33, height=19, units="cm")
       
race <- IRR %>% filter(race!="all") %>% 
  mutate(upper_CI = ifelse(upper_CI>30, NA, upper_CI),
         race = factor(race, levels=c("White","Black","Hispanic")))
summary <- race %>% filter(substance=="Alcohol only") %>% filter(apc=="Period") %>% filter(sex=="Women")

alcoholonly <- ggplot(subset(race,substance=="Alcohol only"), aes(x=category, y=IRR, colour=race, fill=race)) + 
  facet_grid(cols=vars(apc),
             rows=vars(sex), scales="free") + geom_line(size=1) + 
age <- ggplot(subset(race,apc=="Age"), aes(x=category, y=IRR, colour=sex, fill=sex)) + 
  facet_grid(cols=vars(race),
             rows=vars(substance), scales="free") + geom_line(size=1) + 
  theme_bw() + ylim(0,NA) +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        strip.background = element_rect(fill="white")) + 
  ylab("IRR") + xlab("") + 
  geom_ribbon(aes(ymin=lower_CI, ymax=upper_CI, fill=sex), colour=NA, alpha=0.3) + 
  scale_colour_brewer(palette="Set1") + 
  scale_fill_brewer(palette="Set1") + 
  geom_hline(yintercept=1, linetype="dashed") + ggtitle("Alcohol-only")
alcoholonly

ggsave(paste0("Fig3_APC_alcoholonly", DATE, ".png"), alcoholonly, dpi=300, width=33, height=19, units="cm")

<<<<<<< Updated upstream
summary <- race %>% filter(substance=="Alcohol and Opioid") %>% filter(apc=="Period") %>% filter(sex=="Women") %>% 
  mutate(IRR = round(IRR, digits=2),
         lower_CI = round(lower_CI, digits=2),
         upper_CI = round(upper_CI, digits=2))

opioidonly <- ggplot(subset(race,substance=="Opioid only"), aes(x=category, y=IRR, colour=race, fill=race)) + 
  facet_grid(cols=vars(apc),
             rows=vars(sex), scales="free") + geom_line(size=1) + 
=======
period <- ggplot(subset(race,apc=="Period"), aes(x=category, y=IRR, colour=sex, fill=sex)) + 
  facet_grid(cols=vars(race),
             rows=vars(substance), scales="free") + geom_line(size=1) + 
>>>>>>> Stashed changes
  theme_bw() + ylim(0,NA) +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        strip.background = element_rect(fill="white")) + 
  ylab("IRR") + xlab("") + 
  geom_ribbon(aes(ymin=lower_CI, ymax=upper_CI, fill=sex), colour=NA, alpha=0.3) + 
  scale_colour_brewer(palette="Set1") + 
  scale_fill_brewer(palette="Set1") + 
  geom_hline(yintercept=1, linetype="dashed") + ggtitle("Opioid-only")
opioidonly

ggsave(paste0("Fig4_APC_opioidonly", DATE, ".png"), opioidonly, dpi=300, width=33, height=19, units="cm")

<<<<<<< Updated upstream

alcandop <- ggplot(subset(race,substance=="Alcohol and Opioid"), aes(x=category, y=IRR, colour=race, fill=race)) + 
  facet_grid(cols=vars(apc),
             rows=vars(sex), scales="free") + geom_line(size=1) + 
=======
cohort <- ggplot(subset(race,apc=="Cohort"), aes(x=category, y=IRR, colour=sex, fill=sex)) + 
  facet_grid(cols=vars(race),
             rows=vars(substance), scales="free") + geom_line(size=1) + 
>>>>>>> Stashed changes
  theme_bw() + ylim(0,NA) +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        strip.background = element_rect(fill="white")) + 
  ylab("IRR") + xlab("") + 
  geom_ribbon(aes(ymin=lower_CI, ymax=upper_CI, fill=sex), colour=NA, alpha=0.3) + 
  scale_colour_brewer(palette="Set1") + 
  scale_fill_brewer(palette="Set1") + 
  geom_hline(yintercept=1, linetype="dashed") + ggtitle("Alcohol and Opioid")
alcandop

ggsave(paste0("Fig4_APC_alcandop", DATE, ".png"), alcandop, dpi=300, width=33, height=19, units="cm")

#plots for APC effects in raw data - currently not used

# data1 <- read_dta("temp2000-20poi-age-aggregate-population.dta") %>% 
#   zap_labels() %>% zap_formats() %>% 
#   mutate(birth_year = year - age,
#          birth_gp = cut(birth_year,
#                         breaks=c(0,1930,1935,1940,1945,1950, 1955, 1960, 1965,
#                                  1970, 1975, 1980, 1985, 1990, 1995, 2002),
#                         labels=c("1930","1931","1936", "1941","1946","1951","1956",
#                                  "1961","1966","1971","1976","1981","1986","1991","1996")),
#          age_gp = cut(age, breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,79,84,100),
#                       labels=c("18","25","30","35","40","45","50","55","60","65",
#                                "70","75","80","85"))) %>% 
#   pivot_longer(alc_only:alc_opioid) %>% 
#   group_by(age_gp,year,birth_gp, sex,name) %>% 
#   summarise(value = sum(value),
#             pop = sum(pop),
#             rate = (value/ pop) * 100000,
#             birth_gp = as.numeric(as.character(birth_gp)),
#             age_gp = as.numeric(as.character(age_gp))) %>% 
#   pivot_longer(cols=c(age_gp:birth_gp), names_to="type",values_to="number") %>% 
#   group_by(sex, name, type, number) %>% 
#   summarise(value=sum(value),
#             pop=sum(pop),
#             rate = (value/pop)*100000) %>% 
#   mutate(
#     # race = recode(race,
#     #                    "1"="White","2"="Black","3"="Hispanic","4"="Others"),
#     #      race = factor(race, levels=c("White","Black","Hispanic","Others")),
#     # 
#     # edclass = recode(edclass, "1"="High school degree or less",
#     #                       "2"="Some college",
#     #                       "3"="College degree +"),
#     #      edclass = factor(edclass, levels=c("High school degree or less","Some college",
#     #                                         "College degree +")),
#     type = recode(type, "age_gp"="Age","birth_gp"="Cohort","year"="Period"),
#     type = factor(type, levels=c("Age","Period","Cohort")),
#     name = recode(name, "alc_only"="Alcohol only","opioid_only"="Opioid only",
#                   "alc_opioid"="Alcohol and Opioid"),
#     name = factor(name, levels=c("Alcohol only","Alcohol and Opioid","Opioid only")))
# 
# wide <- data1 %>% dplyr::select(-c(value,pop)) %>% pivot_wider(names_from=type, values_from=number)
# 
# men <- data1 %>% filter(sex==1)
# 
# plot1 <- ggplot(men, aes(x=number, y=rate, colour=race)) + 
#   facet_grid(cols=vars(type),
#              rows=vars(name), scales="free") + geom_line(size=1) + 
#   theme_bw() + ylim(0,NA) +
#   theme(legend.title=element_blank(),
#         legend.position="none",
#         strip.background = element_rect(fill="white")) + 
#   ylab("Mortality rate per 100,000 population") + xlab("") + ggtitle("Men") +
#   scale_colour_brewer(palette="Set1")
# plot1
# women <- data1 %>% filter(sex==2)
# 
# plot2 <- ggplot(women, aes(x=number, y=rate, colour=edclass)) + 
#   facet_grid(cols=vars(type),
#              rows=vars(name), scales="free") + geom_line(size=1) + 
#   theme_bw() + ylim(0,NA) +
#   theme(legend.title=element_blank(),
#         legend.position="bottom",
#         strip.background = element_rect(fill="white")) + 
#   ylab("Mortality rate per 100,000 population") + xlab("") + ggtitle("Women") + 
#   scale_colour_brewer(palette="Set1")
# 
# library(gridExtra)
# menwomen <- grid.arrange(plot1, plot2)
# ggsave("Fig2_APC_plot_raw_education.png", menwomen, dpi=300, width=33, height=19, units="cm")
# 
# data1 <- data1 %>% ungroup() %>% group_by(sex, name, type, number) %>% 
#   summarise(value = sum(value),
#             pop = sum(pop),
#             rate = (value/pop)*100000) %>% 
#   mutate(sex = recode(sex, "1"="Men","2"="Women"))
# 
# plot <- ggplot(data1, aes(x=number, y=rate, colour=sex)) + 
#   facet_grid(cols=vars(type),
#              rows=vars(name), scales="free") + geom_line(size=1) + 
#   theme_bw() + ylim(0,NA) +
#   theme(legend.title=element_blank(),
#         legend.position="bottom",
#         strip.background = element_rect(fill="white")) + 
#   ylab("Mortality rate per 100,000 population") + xlab("") + 
#   scale_colour_brewer(palette="Set1")
# 
# plot
# ggsave("Fig1_APC_plot_raw_sex.png", plot, dpi=300, width=33, height=19, units="cm")
