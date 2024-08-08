library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)

# read in the summarised data - comparison of microsimulation and death rates data
df <- read.csv("output_data/microsim_education_summary.csv") %>% 
  mutate(edclass = factor(edclass, levels=c("LEHS","SomeC","College")),
         race = recode(race, "BLA"="Black","WHI"="White","SPA"="Hispanic","OTH"="Other"))

# first without race breakdown 
summary <- df %>% group_by(sex, edclass, datatype, year) %>% 
  summarise(n=sum(n)) %>% ungroup() %>% 
  group_by(sex, datatype, year) %>% 
  mutate(percent=n/sum(n)) %>% filter(year<=2017)

ggplot(data=summary, aes(x=year, y=percent, colour=datatype)) + geom_line() +
  geom_point() + facet_wrap(~sex+edclass) + ylim(0,1) + theme_bw() + theme(legend.position="bottom",
                                                                           legend.title=element_blank())
ggsave("plot_educationAGESQ.png", dpi=300, width=33, height=19, units="cm")


# calculate percentage by education for each data type in each year - split by race 
summary <- df %>% group_by(sex, edclass, race, datatype, year) %>% 
  summarise(n=sum(n)) %>% ungroup() %>%
  group_by(sex, race, datatype, year) %>% 
  mutate(percent=n/sum(n))

# census 2010 doesn't include a breakdown of education by race - replicate Census 2010 for all race categories
# census2010 <- summary %>% filter(year==2010 & datatype=="Census")
# census2010 <- coredata(census2010)[rep(seq(nrow(census2010)),4),]
# # census2010$race <- rep(c("Black","White","Hispanic","Other"), each=6)
# summary <- summary %>% drop_na(race)
# summary <- rbind(summary,census2010)

summary <- summary %>% drop_na(race)

ggplot(data=summary, aes(x=year, y=percent, colour=datatype)) + geom_line() +
  geom_point() + facet_wrap(~sex+edclass+race) + ylim(0,1) + theme_bw()
ggsave("plot_educationbyrace.png", dpi=300, width=33, height=19, units="cm")
