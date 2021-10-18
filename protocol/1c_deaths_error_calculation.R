library(dplyr)
library(tidyr)
library(ggplot2)

k.wd <- c("C:/Users/Marie/Dropbox/NIH2020/")
k.wd <- c("~/Google Drive/SIMAH Sheffield/")
setwd(k.wd)

weights <- read.csv("SIMAH_workplace/protocol/output_data/1_population_weights_2018.csv")
df <- read.csv("SIMAH_workplace/protocol/output_data/1_microsim_deaths_summary.csv")

sum <- df %>% ungroup() %>% group_by(year, datatype, cause, agecat, sex, edclass, .drop=FALSE) %>% 
  summarise(deaths=sum(totaldeaths), population=sum(n))
# join 2018 age percentages with data for all years - to only use age splits for 2018
sum <- left_join(sum, weights)
# calculate crude death rate for each row (year, sex, edclass, agecat, cause) per 100,000
sum <- sum %>% mutate(rate = (deaths/population)*100000,
                      weighted_rate = rate*percent) %>% 
  group_by(year, sex, edclass, cause, datatype) %>% 
  summarise(rate=sum(weighted_rate))

# for in text reporting - RMSE for each cause of death
table <- sum %>% mutate(rate = round(rate, digits=1)) %>% 
                          pivot_wider(names_from=datatype, values_from=rate) %>% 
  mutate(abserror = abs(microsim-target)) %>% 
  dplyr::select(year, sex, edclass, cause, abserror) %>% 
  pivot_wider(names_from=edclass, values_from=abserror)

# for in text reporting - inequalities between educational groups 
table<- sum %>% pivot_wider(names_from=edclass, values_from=rate) %>% 
  filter(datatype=="target") %>% filter(cause!="REST") %>% 
  mutate(absdiff = round(abs(LEHS-College),digits=1),
         ratio = round(LEHS/College,digits=1))

table %>% filter(year==2015) %>% filter(cause=="DM")

table %>% filter(year==2018) %>% filter(cause=="DM")

# recalculate error - just split by education 
sum <- sum %>% pivot_wider(names_from=datatype, values_from=rate) %>% 
  mutate(error=microsim-target,
         errorsq = error^2) %>% 
  group_by(edclass, cause) %>% summarise(rmse = sqrt(mean(errorsq))) %>%
  pivot_wider(names_from=edclass, values_from=rmse) %>% 
  select(cause, LEHS, SomeC, College) %>% 
  mutate(cause= factor(cause, levels=c("AUD","LVDC","IJ","MVACC","UIJ","IHD","HYPHD","ISTR","DM","REST"))) %>% 
  ungroup() %>% group_by(cause) %>% arrange(desc(LEHS), .by_group=T) %>% filter(cause!="REST")

write.csv(sum, "SIMAH_workplace/protocol/output_data/1_SuppTable3.csv", row.names=FALSE)

# recalculate error - just split by race/ethnicity 
sum <- df %>% ungroup() %>% group_by(year, datatype, cause, agecat, sex, raceeth, .drop=FALSE) %>% 
  summarise(deaths=sum(totaldeaths), population=sum(n))
# join 2018 age percentages with data for all years - to only use age splits for 2018
sum <- left_join(sum, weights)
# calculate crude death rate for each row (year, sex, edclass, agecat, cause) per 100,000
sum <- sum %>% mutate(rate = (deaths/population)*100000,
                      weighted_rate = rate*percent) %>% 
  group_by(year, sex, raceeth, cause, datatype) %>% 
  summarise(rate=sum(weighted_rate))

sum <- sum %>% pivot_wider(names_from=datatype, values_from=rate) %>% 
  mutate(error=microsim-target,
         errorsq = error^2) %>% 
  group_by(raceeth, cause) %>% summarise(rmse = sqrt(mean(errorsq))) %>% 
  pivot_wider(names_from=raceeth, values_from=rmse) %>% 
  select(cause, WHI, BLA,SPA,OTH) %>% 
  mutate(cause= factor(cause, levels=c("AUD","LVDC","IJ","MVACC","UIJ","IHD","HYPHD","ISTR","DM","REST"))) %>% 
  ungroup() %>% group_by(cause) %>% arrange(desc(BLA), .by_group=T) %>% filter(cause!="REST")
write.csv(sum, "SIMAH_workplace/protocol/output_data/1_SuppTable4.csv", row.names=FALSE)

# recalculate error - split by both education and race/ethnicity
sum <- df %>% ungroup() %>% group_by(year, datatype, cause, agecat, sex, edclass, raceeth, .drop=FALSE) %>% 
  summarise(deaths=sum(totaldeaths), population=sum(n))
# join 2018 age percentages with data for all years - to only use age splits for 2018
sum <- left_join(sum, weights)
# calculate crude death rate for each row (year, sex, edclass, agecat, cause) per 100,000
sum <- sum %>% mutate(rate = (deaths/population)*100000,
                      weighted_rate = rate*percent) %>% 
  group_by(year, sex, edclass, raceeth, cause, datatype) %>% 
  summarise(rate=sum(weighted_rate))

sum <- sum %>% 
  pivot_wider(names_from=datatype, values_from=rate) %>% mutate(microsim=ifelse(is.na(microsim),0,microsim)) %>%
  mutate(error=microsim-target,
         errorsq = error^2,
         edclass = factor(edclass, levels=c("LEHS","SomeC","College"))) %>% 
  group_by(raceeth, edclass, cause) %>% summarise(rmse = sqrt(mean(errorsq))) %>% 
  pivot_wider(names_from=c(raceeth), values_from=rmse) %>% 
  select(edclass, cause, WHI, BLA, SPA, OTH) %>% 
  mutate(cause= factor(cause, levels=c("AUD","LVDC","IJ","MVACC","UIJ","IHD","HYPHD","ISTR","DM","REST"))) %>% 
  ungroup() %>% group_by(edclass) %>% arrange(cause, .by_group=T) %>% filter(cause!="REST")
sum %>% group_by(edclass) %>% summarise(meanWHI=mean(WHI), meanBLA=mean(BLA),meanSPA=mean(SPA),meanOTH=mean(OTH))
write.csv(sum, "SIMAH_workplace/protocol/output_data/1_SuppTable5.csv", row.names=FALSE)


