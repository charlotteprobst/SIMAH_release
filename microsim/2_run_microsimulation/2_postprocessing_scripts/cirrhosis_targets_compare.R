# for MORBIDITY 
cirrhosismorbidity <- read.csv("CAMH_Targets/LivCir_R_Output.csv")
cirrhosismorbidity <- cirrhosismorbidity %>% gather(agegroup, count, age15to19:age80plus) %>% 
  filter(location==SelectedState) %>% 
  select(year,sex,agegroup,count) %>% 
  mutate(agegroup = gsub("to", "-", agegroup),
         agegroup=gsub("age", "", agegroup),
         sex = recode(sex, "male"="m","female"="f"),
         count = ifelse(agegroup=="15-19", count/5*2, count),
         count = ifelse(agegroup=="80plus", count/5, count),
         count = count*proportion) %>% group_by(year) %>% 
  summarise(count = sum(count)) %>% mutate(datatype="target")


for(i in names(Cirrhosis)){
  Cirrhosis[[paste(i)]]$year <- i
  Cirrhosis[[paste(i)]] <- Cirrhosis[[paste(i)]] %>% select(microsim.init.age, microsim.init.sex,
                                                            microsim.init.alc.gpd, year)
}

simulationdata <- do.call(rbind,Cirrhosis)
simulationdata <- simulationdata %>% group_by(year) %>% tally(name="count") %>% 
  mutate(datatype="microsim")

cirrhosismorbidity <- rbind(cirrhosismorbidity, simulationdata) %>% mutate(year=as.integer(year))

library(ggplot2)
plot1 <- ggplot(data=cirrhosismorbidity, aes(x=year, y=count, colour=datatype)) + geom_line() + geom_point() + 
  ylim(0,NA) + theme_bw()

ggsave("cirrhosismorbidity.png", plot1, dpi=300, width=30, height=30, units="cm")



