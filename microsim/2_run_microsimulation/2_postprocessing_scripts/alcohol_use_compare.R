# comparing the alcohol use in each year to the BRFSS data 

PopPerYear <- readRDS("output_data/PopPerYear.RDS")

for(i in names(PopPerYear)){
  PopPerYear[[paste(i)]]$Year <- i
}

Pop <- do.call(rbind,PopPerYear)

Pop$microsim.init.drinkingstatus <- ifelse(Pop$microsim.init.alc.gpd ==0 , 0,1)

AlcUse <- Pop %>% group_by(Year, microsim.init.sex) %>% filter(microsim.init.drinkingstatus==1) %>% 
  summarise(meangpd = mean(microsim.init.alc.gpd)) %>% mutate(datatype="microsim",
                                                              Year=as.integer(Year))

BRFSS <- read_csv("Processed_Shifted_BRFSS_reweight.csv")

summaryBRFSS <- BRFSS %>% group_by(YEAR,SEX) %>% mutate(SEX=recode(SEX, "Male"="m","Female"="f")) %>% 
  filter(DRINKINGSTATUS_NEW==1) %>% summarise(meangpd = mean(alcgpd_new)) %>% mutate(datatype="BRFSS") %>% 
  rename(microsim.init.sex=SEX,
         Year=YEAR)

names(AlcUse)
names(summaryBRFSS)

AlcUse <- rbind(AlcUse, summaryBRFSS)

ggplot(data=AlcUse, aes(x=Year, y=meangpd, colour=datatype)) + geom_line() + geom_point() + 
  facet_wrap(~microsim.init.sex)
ggsave("alcohol_imputation.png", dpi=300, height=30, width=20, units="cm")
