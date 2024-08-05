SC <- data %>% 
  filter(State=="South Carolina") %>% 
  mutate(surveymonth = recode(surveymonth,
                              "January"="01","February"="02","March"="03","April"="04",
                              "May"="05","June"="06","July"="07","August"="08","September"="09",
                              "October"="10","November"="11","December"="12"),
         yearmonth = paste0(YEAR, "-", surveymonth)) %>% 
           filter(drinkingstatus==1) %>% 
           group_by(YEAR, sex_recode) %>% 
           summarise(meanfrequency = mean(alc_frequency, na.rm=T),
                     meanquantity = mean(quantity_per_occasion, na.rm=T),
                     meangpd = mean(gramsperday, na.rm=T)) %>% drop_na()

test <- data %>% filter(State=="South Carolina") %>% filter(surveymonth=="April" | surveymonth=="May") %>% 
  filter(YEAR==2019) %>% filter(sex_recode=="Men")


ggplot(data=SC, aes(x=YEAR, y=meanquantity, colour=sex_recode)) + geom_point()
         
       