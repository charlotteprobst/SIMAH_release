PopPerYear <- readRDS("output_data/PopPerYear.RDS")
rm(list=setdiff(ls(), "PopPerYear"))

for(i in 1984:2010){
  PopPerYear[[paste(i)]]$Year <- i
}

PopPerYear <- do.call(rbind,PopPerYear)

compare <- read.csv("input_data/CPS_PSID_compare.csv")

summary <- PopPerYear %>% group_by(microsim.init.sex, Year) %>% 
  summarise(meanincome=mean(microsim.init.income),
            medianincome=median(microsim.init.income)) %>% ungroup() %>% 
  rename(year=Year,
         sex = microsim.init.sex) %>% mutate(sex=recode(sex, "f"="Female",
                                                        "m"="Male"),
                                             data=as.factor("microsim"))
  
compare <- rbind(compare, summary)

plot <- ggplot(data=compare, aes(x=year, y=meanincome, colour=data)) + geom_line() + geom_point() + 
  theme_bw() + facet_grid(~sex) + ylim(0,150000) + xlim(1984, 2010)
plot
ggsave("income_microsim_mean.png", plot, dpi=500)

# randomly sample 20 ids
ids <- sample_n(data.frame(microsim.init.id=unique(PopPerYear$microsim.init.id)),20)$microsim.init.id
# look at some individual trajectories 

ids <- sample_n(data.frame(microsim.init.id=unique(subset(PopPerYear, microsim.init.age==60)$microsim.init.id)),
                20)$microsim.init.id

sample <- PopPerYear[PopPerYear$microsim.init.id %in% ids ,]


plot2 <- ggplot(data=sample, aes(x=Year, y=microsim.init.income, colour=
                          microsim.init.sex)) + geom_line() + facet_wrap(~microsim.init.id) +
  geom_text(aes(label=microsim.init.age), size=3)
plot2
ggsave("income_individual_profiles.png", plot2, dpi=500)

# split into quintiles of household income 
summary <- PopPerYear %>% group_by(Year) %>% mutate(incomequintile = ntile(microsim.init.income, 5))

summary <- summary %>% group_by(Year, incomequintile) %>% summarise(max=max(microsim.init.income))

summary <- summary %>% filter(incomequintile==1) %>% select(-c(incomequintile))
summary <- data.frame(summary)

PopPerYear <- PopPerYear %>% group_by(Year) %>% mutate(incomequintile=ntile(microsim.init.income, 5))

PopPerYear <- left_join(PopPerYear, summary)
PopPerYear$lowincome <- ifelse(PopPerYear$microsim.init.income<PopPerYear$max, 1, 0)

summary <- PopPerYear %>% group_by(Year, lowincome) %>% tally()

