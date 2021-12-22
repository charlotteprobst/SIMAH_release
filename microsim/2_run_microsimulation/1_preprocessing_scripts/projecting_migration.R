# projecting migration in and out rates 

# build a regression model to project the rates forwards beyond 2018

RatesForModel <- Rates %>% mutate(period = Year-2000)

ggplot(data=Rates, aes(x=Year, y=MigrationInN, colour=microsim.init.sex)) + 
  geom_line() + facet_grid(rows=vars(agecat),cols=vars(microsim.init.race), scales="free") + 
  theme_bw()

fitIn <- lm(MigrationInN ~ period + agecat + microsim.init.sex + microsim.init.race, 
          data=RatesForModel)
summary(fitIn)

fitOut <- lm(MigrationOutN ~ period + agecat + microsim.init.sex + microsim.init.race, 
            data=RatesForModel)
summary(fitOut)

datatopredict <- expand.grid(period=c(19:25),microsim.init.sex=unique(Rates$microsim.init.sex),
              agecat = unique(Rates$agecat), microsim.init.race=unique(Rates$microsim.init.race)) %>% 
  mutate(MigrationInN = predict(fitIn, .),
         MigrationOutN = predict(fitOut,.),
         MigrationOutN = ifelse(MigrationOutN<0, 0, MigrationOutN),
         Year = period+2000)