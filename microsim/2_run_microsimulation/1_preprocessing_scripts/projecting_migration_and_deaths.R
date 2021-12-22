# # projecting migration in and out rates 
# 
# # build a regression model to project the rates forwards beyond 2018
# 
# RatesForModel <- Rates %>% mutate(period = Year-2000) %>% filter(Year<=2015)
# 
# ggplot(data=RatesForModel, aes(x=Year, y=MigrationInN, colour=microsim.init.sex)) + 
#   geom_line() + facet_grid(rows=vars(agecat),cols=vars(microsim.init.race), scales="free") + 
#   theme_bw()
# 
# fitIn <- lm(MigrationInN ~ period + agecat + microsim.init.sex + microsim.init.race, 
#           data=RatesForModel)
# summary(fitIn)
# 
# fitOut <- lm(MigrationOutN ~ period + agecat + microsim.init.sex + microsim.init.race, 
#             data=RatesForModel)
# summary(fitOut)
# 
# datatopredict <- expand.grid(period=c(19:25),microsim.init.sex=unique(Rates$microsim.init.sex),
#               agecat = unique(Rates$agecat), microsim.init.race=unique(Rates$microsim.init.race)) %>% 
#   mutate(MigrationInN = predict(fitIn, .),
#          MigrationOutN = predict(fitOut,.),
#          MigrationOutN = ifelse(MigrationOutN<0, 0, MigrationOutN),
#          MigrationInN = ifelse(MigrationInN<0, 0, MigrationInN),
#          Year = period+2000) %>% 
#   dplyr::select(Year, agecat, microsim.init.sex, microsim.init.race, MigrationInN, MigrationOutN)
# Rates <- rbind(Rates, datatopredict)
# 
# ggplot(data=Rates, aes(x=Year, y=MigrationInN, colour=microsim.init.sex)) + 
#   geom_line() + facet_grid(rows=vars(agecat),cols=vars(microsim.init.race), scales="free") + 
#   theme_bw()

datatopredict <- expand.grid(Year=c(2019:2025),microsim.init.sex=unique(Rates$microsim.init.sex),
                             agecat = unique(Rates$agecat), microsim.init.race=unique(Rates$microsim.init.race)) %>% 
  mutate(MigrationInN = NA,
         MigrationOutN = NA)
Rates <- rbind(Rates,datatopredict)

Rates <- Rates %>% group_by(microsim.init.sex, agecat, microsim.init.race) %>% 
  fill(c(MigrationInN,MigrationOutN), .direction=c("downup"))


