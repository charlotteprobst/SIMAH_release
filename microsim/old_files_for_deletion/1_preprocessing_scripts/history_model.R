# model for predicting history from grams per day at initialisation
# read in te BRFSS data with drinking history generated
brfss <- read_rds("SIMAH_workplace/microsim/1_input_data/brfss_subset.RDS") %>% 
  filter(YEAR>=2000)

model1 <- lm(grams_10years ~ YEAR*microsim.init.age + microsim.init.race + microsim.init.age + microsim.init.sex+
               microsim.init.education + microsim.init.drinkingstatus + 
               microsim.init.alc.gpd*microsim.init.sex*microsim.init.age + 
               formerdrinker, brfss)
summary(model1)

brfss$newhistory <- predict(model1, brfss)
brfss$newhistory <- ifelse(brfss$newhistory<0, 0, brfss$newhistory)
cor.test(brfss$grams_10years, brfss$newhistory)

saveRDS(model1, "SIMAH_workplace/microsim/1_input_data/history_model.RDS")


