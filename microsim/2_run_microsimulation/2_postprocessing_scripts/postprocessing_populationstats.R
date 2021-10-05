for(i in names(PopPerYear)){
  PopPerYear[[i]] <- PopPerYear[[i]] %>% select(
    microsim.init.id, microsim.init.sex, microsim.init.race,
    microsim.init.age, microsim.init.employment.status,
    microsim.init.BMI, drinkingstatus2, 
    microsim.init.alc.gpd,
    alcdays,
    grams_10years,Cirrhosis_risk)
  PopPerYear[[i]]$Year <- i
}

Pop <- do.call(rbind, PopPerYear)
Pop$Year <- as.numeric(Pop$Year)
install.packages("plotrix")
MeanAlc <- Pop %>% 
  group_by(Year, microsim.init.sex) %>% 
  summarise(meangpd = mean(microsim.init.alc.gpd), segpd = std.error(microsim.init.alc.gpd))
