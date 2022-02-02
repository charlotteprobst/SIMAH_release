# function for applying alcohol consumption transition probabilities
transition_alcohol <- function(data, transitions){
  selected <- unique(data$cat)
  rates <- transitions %>% filter(cat == selected)
  data$newALC <- ifelse(data$prob<=rates$cumsum[1], "Lifetime abstainer",
                       ifelse(data$prob<=rates$cumsum[2] & data$prob>rates$cumsum[1], "Former drinker",
                              ifelse(data$prob<=rates$cumsum[3] & data$prob>rates$cumsum[2],"Low risk",
                                     ifelse(data$prob<=rates$cumsum[4] & data$prob>rates$cumsum[3],"Medium risk",
                                            ifelse(data$prob>rates$cumsum[4],"High risk",NA)))))
  # data$newALC <- ifelse(data$prob<=rates$cumsum[1], "Non-drinker",
  #                       ifelse(data$prob<=rates$cumsum[2] & data$prob>rates$cumsum[1], "Low risk",
  #                              ifelse(data$prob<=rates$cumsum[3] & data$prob>rates$cumsum[2],"Medium risk",
  #                                     ifelse(data$prob<=rates$cumsum[4] & data$prob>rates$cumsum[3],"High risk",
  #                                            NA))))

  return(data)
}
