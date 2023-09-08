transition_ed <- function(data, transitions){
  selected <- unique(data$cat)
  rates <- transitions %>% filter(cat==selected)
  data$newED <- ifelse(data$prob<=rates$cumsum[1], "LHS",
                       ifelse(data$prob<=rates$cumsum[2] & data$prob>rates$cumsum[1], "HS",
                          ifelse(data$prob<=rates$cumsum[3] & data$prob>rates$cumsum[2], "SomeC1",
                              ifelse(data$prob<=rates$cumsum[4] & data$prob>rates$cumsum[3],"SomeC2",
                                     ifelse(data$prob<=rates$cumsum[5] & data$prob>rates$cumsum[4],"SomeC3",
                                            ifelse(data$prob<=rates$cumsum[6] & data$prob>rates$cumsum[5],"College",NA))))))
  return(data)
}