transition_ed <- function(data, transitions){
  selected <- unique(data$cat)
  rates <- transitions %>% filter(cat==selected)
  data$newED <- ifelse(data$prob<=rates$cumsum[1], "LEHS",
                       ifelse(data$prob<=rates$cumsum[2] & data$prob>rates$cumsum[1], "SomeC1",
                              ifelse(data$prob<=rates$cumsum[3] & data$prob>rates$cumsum[2],"SomeC2",
                                     ifelse(data$prob<=rates$cumsum[4] & data$prob>rates$cumsum[3],"SomeC3",
                                            ifelse(data$prob<=rates$cumsum[5] & data$prob>rates$cumsum[4],"College",NA)))))
  return(data)
}
