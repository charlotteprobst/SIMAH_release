# Splitting functions - for smoothing population migration rates 

library(splitstackshape)

split_over_years <- function(data){
    data$tosplit <- ifelse(data$age==18, 1,
                           ifelse(data$age==19, 2,
                                  ifelse(data$age==20, 3,
                                         ifelse(data$age==21, 4,
                                                ifelse(data$age==22, 5,
                                                       ifelse(data$age==23, 6,
                                                              ifelse(data$age==24, 7,
                                                                     ifelse(data$age>=25, 8,NA
                                                                                   ))))))))

  data <- data %>% ungroup() %>% select(Year, age, race, sex, diff, tosplit)
  data$toadd <- data$diff/data$tosplit
  data <- expandRows(data, "tosplit", drop=F)
  origyear <- 2018
  data$birthyear <- origyear-data$age
  data$Year <- ifelse(data$tosplit==1, origyear,
                      ifelse(data$tosplit==2, (origyear-1):origyear,
                             ifelse(data$tosplit==3, (origyear-2):origyear,
                                    ifelse(data$tosplit==4, (origyear-3):origyear,
                                                          ifelse(data$tosplit==5, (origyear-4):origyear,
                                                                 ifelse(data$tosplit==6, (origyear-5):origyear,
                                                                                       ifelse(data$tosplit==7, (origyear-6):origyear,
                                      ifelse(data$tosplit==8, (origyear-7):origyear, NA))))))))
  data$newage <- data$Year - data$birthyear
  data$tosplit <- NULL
  data <- data.frame(data)
  return(data)
}
