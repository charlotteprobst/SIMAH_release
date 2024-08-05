# postprocessing social roles 


summaryroles <- read.csv("summaryroles.csv")

summaryroles$employment <- ifelse(summaryroles$newstate==4, 1,
                                             ifelse(summaryroles$newstate==6, 1,
                                                    ifelse(summaryroles$newstate==7, 1,
                                                           ifelse(summaryroles$newstate==8, 1,
                                                                  0))))

summaryroles$maritalstatus <- ifelse(summaryroles$newstate==3, 1,
                                          ifelse(summaryroles$newstate==5, 1,
                                                 ifelse(summaryroles$newstate==6, 1,
                                                        ifelse(summaryroles$newstate==8, 1,
                                                               0))))

summaryroles$parenthood <- ifelse(summaryroles$newstate==2, 1,
                                             ifelse(summaryroles$newstate==5, 1,
                                                    ifelse(summaryroles$newstate==7, 1,
                                                           ifelse(summaryroles$newstate==8, 1,
                                                                  0))))

summaryemployed <- summaryroles %>% 
  group_by(employment, year) %>% 
  summarise(sum=sum(n))

summaryemployed <- data.frame(summaryemployed)

summaryemployed <- spread(summaryemployed, employment,sum)
names(summaryemployed) <- c("year","unemployed","employed")

summaryemployed$percentemployed <- summaryemployed$employed / 
  (summaryemployed$employed+summaryemployed$unemployed)


library(ggplot2)
ggplot(data=summaryemployed, aes(x=year, y=percentemployed)) + geom_line()+ ylim(0,1)

       