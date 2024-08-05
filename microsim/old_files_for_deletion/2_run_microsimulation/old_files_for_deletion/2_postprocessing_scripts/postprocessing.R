###get the summary files to compare to the census ####

source("running_scripts/migration_functions.R")
State <- "USA"
sum1984 <- read.csv("output_data/cons1984.csv") 

sum1984$state <- ifelse(sum1984$microsim.init.employment.status==0 & 
                          sum1984$microsim.init.marital.status==0 & sum1984$microsim.init.parenthood.status==0,1,
                        ifelse(sum1984$microsim.init.employment.status==0 & 
                                 sum1984$microsim.init.marital.status==0 & sum1984$microsim.init.parenthood.status==1,2,
                               ifelse(sum1984$microsim.init.employment.status==0 & 
                                        sum1984$microsim.init.marital.status==1 & sum1984$microsim.init.parenthood.status==0, 3,
                                      ifelse(sum1984$microsim.init.employment.status==1 & 
                                               sum1984$microsim.init.marital.status==0 & sum1984$microsim.init.parenthood.status==0, 4,
                                             ifelse(sum1984$microsim.init.employment.status==0 & 
                                                      sum1984$microsim.init.marital.status==1 & sum1984$microsim.init.parenthood.status==1, 5,
                                                    ifelse(sum1984$microsim.init.employment.status==1 & 
                                                             sum1984$microsim.init.marital.status==1 & 
                                                             sum1984$microsim.init.parenthood.status==0, 6,
                                                           ifelse(sum1984$microsim.init.employment.status==1 & 
                                                                    sum1984$microsim.init.marital.status==0 & sum1984$microsim.init.parenthood.status==1, 7,
                                                                  ifelse(sum1984$microsim.init.employment.status==1 & 
                                                                           sum1984$microsim.init.marital.status==1 & sum1984$microsim.init.parenthood.status==1, 8,NA)
                                                           )))))))

sum1984 <- sum1984%>% mutate(agecat=cut(sum1984$microsim.init.age,
                                                    breaks=c(0,13,17,19,22,24,28,30,34,39,44,49,59,100),
                                                    labels=c("12.13", "14.17", "18.19", "20.22","23.24",
                                                             "25.28","29.30", "31.34","35.39", "40.44","45.49",
                                                             "50.59","60.80")),
                              microsim.init.sex=recode(microsim.init.sex, "f"="F", "m"="M"),
                              microsim.init.employment.status=recode(microsim.init.employment.status,
                                                                     "0"="unemployed","1"="employed"),
                              microsim.init.parenthood.status=recode(microsim.init.parenthood.status,
                                                                     "0"="notparent","1"="parent"),
                              microsim.init.marital.status=recode(microsim.init.marital.status,
                                                                  "0"="unmarried","1"="married"))

agesexrace <- sum1984 %>% group_by(agecat, microsim.init.sex, microsim.init.race) %>% 
  tally(n="n") %>% mutate(n=ceiling(n*(1/proportion))) %>% ungroup() %>% 
  mutate(cat=paste(microsim.init.race, agecat, microsim.init.sex, sep="")) %>% 
  select(cat,n) %>% spread(cat, n)

rolesbysex <- cbind(sum1984 %>% group_by(microsim.init.sex, microsim.init.employment.status) %>% 
  tally(n="n") %>% mutate(n=ceiling(n*(1/proportion))) %>% ungroup() %>% 
  mutate(cat=paste(microsim.init.employment.status, microsim.init.sex, sep="")) %>% 
  select(cat, n) %>% spread(cat,n),
  sum1984 %>% group_by(microsim.init.sex, microsim.init.marital.status) %>% 
    tally(n="n") %>% mutate(n=ceiling(n*(1/proportion))) %>% ungroup() %>% 
    mutate(cat=paste(microsim.init.marital.status, microsim.init.sex, sep="")) %>% 
    select(cat, n) %>% spread(cat,n),
  sum1984 %>% group_by(microsim.init.sex, microsim.init.parenthood.status) %>% 
    tally(n="n") %>% mutate(n=ceiling(n*(1/proportion))) %>% ungroup() %>% 
    mutate(cat=paste(microsim.init.parenthood.status, microsim.init.sex, sep="")) %>% 
    select(cat, n) %>% spread(cat,n))

rolesstate <- sum1984 %>% group_by(microsim.init.sex, state) %>% 
  tally(n="n") %>% mutate(n=ceiling(n*(1/proportion))) %>% ungroup() %>% 
  mutate(cat=paste("STATE",state,microsim.init.sex, sep="")) %>% 
  select(cat,n) %>% spread(cat,n)

summaryroles <- read.csv("output_data/summaryrolesUSA.csv")
summaryroles$n <- summaryroles$n*(1/proportion)
summaryroles <- subset(summaryroles, year==1984)
summaryroles$microsim.init.sex <- recode(summaryroles$microsim.init.sex, "f"="F", "m"="M")

####bind all the constraints together
cons <- cbind(agesexrace, rolesbysex, rolesstate)
cons <- data.frame(cons)

WorkingDirectory <- "~/Desktop/repos/Wrapper_BRFSS/basepopulation/input_data"
setwd(paste(WorkingDirectory))
write.csv(cons, paste("constraints", State, ".csv", sep=""), row.names=FALSE)
  