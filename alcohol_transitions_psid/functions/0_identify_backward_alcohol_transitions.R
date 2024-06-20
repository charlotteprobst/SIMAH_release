# functions for cleaning education data 

# function 1 - give IDs of everyone that has backwards transitions in the data
getIDs <- function(data){
  
changes <- data %>% group_by(uniqueID) %>% 
  arrange(year, bygroup=T) %>% 
  select(year, uniqueID, sex, education, final_race_using_method_hierarchy, highestAlc, final_alc_cat) %>% 
  mutate(lagged = lag(highestAlc, 1),
         diff = highestAlc-lagged,
         tag = ifelse(diff<0,1,0)) %>% 
  fill(tag, .direction=c("downup")) %>% 
  filter(tag==1) %>% 
  group_by(uniqueID)
backIDs <- unique(changes$uniqueID) 
changes <- data[data$uniqueID %in% backIDs,]
length(unique(changes$uniqueID))==length(backIDs)
changes <- changes %>% group_by(uniqueID) %>% mutate(lagged=lead(highestAlc,1),
                                                  diff=highestAlc-lagged) %>% 
  mutate(tag = ifelse(diff>0, 1,0),
         highestAlc = ifelse(tag==1, NA,
                            highestAlc)) %>% 
  fill(highestAlc, .direction=c("updown")) %>% 
  select(-c(lagged, diff, tag))
data <- data[!data$uniqueID %in% backIDs,]
data <- rbind(data, changes)
return(backIDs)
}


newED <- function(data, backIDs){
  changes <- data %>% group_by(uniqueID) %>% mutate(lagged=lead(highestAlc,1),
                                                  diff=highestAlc-lagged) %>% 
  mutate(tag = ifelse(diff>0, 1,0),
         highestAlc = ifelse(tag==1, NA,
                            highestAlc)) %>% 
  fill(highestAlc, .direction=c("downup")) %>% 
  select(-c(lagged, diff, tag)) %>% 
    mutate(final_alc_cat = ifelse(highestAlc=="Non-drinker", 1,
                                   ifelse(highestAlc=="Low risk", 2,
                                          ifelse(highestAlc=="Medium risk", 3,
                                                 ifelse(highestAlc=="High risk",4,NA)))))
  changes <- changes %>% group_by(uniqueID) %>% mutate(lagged=lead(highestAlc,1),
                                                 diff=highestAlc-lagged) %>% 
    mutate(tag = ifelse(diff>0, 1,0),
           highestAlc = ifelse(tag==1, NA,
                              highestAlc)) %>% 
    fill(highestAlc, .direction=c("updown")) %>% 
    select(-c(lagged, diff, tag)) %>% 
    mutate(final_alc_cat = ifelse(highestAlc=="Non-drinker", 1,
                                  ifelse(highestAlc=="Low risk", 2,
                                         ifelse(highestAlc=="Medium risk", 3,
                                                ifelse(highestAlc=="High risk",4,NA)))))
return(changes)
}
