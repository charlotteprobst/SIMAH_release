# functions for cleaning education data 

# function 1 - give IDs of everyone that has backwards transitions in the data
getIDs <- function(data){
  
changes <- data %>% group_by(newID) %>% arrange(year, bygroup=T) %>% dplyr::select(year, newID, sex, highestEd, educNUM) %>% mutate(lagged = lag(highestEd, 1),
                                                                                                diff = highestEd-lagged,
                                                                                                tag = ifelse(diff<0,1,0)) %>% 
  fill(tag, .direction=c("downup")) %>% filter(tag==1) %>% group_by(newID)
backIDs <- unique(changes$newID) 
changes <- data[data$newID %in% backIDs,]
length(unique(changes$newID))==length(backIDs)
changes <- changes %>% group_by(newID) %>% mutate(lagged=lead(highestEd,1),
                                                  diff=highestEd-lagged) %>% 
  mutate(tag = ifelse(diff>0, 1,0),
         highestEd = ifelse(tag==1, NA,
                            highestEd)) %>% 
  fill(highestEd, .direction=c("updown")) %>% 
  dplyr::select(-c(lagged, diff, tag))
data <- data[!data$newID %in% backIDs,]
data <- rbind(data, changes)
return(backIDs)
}


newED <- function(data, backIDs){
  changes <- data %>% group_by(newID) %>% mutate(lagged=lead(highestEd,1),
                                                  diff=highestEd-lagged) %>% 
  mutate(tag = ifelse(diff>0, 1,0),
         highestEd = ifelse(tag==1, NA,
                            highestEd)) %>% 
  fill(highestEd, .direction=c("downup")) %>% 
  dplyr::select(-c(lagged, diff, tag)) %>% 
    mutate(educNUM = ifelse(highestEd<=12, 1,
                                   ifelse(highestEd==13, 2,
                                          ifelse(highestEd==14, 3,
                                                 ifelse(highestEd==15,4,
                                                        ifelse(highestEd>=16, 5, NA))))))
  changes <- changes %>% group_by(newID) %>% mutate(lagged=lead(highestEd,1),
                                                 diff=highestEd-lagged) %>% 
    mutate(tag = ifelse(diff>0, 1,0),
           highestEd = ifelse(tag==1, NA,
                              highestEd)) %>% 
    fill(highestEd, .direction=c("updown")) %>% 
    select(-c(lagged, diff, tag)) %>% 
    mutate(educNUM = ifelse(highestEd<=12, 1,
                            ifelse(highestEd==13, 2,
                                   ifelse(highestEd==14, 3,
                                          ifelse(highestEd==15,4,
                                                 ifelse(highestEd>=16, 5, NA))))))
return(changes)
}
