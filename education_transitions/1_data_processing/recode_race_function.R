
recode_race_function <- function(data, type){
  head <- c("black","white","hispanic","other","Native","Asian/PI")
  wife <- c("black","white","hispanic","other","Native","Asian/PI")
  combos <- expand.grid(head,wife)
  names(combos) <- c("head","wife")
  combos$head <- as.character(combos$head)
  combos$wife <- as.character(combos$wife)
  combos$racenew <- ifelse(combos$head==combos$wife, combos$head,
                          ifelse(combos$head=="hispanic", "hispanic",
                                 ifelse(combos$wife=="hispanic", "hispanic",
                                        ifelse(combos$head=="black" & combos$wife!="hispanic","black",
                                               ifelse(combos$wife=="black" & combos$head!="hispanic","black",
                                                      ifelse(combos$head=="Native" & combos$wife!="hispanic" & combos$wife!="black", "Native",
                                                             ifelse(combos$wife=="Native" & combos$head!="hispanic" & combos$head!="black","Native",
                                                                    ifelse(combos$head=="Asian/PI" & combos$wife!="hispanic" & combos$wife!="black" & 
                                                                             combos$wife!="Native","Asian/PI",
                                                                           ifelse(combos$wife=="Asian/PI" & combos$head!="hispanic" & combos$head!="black" &
                                                                                    combos$head!="Native","Asian/PI",
                                                                                  ifelse(combos$head=="other" & combos$wife!="hispanic" & combos$wife!="black" & 
                                                                                           combos$wife!="Native" & combos$wife!="Asian/PI","other",
                                                                                         ifelse(combos$wife=="other" & combos$head!="hispanic" & combos$head!="black" & 
                                                                                                  combos$head!="Native" & combos$head!="Asian/PI","other",
                                                                                                ifelse(combos$head=="white", combos$wife,
                                                                                                       ifelse(combos$wife=="white",combos$head, NA
                                                                                                       )))))))))))))
  combos$combo <- paste(combos$head,combos$wife,sep="")
  combos <- combos %>% select(combo, racenew)
  if(type==T){
  data$combo <- paste(data$raceethhead, data$raceethwife, sep="")
  }
  if(type==F){
    data$combo <- paste(data$racemother, data$racefather, sep="")
  }
  data <- left_join(data, combos)
  data$racenew <- ifelse(data$combo=="Asian/PINA", "Asian/PI",
                        ifelse(data$combo=="blackNA","black",
                               ifelse(data$combo=="hispanicNA","hispanic",
                                      ifelse(data$combo=="NAhispanic","hispanic",
                                             ifelse(data$combo=="NativeNA","Native",
                                                    ifelse(data$combo=="otherNA","other",
                                                           ifelse(data$combo=="whiteNA","white",
                                                                  ifelse(data$combo=="NANA",NA,
                                                                         ifelse(data$combo=="NAwhite","white",
                                                                                ifelse(data$combo=="NAblack","black",
                                                                                       ifelse(data$combo=="NAhispanic","hispanic",
                                                                                              ifelse(data$combo=="NAAsian/PI","Asian/PI",
                                                                                                     ifelse(data$combo=="NANative", "Native",
                                                                                                            ifelse(data$combo=="NAother","other",
                                                                                                                   data$racenew)))))))
                                                           )))))))
                                                                
return(data)
}

recode_race_duplicate <- function(data){
  data <- data %>% 
    mutate(newrace2 = ifelse(n==2 & number1=="black" | number2=="black", "black", 
                            ifelse(n==2 & number1=="hispanic" | number2 =="hispanic","hispanic",
                                   ifelse(n==2 & number1=="Native" | number2=="Native","Native",
                                          ifelse(n==2 & number1=="Asian/PI" | number2=="Asian/PI", "Asian/PI",
                                                 ifelse(n==2 & number1=="other" | number2=="other","other", "white"))))),
           newrace3 =  ifelse(n==3 & number1=="black" | number2=="black" | number3=="black", "black", 
                                                               ifelse(n==3 & number1=="hispanic" | number2 =="hispanic" | number3=="hispanic","hispanic",
                                                                      ifelse(n==3 & number1=="Native" | number2=="Native" | number3=="Native","Native",
                                                                             ifelse(n==3 & number1=="Asian/PI" | number2=="Asian/PI" | number3=="Asian/PI", "Asian/PI",
                                                                                    ifelse(n==3 & number1=="other" | number2=="other" | number3=="other","other",
                                                                                           ifelse(n==4 & number3=="hispanic","hispanic",newrace2)))))),
           newrace = ifelse(is.na(newrace3), newrace2, newrace3)) %>% dplyr::select(-c(newrace2,newrace3))
  return(data)
}
