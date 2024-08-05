migration_place_function <- function(data){
    data$prevplace5 <- ifelse(data$MIGPLAC5==006, "California",
                             ifelse(data$MIGPLAC5==008, "Colorado",
                                    ifelse(data$MIGPLAC5==012, "Florida",
                                           ifelse(data$MIGPLAC5==018, "Indiana",
                                                  ifelse(data$MIGPLAC5==021, "Kentucky",
                                                         ifelse(data$MIGPLAC5==022, "Louisiana",
           ifelse(data$MIGPLAC5==025, "Massachusetts",
                  ifelse(data$MIGPLAC5==026, "Michigan",
                         ifelse(data$MIGPLAC5==027, "Minnesota",
                                ifelse(data$MIGPLAC5==029, "Missouri",
                                       ifelse(data$MIGPLAC5==036, "New York",
                                              ifelse(data$MIGPLAC5==041, "Oregon",
                                                     ifelse(data$MIGPLAC5==042, "Pennsylvania",
                                                            ifelse(data$MIGPLAC5==047, "Tennessee",
                                                                   ifelse(data$MIGPLAC5==048, "Texas",
                                                                          ifelse(data$MIGPLAC5==990, "notmoved",
                                                                                 "Other"))))))))))))))))
      data$prevplace1 <- ifelse(data$MIGPLAC1==006, "California",
                               ifelse(data$MIGPLAC1==008, "Colorado",
                                      ifelse(data$MIGPLAC1==012, "Florida",
                                             ifelse(data$MIGPLAC1==018, "Indiana",
                                                    ifelse(data$MIGPLAC1==021, "Kentucky",
                                                           ifelse(data$MIGPLAC1==022, "Louisiana",
                                                                  ifelse(data$MIGPLAC1==025, "Massachusetts",
                                                                         ifelse(data$MIGPLAC1==026, "Michigan",
                                                                                ifelse(data$MIGPLAC1==027, "Minnesota",
                                                                                       ifelse(data$MIGPLAC1==029, "Missouri",
                                                                                              ifelse(data$MIGPLAC1==036, "New York",
                                                                                                     ifelse(data$MIGPLAC1==041, "Oregon",
                                                                                                            ifelse(data$MIGPLAC1==042, "Pennsylvania",
                                                                                                                   ifelse(data$MIGPLAC1==047, "Tennessee",
                                                                                                                          ifelse(data$MIGPLAC1==048, "Texas",
                                                                                                                                 ifelse(data$MIGPLAC1==990, "notmoved",
                                                                                                                                        "Other"))))))))))))))))
  data$currentplace <- ifelse(data$STATEFIP==6, "California",
                              ifelse(data$STATEFIP==8, "Colorado",
                                     ifelse(data$STATEFIP==12, "Florida",
                                            ifelse(data$STATEFIP==18, "Indiana",
                                                   ifelse(data$STATEFIP==21, "Kentucky",
                                                          ifelse(data$STATEFIP==22, "Louisiana",
                                                                 ifelse(data$STATEFIP==25, "Massachusetts",
                                                                        ifelse(data$STATEFIP==26, "Michigan",
                                                                               ifelse(data$STATEFIP==27, "Minnesota",
                                                                                      ifelse(data$STATEFIP==29, "Missouri",
                                                                                             ifelse(data$STATEFIP==36, "New York",
                                                                                                    ifelse(data$STATEFIP==41, "Oregon",
                                                                                                           ifelse(data$STATEFIP==42, "Pennsylvania",
                                                                                                                  ifelse(data$STATEFIP==47, "Tennessee",
                                                                                                                         ifelse(data$STATEFIP==48, "Texas",
                                                                                                                                       "Other"))))
                                                                                             )))))))))))
    return(data)
  }


  
  
    
  