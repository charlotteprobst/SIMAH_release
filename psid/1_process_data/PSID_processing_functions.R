# SIMAH project 2022 - functions to process data from the SIMAH project 

process_education <- function(data){
  varnames <- c("ER30010", "ER30052","ER30076", "ER30100", "ER30126", "ER30147","ER30169", "ER30197", "ER30226", "ER30255",
                     "ER30296", "ER30326", "ER30356", "ER30384",  "ER30413",  "ER30443", "ER30478", "ER30513", "ER30549",
                     "ER30584", "ER30620", "ER30657", "ER30703", "ER30748", "ER30820",  "ER33115", "ER33215", "ER33315", "ER33415",
                     "ER33516","ER33616","ER33716","ER33817","ER33917","ER34020","ER34119","ER34230","ER34349","ER34548", "ER34752")
  
  newdata <- data %>% dplyr::select(uniqueID, familyID, IDmother, IDfather, c(varnames))
  years <- c(1968, 1970:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
  names(newdata)[5:44] <- years
  newdata <- newdata %>% pivot_longer(cols='1968':'2019', names_to="year", values_to="education") %>% 
    mutate(education = ifelse(education==0, NA,
                              ifelse(education==99, NA, 
                                     ifelse(education==98, NA, education))),
           education_cat = ifelse(education<=12, "LEHS",
                                  ifelse(education>12 & education<16, "SomeC",
                                         ifelse(education>=16, "College", NA))),
           education_cat_detailed = ifelse(education<=12, "LEHS",
                                           ifelse(education==13, "SomeC1",
                                                  ifelse(education==14, "SomeC2",
                                                         ifelse(education==15, "SomeC3",
                                                                ifelse(education>=16, "College", NA))))),
           year = as.numeric(year)) %>% group_by(uniqueID) %>% 
    fill(education, .direction=c("downup")) %>% fill(education_cat, .direction=c("downup")) %>% 
    fill(education_cat_detailed, .direction=c("downup")) %>% ungroup()
  mother <- newdata %>% dplyr::select(IDmother, year, education, education_cat) %>% 
    rename(uniqueID = IDmother,
           motherseducation = education,
          motherseducationcat = education_cat) %>% drop_na(uniqueID) %>% group_by(uniqueID) %>%
    fill(motherseducation, .direction=c("downup")) %>% fill(motherseducationcat, .direction=c("downup")) %>% 
    group_by(uniqueID) %>% summarise(motherseducation = max(motherseducation))
    newdata <- left_join(newdata, mother)
  father <- newdata %>% dplyr::select(IDfather, year, education, education_cat) %>% 
    rename(uniqueID = IDfather,
           fatherseducation = education,
           fatherseducationcat = education_cat) %>% drop_na(uniqueID) %>% 
    fill(fatherseducation, .direction=c("downup")) %>% 
    group_by(uniqueID) %>% summarise(fatherseducation=max(fatherseducation, na.rm=T))
  newdata <- left_join(newdata, father)
  return(newdata)
}

process_age <- function(data){
  
varnames <-   c("ER30004", "ER30023", "ER30046", "ER30070", "ER30094", "ER30120", "ER30141", "ER30163", "ER30191", "ER30220",
                "ER30249", "ER30286", "ER30316", "ER30346", "ER30376", "ER30402", "ER30432", "ER30466", "ER30501", "ER30538",
                "ER30573", "ER30609", "ER30645", "ER30692", "ER30736", "ER30809", "ER33104", "ER33204", "ER33304", "ER33404",
                "ER33504", "ER33604", "ER33704", "ER33804", "ER33904", "ER34004", "ER34104", "ER34204", "ER34305", "ER34504",
                "ER34704")
newdata <- data %>% dplyr::select(uniqueID, c(varnames))
years <- c(1968:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
names(newdata)[2:42] <- years
newdata <- newdata %>% pivot_longer(cols='1968':'2019', names_to="year", values_to="age") %>% 
  mutate(age = ifelse(age==0, NA, 
                      ifelse(age==999, NA, age)),
         year = as.numeric(year))
return(newdata)
}

process_relationship <- function(data){
  
varnames <- c("ER30003", "ER30022", "ER30045", "ER30069", "ER30093", "ER30119", "ER30140", "ER30162", "ER30190",
              "ER30219", "ER30248", "ER30285", "ER30315", "ER30345", "ER30375", "ER30401", "ER30431", "ER30465",
              "ER30500", "ER30537", "ER30572", "ER30608", "ER30644", "ER30691", "ER30735", "ER30808", "ER33103",
              "ER33203", "ER33303", "ER33403","ER33503","ER33603","ER33703","ER33803","ER33903","ER34003","ER34103",
                       "ER34203","ER34303","ER34503", "ER34703")
newdata <- data %>% dplyr::select(uniqueID, c(varnames))
years <- c(1968:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
names(newdata)[2:42] <- years
newdata <- newdata %>% pivot_longer(cols='1968':'2019', names_to="year", values_to="relationship") %>% 
  mutate(relationship = ifelse(relationship==10 | relationship==1, "head",
                                           ifelse(relationship==20 | 
                                                    relationship==22 | relationship==2, 
                                                  "wife/partner",
                                                  ifelse(relationship==30 | relationship==37 | relationship==38 |
                                                           relationship==3, "childofhead",
                                                         ifelse(relationship==33 | relationship==35, "childofpartner",
                                                                ifelse(relationship==0, "latino/immigrantsampleunknown",
                                                                       ifelse(relationship==40 | relationship==4, "brotherofhead",
                                                                              ifelse(relationship==47 | relationship==48, "brotherofwife",
                                                                                     ifelse(relationship==50 | relationship==5, "parentofhead",
                                                                                            ifelse(relationship==57 | relationship==58, "parentofwife",
                                                                                                   ifelse(relationship==60 | relationship==65 | relationship==6, "grandchild",
                                                                                                          ifelse(relationship==7, "otherrelative",
                                                                                                          ifelse(relationship==88, "cohabitor",
                                                                                                                 ifelse(relationship==83, "childofcohabitor",
                                                                                                                        ifelse(relationship==98 | relationship==8, "nonrelative",
                                                                                                                               ifelse(relationship==9, "husbandofhead", NA
))))))))))))))))
newdata$year <- as.numeric(newdata$year)
return(newdata)
}

process_sample_weights <- function(data){
  
# varnames <- c("ER33430", "ER33546", "ER33637", "ER33740", "ER33848", "ER33950",
#               "ER34045", "ER34154", "ER34268", "ER34413", "ER34650", "ER34863")
# newdata <- data %>% select(uniqueID, c(varnames))
# years <- c(1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
# names(newdata)[2:13] <- years
# newdata <- newdata %>% pivot_longer(cols='1997':'2019', names_to="year", values_to="sampleweight") %>% 
#   mutate(year = as.numeric(year))

familyweights <- c("ER16518","ER20394","ER24179","ER28078","ER41069","ER47012","ER52436",
                   "ER58257","ER65492","ER71570","ER77631")
data$familyID <- data$ER30001
data$ID <- data$ER30002
data$uniqueID <- (data$familyID*1000) + data$ID

weights <- data %>% dplyr::select(uniqueID, c(familyweights))

years <- c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)

names(weights)[2:12] <- years
weights <- weights %>% pivot_longer(cols='1999':'2019', names_to="year", values_to="weight") %>% 
  mutate(year=as.integer(year))

return(weights)
}

process_race <- function(data){
  
race <- c("V181","V801","V1490","V2202","V2828","V3300","V3720","V4204","V5096","V5662","V6209","V6802",
            "V7447","V8099","V8723","V9408","V11055","V11938","V13565","V14612","V16086",
            "V17483","V18814","V20114","V21420","V23276","ER3944","ER6814","ER9060","ER11848","ER15928",
            "ER19989","ER23426","ER27393","ER40565","ER46543","ER51904","ER57659","ER64810","ER70882", "ER76897")

racevars <- data %>% dplyr::select(uniqueID, familyID, IDmother, IDfather, sex, c(race))
years <- c(1968:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
names(racevars)[6:46] <- years
racevars <- racevars %>% pivot_longer(cols='1968':'2019', names_to="year", values_to="racehead") %>% 
  mutate(racehead = ifelse(racehead==1, "white",
                           ifelse(racehead==2, "black",
                                  ifelse(racehead==9, NA,
                                         ifelse(racehead==3 & year<=1984, "hispanic",
                                                ifelse(racehead==9, NA,
                                                       ifelse(racehead==0, NA,
                                                              ifelse(racehead==5 & year>=1990 & year<=2003, "hispanic",
                                                                     ifelse(racehead==3 & year>=1985, "Native",
                                                                            ifelse(racehead==4 & year>=1985, "Asian/PI",
                                                                                   "other"))))))))))
# recode values and fill for each person any missing years 
# 1968 - 1984 
# 1 = white, 2=black, 3=hispanic, 7 = other, 9=NA 
# 1984 - 1989
# 1 = white, 2=black, 3=other, 4=other, 5=other, 7=other 8=other, 9=NA
# 1990 - 2003 
# 1 = white, 2=black, 3=other, 4=other, 5=hispanic, 6=other, 7=other, 9=NA
# 2004 - 2017
# 1 = white, 2=black, 3=other, 4=other, 5=other, 7=other, 0=NA, 9=NA

# now get the race of wife variable
wife <- c("V12293","V13500","V14547","V16021","V17418","V18749","V20049","V21355","V23212",
          "ER3883","ER6753","ER8999","ER11760","ER15836","ER19897","ER23334","ER27297",
          "ER40472","ER46449","ER51810","ER57549","ER64671","ER70744","ER76752")
wiferace <- data %>% dplyr::select(uniqueID, familyID,  IDmother, IDfather, sex, c(wife))
years <- c(1985:1997,1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
names(wiferace)[6:29] <- years
wiferace <- wiferace %>% pivot_longer(cols='1985':'2019', names_to="year", values_to="racewife") %>% 
  mutate(racewife = ifelse(racewife==1, "white",
                           ifelse(racewife==2, "black",
                                  ifelse(racewife==9, NA,
                                         ifelse(racewife==9, NA,
                                                ifelse(racewife==0, NA,
                                                       ifelse(racewife==5 & year>=1990 & year<=2003, "hispanic",
                                                              ifelse(racewife==3 & year>=1985, "Native",
                                                                     ifelse(racewife==4 & year>=1985, "Asian/PI",
                                                                            "other")))))))))

# now get hispanic info for the head and wife
hispanic1 <- c("V11937","V13564","V14611","V16085","V17482","V18813","V20113",
               "V21419","V23275","ER3941","ER6811","ER9057","ER27392","ER40564",
               "ER46542","ER51903","ER57658","ER64809","ER70881","ER76896")
years <- c(1985:1996, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
hispanic <- data %>% dplyr::select(uniqueID, familyID, IDmother, IDfather, sex, c(hispanic1))
names(hispanic)[6:25] <- years
hispanic <- hispanic %>% pivot_longer(cols='1985':'2019', names_to="year", values_to="hispanichead") %>% 
  mutate(hispanichead = ifelse(hispanichead==9, NA,
                               ifelse(hispanichead==0, "nothispanic",
                                      "hispanic")))

# hispanic wife 
hispanic2 <- c("V12292","V13499","V14546","V16020",
               "V17417","V18748","V20048","V21354",
               "V23211","ER3880","ER6750","ER8996",
               "ER27296","ER40471","ER46448","ER51809",
               "ER57548","ER64670","ER70743","ER76751")
hispanicwife <- data %>% dplyr::select(uniqueID, familyID, IDmother, IDfather, sex, c(hispanic2))
names(hispanicwife)[6:25] <- years
hispanicwife <- hispanicwife %>% pivot_longer(cols='1985':'2019', names_to="year", values_to="hispanicwife") %>% 
  mutate(hispanicwife = ifelse(hispanicwife==9, NA,
                               ifelse(hispanicwife==0, "nothispanic",
                                      "hispanic")))
# combine all race variables together
alldata <- expand.grid(uniqueID = unique(data$uniqueID), 
                       year =  as.character(c(1968:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)))
raceall <- left_join(alldata, racevars)
raceall <- left_join(raceall, wiferace)
raceall <- left_join(raceall, hispanic)
raceall <- left_join(raceall, hispanicwife)
raceall$year <- as.numeric(raceall$year)

raceall <- raceall %>% 
  mutate(raceethhead = ifelse(year<=1984, racehead,
                              ifelse(year>1984 & hispanichead=="hispanic","hispanic",
                                     ifelse(year>1984 & hispanichead!="hispanic",racehead,NA))),
         raceethwife = ifelse(year<=1984, racewife,
                              ifelse(year>1984 & hispanicwife=="hispanic","hispanic",
                                     ifelse(year>1984 & hispanicwife!="hispanic",racewife,NA)))) %>% 
  dplyr::select(uniqueID, familyID, IDmother, IDfather, year, sex, raceethhead, raceethwife)

return(raceall)
}

individual_race <- function(data, type){
  if(type==T){
  data <- data %>% mutate(individualrace = ifelse(relationship=="head", raceethhead,
                                   ifelse(relationship=="wife", raceethwife,
                                          ifelse(relationship=="childofhead",racefamily1,
                                                 ifelse(relationship=="parentofhead", raceethhead,
                                                        ifelse(relationship=="childofpartner", raceethwife,
                                                               ifelse(relationship=="grandchild", racefamily1,
                                                                      ifelse(relationship=="parentofwife",raceethwife,
                                                                             ifelse(relationship=="brotherofhead", raceethhead,
                                                                                    ifelse(relationship=="brotherofwife", raceethwife,
                                                                                           racefamily1))))))))))
  }else if(type==F){
    toallocate <- data[is.na(data$individualrace),]
    toallocate <- toallocate %>% 
      mutate(individualrace = racefamily2)
    data <- data %>% drop_na(individualrace)
    data <- rbind(data, toallocate)
  }
  return(data)
}

recode_race <- function(data, type){
  head <- c("black","white","hispanic","other","Native","Asian/PI")
  wife <- c("black","white","hispanic","other","Native","Asian/PI")
  combos <- expand.grid(head,wife)
  names(combos) <- c("head","wife")
  combos$head <- as.character(combos$head)
  combos$wife <- as.character(combos$wife)
  combos$racefamily <- ifelse(combos$head==combos$wife, combos$head,
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
  combos <- combos %>% dplyr::select(combo, racefamily)
  if(type==T){
    data$combo <- paste(data$raceethhead, data$raceethwife, sep="")
    data <- left_join(data, combos)
    data$racefamily1 <- ifelse(data$combo=="Asian/PINA", "Asian/PI",
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
                                                                                                                         data$racefamily)))))))
                                                                 )))))))
    data <- data %>% 
      group_by(familyID) %>% 
      fill(racefamily1, .direction=c("downup"))
  }
  if(type==F){
    data$combo <- paste(data$mothersrace, data$fathersrace, sep="")
  data <- left_join(data, combos)
  data$racefamily2 <- ifelse(data$combo=="Asian/PINA", "Asian/PI",
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
                                                                                                                    data$racefamily)))))))
                                                            )))))))
  data <- data %>% 
    group_by(familyID) %>% 
    fill(racefamily2, .direction=c("downup"))
  }
  return(data)
}

code_race_parents <- function(data){
  mother <- data %>% dplyr::select(IDmother, individualrace) %>% 
    rename(uniqueID = IDmother,
           mothersrace = individualrace) %>% group_by(uniqueID) %>% distinct() 
  data <- left_join(data, mother)
  father <- data %>% dplyr::select(IDfather, individualrace) %>% 
    rename(uniqueID = IDfather,
           fathersrace = individualrace) %>% group_by(uniqueID) %>% distinct()
  data <- left_join(data, father)
  return(data)
}

process_parent_ed <- function(data){
  headfathered <- 	c("V318", "V793", "V1484", "V2196", "V2822", "V3240", "V3662", "V4138", "V4681", "V5601", "V6150", "V6747", "V7380",
                     "V8032", "V8656", "V9342", "V10989", "V11922", "V13549", "V14596", "V16070", "V17467", "V18798", "V20098",
                     "V21404", "V23260", "ER3924", "ER6794", "ER9040", "ER11816", "ER15894", "ER19955", "ER23392",  "ER27356",
                     "ER40531", "ER46508", "ER51869", "ER57622", "ER64773", "ER70845",  "ER76860")
  headfather <- data %>% dplyr::select(uniqueID, c(headfathered))
  years <- c(1968:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
  names(headfather)[2:42] <- years
  headfather <- headfather %>% pivot_longer(cols='1968':'2019', names_to="year", values_to="head_fathered") %>% 
    # mutate(head_fathered = ifelse(head_fathered<=4, "LEHS",
    #                               ifelse(head_fathered>4 & head_fathered<=6, "SomeC",
    #                                      ifelse(head_fathered> 6 & head_fathered<=8, "College", NA)))) %>% 
    group_by(uniqueID) %>% fill(head_fathered, .direction=c("downup"))
  
  headmothered <- 	c("V3634", "V4139", "V4682", "V5602", "V6151", "V6748", "V7381",  "V8033", "V8657", "V9343", "V10990", "V11923",
                     "V13550", "V14597", "V16071", "V17468", "V18799", "V20099", "V21405", "V23261", "ER3926", "ER6796", "ER9042",
                     "ER11824", "ER15903", "ER19964", "ER23401", "ER27366", "ER40541", "ER46518", "ER51879", "ER57632", "ER64783",
                     "ER70855", "ER76870")
  
  headmother <- data %>% dplyr::select(uniqueID, c(headmothered))
  years <- c(1974:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
  names(headmother)[2:36] <- years
  headmother <- headmother %>% pivot_longer(cols='1974':'2019', names_to="year", values_to="head_mothered") %>% 
    # mutate(head_mothered = ifelse(head_mothered<=4, "LEHS",
    #                               ifelse(head_mothered>4 & head_mothered<=6, "SomeC",
    #                                      ifelse(head_mothered> 6 & head_mothered<=8, "College", NA)))) %>% 
    group_by(uniqueID) %>% fill(head_mothered, .direction=c("downup")) %>% dplyr::select(uniqueID, year, head_mothered)
  
  spousefathered <- 	c("V3608","V4108","V4753","V5572","V6121","V6718","V7351","V8003","V8627",
                       "V9313","V10960","V12277","V13485","V14532","V16006","V17403","V18734","V20034",
                       "V21340","V23197","ER3864","ER6734","ER8980","ER11735","ER15809","ER19870","ER23307",
                       "ER27267", "ER40442", "ER46414", "ER51775", "ER57512", "ER64634", "ER70707","ER76715")
  
  spousefather <- data %>% dplyr::select(uniqueID, c(spousefathered))
  
  years <- c(1974:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
  names(spousefather)[2:36] <- years
  spousefather <- spousefather %>% pivot_longer(cols='1974':'2019', names_to="year", values_to="spouse_fathered") %>% 
    # mutate(spouse_fathered = ifelse(spouse_fathered<=4, "LEHS",
    #                                 ifelse(spouse_fathered>4 & spouse_fathered<=6, "SomeC",
    #                                        ifelse(spouse_fathered> 6 & spouse_fathered<=8, "College", NA)))) %>% 
    group_by(uniqueID) %>% fill(spouse_fathered, .direction=c("downup")) %>% dplyr::select(uniqueID, year, spouse_fathered)
  spousemothered <- 	c("V3609","V4109","V4754","V5573","V6122","V6719","V7352","V8004","V8628",
                       "V9314","V10961","V12278","V13486","V14533","V16007","V17404","V18735","V20035",
                       "V21341","V23198","ER3866","ER6736","ER8982","ER11743","ER15818","ER19879","ER23316",
                       "ER27277", "ER40452", "ER46424", "ER51785", "ER57522", "ER64644", "ER70717","ER76725")
  
  spousemother <- data %>% dplyr::select(uniqueID, c(spousemothered))
  
  years <- c(1974:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
  names(spousemother)[2:36] <- years
  spousemother <- spousemother %>% pivot_longer(cols='1974':'2019', names_to="year", values_to="spouse_mothered") %>% 
    # mutate(spouse_mothered = ifelse(spouse_mothered<=4, "LEHS",
    #                                 ifelse(spouse_mothered>4 & spouse_mothered<=6, "SomeC",
    #                                        ifelse(spouse_mothered> 6 & spouse_mothered<=8, "College", NA)))) %>% 
    group_by(uniqueID) %>% fill(spouse_mothered, .direction=c("downup"))
  
  alldata <- expand.grid(uniqueID = unique(data$uniqueID))
  alldata <- left_join(alldata, headfather)
  alldata <- left_join(alldata, headmother)
  alldata <- left_join(alldata, spousefather)
  alldata <- left_join(alldata, spousemother)
  alldata$year <- as.numeric(alldata$year)
  return(alldata)
}

code_education_parent <- function(data){
  data <- data %>% 
    mutate(mothers_ed_alt = ifelse(relationship=="head", head_mothered,
                                   ifelse(relationship=="wife", spouse_mothered, NA)),
           fathers_ed_alt = ifelse(relationship=="head", head_fathered,
                                   ifelse(relationship=="wife", spouse_fathered, NA)),
           mothers_ed_alt = ifelse(mothers_ed_alt<=4, 1,
                                ifelse(mothers_ed_alt>4 & mothers_ed_alt<=6, 2,
                                       ifelse(mothers_ed_alt> 6 & mothers_ed_alt<=8, 3,NA))),
           fathers_ed_alt = ifelse(fathers_ed_alt<=4, 1,
                                          ifelse(fathers_ed_alt>4 & fathers_ed_alt<=6, 2,
                                                 ifelse(fathers_ed_alt> 6 & fathers_ed_alt<=8, 3,NA))),
           motherseducation = ifelse(motherseducation<=12, 1,
                                     ifelse(motherseducation>12 & motherseducation<16, 2,
                                            ifelse(motherseducation>=16, 3, NA))),
           fatherseducation = ifelse(fatherseducation<=12, 1,
                                     ifelse(fatherseducation>12 & fatherseducation<16, 2,
                                            ifelse(fatherseducation>=16, 3, NA))))
  data$mothers_ed_final <- ifelse(is.na(data$motherseducation), data$mothers_ed_alt, data$motherseducation)
  data$fathers_ed_final <- ifelse(is.na(data$fatherseducation), data$fathers_ed_alt, data$fatherseducation)
  data <- data %>% dplyr::select(uniqueID, year, mothers_ed_final, fathers_ed_final) %>% 
    group_by(uniqueID) %>% 
    fill(mothers_ed_final, .direction="downup") %>% 
    fill(fathers_ed_final, .direction="downup") %>% 
    rowwise() %>% 
    mutate(sumparented = sum(mothers_ed_final,fathers_ed_final, na.rm=TRUE),
           onecollege = ifelse(mothers_ed_final == 3 | fathers_ed_final==3, 1,0),
           onecollegeplus = ifelse(onecollege==1, 1,
                                   ifelse(is.na(mothers_ed_final) & is.na(fathers_ed_final), NA,
                                          0)))

  return(data)
}

process_kessler <- function(data){
  ###Using Kesslers scale (K6) total score variable (0-24)
  ###Kessler scale aggregates scores across 6 variables- Sadness, Nervousness, Restlessness, Hopelessness,Effortlessness and Worthlessness)
  varlist<-c("ER19833A", "ER23268", "ER40402", "ER46375", "ER51736", "ER57482", "ER64604", "ER70680", "ER76688")
  years <- c(2001, 2003, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
  kessler <- data %>% dplyr::select(uniqueID, c(varlist))
  names(kessler)[2:10] <- years
  kessler <- kessler %>% pivot_longer(cols='2001':'2019', names_to="year", values_to="kessler_score") %>% 
    group_by(uniqueID) %>% fill(kessler_score, .direction=c("downup"))
  kessler$kessler_score <- ifelse(kessler$kessler_score==99|kessler$kessler_score==98, NA, kessler$kessler_score)
  kessler$year <- as.numeric(kessler$year)
  return(kessler)
}

recode_PSID_vars <- function(data, varlist, variable, years){
  newdata <- data %>%
    mutate(origINTNO = ER30001,
           ID = ER30002,
           uniqueID = (origINTNO*1000) + ID,
           sex = recode(as.factor(ER32000), "1"="male", "2"="female")) %>%
    dplyr::select(uniqueID, sex, c(varlist))
  names(newdata)[3:length(newdata)] <- years
  newdata <- newdata %>% pivot_longer(cols=as.character(min(years)):'2019', names_to="year", values_to=variable) %>%
    mutate(year=as.numeric(as.character(year)))
  return(newdata)
}


process_employment <- function(data){
  varlist <- c("ER30293", "ER30323", "ER30353", "ER30382", "ER30411",
          "ER30441", "ER30474", "ER30509", "ER30545", "ER30580","ER30616", "ER30653",
          "ER30699", "ER30744", "ER30816", "ER33111", "ER33211", "ER33311", "ER33411",
          "ER33512", "ER33612", "ER33712", "ER33813", "ER33913", "ER34016", "ER34116",
          "ER34216", "ER34317", "ER34516", "ER34716")
  years<-c(1979:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
  employment <- recode_PSID_vars(data=data, varlist, "employment_stat", years)
  employment$employment_stat <- ifelse(employment$employment_stat==1, 1,
                                       ifelse(employment$employment_stat>1, 0, NA))
  return(employment)
}

process_income <- function(data){
  varlist <- c("V81", "V529", "V1514", "V2226", "V2852", "V3256",
               "V3676", "V4154", "V5029", "V5626", "V6173", "V6766", "V7412",
               "V8065", "V8689", "V9375", "V11022", "V12371", "V13623", "V14670",
               "V16144", "V17533", "V18875", "V20175", "V21481", "V23322", "ER4153",
               "ER6993", "ER9244", "ER12079", "ER16462", "ER20456", "ER24099", "ER28037",
               "ER41027", "ER46935", "ER52343", "ER58152", "ER65349", "ER71426", "ER77448")
  years <- c(1968:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
  income <- recode_PSID_vars(data=data, varlist, "total_fam_income", years)
  return(income)
}

process_homeowner <- function(data){
  varlist <- c("V103", "V593", "V1264", "V1967","V2566", "V3108", "V3522", "V3939",
               "V4450", "V5364", "V5864", "V6479", "V7084", "V7675", "V8364", "V8974",
               "V10437", "V11618", "V13023", "V14126", "V15140", "V16641", "V18072",
               "V19372", "V20672", "V22427", "ER2032", "ER5031", "ER7031", "ER10035",
               "ER13040", "ER17043", "ER21042", "ER25028", "ER36028", "ER42029", "ER47329",
               "ER53029", "ER60030", "ER66030", "ER72030")
  years <- c(1968:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
  homeowner <- recode_PSID_vars(data=data, varlist, "homeowner", years)
  homeowner$homeowner <- ifelse(homeowner$homeowner==1, "owns",
                                ifelse(homeowner$homeowner==5, "rents",
                                       ifelse(homeowner$homeowner==8, "neither", homeowner$homeowner)))
  return(homeowner)
}



process_alcohol <- function(data){
  
  years<-c(2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
  # variables for "ever drink" for household head
  varlist <- c("ER27105", "ER38316", "ER44289", "ER49627", "ER55375", "ER62497", "ER68562", "ER74570")
  everdrinkalcoholhd <- recode_PSID_vars(data=data, varlist, "everdrinkhd", years)
  everdrinkalcoholhd$everdrinkhd <- ifelse(everdrinkalcoholhd$everdrinkhd==9|everdrinkalcoholhd$everdrinkhd==8, NA, everdrinkalcoholhd$everdrinkhd)

  # variables for "ever drink" for spouse
  varlist<-c("ER27228", "ER39413", "ER45386", "ER50745", "ER56491", "ER63613", "ER69689", "ER75697")
  everdrinkalcoholspouse <- recode_PSID_vars(data=data, varlist, "everdrinkspouse", years)
  everdrinkalcoholspouse$everdrinkspouse <- ifelse(everdrinkalcoholspouse$everdrinkspouse==9|everdrinkalcoholspouse$everdrinkspouse==8, NA, everdrinkalcoholspouse$everdrinkspouse)

  # variables for "drinking frequency" for household head
  varlist<-c("ER27106", "ER38317", "ER44290", "ER49628", "ER55376", "ER62498", "ER68563", "ER74571")
  frequencydrinkhd <- recode_PSID_vars(data=data, varlist, "frequencydrinkhd", years)
  frequencydrinkhd$frequencydrinkhd <- ifelse(frequencydrinkhd$frequencydrinkhd==9|frequencydrinkhd$frequencydrinkhd==8, NA, frequencydrinkhd$frequencydrinkhd)

  # variables for "drinking frequency" for spouse
  varlist<-c("ER27229", "ER39414", "ER45387", "ER50746", "ER56492", "ER63614", "ER69690", "ER75698")
  frequencydrinkspouse <- recode_PSID_vars(data=data, varlist, "frequencydrinkspouse", years)
  frequencydrinkspouse$frequencydrinkspouse <- ifelse(frequencydrinkspouse$frequencydrinkspouse==9|frequencydrinkspouse$frequencydrinkspouse==8, NA, frequencydrinkspouse$frequencydrinkspouse)

  # variables for "drinking quantity" for household head
  varlist<-c("ER27107", "ER38318", "ER44291", "ER49629", "ER55377", "ER62499", "ER68564", "ER74572")
  drinksperdayhd <- recode_PSID_vars(data=data, varlist, "drinksperdayhd", years)
  drinksperdayhd$drinksperdayhd <- ifelse(drinksperdayhd$drinksperdayhd==99|drinksperdayhd$drinksperdayhd==98, NA, drinksperdayhd$drinksperdayhd)

  # variables for "drinking quantity" for spouse
  varlist<-c("ER27230", "ER39415", "ER45388", "ER50747", "ER56493", "ER63615", "ER69691", "ER75699")
  drinksperdayspouse <- recode_PSID_vars(data=data, varlist, "drinksperdayspouse", years)
  drinksperdayspouse$drinksperdayspouse <- ifelse(drinksperdayspouse$drinksperdayspouse==99|drinksperdayspouse$drinksperdayspouse==98, NA, drinksperdayspouse$drinksperdayspouse)

  # variables for "binge drinking" for household head (no. of days had five drinks in year)
  varlist<-c("ER27108", "ER38319", "ER44292", "ER49630", "ER55378", "ER62500", "ER68565", "ER74573")
  bingedrinkhd <- recode_PSID_vars(data=data, varlist, "bingedrinkhd", years)
  bingedrinkhd$bingedrinkhd <- ifelse(bingedrinkhd$bingedrinkhd==999|bingedrinkhd$bingedrinkhd==998, NA, bingedrinkhd$bingedrinkhd)

  # variables for "binge drinking" for spouse (no. of days had four drinks in year)
  varlist<-c("ER27231", "ER39416", "ER45389", "ER50748", "ER56494", "ER63616", "ER69692", "ER75700")
  bingedrinkspouse <- recode_PSID_vars(data=data, varlist, "bingedrinkspouse", years)
  bingedrinkspouse$bingedrinkspouse <- ifelse(bingedrinkspouse$bingedrinkspouse==999|bingedrinkspouse$bingedrinkspouse==998, NA, bingedrinkspouse$bingedrinkspouse)

  drinking <- left_join(everdrinkalcoholhd, everdrinkalcoholspouse) %>% 
    left_join(., frequencydrinkhd) %>% left_join(., frequencydrinkspouse) %>% 
    left_join(., drinksperdayhd) %>% left_join(., drinksperdayspouse) %>% 
    left_join(., bingedrinkhd) %>% left_join(., bingedrinkspouse)
  return(drinking)
}

recode_alcohol <- function(data){
  data <- data %>%
    mutate(drinkingstatus = ifelse(relationship=="head", everdrinkhd,
                                   ifelse(relationship=="wife/partner" | relationship=="husbandofhead", everdrinkspouse,
                                          ifelse(relationship=="cohabitor", everdrinkspouse, NA))),
           drinkingstatus = ifelse(drinkingstatus==1,1,
                                   ifelse(drinkingstatus==5,0,NA))) %>%
    dplyr::select(-c(everdrinkhd, everdrinkspouse))
  #drinking quantity
  data <- data %>%
    mutate(quantity = ifelse(relationship=="head", drinksperdayhd,
                             ifelse(relationship=="wife/partner" | relationship=="husbandofhead", drinksperdayspouse,
                                    ifelse(relationship=="cohabitor", drinksperdayspouse,NA)))) %>%
    dplyr::select(-c(drinksperdayhd, drinksperdayspouse))
  
  # drinking frequency (frequencydrinkfinal)
  data <- data %>%
    mutate(frequency = ifelse(relationship=="head", frequencydrinkhd,
                              ifelse(relationship=="wife/partner" | relationship=="husbandofhead", frequencydrinkspouse,
                                     ifelse(relationship=="cohabitor", frequencydrinkspouse, NA))),
           frequency = ifelse(frequency==1, 1, 
                              ifelse(frequency==2, 1.5, 
                                     ifelse(frequency==3, 3.5,
                                            ifelse(frequency==4, 5,
                                                   ifelse(frequency==5, 12,
                                                          ifelse(frequency==6, 30, frequency))))))) %>% 
    dplyr::select(-c(frequencydrinkhd,frequencydrinkspouse)) %>% 
    mutate(gpd = (quantity*frequency*14)/30)
  
  # binge drinking
  data$bingedrinkdays<-ifelse(data$relationship=="head", data$bingedrinkhd,
                                ifelse(data$relationship=="wife/partner" | data$relationship=="husbandofhead", data$bingedrinkspouse,
                                       ifelse(data$relationship=="cohabitor", data$bingedrinkspouse,
                                              NA)))
  
  #Recoding to WHO alcohol categories
  data <- data %>%
    mutate(AlcCAT = ifelse(gpd==0,"Non-drinker",
                           ifelse(sex=="male" & gpd>0 & gpd<=40,"Low risk",
                                  ifelse(sex=="female" & gpd>0 & gpd<=20,"Low risk",
                                         ifelse(sex=="male" & gpd>40 & gpd<=60,"Medium risk",
                                                ifelse(sex=="female" & gpd>20 & gpd<=40,"Medium risk",
                                                       ifelse(sex=="male" & gpd>60 & gpd<=100,"High risk",
                                                              ifelse(sex=="female" & gpd>40 & gpd<=60,"High risk",
                                                                     ifelse(sex=="male" & gpd>100,"Very high risk",
                                                                            ifelse(sex=="female" & gpd>60,"Very high risk",NA))))))))))
  return(data)
  
}
