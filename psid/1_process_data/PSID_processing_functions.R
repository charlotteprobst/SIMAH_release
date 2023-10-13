# PSID processing functions

process_education <- function(data){
  varnames <- c("ER30010", "ER30052","ER30076", "ER30100", "ER30126", "ER30147","ER30169", "ER30197", "ER30226", "ER30255",
                     "ER30296", "ER30326", "ER30356", "ER30384",  "ER30413",  "ER30443", "ER30478", "ER30513", "ER30549",
                     "ER30584", "ER30620", "ER30657", "ER30703", "ER30748", "ER30820",  "ER33115", "ER33215", "ER33315", "ER33415",
                     "ER33516","ER33616","ER33716","ER33817","ER33917","ER34020","ER34119","ER34230","ER34349","ER34548", "ER34752", "ER34952")
  
  newdata <- data %>% dplyr::select(uniqueID, familyID, all_of(varnames))
  years <- c(1968, 1970:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)
  names(newdata)[3:43] <- years
  newdata <- newdata %>% pivot_longer(cols='1968':'2021', names_to="year", values_to="education") %>% 
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
    # fill education, first down, then up
    fill(education, .direction=c("downup")) %>% fill(education_cat, .direction=c("downup")) %>% 
    fill(education_cat_detailed, .direction=c("downup")) %>% ungroup()
  return(newdata)
}

process_TAS_education <- function(data){
  varlist<-c("TA110687", "TA130707", "TA150717", "TA170780", "TA190917")
  years <- c(2011, 2013, 2015, 2017, 2019)
  ed <- data %>% dplyr::select(uniqueID, all_of(varlist))
  names(ed)[2:6] <- years
  ed <- ed %>% pivot_longer(cols='2011':'2019', names_to="year", values_to="TAS_education") %>% 
    mutate(TAS_education = ifelse(TAS_education==0, NA,
                                  ifelse(TAS_education>=96, NA, TAS_education))) %>% 
    group_by(uniqueID) %>% fill(TAS_education, .direction=c("down"))
  return(ed)
}

process_age <- function(data){
  
varnames <-   c("ER30004", "ER30023", "ER30046", "ER30070", "ER30094", "ER30120", "ER30141", "ER30163", "ER30191", "ER30220",
                "ER30249", "ER30286", "ER30316", "ER30346", "ER30376", "ER30402", "ER30432", "ER30466", "ER30501", "ER30538",
                "ER30573", "ER30609", "ER30645", "ER30692", "ER30736", "ER30809", "ER33104", "ER33204", "ER33304", "ER33404",
                "ER33504", "ER33604", "ER33704", "ER33804", "ER33904", "ER34004", "ER34104", "ER34204", "ER34305", "ER34504",
                "ER34704", "ER34904")
newdata <- data %>% dplyr::select(uniqueID, all_of(c(varnames)))
years <- c(1968:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)
names(newdata)[2:43] <- years
newdata <- newdata %>% pivot_longer(cols='1968':'2021', names_to="year", values_to="age") %>% 
  mutate(age = ifelse(age==0, NA, 
                      ifelse(age==999, NA, age)),
         year = as.numeric(year),
         birthyear = year - age) %>% 
  group_by(uniqueID) %>% 
  reframe(birthyear = unique(birthyear),
            birthyear = mean(birthyear, na.rm=T)) %>% distinct()
return(newdata)
}

process_relationship <- function(data){
  
varnames <- c("ER30003", "ER30022", "ER30045", "ER30069", "ER30093", "ER30119", "ER30140", "ER30162", "ER30190",
              "ER30219", "ER30248", "ER30285", "ER30315", "ER30345", "ER30375", "ER30401", "ER30431", "ER30465",
              "ER30500", "ER30537", "ER30572", "ER30608", "ER30644", "ER30691", "ER30735", "ER30808", "ER33103",
              "ER33203", "ER33303", "ER33403","ER33503","ER33603","ER33703","ER33803","ER33903","ER34003","ER34103",
              "ER34203","ER34303","ER34503", "ER34703", "ER34903")
newdata <- data %>% dplyr::select(uniqueID, ER30001, all_of(c(varnames)))
years <- c(1968:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)
names(newdata)[3:44] <- years
newdata <- newdata %>% pivot_longer(cols='1968':'2021', names_to="year", values_to="relationship") %>% 
  mutate(relationship = ifelse(relationship==10 | relationship==1, "head",
                                           ifelse(relationship==20 | 
                                                    relationship==22 | relationship==2, 
                                                  "wife/partner",
                                                  ifelse(relationship==30 | relationship==37 | relationship==38 |
                                                           relationship==3, "childofhead",
                                                         ifelse(relationship==33 | relationship==35, "childofpartner",
                                                                ifelse(relationship==0, "born after this year or nonresponse",
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
))))))))))))))),
relationship = ifelse(ER30001>=3001 & ER30001<=3511, "Immigrant/Latino",
                      ifelse(ER30001>=4001 & ER30001<=4851, "Immigrant/Latino",
                             ifelse(ER30001>=7001 & ER30001<=9308, "Immigrant/Latino", relationship))))
newdata$year <- as.numeric(newdata$year)
return(newdata)
}

process_sample_weights <- function(data){
familyweights <- c("ER16518","ER20394","ER24179","ER28078","ER41069","ER47012","ER52436",
                   "ER58257","ER65492","ER71570","ER77631", "ER81958")
data$familyID <- data$ER30001
data$ID <- data$ER30002
data$uniqueID <- (data$familyID*1000) + data$ID

weights <- data %>% dplyr::select(uniqueID, all_of(familyweights))

years <- c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)

names(weights)[2:13] <- years
weights <- weights %>% pivot_longer(cols='1999':'2021', names_to="year", values_to="weight") %>% 
  mutate(year=as.integer(year))

return(weights)
}

# PROCESSING RACE

###### FUNCTION 1: PROCESS_RACE

process_race <- function(data){

  # Race of head
  race <- c("V181","V801","V1490","V2202","V2828","V3300","V3720","V4204","V5096","V5662","V6209","V6802",
            "V7447","V8099","V8723","V9408","V11055","V11938","V13565","V14612","V16086",
            "V17483","V18814","V20114","V21420","V23276","ER3944","ER6814","ER9060","ER11848","ER15928",
            "ER19989","ER23426","ER27393","ER40565","ER46543","ER51904","ER57659","ER64810","ER70882", "ER76897",
            "ER81144")
  
  racevars <- data %>% dplyr::select(uniqueID, familyID, IDmother, IDfather, sex, all_of(race))
  years <- c(1968:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)
  names(racevars)[6:47] <- years
  racevars <- racevars %>% pivot_longer(cols='1968':'2021', names_to="year", values_to="racehead") %>% 
    mutate(racehead = 
             case_when(
               # White
               racehead==1 ~ "white",
               # Black
               racehead==2 ~ "black",
               # Hispanic
               year <=1984 & racehead==3 | year>=1990 & racehead==5 ~ "hispanic",
               # Native American
               year>=1985 & racehead==3 ~ "Native",
               # Asian/PI
               racehead==4 & year>=1985 | racehead==5 & year>=2005 ~ "Asian/PI", # recoding of 5 as Asian here is a new addition
               # other
               racehead==7 | racehead==8 ~ "other",
               # NA
               racehead==9 | racehead==0 ~ NA))
 
  # Race of wife
  wife <- c("V12293","V13500","V14547","V16021","V17418","V18749","V20049","V21355","V23212",
            "ER3883","ER6753","ER8999","ER11760","ER15836","ER19897","ER23334","ER27297",
            "ER40472","ER46449","ER51810","ER57549","ER64671","ER70744","ER76752", "ER81017")
  wiferace <- data %>% dplyr::select(uniqueID, all_of(wife))
  years <- c(1985:1997,1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)
  names(wiferace)[2:26] <- years
  wiferace <- wiferace %>% pivot_longer(cols='1985':'2021', names_to="year", values_to="racewife") %>% 
    mutate(racewife = 
             case_when(
               # White
               racewife==1 ~ "white",
               # Black
               racewife==2 ~ "black",
               # Hispanic
               year <=1984 & racewife==3| year>=1990 & racewife==5 ~ "hispanic", 
               # Native American
               year>=1985 & racewife==3 ~ "Native",
               # Asian/PI
               racewife==4 & year>=1985 | racewife==5 & year>=2005 ~ "Asian/PI",
               # other
               racewife==7| racewife==8 ~ "other",
               # NA
               racewife==9| racewife==0 ~ NA))
  
  # Hispanic status of head
  hispanic1 <- c("V11937","V13564","V14611","V16085","V17482","V18813","V20113",
                 "V21419","V23275","ER3941","ER6811","ER9057","ER27392","ER40564",
                 "ER46542","ER51903","ER57658","ER64809","ER70881","ER76896", "ER81143")
  years <- c(1985:1996, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)
  hispanic <- data %>% dplyr::select(uniqueID, all_of(hispanic1))
  names(hispanic)[2:22] <- years
  hispanic <- hispanic %>% pivot_longer(cols='1985':'2021', names_to="year", values_to="hispanichead") %>% 
    mutate(hispanichead = case_when(
      # Not hispanic
      hispanichead==0 ~ "nothispanic",
      # Hispanic
      hispanichead==1| hispanichead==2| hispanichead ==3| hispanichead==4| hispanichead==5| hispanichead==6| hispanichead==7 ~ "hispanic",
      # NA
      hispanichead==9 ~ NA))
  
  # Hispanic status of wife 
  hispanic2 <- c("V12292","V13499","V14546","V16020",
                 "V17417","V18748","V20048","V21354",
                 "V23211","ER3880","ER6750","ER8996",
                 "ER27296","ER40471","ER46448","ER51809",
                 "ER57548","ER64670","ER70743","ER76751", "ER81016")
  hispanicwife <- data %>% dplyr::select(uniqueID, all_of(hispanic2))
  years <- c(1985:1996, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)
  names(hispanicwife)[2:22] <- years
  hispanicwife <- hispanicwife %>% pivot_longer(cols='1985':'2021', names_to="year", values_to="hispanicwife") %>% 
    mutate(hispanicwife= case_when(
      # Not hispanic
      hispanicwife==0 ~ "nothispanic",
      # Hispanic
      hispanicwife==1| hispanicwife==2| hispanicwife==3| hispanicwife==4| hispanicwife==5| hispanicwife==6| hispanicwife==7 ~ "hispanic",
      # NA
      hispanicwife==9 ~ NA))
  
  ## Combine information from race variable and hispanic variable to generate a combined 'raceeth' variable
  
  # Merge data
  alldata <- expand.grid(uniqueID = unique(data$uniqueID), 
                         year =  as.character(c(1968:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)))
  raceall <- left_join(alldata, racevars)
  raceall <- left_join(raceall, wiferace)
  raceall <- left_join(raceall, hispanic)
  raceall <- left_join(raceall, hispanicwife)
  raceall$year <- as.numeric(raceall$year)
  
  # Resolve any conflicts so that if either one of the ethnicity or race variables is recorded as Hispanic that will be assigned as the individuals primary raceeth:
  # Gives resulting categories: NH White, NH Black, NH Asian, NH other, Hispanic (i.e. Hispanic White, Hispanic other, Hispanic Black etc. all included within Hispanic)
  # nb. pre 1985 no Hispanic ethnicity Q available. 
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

###### FUNCTION 2: GENERATE_FAMILY_RACE (previously "recode_race(type=T)")
generate_family_race <- function(data){
  # generate a grid with all possible combinations of head and wife raceeth
  head <- c("black","white","hispanic","other","Native","Asian/PI")
  wife <- c("black","white","hispanic","other","Native","Asian/PI")
  combos <- expand.grid(head,wife)
  # rename the column headings
  names(combos) <- c("head","wife") 
  combos$head <- as.character(combos$head)
  combos$wife <- as.character(combos$wife)

# Generate a new variable, "racefamily_both_known", when both the head and wife have race data available
  # Assign racefamily_both_known based on the highest priority rated Race, with the following order:
  # Hispanic > Black > Native > Asian/PI > other > White
  combos <- combos %>% mutate(
    racefamily_both_known = case_when(
      head==wife ~ head,
      head=="hispanic"|wife=="hispanic" ~ "hispanic",
      head=="black" | wife=="black" ~ "black",
      head=="Native" | wife=="Native" ~ "Native",
      head=="Asian/PI" | wife=="Asian/PI" ~ "Asian/PI",
      head=="other" | wife=="other" ~ "other",
      head=="white" | wife=="white" ~ "white"))
  combos$combo <- paste(combos$head,combos$wife,sep="")
  
# Generate a new variable "race_family_best_guess", based on all available head or wife have data
  data$combo <- paste(data$raceethhead, data$raceethwife, sep="")
  data <- left_join(data, combos)
  data <- data %>% mutate(
    racefamily_best_guess = case_when(
      head==wife ~ head,
      head=="hispanic"|wife=="hispanic" ~ "hispanic",
      head=="black" | wife=="black" ~ "black",
      head=="Native" | wife=="Native" ~ "Native",
      head=="Asian/PI" | wife=="Asian/PI" ~ "Asian/PI",
      head=="other" | wife=="other" ~ "other",
      head=="white" | wife=="white" ~ "white",
      combo=="hispanicNA" | combo=="NAhispanic" ~ "hispanic",
      combo=="blackNA" | combo=="NAblack" ~ "black",
      combo=="NativeNA" | combo=="NANative" ~ "Native",
      combo=="Asian/PINA" | combo=="NAAsian/PI" ~ "Asian/PI",
      combo=="otherNA" | combo=="NAother" ~ "other",
      combo=="whiteNA" | combo=="NAwhite" ~ "white",
      combo=="NANA" ~ NA),
    racefamily_one_known = case_when(
      combo=="hispanicNA" | combo=="NAhispanic" ~ "hispanic",
      combo=="blackNA" | combo=="NAblack" ~ "black",
      combo=="NativeNA" | combo=="NANative" ~ "Native",
      combo=="Asian/PINA" | combo=="NAAsian/PI" ~ "Asian/PI",
      combo=="otherNA" | combo=="NAother" ~ "other",
      combo=="whiteNA" | combo=="NAwhite" ~ "white",
      combo=="NANA" ~ NA)) %>%
    dplyr::select(-c("head", "wife"))
  return(data)
}

###### FUNCTION 3:ASSIGN_INDIVIDUAL_FAMILY_RACE (previously "individual_race(type = T)")
## Assign each individual their raceeth.  As the race question is only asked of the head and of their spouse directly, individuals may have their raceeth information imputed based on their nearest family member. 
# For example, the parent and child of the head is given the same race as the head.

# Allocate race based on the head or wife
assign_individual_family_race <- function(data){
  data <- data %>% mutate(
    individualrace = case_when(
      relationship=="head" | relationship=="childofhead" | relationship=="parentofhead" | relationship=="brotherofhead" ~ raceethhead,
      relationship=="wife/partner" | relationship=="childofpartner" | relationship=="parentofwife" | relationship=="brotherofwife" ~ raceethwife,
      relationship=="grandchild" ~ racefamily_best_guess,
      relationship=="born after this year or nonresponse" | is.na(relationship) | relationship=="Immigrant/Latino" | relationship=="nonrelative" | relationship=="cohabitor" ~ NA)) %>%
    dplyr::group_by(uniqueID)
  return(data)
}

###### FUNCTION 4: PROCESS_RACE_PARENTS
# All individuals have a unique ID and a IDmother (their mothers unique ID number).
# Most indiviudals have their parents in the dataset due to the survey design, and their parents will have info on their race/ethnicity
# from when they were interviewed as heads/wives themselves.
# We therefore generate a look up table for mothers and fathers, renaming the mothers UniqueID as IDmother
# in order to match them with their children, and add a column to each of their children, stating their mother's race 

process_race_parents <- function(df){
  mother <- df %>% ungroup() %>% dplyr::select(uniqueID, year, individualrace) %>% 
    rename(IDmother = uniqueID,
           mothersrace = individualrace) %>% distinct() 
  df <- left_join(df, mother)
  father <- df %>% ungroup() %>% dplyr::select(uniqueID, year, individualrace) %>% 
    rename(IDfather = uniqueID,
           fathersrace = individualrace) %>% distinct()
  df <- left_join(df, father)
  return(df)
}

###### FUNCTION 5: GENERATE_RACE_PARENTS (previously recode_race(type=F)")
# Generate a new variable, "racefamily_parents", based on the parents race data
generate_race_parents <- function(data) {
  data$parentscombo <- paste(data$mothersrace, data$fathersrace, sep="")
  # data <- left_join(data, combos)
  ## from here Hispanic, black, native, asian, other, white
  data %>% mutate(
    race_parents = case_when(
      mothersrace==fathersrace ~ fathersrace,
      fathersrace=="hispanic"|mothersrace=="hispanic" ~ "hispanic",
      fathersrace=="black" | mothersrace=="black" ~ "black",
      fathersrace=="Native" | mothersrace=="Native" ~ "Native",
      fathersrace=="Asian/PI" | mothersrace=="Asian/PI" ~ "Asian/PI",
      fathersrace=="other" | mothersrace=="other" ~ "other",
      fathersrace=="white" | mothersrace=="white" ~ "white",
      parentscombo=="hispanicNA" | combo=="NAhispanic" ~ "hispanic",
      parentscombo=="blackNA" | combo=="NAblack" ~ "black",
      parentscombo=="NativeNA" | combo=="NANative" ~ "Native",
      parentscombo=="Asian/PINA" | combo=="NAAsian/PI"  ~ "Asian/PI",
      parentscombo=="otherNA" | combo=="NAother" ~ "other",
      parentscombo=="whiteNA" | combo=="NAwhite" ~ "white",
      parentscombo=="NANA" ~ NA)
  ) %>% 
    return(data)
}

###### FUNCTION 6: INDIVIDUAL_RACE_PARENTS
# If race not reported, or unknown for head and wife, allocate race based on the parents
assign_individual_race_parents <- function(data){    
  toallocate <- data[is.na(data$individualrace),]
  toallocate <- toallocate %>% 
    mutate(individualrace = race_parents)
  data <- data %>% drop_na(individualrace)
  data <- rbind(data, toallocate) 
  return(data)
}

###### FUNCTION 7 ASSIGN_RACE_METHOD

assign_race_method <- function(data){
  data <- data %>% mutate(
    race_method = case_when(
      relationship=="head" & !(is.na(raceethhead))  ~ "self reported",
      relationship=="wife/partner" & !(is.na(raceethwife))  ~ "reported by head",
      (relationship=="head" | relationship=="childofhead" | relationship=="parentofhead" | relationship=="brotherofhead") & !(is.na(raceethhead))| 
      (relationship=="wife" | relationship=="childofpartner" | relationship=="parentofwife" | relationship=="brotherofwife") & !(is.na(raceethwife))| 
      relationship=="grandchild" & !(is.na(racefamily_best_guess)) ~ "imputed based on nearest family member",
      !(is.na(individualrace)) & !(is.na(race_parents)) ~ "imputed based on parents",
      individualrace==NA ~ NA))
  return(data)
}

assign_race_method_TAS <- function(data){
  data <- data %>% mutate(
    race_method = ifelse(is.na(TAS_race), race_method, "self reported"))
  return(data)
}

###### FUNCTION 8 PROCESS_TAS_RACE
process_TAS_race <- function(data){
  varlist<-c("TA050884", "TA070865", "TA090925", "TA111057", "TA131092", "TA151132", "TA171955", "TA192131")
  years <- c(2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019) # 2021 data not yet available
  race <- data %>% dplyr::select(uniqueID, all_of(varlist))
  names(race)[2:9] <- years
  race <- race %>% pivot_longer(cols='2005':'2019', names_to="year", values_to="TAS_race") %>% 
    mutate(TAS_race = 
             case_when(
               # White
               TAS_race==1 ~ "white",
               # Black
               TAS_race==2 & year<=2015 | TAS_race==3 & year>=2017 ~ "black", 
               # Hispanic
               TAS_race==2 | year>=2017 ~ "hispanic",
               # Native American
               TAS_race==3 & year<=2015 | TAS_race==5 & year>=2017 ~ "Native",
               # Asian/PI
               TAS_race==4 | TAS_race==5 & year <=2015 | TAS_race==7 & year>=2017 ~ "Asian/PI", # recoding of 5 as Asian here is a new addition
               # other
               TAS_race==7 & year<=2015 | TAS_race==6 & year>=2017 | TAS_race==8 & year>=2017 ~ "other",
               # NA
               TAS_race==6 & year<=2015 | TAS_race==8 & year<=2015 | TAS_race==8 | TAS_race==9 | TAS_race==98 | TAS_race==99 ~ NA)) %>% 
    dplyr::select(uniqueID, TAS_race, year) %>% distinct()
  return(race)
}
# 
# ###### FUNCTION 8: ALLOCATE_RACE_FINAL()
# # ensure that each person has a unique value for race and ethnicity over time
# allocate_final_race <- function(data){
#   races <- unique(data$race_new)
#   # recode according to hierarchy - black, hispanic, native american, asian/pi, other
#   newrace <- ifelse("black" %in% races, "black",
#                     ifelse("hispanic" %in% races, "hispanic", 
#                            ifelse("Native" %in% races, "Native",
#                                   ifelse("Asian/PI" %in% races, "Asian/PI",
#                                          ifelse("other" %in% races, "other",
#                                                 ifelse("white" %in% races, "white", NA))))))
#   data$race_new_unique <- newrace
#   return(data)
# }

process_kessler <- function(data){
  ###Using Kesslers scale (K6) total score variable (0-24)
  ###Kessler scale aggregates scores across 6 variables- Sadness, Nervousness, Restlessness, Hopelessness,Effortlessness and Worthlessness)
  varlist<-c("ER19833A", "ER23268", "ER40402", "ER46375", "ER51736", "ER57482", "ER64604", "ER70680", "ER76688", "ER80952")
  years <- c(2001, 2003, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)
  kessler <- data %>% dplyr::select(uniqueID, all_of(varlist))
  names(kessler)[2:11] <- years
  kessler <- kessler %>% pivot_longer(cols='2001':'2021', names_to="year", values_to="kessler_score") %>% 
    group_by(uniqueID) %>% fill(kessler_score, .direction=c("downup"))
  kessler$kessler_score <- ifelse(kessler$kessler_score==99|kessler$kessler_score==98, NA, kessler$kessler_score)
  kessler$year <- as.numeric(kessler$year)
  return(kessler)
}

## recode_PSID_vars does not currently seem to be called upon in the 1_PSID_processing_clean script.
recode_PSID_vars <- function(data, varlist, variable, years){
  newdata <- data %>%
    mutate(origINTNO = ER30001,
           ID = ER30002,
           uniqueID = (origINTNO*1000) + ID,
           sex = recode(as.factor(ER32000), "1"="male", "2"="female")) %>%
    dplyr::select(uniqueID, sex, all_of(varlist))
  names(newdata)[3:length(newdata)] <- years
  newdata <- newdata %>% pivot_longer(cols=as.character(min(years)):'2021', names_to="year", values_to=variable) %>%
    mutate(year=as.numeric(as.character(year)))
  return(newdata)
}

process_employment <- function(data){
  varlist <- c("ER30293", "ER30323", "ER30353", "ER30382", "ER30411",
          "ER30441", "ER30474", "ER30509", "ER30545", "ER30580","ER30616", "ER30653",
          "ER30699", "ER30744", "ER30816", "ER33111", "ER33211", "ER33311", "ER33411",
          "ER33512", "ER33612", "ER33712", "ER33813", "ER33913", "ER34016", "ER34116",
          "ER34216", "ER34317", "ER34516", "ER34716", "ER34916")
  years<-c(1979:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)
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
               "ER41027", "ER46935", "ER52343", "ER58152", "ER65349", "ER71426", "ER77448", "ER81775")
  years <- c(1968:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)
  income <- recode_PSID_vars(data=data, varlist, "total_fam_income", years)
  return(income)
}

process_homeowner <- function(data){
  varlist <- c("V103", "V593", "V1264", "V1967","V2566", "V3108", "V3522", "V3939",
               "V4450", "V5364", "V5864", "V6479", "V7084", "V7675", "V8364", "V8974",
               "V10437", "V11618", "V13023", "V14126", "V15140", "V16641", "V18072",
               "V19372", "V20672", "V22427", "ER2032", "ER5031", "ER7031", "ER10035",
               "ER13040", "ER17043", "ER21042", "ER25028", "ER36028", "ER42029", "ER47329",
               "ER53029", "ER60030", "ER66030", "ER72030", "ER78031")
  years <- c(1968:1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)
  homeowner <- recode_PSID_vars(data=data, varlist, "homeowner", years)
  homeowner$homeowner <- ifelse(homeowner$homeowner==1, "owns",
                                ifelse(homeowner$homeowner==5, "rents",
                                       ifelse(homeowner$homeowner==8, "neither", homeowner$homeowner)))
  return(homeowner)
}

process_alcohol <- function(data){
  
  years<-c(2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021)
  # variables for "ever drink" for household head
  varlist <- c("ER27105", "ER38316", "ER44289", "ER49627", "ER55375", "ER62497", "ER68562", "ER74570", "ER80721")
  everdrinkalcoholhd <- recode_PSID_vars(data=data, varlist, "everdrinkhd", years)
  everdrinkalcoholhd$everdrinkhd <- ifelse(everdrinkalcoholhd$everdrinkhd==9|everdrinkalcoholhd$everdrinkhd==8, NA, everdrinkalcoholhd$everdrinkhd)

  # variables for "ever drink" for spouse
  varlist<-c("ER27228", "ER39413", "ER45386", "ER50745", "ER56491", "ER63613", "ER69689", "ER75697", "ER80919")
  everdrinkalcoholspouse <- recode_PSID_vars(data=data, varlist, "everdrinkspouse", years)
  everdrinkalcoholspouse$everdrinkspouse <- ifelse(everdrinkalcoholspouse$everdrinkspouse==9|everdrinkalcoholspouse$everdrinkspouse==8, NA, everdrinkalcoholspouse$everdrinkspouse)

  # variables for "drinking frequency" for household head
  varlist<-c("ER27106", "ER38317", "ER44290", "ER49628", "ER55376", "ER62498", "ER68563", "ER74571", "ER80722")
  frequencydrinkhd <- recode_PSID_vars(data=data, varlist, "frequencydrinkhd", years)
  frequencydrinkhd$frequencydrinkhd <- ifelse(frequencydrinkhd$frequencydrinkhd==9|frequencydrinkhd$frequencydrinkhd==8, NA, frequencydrinkhd$frequencydrinkhd)

  # variables for "drinking frequency" for spouse
  varlist<-c("ER27229", "ER39414", "ER45387", "ER50746", "ER56492", "ER63614", "ER69690", "ER75698", "ER80920")
  frequencydrinkspouse <- recode_PSID_vars(data=data, varlist, "frequencydrinkspouse", years)
  frequencydrinkspouse$frequencydrinkspouse <- ifelse(frequencydrinkspouse$frequencydrinkspouse==9|frequencydrinkspouse$frequencydrinkspouse==8, NA, frequencydrinkspouse$frequencydrinkspouse)

  # variables for "drinking quantity" for household head
  varlist<-c("ER27107", "ER38318", "ER44291", "ER49629", "ER55377", "ER62499", "ER68564", "ER74572", "ER80723")
  drinksperdayhd <- recode_PSID_vars(data=data, varlist, "drinksperdayhd", years)
  drinksperdayhd$drinksperdayhd <- ifelse(drinksperdayhd$drinksperdayhd==99|drinksperdayhd$drinksperdayhd==98, NA, drinksperdayhd$drinksperdayhd)

  # variables for "drinking quantity" for spouse
  varlist<-c("ER27230", "ER39415", "ER45388", "ER50747", "ER56493", "ER63615", "ER69691", "ER75699", "ER80919")
  drinksperdayspouse <- recode_PSID_vars(data=data, varlist, "drinksperdayspouse", years)
  drinksperdayspouse$drinksperdayspouse <- ifelse(drinksperdayspouse$drinksperdayspouse==99|drinksperdayspouse$drinksperdayspouse==98, NA, drinksperdayspouse$drinksperdayspouse)

  # variables for "binge drinking" for household head (no. of days had five drinks in year)
  varlist<-c("ER27108", "ER38319", "ER44292", "ER49630", "ER55378", "ER62500", "ER68565", "ER74573", "ER80724")
  bingedrinkhd <- recode_PSID_vars(data=data, varlist, "bingedrinkhd", years)
  bingedrinkhd$bingedrinkhd <- ifelse(bingedrinkhd$bingedrinkhd==999|bingedrinkhd$bingedrinkhd==998, NA, bingedrinkhd$bingedrinkhd)

  # variables for "binge drinking" for spouse (no. of days had four drinks in year)
  varlist<-c("ER27231", "ER39416", "ER45389", "ER50748", "ER56494", "ER63616", "ER69692", "ER75700", "ER80922")
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

