# Process race new method

###### FUNCTION 1: PROCESS_RACE

process_race <- function(data){

## HEAD
  
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
             racehead==4 & year>=1985 | racewife==5 & year>=2005 ~ "Asian/PI", # recoding of 5 as Asian here is a new addition
             # Other
             racehead==7 | racehead==8 ~ "Other",
             # NA
             racehead==9 | racehead==0 ~ "NA"))
             
  # White coded as 1
  # Black coded as 2
  # Hispanic as race dependant on year (1968-1984 = 3,
  #                             1985-1989, no category,
  #                             1990-2003 = 5,
  #                             2004-2021, no category)
  # Native American dependant on year (<1985 no category,
  #                                    1985-2021 = 3)
  # Asian/PI dependant on year (<1985 no category,
  #                                 1985-2021 = 4,
  #                                  AND 2005-2021 = 5)
  # NA/DK/Refused always coded as 9/0
  # Other coded as 7/8
  
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
  
## WIFE
  
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
             # Other
             racewife==7| racewife==8 ~ "Other",
             # NA
             racewife==9| racewife==0 ~ "NA"))
  
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

# Resolve any conflicts as follows:
# Pre 1985: Take hispanic variable based on race data only (as no Hispanic ethnicity Q available)
# 1985 onward: If Hispanic recorded within Hispanic ethnicity variable, ignore race and code as hispanic
#              If Hispanic question is no, give race category (of which hispanic is an option)
# Gives resulting categories: NH White, NH Black, NH Asian, NH Other, Hispanic (i.e. Hispanic White, Hispanic Other, Hispanic Black etc. all included within Hispanic)
# NB. This method means that if either one of the hispanic ethnicity or race variables are recorded as Hispanic that will be the individuals primary catgeorisation.  
  
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

###### FUNCTION 2: RECODE_RACE

recode_race <- function(data, type){
  # generate a grid with all possible combinations of head and wife raceeth
  head <- c("black","white","hispanic","other","Native","Asian/PI")
  wife <- c("black","white","hispanic","other","Native","Asian/PI")
  combos <- expand.grid(head,wife)
  # rename the column headings
  names(combos) <- c("head","wife") 
  combos$head <- as.character(combos$head)
  combos$wife <- as.character(combos$wife)
  # generate a new variable called "racefamily"
  # Whichever family member (head or wife) has the highest priority rated Race, with the following order:
  # Hispanic > Black > Native > Asian/PI > Other > White
  combos$racefamily <- ifelse(combos$head==combos$wife, combos$head,
                              ifelse(combos$head=="hispanic", "hispanic",
                                     ifelse(combos$wife=="hispanic", "hispanic",
                                            ifelse(combos$head=="black" & combos$wife!="hispanic","black", # If using an ifelse statement ? don't need the & ! part
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

  
  # Generate a racefamily1 variable: taking the raceeth of whichever of the head or wife does not have missing data
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
    # data <- data %>% 
    #   group_by(familyID) %>% 
    #   fill(racefamily1, .direction=c("downup"))
  }
  # Next, do the same for the head's mother and father (e.g. assign the head's mother's raceeth to the head's father if his raceeth is NA)
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
    # data <- data %>% 
    #   group_by(familyID) %>% 
    #   fill(racefamily2, .direction=c("downup"))
  }
  return(data)
}

###### FUNCTION 3:INDIVIDUAL_RACE

## Assign each individual their raceeth

# As the race question is only asked of the head and of their spouse directly, individuals may have their raceeth information imputed based on their nearest family member. 
# For example, the parent and child of the head is given the same race as the head.

individual_race <- function(data, type){
  if(type==T){  
    data <- data %>% mutate(individualrace = ifelse(relationship=="head", raceethhead,
                                                    ifelse(relationship=="wife", raceethwife,
                                                           ifelse(relationship=="childofhead",raceethhead,
                                                                  ifelse(relationship=="parentofhead", raceethhead,
                                                                         ifelse(relationship=="childofpartner", raceethwife,
                                                                                ifelse(relationship=="grandchild", racefamily1, 
                                                                                       ifelse(relationship=="parentofwife",raceethwife,
                                                                                              ifelse(relationship=="brotherofhead", raceethhead,
                                                                                                     ifelse(relationship=="brotherofwife", raceethwife,
                                                                                                            NA)))))))))) %>% 
      group_by(uniqueID) %>% fill(individualrace, .direction=c("downup"))
  }else if(type==F){
    # Individuals who do not yet have a race allocated are given the race of their parent (i.e. racefamily2)
    toallocate <- data[is.na(data$individualrace),]
    toallocate <- toallocate %>% 
      mutate(individualrace = racefamily2)
    data <- data %>% drop_na(individualrace)
    data <- rbind(data, toallocate) %>% 
      group_by(uniqueID) %>% fill(individualrace, .direction=c("downup"))
  }
  return(data)
}

###### FUNCTION 4: CODE_RACE_PARENTS

# All individuals have a unique ID and a IDmother (their mothers unique ID number).
# Most indiviudals have their parents in the dataset due to the survey design, and their parents will have info on their race/ethnicity
# from when they were interviewed as heads/wives themselves.
# We therefore generate a look up table for mothers and fathers, renaming the mothers UniqueID as IDmother
# in order to match them with their children, and add a column to each of their children, stating their mother's race 

code_race_parents <- function(data){
  mother <- data %>% ungroup() %>% dplyr::select(uniqueID, year, individualrace) %>% 
    rename(IDmother = uniqueID, 
           mothersrace = individualrace) %>% group_by(IDmother) %>% fill(mothersrace, .direction=c("downup")) %>% distinct() 
  data <- left_join(race, mother)
  father <- data %>% rename(IDfather = uniqueID %>% dplyr::select(uniqueID, year, individualrace) 
                            %>% ungroup(),
           fathersrace = individualrace) %>% fill(fathersrace, .direction=c("downup")) %>%
    group_by(IDfather) %>% distinct()
  data <- left_join(data, father)
  return(data)
}