#' Loads death rate data for SIMAH or CASCADE versions
#'
#' @param
#' @keywords death rates
#' @export
#' @examples
#' load_death_rates
load_death_rates <- function(model="SIMAH", proportion, SelectedState, WorkingDirectory){

if(model=="SIMAH"){

fun <- function(x){
  x <- x*proportion
  return(x)
}

if(SelectedState=="USA"){
deathrates <- read.csv(paste0(WorkingDirectory,"allethn_sumCOD_0019_final.csv")) %>%
  mutate(Sex=recode(sex, "1"="m","2"="f"),
                      agecat = recode(age_gp, "18"="18-24","25"="25-29",
                                         "30"="30-34","35"="35-39","40"="40-44",
                                         "45"="45-49","50"="50-54","55"="55-59",
                                         "60"="60-64","65"="65-69","70"="70-74","75"="75-79","80"="80+"),
                      Race = recode(race, "White"="WHI","Black"="BLA","Hispanic"="SPA","Other"="OTH"),
                      edclass = recode(edclass, "4+yrs"="College")) %>%
  mutate(cat=paste(Sex,agecat,Race,edclass, sep="")) %>%   filter(agecat!="80+") %>%
  dplyr::select(year,cat, LVDCmort, DMmort,
         IHDmort, ISTRmort, HYPHDmort, AUDmort, UIJmort, MVACCmort, IJmort, RESTmort) %>%
  mutate_at(vars(LVDCmort, DMmort,
                 IHDmort, ISTRmort, HYPHDmort, AUDmort, UIJmort, MVACCmort, IJmort, RESTmort), fun)
}else{deathrates <- read.csv(paste0(WorkingDirectory,"sumCOD_0018_mortcount_state.csv")) %>% filter(age_gp!=80) %>% mutate(Sex=recode(sex, "1"="m","2"="f"),
       agecat = recode(age_gp, "18"="18-24","25"="25-29",
                       "30"="30-34","35"="35-39","40"="40-44",
                       "45"="45-49","50"="50-54","55"="55-59",
                       "60"="60-64","65"="65-69","70"="70-74","75"="75-79","80"="80+"),
       Race = recode(race, "White"="WHI","Black"="BLA","Hispanic"="SPA","Other"="OTH")) %>%
    filter(agecat!="80+") %>%
    mutate(State = ifelse(fipsstr=="CA","California",
                          ifelse(fipsstr=="CO","Colorado",
                                 ifelse(fipsstr=="FL","Florida",
                                        ifelse(fipsstr=="IN","Indiana",
                                               ifelse(fipsstr=="KY","Kentucky",
                                                      ifelse(fipsstr=="LA","Louisiana",
             ifelse(fipsstr=="MA","Massachusetts",
                    ifelse(fipsstr=="MI","Michigan",
                           ifelse(fipsstr=="MN","Minnesota",
                                  ifelse(fipsstr=="MO","Missouri",
                                         ifelse(fipsstr=="NY","New York",
                                                ifelse(fipsstr=="OR","Oregon",
                                                       ifelse(fipsstr=="PA","Pennsylvania",
               ifelse(fipsstr=="TN","Tennessee",
                      ifelse(fipsstr=="TX","Texas",
                             fipsstr)))))))))))))))) %>%
    filter(State==SelectedState) %>%
    mutate(cat=paste(Sex,agecat,Race,edclass, sep=""),
           U_RESTmort = U_RESTmort) %>%
    dplyr::select(year,cat, U_LVDCmort, U_DMmort,
           U_IHDmort, U_ISTRmort, U_HYPHDmort, U_AUDmort, U_UIJmort, U_MVACCmort, U_IJmort, U_RESTmort)
  names(deathrates)[3:12] <- gsub("U_", "", names(deathrates)[3:12])
  deathrates <- deathrates %>% mutate_at(vars(LVDCmort, DMmort,
                   IHDmort, ISTRmort, HYPHDmort, AUDmort, UIJmort, MVACCmort, IJmort, RESTmort), fun)

}
latest <- deathrates %>% filter(year==2018)
rep <- as.data.frame(sapply(latest, rep.int, times=10))
rep$year <- rep(2019:2028, each=288)
summary(rep$year)
deathrates <- rbind(deathrates,rep)
deathrates <- deathrates %>% mutate_at(vars(LVDCmort:RESTmort), as.numeric)
rm(latest,rep)
}else if(model=="CASCADE"){
    # read in 1979-1998 death rates first
    deathrates1 <- read.delim("SIMAH_workplace/microsim/1_input_data/Compressed Mortality, 1979-1998 (1).txt")

    # 1999 death rates onwards come from different CDC Wonder system so read in separately
    deathrates2 <- read.delim("SIMAH_workplace/microsim/1_input_data/Compressed Mortality, 1999-2016 (1).txt")

    deathrates3 <- read.delim("SIMAH_workplace/microsim/1_input_data/Underlying Cause of Death, 1999-2020.txt") %>%
      rename(Age.Group=Five.Year.Age.Groups,
             Age.Group.Code = Five.Year.Age.Groups.Code) %>%
      mutate(Age.Group.Code  = ifelse(Age.Group.Code=="25-29" | Age.Group.Code=="30-34","25-34",
                                      ifelse(Age.Group.Code=="35-39" | Age.Group.Code=="40-44", "35-44",
                                             ifelse(Age.Group.Code=="45-49" | Age.Group.Code=="50-54","45-54",
                                                    ifelse(Age.Group.Code=="55-59" | Age.Group.Code=="60-64", "55-64",
                                                           ifelse(Age.Group.Code=="65-69" | Age.Group.Code=="70-74", "65-74",
                                                                  ifelse(Age.Group.Code=="75-79" | Age.Group.Code=="80-84", "75-84", Age.Group.Code)))))))



    setdiff(levels(deathrates1$Age.Group), levels(deathrates3$Age.Group))
    setdiff(levels(deathrates3$Age.Group), levels(deathrates1$Age.Group))

    deathrates <- rbind(deathrates1, deathrates3)
    rm(deathrates1, deathrates2,deathrates3)

    deathrates <- deathrates %>% dplyr::select(Year, Gender, Age.Group.Code, State, Deaths) %>% drop_na() %>%
      mutate(Gender = factor(Gender),
             Age = factor(Age.Group.Code),
             State=factor(State),
             Sex = recode(Gender, "Male"="m", "Female"="f")) %>% dplyr::select(-c(Age.Group.Code, Gender)) %>% filter(Year>=1980)

    deathratesUSA <- deathrates %>% group_by(Year,Age,Sex) %>% summarise(Deaths=sum(Deaths))
    deathratesUSA$State <- "USA"

    deathrates <- rbind(data.frame(deathrates), data.frame(deathratesUSA))
    rm(deathratesUSA)

    deathrates <- deathrates %>% filter(State==SelectedState) %>%
      filter(Age!="10-14") %>%
      mutate(Deaths = Deaths*proportion,
             Age=as.character(Age),
             Age = ifelse(Age=="75-84", "75.", Age),
             Deaths = ifelse(Age=="15-19", Deaths/5*2,
                             ifelse(Age=="75.", Deaths/10*6, Deaths)),
             cat = paste(Age, Sex, sep="_"),
             Count = Deaths) %>% dplyr::select(Year, cat, Count) %>% filter(Year>=1980)
    #adjust for the proportion currently being run in the microsim
    # adjust the totals for the age groups that aren't fully in the microsim - 75-84 and 15-19
    # assuming equal death rates for each year group

    # READ IN CIRRHOSIS DEATHS - TO WORK REFERENCE RATE AND REMOVE CIRRHOSIS DEATHS FROM THE TOTAL DEATHS
    if(cirrhosis==1 & mortality==1){
      cirrhosisdata <- cirrhosismortality %>% ungroup() %>%
        mutate(cat=paste(agegroup, sex, sep="_")) %>% dplyr::select(Year, cat, count)

      cirrhosisdeaths1984 <- cirrhosisdata %>% filter(Year==1984)

      # # ADJUST DEATH RATES = total deaths - cirrhosis deaths
      setdiff(names(cirrhosisdata), names(deathrates))
      setdiff(levels(as.factor(cirrhosisdata$cat)), levels(as.factor(deathrates$cat)))
      deathrates <- left_join(deathrates, cirrhosisdata)
      deathrates[is.na(deathrates)] <- 0
      deathrates$Count <- deathrates$Count - (deathrates$count/100)
      deathrates <- deathrates %>% dplyr::select(Year, cat, Count)
      # cirrhosisdeaths1984$count <- cirrhosisdeaths1984$count*100
      # cirrhosisdeaths1984 <- cirrhosisdeaths1984 %>% separate(cat, into=c("age","cat"), sep="_") %>%
      #   group_by(cat) %>% mutate(count=sum(count))
    }else if(cirrhosis==1 & mortality==0){
      cirrhosisdata <- read.csv("SIMAH_workplace/microsim/1_input_data/LC_hosp_Output.csv") %>%
        dplyr::select(c(year,location,sex_id,tups15to19:tupsplus80)) %>%
        filter(location==SelectedState) %>%
        mutate(tupsplus80 = tupsplus80/10) %>%
        pivot_longer(cols=tups15to19:tupsplus80) %>%
        mutate(name=gsub("tups","",name),
               agecat = ifelse(name=="15to19","15-19",
                               ifelse(name=="20to24","20-24",
                                      ifelse(name=="25to29"|name=="30to34","25-34",
                                             ifelse(name=="35to39"|name=="40to44","35-44",
                                                    ifelse(name=="45to49"|name=="50to54","45-54",
                                                           ifelse(name=="55to59"|name=="60to64","55-64",
                                                                  ifelse(name=="65to69"|name=="70to74","65-74","75.")))))))) %>%
        group_by(year, sex_id, agecat) %>% summarise(value=sum(value)) %>%
        mutate(value=value*proportion,
               count=ifelse(agecat=="15-19",value/5*2,value),
               sex = ifelse(sex_id=="female","f","m"),
               cat=paste(agecat, sex, sep="_")) %>% rename(Year=year) %>% ungroup() %>% dplyr::select(Year, cat, count)
      cirrhosisdeaths1984 <- cirrhosisdata %>% filter(Year==1984) %>%
        mutate(count=round(count*100))
    }
}
  return(deathrates)
}







