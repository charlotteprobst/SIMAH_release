#' Loads death rate data for SIMAH or CASCADE versions
#'
#' @param
#' @keywords death rates
#' @export
#' @examples
#' load_death_rates
load_death_rates <- function(model="SIMAH", proportion, SelectedState, DataDirectory){

if(model=="SIMAH"){

fun <- function(x){
  x <- x*proportion
  return(x)
}

if(SelectedState=="USA"){
deathrates <- read.csv(paste0(DataDirectory,"allethn_sumCOD_0020_SIMAH.csv")) %>%
  mutate(Sex=recode(sex, "1"="m","2"="f"),
                      agecat = recode(age_gp, "18"="18-24","25"="25-29",
                                         "30"="30-34","35"="35-39","40"="40-44",
                                         "45"="45-49","50"="50-54","55"="55-59",
                                         "60"="60-64","65"="65-69","70"="70-74","75"="75-79","80"="80+"),
                      Race = recode(race, "White"="WHI","Black"="BLA","Hispanic"="SPA","Other"="OTH"),
                      edclass = recode(edclass, "4+yrs"="College")) %>%
  mutate(cat=paste(Sex,agecat,Race,edclass, sep="")) %>%   filter(agecat!="80+") %>%
  dplyr::select(year,cat, LVDCmort, HLVDCmort, DMmort,
         IHDmort, ISTRmort, HYPHDmort, AUDmort, UIJmort, MVACCmort, IJmort, RESTmort) %>%
  mutate_at(vars(LVDCmort, HLVDCmort, DMmort,
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
latest <- deathrates %>% filter(year==2020)
rep <- as.data.frame(sapply(latest, rep.int, times=10))
rep$year <- rep(2021:2030, each=288)
summary(rep$year)
deathrates <- rbind(deathrates,rep)
deathrates <- deathrates %>% mutate_at(vars(LVDCmort:RESTmort), as.numeric)
rm(latest,rep)
}
  return(deathrates)
}







