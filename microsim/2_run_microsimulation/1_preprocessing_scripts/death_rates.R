# death rates processing script 
#####read in the death rates - raw deaths per year by age and sex 

fun <- function(x){
  x <- x*proportion
  return(x)
}

if(SelectedState=="USA"){
deathrates <- read.csv("SIMAH_workplace/microsim/1_input_data/allethn_sumCOD_0019_final.csv") %>% 
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
}else{
  deathrates <- read.csv("SIMAH_workplace/microsim/1_input_data/sumCOD_0018_mortcount_state.csv") %>% filter(age_gp!=80) %>% mutate(Sex=recode(sex, "1"="m","2"="f"),
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
# 
# LC <- deathrates %>% dplyr::select(year, cat, LVDCmort) %>% 
#   separate(cat, into=c("sex","age","race","education"), sep=c(1,6,9)) %>% 
#   mutate(agecat = recode(age, "18-24"="18-24",
#                          "25-29"="25-34", "30-34"="25-34",
#                          "35-39"="35-44","40-44"="35-44",
#                          "45-49"="45-54","50-54"="45-54",
#                          "55-59"="55-64","60-64"="55-64",
#                          "65-69"="65-74","70-74"="65-74",
#                          "75-79"="75-79")) %>% 
#   group_by(year, sex, agecat) %>% summarise(total=sum(LVDCmort))
# write.csv(LC, "SIMAH_workplace/LC_SIMAH.csv", row.names=F)
# ggplot(data=LC, aes(x=year, y=total)) + geom_line() + 
#   facet_grid(rows=vars(sex), cols=vars(agecat))
# 
# TPop <- read.csv("SIMAH_workplace/microsim/1_input_data/allethn_rates_0019_final.csv") %>% 
#   filter(age_gp!="80") %>% filter(year!=2019) %>% 
#   dplyr::select(year, race, sex, edclass, age_gp, TPop) %>% 
#   rename(Sex=sex, raceeth=race, agecat=age_gp) %>% 
#   mutate(Sex=recode(Sex, "1"="m","2"="f"),
#          raceeth = recode(raceeth, "White"="WHI","Black"="BLA","Hispanic"="SPA","Other"="OTH")) %>% 
#   mutate(agecat = recode(agecat,
#                          "18"="18-24","25"="25-29","30"="30-34","35"="35-39",
#                          "40"="40-44","45"="45-49","50"="50-54","55"="55-59",
#                          "60"="60-64","65"="65-69","70"="70-74","75"="75-79")) %>% ungroup() %>% 
#   mutate(cat=paste0(Sex,agecat,raceeth, edclass)) %>% 
#   group_by(cat) %>% summarise(n=(sum(TPop))*proportion)
# deathrates <- left_join(deathrates, TPop)
# 
# LC <- deathrates %>% dplyr::select(year, cat, LVDCmort,n) %>% 
#   separate(cat, into=c("sex","age","race","education"), sep=c(1,6,9)) %>% 
#   mutate(agecat = recode(age, "18-24"="18-24",
#                          "25-29"="25-34", "30-34"="25-34",
#                          "35-39"="35-44","40-44"="35-44",
#                          "45-49"="45-54","50-54"="45-54",
#                          "55-59"="55-64","60-64"="55-64",
#                          "65-69"="65-74","70-74"="65-74",
#                          "75-79"="75-79")) %>% 
#   group_by(year, sex, agecat, education) %>% summarise(total=sum(LVDCmort),
#                                                        totaln = sum(n),
#                                                        rateper100000 = (total/totaln)*100000) %>% 
#   mutate(education=ifelse(education=="LEHS","High school degree or less",
#                           ifelse(education=="SomeC","Some college",
#                                  "College degree or more")),
#          education = factor(education, levels=c("High school degree or less",
#                                                 "Some college", "College degree or more")))
# 
# ggplot(data=LC, aes(x=year, y=total, colour=education)) + geom_line() + 
#   facet_grid(cols=vars(agecat), rows=vars(sex)) + theme_bw() + 
#   theme(legend.position="bottom",
#         legend.title=element_blank()) + ylab("Total N deaths")
# ggsave("SIMAH_workplace/microsim/2_output_data/plots/LC_by_education_totalN.png",
#        dpi=300, width=33, height=19, units="cm")
# 
# ggplot(data=LC, aes(x=year, y=rateper100000, colour=education)) + geom_line() + 
#   facet_grid(cols=vars(agecat), rows=vars(sex)) + theme_bw() + 
#   theme(legend.position="bottom",
#         legend.title=element_blank()) + ylab("Rate per 100,000")
# ggsave("SIMAH_workplace/microsim/2_output_data/plots/LC_by_education_rate.png",
#        dpi=300, width=33, height=19, units="cm")
#   
# write.csv(LC, "SIMAH_workplace/LC_SIMAH.csv", row.names=F)


latest <- deathrates %>% filter(year==2018)
rep <- as.data.frame(sapply(latest, rep.int, times=10))
rep$year <- rep(2019:2028, each=288)
summary(rep$year)
deathrates <- rbind(deathrates,rep)
deathrates <- deathrates %>% mutate_at(vars(LVDCmort:RESTmort), as.numeric)
rm(latest,rep)





