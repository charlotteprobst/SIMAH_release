#' Compares mortality consumption by subgroup to brfss data
#'
#' @param
#' @keywords mortality postprocessing
#' @export
#' @examples
#' summarise_mortality_output
summarise_mortality_output <- function(Output, SelectedState, WorkingDirectory){

age2010 <- Output[[2]] %>% filter(year=="2000") %>%
  ungroup() %>%
  group_by(year, microsim.init.sex, microsim.init.education, agecat) %>%
  summarise(totalpop = sum(totalpop)) %>% ungroup() %>%
  group_by(year, microsim.init.sex, microsim.init.education) %>%
  mutate(percent = totalpop / sum(totalpop)) %>% ungroup() %>% dplyr::select(microsim.init.sex, microsim.init.education,
                                                                             agecat, percent)
simulation <- Output[[1]] %>%
  group_by(year, cause, microsim.init.sex, microsim.init.education, agecat) %>%
  summarise(totaldeaths = sum(ndeaths))

totalpop <- Output[[2]] %>% ungroup() %>%
  group_by(year, microsim.init.sex, microsim.init.education, agecat) %>%
  summarise(totalpop=sum(totalpop))

simulation <- left_join(simulation, totalpop) %>%
  left_join(.,age2010) %>%
  group_by(year, cause, microsim.init.sex, microsim.init.education, agecat) %>%
  mutate(weightedrate = (totaldeaths / totalpop*100000)*percent) %>%
  ungroup() %>%
  group_by(year, cause, microsim.init.sex, microsim.init.education) %>%
  summarise(mortalityrate=sum(weightedrate)) %>%
  mutate(datatype="Simulation output",
         year = as.numeric(as.character(year)),
         microsim.init.sex=as.character(microsim.init.sex),
         microsim.init.education = as.character(microsim.init.education),
         cause = as.character(cause))

# now compare to target data
deathrates <- read.csv(paste0(WorkingDirectory,"allethn_sumCOD_0019_final.csv")) %>%
  mutate(microsim.init.sex=recode(sex, "1"="m","2"="f"),
         agecat = recode(age_gp, "18"="18-24","25"="25-29",
                         "30"="30-34","35"="35-39","40"="40-44",
                         "45"="45-49","50"="50-54","55"="55-59",
                         "60"="60-64","65"="65-69","70"="70-74","75"="75-79","80"="80+"),
         microsim.init.race = recode(race, "White"="WHI","Black"="BLA","Hispanic"="SPA","Other"="OTH"),
         microsim.init.education = recode(edclass, "4+yrs"="College")) %>%   filter(agecat!="80+") %>%
  dplyr::select(year,microsim.init.sex, microsim.init.education, agecat, LVDCmort, DMmort,
                IHDmort, ISTRmort, HYPHDmort, AUDmort, UIJmort, MVACCmort, IJmort, RESTmort) %>%
  pivot_longer(cols=c(LVDCmort:RESTmort),names_to="cause",values_to="totaldeaths") %>%
  mutate(cause=gsub("mort","",cause))

popcounts <- read.csv(paste0(WorkingDirectory,"CPS_2000_2020_agegp.csv")) %>%
  rename(microsim.init.sex=sex, microsim.init.race=race,
         agecat = age_gp, microsim.init.education=edclass) %>%
  mutate(agecat = as.character(agecat),
    agecat = recode(agecat, "18"="18-24","25"="25-29","30"="30-34","35"="35-39",
                    "40"="40-44","45"="45-49","50"="50-54","55"="55-59","60"="60-64",
                    "65"="65-69","70"="70-74","75"="75-79","80"="80+"),
    microsim.init.sex=ifelse(microsim.init.sex==1,"m","f"),
    microsim.init.race = recode(microsim.init.race, "White"="WHI","Black"="BLA",
                                "Hispanic"="SPA","Other"="OTH")) %>%
  filter(agecat!="80+") %>% ungroup() %>%
  group_by(year, agecat, microsim.init.sex, microsim.init.education) %>%
  summarise(totalpop=sum(TPop))

deathrates <- left_join(deathrates,popcounts)

age2010 <- popcounts %>% filter(year=="2000") %>%
  ungroup() %>%
  group_by(year, microsim.init.sex, microsim.init.education, agecat) %>%
  summarise(totalpop = sum(totalpop)) %>% ungroup() %>%
  group_by(year, microsim.init.sex, microsim.init.education) %>%
  mutate(percent = totalpop / sum(totalpop)) %>% ungroup() %>% dplyr::select(microsim.init.sex, microsim.init.education,
                                                                             agecat, percent)

deathrates <- left_join(deathrates, age2010) %>%
  group_by(year, cause, microsim.init.sex, microsim.init.education, agecat) %>%
  mutate(weightedrate = (totaldeaths / totalpop*100000)*percent) %>%
  ungroup() %>%
  group_by(year, cause, microsim.init.sex, microsim.init.education) %>%
  summarise(mortalityrate=sum(weightedrate)) %>%
  mutate(datatype="Observed output")

compare <- rbind(simulation,deathrates)

plot <- ggplot(data=subset(compare,microsim.init.sex=="f"), aes(x=year, y=mortalityrate, colour=datatype)) + geom_line(size=2) +
  facet_grid(cols=vars(microsim.init.education),
             rows=vars(cause), scales="free") +
  ylab("Mortality per 100,000") +
  xlab("") + theme_bw() + theme(legend.position = "bottom",
                                legend.title=element_blank(),
                                strip.background=element_rect(fill="white"),
                                text = element_text(size=18)) +
  # geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci, colour=name, fill=name)) +
  scale_colour_manual(values=c("#93aebf","#132268"))
plot
list <- list(compare, plot)
return(list)
}

