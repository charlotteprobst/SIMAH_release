#' Compares alcohol consumption by subgroup to brfss data
#'
#' @param
#' @keywords alcohol postprocessing
#' @export
#' @examples
#' summarise_alcohol_output
summarise_alcohol_output <- function(Output, SelectedState, WorkingDirectory){
  # target <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_reweighted_upshifted_1984_2020.RDS") %>%
  #   filter(age_var<=79) %>% filter(YEAR>=2000) %>%
  #   mutate(microsim.init.race = recode(race_eth,"White"="WHI",
  #                                      "Black"="BLA", "Hispanic"="SPA", "Other"="OTH"),
  #          microsim.init.sex = recode(sex_recode,"Male"="m","Female"="f"),
  #          microsim.init.education = education_summary,
  #          agecat = cut(age_var,
  #                       breaks=c(0,24,34,44,54,64,79),
  #                       labels=c("18.24","25.34","35.44","45.54","55.64","65.79")),
  #          formerdrinker = ifelse(drinkingstatus_detailed=="Former drinker", 1,0)) %>%
  #   rename(microsim.init.age = age_var,
  #          microsim.init.drinkingstatus=drinkingstatus_updated,
  #          microsim.init.alc.gpd=gramsperday_upshifted_crquotient,
  #          microsim.init.income = household_income) %>%
  #   dplyr::select(YEAR, State, region, microsim.init.race, microsim.init.age,
  #                 microsim.init.sex, microsim.init.education, microsim.init.drinkingstatus,
  #                 microsim.init.alc.gpd, formerdrinker, microsim.init.income, agecat) %>% filter(YEAR>=2000) %>%
  # mutate(AlcCAT = ifelse(formerdrinker==1, "Former drinker",
  #                        ifelse(formerdrinker!=1 & microsim.init.alc.gpd==0, "Lifetime abstainer",
  #                               ifelse(formerdrinker!=1 & microsim.init.sex=="m" & microsim.init.alc.gpd>0 &
  #                                        microsim.init.alc.gpd<=40, "Low risk",
  #                                      ifelse(formerdrinker!=1 & microsim.init.sex=="f" & microsim.init.alc.gpd>0 &
  #                                               microsim.init.alc.gpd<=20, "Low risk",
  #                                             ifelse(formerdrinker!=1 & microsim.init.sex=="m" & microsim.init.alc.gpd>40 &
  #                                                      microsim.init.alc.gpd<=60, "Medium risk",
  #                                                    ifelse(formerdrinker!=1 & microsim.init.sex=="f" & microsim.init.alc.gpd>20 &
  #                                                             microsim.init.alc.gpd<=40, "Medium risk",
  #                                                           ifelse(formerdrinker!=1 & microsim.init.sex=="m" & microsim.init.alc.gpd>60,
  #                                                                  "High risk",
  #                                                                  ifelse(formerdrinker!=1 & microsim.init.sex=="f" & microsim.init.alc.gpd>40,
  #                                                                         "High risk", NA)))))))),
  #        AlcCAT=ifelse(AlcCAT=="Former drinker","Non-drinker",
  #                      ifelse(AlcCAT=="Lifetime abstainer","Non-drinker",AlcCAT))
  # ) %>%
  # group_by(YEAR, State, microsim.init.sex, microsim.init.race,microsim.init.education, AlcCAT) %>%
  # tally() %>% ungroup()

target <- read.csv(paste0(WorkingDirectory,"SIMAH_workplace/microsim/1_input_data/brfss_alcohol_summary.csv")) %>%
  group_by(YEAR, microsim.init.sex, AlcCAT) %>%
  summarise(n=sum(n)) %>% rename(sex=microsim.init.sex, year=YEAR) %>%
  group_by(year, sex) %>%
  mutate(targetpercent=n/sum(n),
         sepercent = sqrt((targetpercent*(1-targetpercent))/sum(n)),
         lower_ci = targetpercent - (1.96*sepercent),
         upper_ci = targetpercent + (1.96*sepercent)) %>%
  dplyr::select(-c(n, sepercent))

output <- Output %>%
  group_by(year, microsim.init.sex, AlcCAT) %>%
  summarise(n=sum(n)) %>%
  mutate(year = as.integer(as.character(year))) %>%
  rename(sex=microsim.init.sex) %>% ungroup() %>%
  group_by(year, sex) %>%
  mutate(simulatedpercent=n/sum(n)) %>%
  dplyr::select(-n)

summary <- left_join(target,output) %>%
  pivot_longer(cols=c(targetpercent,simulatedpercent)) %>%
  mutate(sex = ifelse(sex=="m","Men","Women"),
         AlcCAT = recode(AlcCAT, "Non-drinker"="Non-drinker",
                         "Low risk"="<20 / <40 gpd",
                         "Medium risk" = "21-40 / 41-60 gpd",
                         "High risk"="41+ / 61+ gpd"),
         AlcCAT = factor(AlcCAT, levels=c("Non-drinker",
                                          "<20 / <40 gpd",
                                          "21-40 / 41-60 gpd",
                                          "41+ / 61+ gpd")),
         value = round(value, digits=4),
         name = ifelse(name=="targetpercent","BRFSS","Microsimulation"))
          # filter(AlcCAT == "Non-drinker" | AlcCAT=="<20 / <40 gpd")
          #  filter(AlcCAT=="21-40 / 41-60 gpd" | AlcCAT=="41+ / 61+ gpd")

plot <- ggplot(data=summary, aes(x=year, y=value, colour=name)) + geom_line(size=2) +
  facet_grid(cols=vars(AlcCAT),
             rows=vars(sex), scales="free") +
  scale_y_continuous(labels=scales::percent, limits=c(0,0.7)) + ylab("Proportion (%)") +
  xlab("") + theme_bw() + theme(legend.position = "bottom",
                                legend.title=element_blank(),
                                strip.background=element_rect(fill="white"),
                                text = element_text(size=18)) +
  # geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci, colour=name, fill=name)) +
  scale_colour_manual(values=c("#93aebf","#132268"))
plot
list <- list(summary, plot)
return(list)
}

