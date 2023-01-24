#' Compares alcohol consumption by subgroup to brfss data
#'
#' @param
#' @keywords alcohol postprocessing
#' @export
#' @examples
#' summarise_alcohol_output
summarise_alcohol_output <- function(Output, SelectedState, DataDirectory){
  # brfss <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_1984_2020_final.RDS")
  #
  # targets <- brfss %>%
  #     filter(State=="USA") %>% filter(YEAR>=2000) %>%
  #     rename(microsim.init.sex=sex_recode) %>%
  #     mutate(microsim.init.sex=ifelse(microsim.init.sex=="Male","m","f")) %>%
  #     mutate(AlcCAT = ifelse(formerdrinker==1, "Former drinker",
  #                            ifelse(formerdrinker!=1 & gramsperday==0, "Lifetime abstainer",
  #                                   ifelse(formerdrinker!=1 & microsim.init.sex=="m" & gramsperday>0 &
  #                                            gramsperday<=40, "Low risk",
  #                                          ifelse(formerdrinker!=1 & microsim.init.sex=="f" & gramsperday>0 &
  #                                                   gramsperday<=20, "Low risk",
  #                                                 ifelse(formerdrinker!=1 & microsim.init.sex=="m" & gramsperday>40 &
  #                                                          gramsperday<=60, "Medium risk",
  #                                                        ifelse(formerdrinker!=1 & microsim.init.sex=="f" & gramsperday>20 &
  #                                                                 gramsperday<=40, "Medium risk",
  #                                                               ifelse(formerdrinker!=1 & microsim.init.sex=="m" & gramsperday>60,
  #                                                                      "High risk",
  #                                                                      ifelse(formerdrinker!=1 & microsim.init.sex=="f" & gramsperday>40,
  #                                                                             "High risk", NA)))))))),
  #            AlcCAT=ifelse(AlcCAT=="Former drinker","Non-drinker",
  #                          ifelse(AlcCAT=="Lifetime abstainer","Non-drinker",AlcCAT))
  #     ) %>% ungroup() %>%
  #   dplyr::select(YEAR, microsim.init.sex, education_summary, race_eth, AlcCAT) %>%
  #   group_by(YEAR, microsim.init.sex, education_summary, race_eth, AlcCAT) %>%
  #   tally()

  # write.csv(targets, "SIMAH_workplace/microsim/1_input_data/brfss_alcohol_summary.csv", row.names=F)

target <- read.csv(paste0(DataDirectory,"brfss_alcohol_summary.csv")) %>%
  group_by(YEAR, microsim.init.sex, education_summary, AlcCAT) %>%
  summarise(n=sum(n)) %>% rename(sex=microsim.init.sex, year=YEAR, education=education_summary) %>%
  group_by(year, sex,education) %>%
  mutate(targetpercent=n/sum(n),
         sepercent = sqrt((targetpercent*(1-targetpercent))/sum(n)),
         lower_ci = targetpercent - (1.96*sepercent),
         upper_ci = targetpercent + (1.96*sepercent)) %>%
  dplyr::select(-c(n, sepercent))

output <- Output %>%
  group_by(year, microsim.init.sex, microsim.init.education, AlcCAT) %>%
  summarise(n=sum(n)) %>%
  mutate(year = as.integer(as.character(year))) %>%
  rename(sex=microsim.init.sex, education=microsim.init.education) %>% ungroup() %>%
  group_by(year, sex, education) %>%
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
         name = ifelse(name=="targetpercent","BRFSS","Microsimulation"),
         education = factor(education, levels=c("LEHS","SomeC","College")))
          # filter(AlcCAT == "Non-drinker" | AlcCAT=="<20 / <40 gpd")
          #  filter(AlcCAT=="21-40 / 41-60 gpd" | AlcCAT=="41+ / 61+ gpd")

plotmen <- ggplot(data=subset(summary, sex=="Men"), aes(x=year, y=value, colour=name)) + geom_line(size=2) +
  facet_grid(cols=vars(education),
             rows=vars(AlcCAT), scales="free") +
  scale_y_continuous(labels=scales::percent, limits=c(0,NA)) + ylab("Proportion (%)") +
  xlab("") + theme_bw() + theme(legend.position = "bottom",
                                legend.title=element_blank(),
                                strip.background=element_rect(fill="white"),
                                text = element_text(size=18)) +
  # geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci, colour=name, fill=name)) +
  scale_colour_manual(values=c("#93aebf","#132268")) + ggtitle("Men")
plotmen
plotwomen <- ggplot(data=subset(summary, sex=="Women"), aes(x=year, y=value, colour=name)) + geom_line(size=2) +
  facet_grid(cols=vars(education),
             rows=vars(AlcCAT), scales="free") +
  scale_y_continuous(labels=scales::percent, limits=c(0,NA)) + ylab("Proportion (%)") +
  xlab("") + theme_bw() + theme(legend.position = "bottom",
                                legend.title=element_blank(),
                                strip.background=element_rect(fill="white"),
                                text = element_text(size=18)) +
  # geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci, colour=name, fill=name)) +
  scale_colour_manual(values=c("#93aebf","#132268")) + ggtitle("Women")
plotwomen




list <- list(summary, plotmen, plotwomen)
return(list)
}

