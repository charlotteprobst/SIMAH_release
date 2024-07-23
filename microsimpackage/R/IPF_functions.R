#' processes the BRFSS data for IPF for generating baseline population
#'
#' @param
#' @keywords brfss
#' @export
#' @examples
#' process_for_IPF
process_for_IPF <- function(data, selectedstate, variables){
  data <- data %>%
    filter(age_var<=79) %>%
    mutate(race = recode(race_eth,"White"="WHI",
                         "Black"="BLA", "Hispanic"="SPA", "Other"="OTH"),
           sex = recode(sex_recode,"Male"="M","Female"="F"),
           education = education_summary,
           agecat = cut(age_var,
                        breaks=c(0,24,34,44,54,64,79),
                        labels=c("18.24","25.34","35.44","45.54","55.64","65.79")),
           frequency = ifelse(frequency_upshifted==0 & gramsperday_upshifted>0, 1, frequency_upshifted),
           quantity_per_occasion = (gramsperday_upshifted/14 * 30) / frequency_upshifted,
           quantity_per_occasion = ifelse(gramsperday_upshifted==0, 0, quantity_per_occasion),
           gramsperday = gramsperday_upshifted,
           formerdrinker=ifelse(drinkingstatus_detailed=="Former drinker", 1,0))

  selected <- data %>% filter(State==selectedstate) %>%
    ungroup() %>%
    dplyr::select(all_of(variables)) %>% drop_na(agecat,race,sex,education)

  # work out if there are any missing categories for this state and if so do something about that
  if(selectedstate!="USA"){
      missing <- selected %>% mutate(sex = as.factor(sex),
                                     agecat = as.factor(agecat),
                                     education=as.factor(education),
                                     race=as.factor(race)) %>%
        group_by(sex,agecat, education, race, .drop=FALSE) %>%
        tally() %>%
        mutate(cat = paste(race, sex, agecat, education, sep="")) %>% ungroup() %>%
        dplyr::select(cat, n) %>% filter(n==0)
      missingcats <- unique(missing$cat)
      # cons <- cons %>% filter(state==selectedstate) %>% dplyr::select()
      # 100-round(rowSums(cons[c(missingcats)]) / rowSums(cons)*100,digits=2)
      if(length(missingcats>=1)){
        toreplace <- data %>% drop_na() %>% filter(region==unique(selected$region)) %>%
          mutate(cat=paste(race,sex,agecat,education,sep="")) %>%
          filter(cat %in% missingcats) %>% dplyr::select(-c(cat))
        selected <- rbind(toreplace, selected)
      }
  }
  return(selected)
}

