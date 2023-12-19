
adjust_CIs <- function(model, timeperiod, data){
  dat <- print(model)
  if(length(dat)>1){
    dat <- t(dat) # swap the rows and columns
    dat <- data.frame(dat) # convert to a data frame
    dat$names <- row.names(dat)
    origsample <- length(unique(data$uniqueID))
    expandedsample <- length(unique(data$newID))
    dat <- dat %>% 
      mutate(Type = ifelse(grepl("Estimate",names),"Estimate",
                           ifelse(grepl(".L", names), "Lower",
                                  ifelse(grepl(".U",names),"Upper",NA)))) %>%
      mutate(Variable = gsub(".Estimate","",names),
             Variable = gsub(".L","",Variable),
             Variable = gsub(".U","",Variable)) %>% filter(Variable!="base.Fixed") %>% 
      rename("LEHS->LEHS"=State.1...State.1,
             "LEHS->SomeC1"=State.1...State.2,
             "SomeC1->SomeC1"=State.2...State.2,
             "SomeC1->SomeC2"=State.2...State.3,
             "SomeC2->SomeC2"=State.3...State.3,
             "SomeC2->SomeC3"=State.3...State.4,
             "SomeC3->SomeC3"=State.4...State.4,
             "SomeC3->College"=State.4...State.5) %>% select(-c(names)) %>% 
      pivot_wider(names_from=Type, values_from=c("LEHS->LEHS":"SomeC3->College")) %>% 
      pivot_longer(`LEHS->LEHS_Estimate`:`SomeC3->College_Upper`) %>% 
      separate(name, into=c("Transition","Type"),sep="_") %>% 
      filter(Variable!="base") %>% 
      drop_na() %>%
      mutate(time = timeperiod) %>%
      pivot_wider(names_from=Type, values_from=value) %>%
      group_by(Variable, Transition, time) %>%
      mutate(SE = (log(Upper) - log(Lower)) / 3.92,
             SD = SE * sqrt(expandedsample),
             estimate = round(Estimate, digits=2),
             corrected_SE = SD / sqrt(origsample), 
             corrected_lower = log(Estimate) - (corrected_SE * 1.96),
             corrected_upper = log(Estimate) + (corrected_SE * 1.96),
             corrected_lower = round(exp(corrected_lower), digits=2),
             corrected_upper = round(exp(corrected_upper), digits=2),
             corrected_upper = ifelse(corrected_upper>100, 100, corrected_upper)) %>%
      mutate(estimate_CI = paste(estimate, " (", corrected_lower, "-", corrected_upper, ")", sep = "")) %>%
      ungroup() %>%
      dplyr::select(Variable, Transition, estimate_CI) %>%
      pivot_wider(names_from = "Transition", values_from = estimate_CI)
#      dat$Variable <- gsub("agecat", "age",dat$Variable) 
 #     dat$Variable <- gsub("racefinal2|timevary", "",dat$Variable) 
     }
  return(dat)
}
