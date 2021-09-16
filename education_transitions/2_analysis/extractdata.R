# saving output to a .csv table for importing 
extractdata <- function(model){
  dat <- print(model)
  dat <- t(dat)
  dat <- data.frame(dat)
  dat$names <- row.names(dat)
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
           "SomeC3->College"=State.4...State.5) %>% select(-c(names)) %>% pivot_wider(names_from=Type, values_from=c("LEHS->LEHS":"SomeC3->College"))
  return(dat)
}