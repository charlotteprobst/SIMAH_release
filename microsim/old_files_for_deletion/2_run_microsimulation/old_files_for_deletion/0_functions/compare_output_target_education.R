compare_output_target_education <- function(output){
  source("1_preprocessing_scripts/process_target_comparison_education.R")
  for(i in 2000:2018){
    output[[paste(i)]] <- output[[paste(i)]] %>% 
      mutate(YEAR= i) %>% 
      group_by(YEAR, microsim.init.sex, microsim.init.education) %>% tally() %>% 
      rename(SEX = microsim.init.sex, EDUC=microsim.init.education) %>% 
      mutate(data="microsim",
             SEX = recode(SEX, "m"="M","f"="F"))
  }
  
  output <- do.call(rbind, output) %>% 
    rename(microsim=n) %>% select(YEAR, SEX, EDUC, microsim)
  compare <- left_join(target, output) %>% ungroup() %>% 
    mutate(diff = n-microsim,
           diffscaled = diff*(1/proportion)) %>% 
    rename(Year = YEAR, microsim.init.sex = SEX, microsim.init.education=EDUC) %>% 
    mutate(microsim.init.sex = recode(microsim.init.sex, "M"="m","F"="f"),
           microsim.init.education=recode(microsim.init.education,"LEHS"="High school or less",
                                          "SomeC"="Some college", "College"="College degree +"))  
  return(compare)
}
