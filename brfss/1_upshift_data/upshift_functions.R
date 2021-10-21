summariseprevalence <- function(data){
  data <- data %>% group_by(YEAR, State, sex_recode,drinkingstatus) %>% 
    summarise(total = sum(final_sample_weight)) %>% 
    ungroup() %>% 
    group_by(YEAR, State, sex_recode) %>% 
    mutate(percentage = total / sum(total))
}
