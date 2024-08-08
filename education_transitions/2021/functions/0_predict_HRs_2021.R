
predict_HRs <- function(model) {
  
  table <- data.frame(hazard.msm(model))%>% round(2) 
  # Update column names
  colnames(table) <- gsub(x=colnames(table), "agecat", "age",)
  colnames(table) <- gsub(x=colnames(table), "racefinal2", "",)    
  table <- table %>% mutate(transition = row.names(.)) %>%
    pivot_longer(cols=-transition) %>%
    extract(name, into=c("Variable","Type"), regex="(.*)\\.(.*)") %>%   # Separate the string (name) into the variable and type of estimate (HR, Upper, Lower), separate at last occuring period .
    pivot_wider(names_from="Type", values_from = "value") %>%
    mutate(EstimateCI = paste0(HR, " (", L, ", ", U, ")")) %>%
    select(transition, Variable, EstimateCI) %>%
    pivot_wider(names_from = "transition", values_from = EstimateCI)
  colnames(table) <- gsub(x=colnames(table), "State 1", "LEHS",)
  colnames(table) <- gsub(x=colnames(table), "State 2", "1 year",)
  colnames(table) <- gsub(x=colnames(table), "State 3", "2 years",)
  colnames(table) <- gsub(x=colnames(table), "State 4", "3 years",)
  colnames(table) <- gsub(x=colnames(table), "State 5", "college +",)
  
  return(table)
}