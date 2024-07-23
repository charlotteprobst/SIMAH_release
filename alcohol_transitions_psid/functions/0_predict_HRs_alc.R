predict_HRs <- function(model) {
  # Create table with rounded values
  table <- data.frame(hazard.msm(model)) %>% 
    round(2) 
  
  # Update column names
  replacements <- c(
    "age_cat" = "age",
    "race" = "",
    "education" = "",
    "sex1" = "Female",
    "Less.than.or.equal.to.high.school" = "LEHS",
    "Some.college" = "Some college"
  )
  colnames(table) <- str_replace_all(colnames(table), replacements)
  
  # Mutate and pivot data
  table <- table %>%
    mutate(transition = row.names(.)) %>%
    pivot_longer(cols = -transition) %>%
    extract(name, into = c("Variable","Type"), regex = "(.*)\\.(.*)") %>%
    pivot_wider(names_from = "Type", values_from = "value") %>%
    mutate(EstimateCI = paste0(HR, " (", L, ", ", U, ")")) %>%
    select(transition, Variable, EstimateCI) %>%
    pivot_wider(names_from = "transition", values_from = EstimateCI)
  
  # Rename transition states
  colnames(table) <- str_replace_all(colnames(table), c("State 1" = "Non-drinker", "State 2" = "Low risk", "State 3" = "Medium risk", "State 4" = "High risk"))
  
  return(table)
}
