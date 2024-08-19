#' Apply taxation policy 
#'
#' This function simulates tax policy
#' @param
#' @keywords tax policy
#' @export 
#' @examples


apply_tax_policy <- function(data = basepop, policy_setting, scenario = scenario){
  
  # PARTICIPATION ELASTICITY 
  
  # apply participation elasticity
  if (participation == 0){
    print("no policy effect on drinking participation")
  } 
  
  if (participation == 1){
    
    # determine weighted number of drinkers to become non-drinkers
    temp <- prob_alcohol_transitions %>% filter(alc_cat != "Non-drinker") %>%
      mutate(race = case_when(
        race == "Black, non-Hispanic" ~ "Black",
        race == "Hispanic" ~ "Hispanic",
        race == "Other, non-Hispanic" ~ "Others",
        race == "White, non-Hispanic" ~ "White"), 
        sex = case_when(
          sex == "Men" ~ "m",
          sex == "Women" ~ "f"))
    
    temp2 <- data %>%
      mutate(agecat = cut(age,
                          breaks=c(0,24,64,100),
                          labels=c("18-24","25-64","65+"))) %>%
      filter(alc_cat != "Non-drinker") %>% left_join(., temp) 
    
    
    # original approach 
    temp3 <- temp2 %>% group_by(alc_cat) %>% 
      
      # obtain average transition probability to become non-drinker by alcohol category
      summarise(prob_quit = mean(prob_nondrinker),
                n = n()) %>% 
      
      # obtain number of drinkers to become non-drinkers, weighted by average transition probability 
      mutate(prop_alc_cat = n / sum(n), # get proportion of people within each alcohol category
             prop_alc = prop_alc_cat * prob_quit, # multiple proportion of people within each alcohol category by alc TPs
             ratio = prop_alc/min(prop_alc), 
             prop_change = (-1 * part_elasticity * scenario) / sum(ratio) * ratio, # split up participation elasticity by calculated ratio based on population and TP by alchol category
             tochange_by_alc_cat = round(prop_change*sum(n), 0)) # multiple this group-specific elasticity by total sample size 
    
    # sample IDs to become non-drinkers
    tochange <- c(sample(data[data$alc_cat == "Low risk",]$ID, temp3[temp3$alc_cat == "Low risk",]$tochange_by_alc_cat),
                  sample(data[data$alc_cat == "Medium risk",]$ID, temp3[temp3$alc_cat == "Medium risk",]$tochange_by_alc_cat),
                  sample(data[data$alc_cat == "High risk",]$ID, temp3[temp3$alc_cat == "High risk",]$tochange_by_alc_cat))
    
    # change alcohol category to former drinker 
    data <- data %>% mutate(alc_cat = ifelse(ID %in% tochange, "Non-drinker", alc_cat),
                            alc_gpd = ifelse(alc_cat == "Non-drinker", 0, alc_gpd),
                            drinkingstatus = ifelse(alc_cat == "Non-drinker", 0, 1))
  }
  
  # CONSUMPTION ELASTICITY 
  
  # linear association
  newGPD <- data %>% filter(alc_cat != "Non-drinker") %>%
    mutate(percentreduction = rnorm_pre(log(alc_gpd)^2, mu = cons_elasticity, sd = cons_elasticity_se, r = r, empirical = T),
           newGPD = alc_gpd + (alc_gpd*percentreduction*scenario)) %>%
    dplyr::select(ID, percentreduction, newGPD) 
  
  # merge simulated percentreduction into data
  data <- merge(data, newGPD, by = "ID", all.x = T) %>% 
    mutate(alc_gpd = ifelse(alc_gpd != 0, newGPD, alc_gpd)) %>%
    dplyr::select(-c(newGPD, percentreduction))
  
  return(data)
}