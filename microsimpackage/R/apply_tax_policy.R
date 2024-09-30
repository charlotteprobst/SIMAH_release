#' Apply taxation policy 
#'
#' This function simulates tax policy
#' @param
#' @keywords tax policy
#' @export 
#' @examples


apply_tax_policy <- function(data = basepop, scenario = scenario,
                             participation = participation, part_elasticity = part_elasticity, prob_alcohol_transitions = prob_alcohol_transitions,
                             cons_elasticity = cons_elasticity, cons_elasticity_se = cons_elasticity_se, r_sim_obs = r_sim_obs){
  
  # PARTICIPATION ELASTICITY 
  
  # apply participation elasticity
  if (participation == 0){
    print("no policy effect on drinking participation")
  } 
  
  if (participation == 1){
    if (mean(scenario) != 0) { print("applying participation elasticities") }
    
    # determine weighted number of drinkers to become non-drinkers
    temp <- prob_alcohol_transitions %>% filter(alc_cat != "Non-drinker") %>%
      mutate(race = case_when(
        race == "Black, non-Hispanic" ~ "Black",
        race == "Hispanic" ~ "Hispanic",
        race == "Other, non-Hispanic" ~ "Others",
        race == "White, non-Hispanic" ~ "White"), 
        sex = case_when(
          sex == "Men" ~ "m",
          sex == "Women" ~ "f")) %>% 
      dplyr::select(c("sex", "agecat", "race", "education", "alc_cat", "prob_nondrinker"))
    
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
             prop_change = (-1 * part_elasticity * min(scenario)) / sum(ratio) * ratio, # split up participation elasticity by calculated ratio based on population and TP by alchol category
             tochange_by_alc_cat = round(prop_change*sum(n), 0)) # multiple this group-specific elasticity by total sample size 
    
    # sample IDs to become non-drinkers
    tochange <- c(sample(x = data[data$alc_cat == "Low risk",]$ID, size = temp3[temp3$alc_cat == "Low risk",]$tochange_by_alc_cat),
                  sample(x = data[data$alc_cat == "Medium risk",]$ID, size = temp3[temp3$alc_cat == "Medium risk",]$tochange_by_alc_cat),
                  sample(x = data[data$alc_cat == "High risk",]$ID, size = temp3[temp3$alc_cat == "High risk",]$tochange_by_alc_cat))
    
    # change alcohol category to former drinker 
    data <- data %>% mutate(alc_cat = ifelse(ID %in% tochange, "Non-drinker", alc_cat),
                            alc_gpd = ifelse(alc_cat == "Non-drinker", 0, alc_gpd),
                            drinkingstatus = ifelse(alc_cat == "Non-drinker", 0, 1))
  }
  
  # CONSUMPTION ELASTICITY 
  
  # generic taxation 
  if (policy_model == "tax"){
    
    if (scenario != 0) { print("applying generic consumption elasticities") }
    
  # linear association
  newGPD <- data %>% filter(alc_cat != "Non-drinker") %>%
    mutate(percentreduction = rnorm_pre(log(alc_gpd)^2, mu = cons_elasticity, sd = cons_elasticity_se, r = r_sim_obs, empirical = T),
           newGPD = alc_gpd + (alc_gpd*percentreduction*scenario)) %>%
    dplyr::select(ID, percentreduction, newGPD) 
  
  # merge simulated percentreduction into data
  data <- merge(data, newGPD, by = "ID", all.x = T) %>% 
    mutate(alc_gpd = ifelse(alc_gpd != 0, newGPD, 0)) %>%
    dplyr::select(-c(newGPD, percentreduction))
  
  }
  
  # beverage-specific taxation
  if (policy_model == "price"){
    if (mean(scenario) != 0) { print("applying beverage-specific consumption elasticities") }
    
    data <- assign_beverage_preferences(data)
      
    # get individual-level percent reduction for price policies
    newGPD <- data %>% filter(alc_cat != "Non-drinker") %>%
        mutate(beer_percentreduction = rnorm_pre(log(beergpd)^2, mu = cons_elasticity[1], sd = cons_elasticity_se[1], r = r_sim_obs, empirical = T),
               beer_newGPD = beergpd + (beergpd*beer_percentreduction*scenario[1]),
               wine_percentreduction = rnorm_pre(log(winegpd)^2, mu = cons_elasticity[2], sd = cons_elasticity_se[2], r = r_sim_obs, empirical = T),
               wine_newGPD = winegpd + (winegpd*wine_percentreduction*scenario[2]),
               liq_percentreduction = rnorm_pre(log(liqgpd)^2, mu = cons_elasticity[3], sd = cons_elasticity_se[3], r = r_sim_obs, empirical = T),
               liq_newGPD = liqgpd + (liqgpd*liq_percentreduction*scenario[3]),
               newGPD = beer_newGPD + wine_newGPD + liq_newGPD) %>%
        dplyr::select(ID, newGPD) 
  
    # merge simulated percentreduction into data
    data <- merge(data, newGPD, by = "ID", all.x = T) %>% 
      mutate(alc_gpd = ifelse(alc_gpd != 0, newGPD, 0),
             alc_gpd = ifelse(alc_gpd > 200, 200, alc_gpd)) %>%
      dplyr::select(-c(newGPD, beergpd, winegpd, liqgpd))
    
  }
  
  return(data)
}