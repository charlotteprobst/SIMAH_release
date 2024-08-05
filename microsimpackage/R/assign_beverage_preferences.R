#' Assign each individual in the microsimulation with a proportion of beer, wine, liquor, coolers
#' @param
#' @keywords assign beverage preferences
#' @export
#' @examples
#' assign beverage preferences
assign_beverage_preferences <- function(data){
  
  # read in beverage preference data and recode
  beverages <- read_csv("SIMAH_workplace/microsim/1_input_data/NESARC1+3_beverage preference.csv") %>%
    mutate(microsim.init.race = case_when(race4=="White, non-Hispanic" ~ "WHI",
                            race4=="Black, non-Hispanic" ~ "BLA",
                            race4=="Hispanic" ~ "SPA",
                            race4=="Other, non-Hispanic" ~ "OTH"),
           microsim.init.education = case_when(edu3=="Low" ~ "LEHS",
                                 edu3=="Med" ~ "SomeC",
                                 edu3=="High" ~ "College"),
           microsim.init.sex = case_when(sex=="Men" ~ "m",
                                         sex=="Women" ~ "f"),
           age_group = age3, 
           AlcCAT = case_when(
             AlcUse4 == "Category I" ~ "Low risk",
             AlcUse4 == "Category II" ~ "Medium risk",
             AlcUse4 == "Category III" ~ "High risk",
             AlcUse4 == "Non-drinker" ~ "Non-drinker"),
           beerp = beers_prop_wcoolers / 100,
           beerse = beer_se / 100,
           winep = wine_prop / 100,
           winese = wine_se / 100,
           liqp = liquor_prop / 100,
           liqse = liquor_se / 100) %>%
    dplyr::select(c("microsim.init.sex", "age_group", "microsim.init.education", "microsim.init.race", "AlcCAT",
                    "beerp", "beerse", "winep", "winese", "liqp", "liqse")) 
  
  # now sort out the population data to get in the correct categories
  data <- data %>% 
    mutate(age_group = dplyr::case_when(
      (microsim.init.age >= 18 & microsim.init.age <= 25) ~ "18-25",
      (microsim.init.age >= 26 & microsim.init.age <= 49) ~ "26-49",
      (microsim.init.age >=50) ~ "50+"))
  
  library(truncnorm)
  
  data <- left_join(data, beverages) %>% 
    
    # sample individual beverage preference 
    mutate(beerprop = case_when(
             microsim.init.drinkingstatus == 1 ~ rtruncnorm(nrow(.), a = 0, b = 1, m = beerp, sd = beerse),
             TRUE ~ 0), 
           wineprop = case_when(
             microsim.init.drinkingstatus == 1 ~ rtruncnorm(nrow(.), a = 0, b = 1, m = winep, sd = winese),
             TRUE ~ 0), 
           liquorprop = case_when(
             microsim.init.drinkingstatus == 1 ~ rtruncnorm(nrow(.), a = 0, b = 1, m = liqp, sd = liqse),
             TRUE ~ 0)) %>%
    
    # re-normalize proportions so that they add up to 1
    mutate(rbeerprop = case_when(
             microsim.init.drinkingstatus == 1 ~ beerprop / (beerprop + wineprop + liquorprop),
             TRUE ~ 0),
           rwineprop = case_when(
             microsim.init.drinkingstatus == 1 ~ wineprop / (beerprop + wineprop + liquorprop),
             TRUE ~ 0),
           rliquorprop = case_when(
             microsim.init.drinkingstatus == 1 ~ liquorprop / (beerprop + wineprop + liquorprop),
             TRUE ~ 0))

  # calculate beverage-specific gpd
  data <- data %>%
    mutate(beergpd = rbeerprop*microsim.init.alc.gpd,
           winegpd = rwineprop*microsim.init.alc.gpd,
           liqgpd = rliquorprop*microsim.init.alc.gpd) %>% 
    dplyr::select(-c(age_group, beerp, beerse, winep, winese, liqp, liqse, 
                     beerprop, wineprop, liquorprop, rbeerprop, rwineprop, rliquorprop))
  
  #check
  #pdat <- data %>% mutate(check = microsim.init.alc.gpd - beergpd - winegpd - liqgpd)
  # pdat <- data %>%
  #   mutate(age_group = dplyr::case_when(
  #     (microsim.init.age >= 18 & microsim.init.age <= 25) ~ "18-25",
  #     (microsim.init.age >= 26 & microsim.init.age <= 49) ~ "26-49",
  #     (microsim.init.age >=50) ~ "50+"),
  #     beerprop = beergpd / microsim.init.alc.gpd,
  #     wineprop = winegpd / microsim.init.alc.gpd,
  #     liquorprop = liqgpd / microsim.init.alc.gpd) %>%
  #   group_by(microsim.init.sex, age_group, microsim.init.education, microsim.init.race, AlcCAT) %>%
  #   summarise(beerprop = mean(beerprop),
  #             wineprop = mean(wineprop),
  #             liquorprop = mean(liquorprop))
  # 
  # pdat <- pdat %>% filter(AlcCAT != "Non-drinker") %>%
  #   pivot_longer(cols = c("beerprop", "wineprop", "liquorprop"),
  #                names_to = "beverage", values_to = "prop") %>%
  #   mutate(education = factor(microsim.init.education, levels = c("LEHS", "SomeC", "College")),
  #          race = factor(microsim.init.race, levels = c("WHI", "BLA", "SPA", "OTH")),
  #          AlcCAT = factor(AlcCAT, levels = c("Low risk", "Medium risk", "High risk")))
  # 
  # ggplot(pdat[pdat$age_group == "26-49" & pdat$microsim.init.sex == "m",]) +
  #   geom_col(aes(x = AlcCAT, y = prop, group = as.factor(beverage), fill = as.factor(beverage))) +
  #   facet_grid(rows = vars(education), cols = vars(race)) +
  #   ggtitle("Men | 26-49")
  
  return(data)
}
