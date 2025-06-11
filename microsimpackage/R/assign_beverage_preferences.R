#' Assign each individual in the microsimulation with a proportion of beer, wine, liquor, coolers
#' @param
#' @keywords assign beverage preferences
#' @export
#' @examples
#' assign beverage preferences
assign_beverage_preferences <- function(data){

  # read in beverage preference data and recode
  beverages <- read_csv("SIMAH_workplace/microsim/1_input_data/NESARC1+3_beverage preference.csv",
                        show_col_types = FALSE) %>%
    mutate(race = case_when(race4=="White, non-Hispanic" ~ "White",
                            race4=="Black, non-Hispanic" ~ "Black",
                            race4=="Hispanic" ~ "Hispanic",
                            race4=="Other, non-Hispanic" ~ "Others"),
           education = case_when(edu3=="Low" ~ "LEHS",
                                 edu3=="Med" ~ "SomeC",
                                 edu3=="High" ~ "College"),
           sex = case_when(sex=="Men" ~ "m",
                           sex=="Women" ~ "f"),
           age_group = age3,
           alc_cat = case_when(
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
    dplyr::select(c("sex", "age_group", "education", "race", "alc_cat",
                    "beerp", "beerse", "winep", "winese", "liqp", "liqse"))

  # now sort out the population data to get in the correct categories
  
  data <- data %>%
    mutate(age_group = dplyr::case_when(
      (age >= 18 & age < 25) ~ "18-24",
      (age >= 25 & age <= 49) ~ "25-49",
      (age >=50) ~ "50+"))

  library(truncnorm)

  data <- suppressMessages(left_join(data, beverages)) %>%

    # sample individual beverage preference
    mutate(beerprop = case_when(
             alc_cat != "Non-drinker" ~ rtruncnorm(nrow(.), a = 0, b = 1, m = beerp, sd = beerse),
             TRUE ~ 0),
           wineprop = case_when(
             alc_cat != "Non-drinker" ~ rtruncnorm(nrow(.), a = 0, b = 1, m = winep, sd = winese),
             TRUE ~ 0),
           liquorprop = case_when(
             alc_cat != "Non-drinker" ~ rtruncnorm(nrow(.), a = 0, b = 1, m = liqp, sd = liqse),
             TRUE ~ 0)) %>%

    # re-normalize proportions so that they add up to 1
    mutate(rbeerprop = case_when(
             alc_cat != "Non-drinker" ~ beerprop / (beerprop + wineprop + liquorprop),
             TRUE ~ 0),
           rwineprop = case_when(
             alc_cat != "Non-drinker" ~ wineprop / (beerprop + wineprop + liquorprop),
             TRUE ~ 0),
           rliquorprop = case_when(
             alc_cat != "Non-drinker" ~ liquorprop / (beerprop + wineprop + liquorprop),
             TRUE ~ 0))

  # calculate beverage-specific gpd
  data <- data %>%
    mutate(beergpd = rbeerprop*alc_gpd,
           winegpd = rwineprop*alc_gpd,
           liqgpd = rliquorprop*alc_gpd) %>%
    dplyr::select(-c(age_group, beerp, beerse, winep, winese, liqp, liqse,
                     beerprop, wineprop, liquorprop, rbeerprop, rwineprop, rliquorprop))

  #check
  #pdat <- data %>% mutate(check = alc_gpd - beergpd - winegpd - liqgpd)
  # pdat <- data %>%
  #   mutate(age_group = dplyr::case_when(
  #     (age >= 18 & age <= 25) ~ "18-25",
  #     (age >= 26 & age <= 49) ~ "26-49",
  #     (age >=50) ~ "50+"),
  #     beerprop = beergpd / alc_gpd,
  #     wineprop = winegpd / alc_gpd,
  #     liquorprop = liqgpd / alc_gpd) %>%
  #   group_by(sex, age_group, education, race, alc_cat) %>%
  #   summarise(beerprop = mean(beerprop),
  #             wineprop = mean(wineprop),
  #             liquorprop = mean(liquorprop))
  #
  # pdat <- pdat %>% filter(alc_cat != "Non-drinker") %>%
  #   pivot_longer(cols = c("beerprop", "wineprop", "liquorprop"),
  #                names_to = "beverage", values_to = "prop") %>%
  #   mutate(education = factor(education, levels = c("LEHS", "SomeC", "College")),
  #          race = factor(race, levels = c("WHI", "BLA", "SPA", "OTH")),
  #          alc_cat = factor(alc_cat, levels = c("Low risk", "Medium risk", "High risk")))
  #
  # ggplot(pdat[pdat$age_group == "26-49" & pdat$sex == "m",]) +
  #   geom_col(aes(x = alc_cat, y = prop, group = as.factor(beverage), fill = as.factor(beverage))) +
  #   facet_grid(rows = vars(education), cols = vars(race)) +
  #   ggtitle("Men | 26-49")

  return(data)
}
