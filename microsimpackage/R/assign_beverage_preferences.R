#' Assign each individual in the microsimulation with a proportion of beer, wine, liquor, coolers
#' @param
#' @keywords assign beverage preferences
#' @export
#' @examples
#' assign beverage preferences
assign_beverage_preferences <- function(data){
  # read in beverage preference data
  white <- readxl::read_excel("SIMAH_workplace/microsim/1_input_data/NESARC 1_by race.xlsx", sheet = "White") %>%
    rename(risk = riskpttn4, education = educ_cat, age_group = age6, race = race4) %>%
    mutate(race=1)
  black <- readxl::read_excel("SIMAH_workplace/microsim/1_input_data/NESARC 1_by race.xlsx", sheet = "Black") %>%
    rename(risk = riskpttn3, education = educ2, age_group = age3, race = race4) %>% mutate(race=2)
  hispanic <- readxl::read_excel("SIMAH_workplace/microsim/1_input_data/NESARC 1_by race.xlsx", sheet = "Hispanic") %>%
    rename(risk = riskpttn3, education = educ2, age_group = age3, race = race4) %>% mutate(race=3)
  other <- readxl::read_excel("SIMAH_workplace/microsim/1_input_data/NESARC 1_by race.xlsx", sheet = "Other") %>%
    rename(risk = riskpttn3, education = educ2, age_group = age3, race = race4) %>% mutate(race=4) %>%
    dplyr::select(-c(`...27`))
  # recode beverage specific categories
  beverages <- rbind(white, black, hispanic,other) %>%
    mutate(microsim.init.race = case_when(race==1 ~ "WHI",
                            race==2 ~ "BLA",
                            race==3 ~ "SPA",
                            race==4 ~ "OTH"),
           microsim.init.education = case_when(education=="HS grad or less" ~ "LEHS",
                                 education=="HS grad/GED" ~ "LEHS",
                                 education=="less than HS" ~ "LEHS",
                                 education=="some college" ~ "SomeC",
                                 education=="some college plus" ~ "SomeC",
                                 education=="4-yr college/advanced degree" ~ "College"),
           microsim.init.sex = case_when(sex=="male" ~ "m",
                                         sex=="female" ~ "f")) %>%
    group_by(microsim.init.race, microsim.init.education, microsim.init.sex,
             risk, age_group) %>%
    summarise(coolpmn = mean(coolpmn, na.rm=T),
              coolpse = mean(coolpse, na.rm=T),
              beerpmn = mean(beerpmn, na.rm=T),
              beerpse = mean(beerpse, na.rm=T),
              winepmn = mean(winepmn, na.rm=T),
              winepse = mean(winepse, na.rm=T),
              liqpmn = mean(liqpmn, na.rm=T),
              liqpse = mean(liqpse, na.rm=T))

  # impute the missing date
  library(mice)
  imputed <- mice(beverages)
  imputed <- complete(imputed)
  beverages <- imputed

  # now sort out the population data to get in the correct categories
  data <- data %>% dplyr::mutate(
    risk = dplyr::case_when(
      (microsim.init.sex == "m" & microsim.init.alc.gpd < 40) ~ "low risk",
      (microsim.init.sex == "f" & microsim.init.alc.gpd < 20) ~ "low risk",
      (microsim.init.sex == "m" & microsim.init.alc.gpd > 40 & microsim.init.alc.gpd < 60) ~ "medium risk",
      (microsim.init.sex == "f" & microsim.init.alc.gpd > 20 & microsim.init.alc.gpd < 40) ~ "medium risk",
      (microsim.init.sex == "m" & microsim.init.race == "WHI" & microsim.init.alc.gpd > 60 & microsim.init.alc.gpd < 100) ~ "high risk",
      (microsim.init.sex == "f" & microsim.init.race == "WHI" & microsim.init.alc.gpd > 40 & microsim.init.alc.gpd < 60) ~ "high risk",
      (microsim.init.sex == "m" & microsim.init.race == "WHI" & microsim.init.alc.gpd > 100) ~ "very high risk",
      (microsim.init.sex == "f" & microsim.init.race == "WHI" & microsim.init.alc.gpd > 60) ~ "very high risk",
      (microsim.init.sex == "m" & microsim.init.race == "BLA" & microsim.init.alc.gpd > 60) ~ "high/very high risk",
      (microsim.init.sex == "f" & microsim.init.race == "BLA" & microsim.init.alc.gpd > 40) ~ "high/very high risk",
      (microsim.init.sex == "m" & microsim.init.race == "OTH" & microsim.init.alc.gpd > 60) ~ "high/very high risk",
      (microsim.init.sex == "f" & microsim.init.race == "OTH" & microsim.init.alc.gpd > 40) ~ "high/very high risk",
      (microsim.init.sex == "m" & microsim.init.race == "SPA" & microsim.init.alc.gpd > 60) ~ "high/very high risk",
      (microsim.init.sex == "f" & microsim.init.race == "SPA" & microsim.init.alc.gpd > 40) ~ "high/very high risk",
    ),
    age=microsim.init.age,
    race = microsim.init.race,
    education = microsim.init.education,
    age_group = dplyr::case_when(
      (age >= 18 & age <= 24 & race=="WHI") ~ "18-24 yrs",
      (age >= 25 & age <= 34 & race=="WHI") ~ "25-34 yrs",
      (age >=18 & age <= 34 & race!="WHI") ~ "18-34 yrs",
      (age >= 35 & age <= 54 & race!="WHI") ~ "35-54 yrs",
      (age >= 55 & race!="WHI") ~ "55+ yrs",
      (age >= 35 & age <= 44 & race=="WHI") ~ "35-44 yrs",
      (age >= 45 & age <= 54 & race=="WHI") ~ "45-54 yrs",
      (age >= 55 & age <= 64 & race=="WHI") ~ "55-64 yrs",
      (age >=65 & race=="WHI") ~ "65+ yrs"),
    
    ## Why that? Check and potentially drop these two lines of code 
   microsim.init.education=ifelse(race!="WHI"& education=="SomeC","SomeCPlus",
                                 ifelse(race!="WHI"& education=="College","SomeCPlus",education))) %>%
    
    dplyr::select(-c(age,race,education))

  # now combine the two together
  data <- left_join(data, beverages)

  
  
  beverages <- beverages %>%
    mutate(cat = factor(paste0(microsim.init.race, "_", microsim.init.sex, "_", age_group, "_",
                               microsim.init.education, "_", risk)))
  
  pdat <- data %>% filter(!is.na(beerpse) & !is.na(winepse) & !is.na(liqpse)) %>% 
    filter(microsim.init.drinkingstatus == 1) %>% 
    mutate(risk = ifelse(AlcCAT == "Low risk", "low risk", 
                         ifelse(AlcCAT == "Medium risk", "medium risk",
                                ifelse(AlcCAT == "High risk", "high risk", NA))), 
           cat = factor(paste0(microsim.init.race, "_", microsim.init.sex, "_", age_group, "_", 
                               microsim.init.education, "_", risk)))
  
library(truncnorm)
  
beerp <- list()
liqp <- list()
winep <- list()

# Cumulative probability required for sampling?
# Predict probability from fractional regression model instead of mean/SDs by beverage?
# https://stats.stackexchange.com/questions/70855/generating-random-variables-from-a-mixture-of-normal-distributions

  for(i in seq_along(levels(pdat$cat))) {
    
    name <- levels(pdat$cat)[i]
    
    temp <- pdat %>% filter(cat %in% c(levels(cat)[i]))
    bev <- beverages %>% filter(cat %in% c(levels(cat)[i]))
    
    beerp[[paste0(name)]] <- rtruncnorm(nrow(temp), a = 0, b = 1, 
                                        m = bev$beerpmn, sd = bev$beerpse)
    liqp[[paste0(name)]] <- rtruncnorm(nrow(temp), a = 0, b = 1, 
                                       m = bev$liqpmn, sd = bev$liqpse)

  }

  
test <- NULL

for(i in seq_along(levels(pdat$cat))){
  
  test <- rbind(test, cbind(cat = levels(pdat$cat)[i], beerp = beerp[[paste0(name)]], liqp = liqp[[paste0(name)]]))
    
}

check <- as.data.frame(test) %>% 
  mutate(winep = 1 - as.numeric(beerp) - as.numeric(liqp),
         rsum = as.numeric(beerp) + as.numeric(winep) + as.numeric(liqp)) %>%
  filter(winep >= 0)
  


  data <- data %>%
    mutate(beergpd = beerpmn*microsim.init.alc.gpd,
           winegpd = winepmn*microsim.init.alc.gpd,
           liqgpd = liqpmn*microsim.init.alc.gpd,
           coolgpd = coolpmn*microsim.init.alc.gpd)
  return(data)
}
