formerdrinkers_history <- function(data,lhsSample,sex){
  parameters <- read.csv("SIMAH_workplace/microsim/1_input_data/parameters_lognormal.csv") %>% 
    mutate(sex = recode(sex, "Men"="m","Women"="f"),
           group = paste0(sex, agecat))
  sampling <- function(data,parameters){
    samples <- parameters %>% filter(group==unique(data$group))
    shape1 <- as.numeric(samples$shape1)
    shape2 <- as.numeric(samples$shape2)
    data$former_history <- rlnorm(nrow(data), shape1,shape2)
    return(data)
  }
  history <- data %>% filter(formerdrinker==1) %>% 
    mutate(agecatnew = cut(as.numeric(microsim.init.age), 
                           breaks=c(0,24,34,44,54,64,74,100),
                           labels=c("18-24","25-34","35-44",
                                    "45-54","55-64","65-74","75+")),
           group = paste0(microsim.init.sex, agecatnew)) %>%
    group_by(group) %>% do(sampling(., parameters)) %>% ungroup() %>% 
    dplyr::select(-c(agecatnew, group))
  data <- left_join(data,history)
  
  # impute number of years since last drink 
  parameters <- read.csv("SIMAH_workplace/microsim/1_input_data/yearsincedrinkparams.csv") %>% 
    mutate(sex = recode(sex, "Men"="m","Women"="f"),
           group = paste0(sex, agecat)) %>% 
    dplyr::select(group, b1, b2, min, max)
  
  sampling <- function(data,parameters){
    samples <- parameters %>% filter(group==unique(data$group))
    data$scaled <- rbeta(nrow(data), samples$b1,samples$b2)
    data$yearsincedrink <- (samples$max - samples$min - 10e-10)*
      data$scaled + (samples$min - 10e-9)
    data$yearsincedrink <- round(data$yearsincedrink)
    return(data)
  }
  
  history <- data %>% filter(formerdrinker==1) %>% 
    mutate(agecatnew = cut(as.numeric(microsim.init.age), 
                           breaks=c(0,24,34,44,54,64,74,100),
                           labels=c("18-24","25-34","35-44",
                                    "45-54","55-64","65-74","75+")),
           group = paste0(microsim.init.sex, agecatnew)) %>%
    group_by(group) %>% do(sampling(., parameters)) %>% ungroup() %>% 
    dplyr::select(-c(agecatnew, group, scaled))
  data <- left_join(data,history)
  
  data$yearsincedrink <- ifelse(data$formerdrinker==0, 0, data$yearsincedrink)
  
  # now implement the baseline decay function
  # THRESHOLD <- as.numeric(lhsSample["THRESHOLD"])
  # THRESHOLD_women <- as.numeric(lhsSample["THRESHOLD"]) * as.numeric(lhsSample["THRESHOLD_MODIFIER"])
  THRESHOLD <- 100000
  THRESHOLD_women <- 100000*0.66
  data$Cirrhosis_risk <- ifelse(data$formerdrinker==1 & data$yearsincedrink<=8 & 
                                  data$microsim.init.sex=="m" & 
                                  data$former_history >= THRESHOLD, 1,
                                ifelse(data$formerdrinker==1 & data$yearsincedrink<=8 & 
                                         data$microsim.init.sex=="f" &
                                         data$former_history >= THRESHOLD_women, 1,
                                       ifelse(data$formerdrinker==0, NA, 0)))
  return(data)
}
