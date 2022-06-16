#' Apply death rates in each year of simulation
#' @param 
#' @keywords microsimulation
#' @export
#' @examples
#' apply_death_rates

apply_death_rates <- function(basepop, deathrates, y){
  basepop <- basepop %>% mutate(agecat = cut(microsim.init.age,
                                             breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,100),
                                             labels=c("18-24","25-29","30-34","35-39","40-44","45-49",
                                                      "50-54","55-59","60-64","65-69","70-74","75-79")),
                                microsim.init.education = recode(microsim.init.education,
                                                                 "highschool"="LEHS",
                                                                 "somecollege"="SomeC",
                                                                 "collegeplus"="College"),
                                cat = paste(microsim.init.sex,agecat,microsim.init.race,microsim.init.education, sep=""))
  summary <- basepop %>% 
    mutate(n=1) %>% 
    complete(cat, fill=list(n=0)) %>%
    group_by(cat, .drop=FALSE) %>% 
    summarise(n=sum(n))
  deathrates <- deathrates %>% dplyr::filter(year==y)
  summary <- left_join(summary, deathrates)
  summary <- summary %>% pivot_longer(cols=LVDCmort: RESTmort, names_to="cause", values_to="count")
  summary <- summary %>% 
    mutate(count = round(count, digits=0), proportion = count/n) %>% group_by(cat) %>% mutate(rate=cumsum(proportion))
  options(digits=22)
  basepop$prob <- runif(nrow(basepop))
  fun <- function(x,summary){
    selected <- unique(x$cat)
    rates <- summary %>% filter(cat==selected)
    x$dead <- ifelse(x$prob<=rates$rate[10],1,0)
    x$cause <- ifelse(x$prob<=rates$rate[1], "LVDC",
                      ifelse(x$prob>rates$rate[1] & x$prob<=rates$rate[2], "DM",
                             ifelse(x$prob>rates$rate[2] & x$prob<=rates$rate[3], "IHD",
                                    ifelse(x$prob>rates$rate[3] & x$prob<=rates$rate[4], "ISTR",
                                           ifelse(x$prob>rates$rate[4] & x$prob<=rates$rate[5], "HYPHD",
                                                  ifelse(x$prob>rates$rate[5] & x$prob<=rates$rate[6], "AUD",
                                                         ifelse(x$prob>rates$rate[6] & x$prob<=rates$rate[7], "UIJ",
                                                                ifelse(x$prob>rates$rate[7] & x$prob<=rates$rate[8], "MVACC",
                                                                       ifelse(x$prob>rates$rate[8] & x$prob<=rates$rate[9], "IJ",
                                                                              ifelse(x$prob>rates$rate[9] & x$prob<=rates$rate[10], "REST",
                                                                                     NA))))))))))
    
    
    
    
    return(x)
  }
  basepop <- basepop %>% group_by(cat) %>% do(fun(., summary))
  basepop <- basepop %>% dplyr::select(-c(cat, prob))
  return(basepop)
}