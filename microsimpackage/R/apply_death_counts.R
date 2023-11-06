#' Apply death counts - converted to rates in each year of simulation
#' @param
#' @keywords microsimulation
#' @export
#' @examples
#' apply_death_counts

apply_death_counts <- function(basepop, death_counts, y, diseases){
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
  death_counts <- death_counts %>% dplyr::filter(year==y)
  summary <- left_join(summary, death_counts, by=c("cat"))
  summary <- summary %>% pivot_longer(cols = contains("mort"), names_to="cause", values_to="count") %>%
    mutate(cause = gsub("mort","",cause)) %>%
    filter(!cause %in% diseases) #remove causes of death being modelled
  summary <- summary %>%
    mutate(count = round(count, digits=0), proportion = count/n) %>%
    group_by(cat) %>% mutate(rate=cumsum(proportion))
  options(digits=22)
  rates <- summary %>% dplyr::select(cat, rate) %>% group_by(cat) %>%
    summarise(overallrate = max(rate))
  basepop$prob <- runif(nrow(basepop))
  basepop <- left_join(basepop, rates) %>%
    mutate(dead = ifelse(prob <= overallrate, 1,0))

  # dead <- basepop %>% filter(dead==1)
#   fun <- function(x,summary){
#     selected <- unique(x$cat)
#     rates <- summary %>% filter(cat==selected)
#     x$prob <- runif(nrow(x)
    # causes <- unique(rates$cause)
    # idcause <- expand.grid(microsim.init.id = dead$microsim.init.id, cause = causes)
    # probs <- dead %>% ungroup() %>% dplyr::select(microsim.init.id, prob)
    # idcause <- left_join(idcause, probs, by=c("microsim.init.id"))
    # rates <- rates %>% ungroup() %>% dplyr::select(cause, proportion)
    # idcause <- left_join(idcause, rates, by=c("cause")) %>%
    #   group_by(microsim.init.id) %>%
    #   mutate(cprob = cumsum(proportion))
    # idcause <- idcause[order(idcause$microsim.init.id),]
    # causes <- list()
    # for(i in unique(idcause$microsim.init.id)){
    #   data <- idcause %>% filter(microsim.init.id==i) %>%
    #     mutate(cprob = cumsum(proportion))
    #   data$selectedcause <- 0
    #
    #   for(j in 1:nrow(data)){
    #     cause <- data$cause[j]
    #     if(cause=="LVDC"){
    #       data$cause[j] <- ifelse(data$prob[j] <= data$cprob[j], cause, 0)
    #     }else{
    #       data$cause[j] <- ifelse(data$prob[j]>data$cprob[j-1] & data$prob[j]<=data$cprob[j], cause, 0)
    #     }
    #   }
    #   data <- data %>% dplyr::select(microsim.init.id, cause) %>% filter(cause!=0)
    #   causes[[paste(i)]] <- data
    # }
    # causes <- do.call(rbind, causes)
    # x <- left_join(x,causes, by=c("microsim.init.id"))
    # x$cause <- ifelse(x$prob<=rates$rate[1], "LVDC",
    #                   ifelse(x$prob>rates$rate[1] & x$prob<=rates$rate[2], "HLVDCmort",
    #                          ifelse(x$prob>rates$rate[2] & x$prob<=rates$rate[3], "DM",
    #                                 ifelse(x$prob>rates$rate[3] & x$prob<=rates$rate[4], "IHD",
    #                                        ifelse(x$prob>rates$rate[4] & x$prob<=rates$rate[5], "ISTR",
    #                                               ifelse(x$prob>rates$rate[5] & x$prob<=rates$rate[6], "HYPHD",
    #                                                      ifelse(x$prob>rates$rate[6] & x$prob<=rates$rate[7], "AUD",
    #                                                             ifelse(x$prob>rates$rate[7] & x$prob<=rates$rate[8], "UIJ",
    #                                                                    ifelse(x$prob>rates$rate[8] & x$prob<=rates$rate[9], "MVACC",
    #                                                                           ifelse(x$prob>rates$rate[9] & x$prob<=rates$rate[10], "IJ",
  #   #                                                                                  ifelse(x$prob>rates$rate[10] & x$prob<=rates$rate[11], "REST",
  #   #                                                                                  NA)))))))))))
  #   return(x)
  # }
  # dead <- dead %>% group_by(cat) %>% do(fun(., summary))
  basepop <- basepop %>% ungroup() %>% dplyr::select(-c(cat, prob)) %>% mutate(cause=NA)
  return(basepop)
}
