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
  # remove hard coding of the mortality causes
  # select all cases where "mort" is in the name of the column
  # use that to recode the line below in pivot_longer
  # potentially need to use the naming conventions for selecting names we use in other parts of the simulation
  summary <- summary %>% pivot_longer(cols = contains("mort"), names_to="cause", values_to="count") %>%
    mutate(cause = gsub("mort","",cause))
    # filter(!cause %in% diseases) #remove causes of death being modelled
  summary <- summary %>% group_by(cat) %>%
    mutate(count = round(count, digits=0), proportion = count/n,
           cprob=cumsum(proportion))
  options(digits=22)

  rates <- summary %>% dplyr::select(cat,cause,cprob) %>% group_by(cat)

  alldiseases <- gsub("mort", "", names(death_counts)[grepl("mort", names(death_counts))])

  sample_causes <- function(data, rates){
    selectedcat <- unique(data$cat)
    rate <- rates %>% filter(cat==selectedcat) %>%
      pivot_wider(names_from=cause, values_from=cprob)
    data$sampledprob <- runif(nrow(data))
    data <- left_join(data, rate, by=c("cat"))
    # max <- length(alldiseases)-1
    data <- data %>%
      mutate(cause =
               case_when(
                 sampledprob <= !!sym(paste0(alldiseases[1])) ~ alldiseases[1],
                 sampledprob > !!sym(paste0(alldiseases[1])) & sampledprob<= !!sym(paste0(alldiseases[2])) ~ alldiseases[2],
                 sampledprob > !!sym(paste0(alldiseases[2])) & sampledprob<= !!sym(paste0(alldiseases[3])) ~ alldiseases[3],
                 sampledprob > !!sym(paste0(alldiseases[3])) & sampledprob<= !!sym(paste0(alldiseases[4])) ~ alldiseases[4],
                 sampledprob > !!sym(paste0(alldiseases[4])) & sampledprob<= !!sym(paste0(alldiseases[5])) ~ alldiseases[5],
                 sampledprob > !!sym(paste0(alldiseases[5])) & sampledprob<= !!sym(paste0(alldiseases[6])) ~ alldiseases[6],
                 sampledprob > !!sym(paste0(alldiseases[6])) & sampledprob<= !!sym(paste0(alldiseases[7])) ~ alldiseases[7],
                 sampledprob > !!sym(paste0(alldiseases[7])) & sampledprob<= !!sym(paste0(alldiseases[8])) ~ alldiseases[8],
                 sampledprob > !!sym(paste0(alldiseases[8])) & sampledprob<= !!sym(paste0(alldiseases[9])) ~ alldiseases[9],
                 sampledprob > !!sym(paste0(alldiseases[9])) & sampledprob<= !!sym(paste0(alldiseases[10])) ~ alldiseases[10],
                 sampledprob > !!sym(paste0(alldiseases[10])) & sampledprob<= !!sym(paste0(alldiseases[11])) ~ alldiseases[11],
                 sampledprob > !!sym(paste0(alldiseases[11])) ~ "alive"
                 ))
    # filter out those who are alive
    deaths <- data %>% filter(cause!="alive")
    # filter out those who need to die from alcohol-related mortality later
    # (by filtering on disease vector)
    deaths <- deaths %>%
      filter(!cause %in% diseases)
    deaths <- deaths %>% dplyr::select(microsim.init.id)
    # return a list of who died
    return(deaths)
  }

  print("sampling all-cause mortality")
  deaths <- basepop %>%
    group_by(cat) %>%
    do(sample_causes(., rates=rates))

  basepopremoved <-basepop %>% filter(!microsim.init.id %in% deaths$microsim.init.id) %>%
    dplyr::select(-cat)



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
  # basepop <- basepop %>% ungroup() %>% dplyr::select(-c(cat, prob)) %>% mutate(cause=NA)
  return(basepopremoved)
}
