#' Apply death counts - converted to rates in each year of simulation
#' @param
#' @keywords microsimulation
#' @export
#' @examples
#' apply_death_counts

apply_death_counts <- function(basepop, death_counts, y, diseases){
  basepop <- basepop %>% mutate(agecat = cut(age,
                                             breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,100),
                                             labels=c("18-24","25-29","30-34","35-39","40-44","45-49",
                                                      "50-54","55-59","60-64","65-69","70-74","75-79")),
                                cat = paste(sex,agecat,race,education, sep=""))
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
    deaths <- deaths %>% dplyr::select(ID)
    # return a list of who died
    return(deaths)
  }

  print("sampling all-cause mortality")
  deaths <- basepop %>%
    group_by(cat) %>%
    do(sample_causes(., rates=rates))

  # # quicker alternative for alcohol calibration purposes
  # rates <- rates %>% group_by(cat) %>% summarise(probdeath=max(cprob))
  # basepop <- left_join(basepop, rates, by=c("cat"))
  #
  # basepop$prob <- runif(nrow(basepop))
  # basepop$death <- ifelse(basepop$prob <= basepop$probdeath, 1,0)
  #
  # deaths <- basepop %>% filter(death==1)
  basepopremoved <-basepop %>% filter(!ID %in% deaths$ID) %>%
    dplyr::select(-c(cat))
  return(basepopremoved)
}
