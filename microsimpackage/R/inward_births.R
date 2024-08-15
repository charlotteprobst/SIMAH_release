#' Adds new births- estimating rates version
#' @param
#' @keywords microsimulation
#' @export
#' @examples
#' inward_births_estimate_rate
inward_births <- function(basepop, migration_counts, y, brfss, model){
  # convert from a rate to the N to remove
  births <- migration_counts %>% filter(agecat=="18") %>%
    filter(Year==y) %>%
    mutate(toadd = BirthsInN) %>%
    dplyr::select(agecat, race, sex, toadd) %>% drop_na()

  # denominator <- basepop %>%
  #   mutate(agecat = cut(age,
  #                       breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,100),
  #                       labels=c("18","19-24","25-29","30-34","35-39","40-44",
  #                                "45-49","50-54","55-59","60-64","65-69",
  #                                "70-74","75-79"))) %>%
  #   group_by(race, sex) %>%
  #   tally()
  # births <- left_join(births, denominator)
  # births$rate <- births$toadd/births$n

  births$cat <- paste(births$sex, births$agecat, births$race, sep="_")
  tojoin <- births %>% ungroup() %>% dplyr::select(cat, toadd)

  cats <- unique(tojoin$cat)

  windowmin <- y-1
  windowmax <- y+1

  pool <- brfss %>% filter(State==SelectedState) %>% filter(YEAR>=windowmin & YEAR<=windowmax) %>%
    mutate(agecat = cut(age, breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,100),
                        labels=c("18","19-24","25-29","30-34","35-39",
                                 "40-44","45-49","50-54","55-59",
                                 "60-64","65-69","70-74","75-79")),
           cat = paste(sex, agecat, race, sep="_"))
  brfsscats <- unique(pool$cat)
  missing <- setdiff(cats, brfsscats)
  # summary <- tojoin %>% filter(cat %in% missing)
  # summary$toadd / sum(tojoin$toadd)

  if(length(missing)>0){
    summarymissing <- data.frame(YEAR = y,
                                 ncatsmissing = length(missing),
                                 whichcatsmissing = paste(missing),
                                 npopmissing = summary$toadd,
                                 npoptotal = sum(tojoin$toadd),
                                 percentmissing = summary$toadd/sum(tojoin$toadd))
    supp <- brfss %>% filter(region==unique(pool$region)) %>% filter(YEAR>=windowmin &
                                                                       YEAR<=windowmax) %>%
      mutate(agecat = cut(age, breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,79),
                          labels=c("18","19-24","25-29","30-34","35-39",
                                   "40-44","45-49","50-54","55-59",
                                   "60-64","65-69","70-74","75-79")),
             cat = paste(sex, agecat, race, sep="_")) %>%
      filter(cat %in% missing)
    pool <- rbind(pool, supp)
  }else {
    summarymissing <- data.frame(YEAR = y,
                                 ncatsmissing = 0,
                                 whichcatsmissing = 0,
                                 npopmissing = 0,
                                 npoptotal = sum(tojoin$toadd,na.rm=T),
                                 percentmissing = 0)
  }

  if(model=="SIMAH"){
    toadd <- left_join(pool, tojoin, by=c("cat")) %>% filter(toadd!=0) %>% group_by(cat) %>%
      # mutate(toadd=round(toadd, digits=0)) %>%
      do(dplyr::sample_n(.,size=unique(toadd), replace=TRUE)) %>%
      # do(slice_sample(.,n=toadd, replace = T)) %>%
      mutate(spawn_year=y) %>% ungroup() %>%
      dplyr::select(age, race, sex, education, drinkingstatus,
                    alc_gpd, BMI,
                    income, spawn_year, agecat, formerdrinker, education_detailed, alc_cat)
  }else if(model=="CASCADE"){
    toadd <- left_join(pool, tojoin, by=c("cat")) %>% filter(toadd!=0) %>% group_by(cat) %>%
      # mutate(toadd=round(toadd, digits=0)) %>%
      do(dplyr::sample_n(.,size=unique(toadd), replace=TRUE)) %>%
      # do(slice_sample(.,n=toadd, replace = T)) %>%
      mutate(spawn_year=y) %>% ungroup() %>%
      dplyr::select(age, race, sex, education, drinkingstatus,
                    alc_gpd, BMI,
                    income, spawn_year, agecat, formerdrinker, education_detailed, alc_cat,
                    chronicB, chronicC, yearsincedrink, Cirrhosis_risk, grams_10years)

  }
  from <- max(basepop$ID)+1
  to <- (nrow(toadd)) + max(basepop$ID)
  ID <- from:to
  toadd <- cbind(ID, toadd)
  basepopnew <- rbind(basepop, toadd)
  # list <- list(basepop,summarymissing)
  return(basepopnew)
}

