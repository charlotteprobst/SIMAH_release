#' Adds new births- estimating rates version
#' @param
#' @keywords microsimulation
#' @export
#' @examples
#' inward_migration_estimate_rate
inward_migration_estimate_rate <- function(basepop, migration_counts, y, brfss){
  # convert from a rate to the N to remove
  summary <- migration_counts %>%
    filter(Year==y) %>%
    mutate(toadd = MigrationInN) %>%
    dplyr::select(agecat, microsim.init.race, microsim.init.sex, toadd) %>%
    drop_na()

  denominator <- basepop %>%
    mutate(agecat = cut(microsim.init.age,
                        breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,100),
                        labels=c("18","19-24","25-29","30-34","35-39","40-44",
                                 "45-49","50-54","55-59","60-64","65-69",
                                 "70-74","75-79"))) %>%
    group_by(microsim.init.race, microsim.init.sex,agecat) %>%
    tally()

  migration <- left_join(summary, denominator)
  migration$rate <- migration$toadd/migration$n

  summary$cat <- paste(summary$microsim.init.sex, summary$agecat, summary$microsim.init.race, sep="_")
  tojoin <- summary %>% ungroup() %>% dplyr::select(cat, toadd)

  cats <- unique(tojoin$cat)

  windowmin <- y-1
  windowmax <- y+1

  pool <- brfss %>% filter(State==SelectedState) %>% filter(YEAR>=windowmin & YEAR<=windowmax) %>%
    mutate(agecat = cut(microsim.init.age, breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,100),
                        labels=c("18","19-24","25-29","30-34","35-39",
                                 "40-44","45-49","50-54","55-59",
                                 "60-64","65-69","70-74","75-79")),
           cat = paste(microsim.init.sex, agecat, microsim.init.race, sep="_"))
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
      mutate(agecat = cut(microsim.init.age, breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,79),
                          labels=c("18","19-24","25-29","30-34","35-39",
                                   "40-44","45-49","50-54","55-59",
                                   "60-64","65-69","70-74","75-79")),
             cat = paste(microsim.init.sex, agecat, microsim.init.race, sep="_")) %>%
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
  toadd <- left_join(pool, tojoin, by=c("cat")) %>% filter(toadd!=0) %>% group_by(cat) %>%
    # mutate(toadd=round(toadd, digits=0)) %>%
    do(dplyr::sample_n(.,size=unique(toadd), replace=TRUE)) %>%
    # do(slice_sample(.,n=toadd, replace = T)) %>%
    mutate(microsim.init.spawn.year=y) %>% ungroup() %>%
    dplyr::select(microsim.init.age, microsim.init.race, microsim.init.sex, microsim.init.education, microsim.init.drinkingstatus,
                  microsim.init.alc.gpd, microsim.init.BMI,
                  microsim.init.income, microsim.init.spawn.year, agecat, formerdrinker, microsimnewED, AlcCAT)

  from <- max(basepop$microsim.init.id)+1
  to <- (nrow(toadd)) + max(basepop$microsim.init.id)
  microsim.init.id <- from:to
  toadd <- cbind(microsim.init.id, toadd)
  basepopnew <- rbind(basepop, toadd)
  # list <- list(basepop,summarymissing)
  return(list(basepopnew,migration))
}

