#' Transitions alcohol consumption of individuals - through deterministic method
#' @param
#' @keywords microsimulation, alcohol
#' @export
#' @examples
#' transition_alcohol_determ
transition_alcohol_determ <- function(data, brfssdata, y){
  # create a dataset with the required variables dummy coded
  data <- as.data.table(data)
  brfssdata <- as.data.table(brfssdata)

  # subset by year - y+1
  brfssdata <- brfssdata[YEAR==y+1]

  # create required age categories in both data
  data[, c("agecat") := .(cut(microsim.init.age,
                              breaks = c(0, 24, 64, 100),
                              labels = c("18-24","25-64","65+")))]
  brfssdata[, c("agecat") := .(cut(microsim.init.age,
                                   breaks = c(0, 24, 64, 100),
                                   labels = c("18-24","25-64","65+")))]

  brfssdata <- brfssdata[, .(agecat, microsim.init.sex, microsim.init.race,
                             microsim.init.education, microsim.init.alc.gpd)]

  Nsmicrosim <- data[, .(nmicrosim = .N), by = .(agecat, microsim.init.sex, microsim.init.race,
                                                 microsim.init.education)]
  NsBRFSS <- brfssdata[, .(nbrfss = .N), by = .(agecat, microsim.init.sex, microsim.init.race,
                                                microsim.init.education)]

  Ns <- merge(Nsmicrosim, NsBRFSS, by = c("microsim.init.sex", "agecat", "microsim.init.race",
                                          "microsim.init.education"))

  Ns[, max := ifelse(nmicrosim > nbrfss, "microsim", "brfss")]
  Ns[, which := ifelse(max == "microsim", "brfss", "microsim")]
  Ns[, torank := ifelse(which == "microsim", nmicrosim, nbrfss)]

  data <- merge(data, Ns, by = c("microsim.init.sex", "agecat", "microsim.init.race","microsim.init.education"))
  brfssdata <- merge(brfssdata, Ns, by = c("microsim.init.sex", "agecat", "microsim.init.race","microsim.init.education"))

  distribution <- data %>%
    mutate(random_no = runif(nrow(.))) %>%
    group_by(microsim.init.sex, agecat, microsim.init.race, microsim.init.education) %>%
    mutate(microsim.init.alc.gpd_random = ifelse(microsim.init.alc.gpd==0,
                                                 floor(rgamma(100, 1.8, rate=2)),
                                                 ifelse(random_no>=0.8,
                                                        microsim.init.alc.gpd*1+rtruncnorm(n(), a=0,b=1, mean=0, sd=20),
                                                        microsim.init.alc.gpd)),
           microsim.init.alc.gpd_random = ifelse(microsim.init.alc.gpd_random>200, 200,
                                                 ifelse(microsim.init.alc.gpd_random<0, 0, microsim.init.alc.gpd_random))) %>%
    #allow some people to change their alc use - but most don't - some up some down
    arrange(microsim.init.alc.gpd_random) %>%
    mutate(rank = ntile(microsim.init.alc.gpd_random, unique(torank)))

  brfssdata <- brfssdata %>% group_by(microsim.init.sex, agecat, microsim.init.race,
                                      microsim.init.education) %>%
    arrange(microsim.init.alc.gpd) %>%
    mutate(rank = (row_number() - 1) %/% ceiling(n() / unique(torank)) + 1,
           rank = ntile(microsim.init.alc.gpd, unique(torank))) %>%
    group_by(microsim.init.sex, agecat, microsim.init.race,
             microsim.init.education, rank) %>%
    summarise(newGPD = mean(microsim.init.alc.gpd))

  distribution <- as.data.table(distribution)

  distribution <- merge(distribution, brfssdata, by = c("microsim.init.sex", "agecat", "microsim.init.race",
                                                        "microsim.init.education", "rank"))

  distribution <- distribution[, .(microsim.init.id, newGPD)]

  data <- left_join(data, distribution, by=c("microsim.init.id"))

  data <- data %>%
    mutate(formerdrinker = ifelse(formerdrinker==1 & newGPD==0, 1,
                                  ifelse(formerdrinker==1 & newGPD>0, 0,
                                         ifelse(microsim.init.alc.gpd>0 & newGPD==0, 1,
                                                formerdrinker))),
           newGPD = ifelse(newGPD>200, 200, newGPD),
           microsim.init.alc.gpd = newGPD) %>%
    dplyr::select(-c(nmicrosim, nbrfss, max, which, torank, newGPD))
  return(data)
}
