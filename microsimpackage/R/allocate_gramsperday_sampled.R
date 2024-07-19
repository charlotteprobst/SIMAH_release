#' Allocate individuals a grams per day of consumption - based on categories
#'
#' @param
#' @keywords allocate alcohol
#' @export
#' @examples
#' allocate_gramsperday_sampled
allocate_gramsperday_sampled <- function(basepop,y,catcontmodel){
  prepdata <- basepop %>% filter(AlcCAT!="Non-drinker" & totransitioncont == 1) %>%
    mutate(age_var = microsim.init.age, sex_recode = ifelse(microsim.init.sex=="m","Male","Female"),
           agecat = cut(microsim.init.age,
                        breaks=c(0,24,64,100),
                        labels=c("18-24","25-64","65+")),
           microsim.init.education = ifelse(agecat=="18-24" & microsim.init.education=="College","SomeC",
                                            microsim.init.education),
           education_summary = microsim.init.education, race_eth = ifelse(microsim.init.race=="BLA","Black",
                                                                                    ifelse(microsim.init.race=="WHI","White",
                                                                                           ifelse(microsim.init.race=="SPA","Hispanic",
                                                                                                  ifelse(microsim.init.race=="OTH","Other",NA)))),
           yearcat = ifelse(y<=2010, "2000-2010","2011+"),
           # lambda = ifelse(sex_recode=="Male",0.06, -0.22),
           group = paste(AlcCAT, education_summary, agecat, race_eth, sex_recode, sep="_"))

  prepdata <- left_join(prepdata, catcontmodel, by=c("group"))

  samplegpd <- function(data){
    # shape <- unique(data$shape)
    # rate <- unique(data$rate)
    shape1 <- unique(data$shape1)
    shape2 <- unique(data$shape2)
    min <- unique(data$min)
    max <- unique(data$max)
    # newgpd <- rgamma(nrow(data), shape, rate)
    raw <- rbeta(nrow(data), shape1, shape2)
    newgpd <- ((max - min + 10e-10)*raw) + (min - 10e-9)
    # newgpd <- order(newgpd)
    data <- data[order(data$microsim.init.alc.gpd),]
    newgpd <- sort(newgpd)
    data$newgpd <- newgpd
    data$newgpd <- ifelse(data$newgpd > 200, 200, data$newgpd)
    return(data)
  }

  prepdata <- prepdata %>% group_by(group) %>% do(samplegpd(.)) %>%
    mutate(microsim.init.alc.gpd = newgpd) %>% ungroup() %>%
    dplyr::select(microsim.init.id, newgpd)

  # # lambda <- 0.129 #lambda for transforming consumption taken from M.Strong / D.Moyo script
  # back.tran <- function(x, lambda){(lambda * x + 1) ^ (1/lambda)}
  #
  # women <- prepdata %>% filter(sex_recode=="Female")
  # women$newgpd <- back.tran(predict(model[[1]], women), lambda=-0.22)
  # women <- women %>% dplyr::select(microsim.init.id, newgpd)
  #
  # men <- prepdata %>% filter(sex_recode=="Male")
  # men$newgpd <- back.tran(predict(model[[2]], men), lambda=0.06)
  # men <- men %>% dplyr::select(microsim.init.id, newgpd)
  # prepdata <- rbind(men, women)
  basepop <- left_join(basepop, prepdata, by=c("microsim.init.id"))
  # basepop$microsim.init.alc.gpd <- ifelse(basepop$AlcCAT=="Non-drinker", 0,
  #                                         basepop$newgpd)
  # basepop$newgpd <- NULL
  basepop$newgpd <-  ifelse(basepop$totransitioncont==1 & basepop$AlcCAT!="Non-drinker", basepop$newgpd,
                            ifelse(basepop$totransitioncont==0 & basepop$AlcCAT!="Non-drinker", basepop$microsim.init.alc.gpd,
                                   ifelse(basepop$AlcCAT=="Non-drinker", 0, NA)))
  basepop$microsim.init.alc.gpd <- basepop$newgpd
  basepop$newgpd <- NULL
  basepop$totransitioncont <- NULL

  return(basepop)
}

