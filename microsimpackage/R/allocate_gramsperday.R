#' Allocate individuals a grams per day of consumption - based on categories
#'
#' @param
#' @keywords allocate alcohol
#' @export
#' @examples
#' allocate_gpd
allocate_gramsperday <- function(basepop, y, model, DataDirectory){
  prepdata <- basepop %>% filter(AlcCAT!="Non-drinker") %>%
    mutate(age_var = microsim.init.age, sex_recode = ifelse(microsim.init.sex=="m","Male","Female"),
           YEAR = y, education_summary = microsim.init.education, race_eth = ifelse(microsim.init.race=="BLA","Black",
                                                                                    ifelse(microsim.init.race=="WHI","White",
                                                                                           ifelse(microsim.init.race=="SPA","Hispanic",
                                                                                                  ifelse(microsim.init.race=="OTH","Other",NA)))),
           # lambda = ifelse(sex_recode=="Male",0.06, -0.22),
           group = paste(AlcCAT, microsim.init.education, sex_recode, sep="_"))

  distribution <- read.csv("SIMAH_workplace/microsim/1_input_data/CatContDistr.csv")
  prepdata <- left_join(prepdata, distribution)

samplegpd <- function(data){
    shape <- unique(data$shape)
    rate <- unique(data$rate)
    # min <- unique(data$min)
    # max <- unique(data$max)
    # data$newgpd <- rtrunc(nrow(data), "gamma", min, max, shape=shape, scale=rate)
    newgpd <- rgamma(nrow(data), shape, rate)
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
  basepop <- left_join(basepop, prepdata)
  basepop$microsim.init.alc.gpd <- ifelse(basepop$AlcCAT=="Non-drinker", 0,
                                          basepop$newgpd)
  basepop$newgpd <- NULL
  return(basepop)
}

