# historic ages of agents 
HistoryFunction <- function(basepop, ages, lhsSample){
# ages <- basepop %>% dplyr::select(microsim.init.id, microsim.init.sex, microsim.init.age, microsim.init.alc.gpd) %>%
#   mutate(yearstoadd = microsim.init.age-17)
# ages <- expandRows(ages, "yearstoadd", drop=FALSE)
# 
# AgeFunction <- function(data){
#   from <- 18
#   to <- unique(data$microsim.init.age)
#   age <- from:to
#   data$newage <- age
#   return(data)
# }
# # # apply the function to each unique individual
# ages <- ages %>% group_by(microsim.init.id) %>%
#    group_modify(~AgeFunction(.))
# # categorise age in same categories as Kerr 2013
# ages <- ages %>% mutate(agecatnew = cut(newage,
#                    breaks=c(0,20,25,30,40,50,60,70,100),
#                    labels=c("18-20","21-25","26-30","31-40",
#                             "41-50","51-60","61-70","71+")),
#                    agecatorig = cut(microsim.init.age,
#                                     breaks=c(0,20,25,30,40,50,60,70,100),
#                                     labels=c("18-20","21-25","26-30","31-40",
#                                              "41-50","51-60","61-70","71+"))) %>%
#   dplyr::select(microsim.init.id, microsim.init.sex, microsim.init.age, microsim.init.alc.gpd, newage, agecatnew, agecatorig)
# write.csv(ages, "SIMAH_workplace/microsim/1_input_data/agesforhistory.csv")
age <- levels(as.factor(ages$agecatnew))
age <- c("18-20","21-25","26-30","31-40","41-50","51-60","61-70","71+")
IRR <- data.frame(microsim.init.sex = rep(c("m","f"), each=8), agecatnew = rep(age, times=2),
                    IRR=c(0.901, 1.205, 1.086,
                          1.069, 1.00, 0.963, 0.969, 0.88,
                          0.867, 1.26, 1.125, 1.082,
                          1.00, 1.019, 0.982, 0.841),
                    Lower=c(0.76, 1.07, 0.98, 1.00, 1.00, 0.89, 0.85, 0.72,
                            0.72, 1.09, 0.99, 0.99, 1.00, 0.94, 0.86, 0.7),
                    Upper=c(1.06, 1.36, 1.20, 1.15, 1.00, 1.04, 1.11, 1.08,
                            1.04, 1.46, 1.27, 1.18, 1.00, 1.11, 1.12, 1.02))

N <- read.csv("SIMAH_workplace/microsim/1_input_data/NAS_History_N.csv") %>% rename(microsim.init.sex=sex,
                                                         agecatnew=agecat)
IRR <- left_join(IRR, N)
C <- 3.92
IRR$SD <- sqrt(IRR$n) * (IRR$Upper - IRR$Lower) / C
# update the Ns based on the NAS data for each group 
# male samples
correlation <- as.numeric(lhsSample["IRR_correlation"])
IDS <- unique(subset(ages, microsim.init.sex=="m")$microsim.init.id)
male <- rnorm_multi(n=length(IDS),
                    mu=IRR$IRR[1:8],
                    sd=IRR$SD[1:8],
                    r=correlation,
                    varnames=age,
                    empirical=TRUE)
male$microsim.init.sex <- "m"
male$microsim.init.id <- IDS

male <- pivot_longer(male, cols=`18-20`:`71+`, names_to="agecatnew",
                     values_to="IRR")

IDS <- unique(subset(ages, microsim.init.sex=="f")$microsim.init.id)

female <- rnorm_multi(n=length(IDS),
                      mu=IRR$IRR[9:16],
                      sd=IRR$SD[9:16],
                      r=correlation,
                      varnames=age,
                      empirical=TRUE)

female$microsim.init.sex <- "f"

female$microsim.init.id <- IDS

female <- pivot_longer(female, cols=`18-20`:`71+`, names_to="agecatnew",
                       values_to="IRR")

samples <- rbind(male, female)

IRR <- samples %>% dplyr::select(microsim.init.id, microsim.init.sex, agecatnew, IRR)
# recompute lower bound using SD instead of SE for the lowest group (71+)
# anything lower than that value, set to that value 
# worry if it's below 0.1
# IRR$IRR <- ifelse(IRR$IRR<0, as.numeric(lhsSample["max_IRR"]), IRR$IRR)
IRR$IRR <- ifelse(IRR$IRR<0, 0.5, IRR$IRR)
ages$agecatnew <- as.character(ages$agecatnew)
ages <- left_join(ages, IRR, by=c("microsim.init.id","agecatnew"))
IRR$agecatorig <- IRR$agecatnew
IRR$agecatnew <- NULL
IRR$IRRbase <- IRR$IRR
IRR$IRR <- NULL
ages <- left_join(ages, IRR, by=c("microsim.init.id","agecatorig"))
ages$newgpd<- (ages$IRR/ages$IRRbase * ages$microsim.init.alc.gpd)
ages$newgpd <- ifelse(ages$newgpd>200, 200, ages$newgpd)
ages$newdrinking <- ages$newgpd*365
# write.csv(ages, "output_data/ages.csv", row.names=FALSE)
basepophistory <- ages %>% group_by(microsim.init.id) %>% summarise(grams_10years = sum(newdrinking))
# basepophistory$grams_10years <- basepophistory$grams_10years - (basepophistory$grams_10years*0.5)
return(basepophistory)
}

