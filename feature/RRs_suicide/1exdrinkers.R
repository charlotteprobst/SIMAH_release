library(tidyverse)
library(meta)
library(metafor)

library(readxl)
exdrinkers <- read_excel("CAMH/Suicide/SIMAH_workplace/1exdrinkers.xlsx", 
                           col_types = c("numeric", "numeric", "numeric", 
                                         "numeric", "text", "numeric", "text", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric"))
exdrinkers$sex <- as.factor(exdrinkers$sex)

ex.male <- exdrinkers %>%
  filter(sex == 0 & outcome == 0)
ex.female <- exdrinkers %>%
  filter(sex == 1 & outcome == 0)

ex.male <- exdrinkers %>%
  filter(sex == 0)
ex.female <- exdrinkers %>%
  filter(sex == 1)

meta.ex.male <- metagen(TE = lnor,
                      lower = ln.low,
                      upper = ln.hi,
                      studlab = cohort_id,
                      data = ex.male,
                      sm = "RR",
                      fixed = FALSE,
                      random = TRUE,
                      method.tau = "REML")
summary(meta.ex.male)

meta.ex.female <- metagen(TE = lnor,
                          lower = ln.low,
                          upper = ln.hi,
                          studlab = cohort_id,
                          data = ex.female,
                          sm = "RR",
                        fixed = FALSE,
                        random = TRUE,
                        method.tau = "REML")
summary(meta.ex.female)

forest.meta(meta.ex.male,
            print.tau2 = FALSE, leftcols = c("author"), 
            leftlabs = c("Studies"), text.random = "Overall effect")

forest.meta(meta.ex.female,
            print.tau2 = FALSE, leftcols = c("author"), 
            leftlabs = c("Studies"), text.random = "Overall effect")
