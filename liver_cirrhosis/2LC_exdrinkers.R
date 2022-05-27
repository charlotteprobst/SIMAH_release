library(tidyverse)
library(meta)
library(metafor)

# Personal computer; specify locations 
data   <- "C:/Users/laura/Documents/CAMH/SIMAH/SIMAH_workplace/liver_cirrhosis/"    # Location of data

# load data
exdrinkers <- readRDS (paste0(data, "2LC_exdrinkers.xlsx"))

library(readxl)
exdrinkers <- read_excel("CAMH/SIMAH/SIMAH_workplace/liver_cirrhosis/2LC_exdrinkers.xlsx", 
                         col_types = c("text", "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "text", "text"))

exdrinkers$sex <- as.factor(exdrinkers$sex)

ex.male <- exdrinkers %>%
  filter(sex %in% c(0,1) & alc == 1)
ex.female <- exdrinkers %>%
  filter(sex %in% c(0,2) & alc == 1)
both <- exdrinkers %>%
  filter(alc==1)

ex.male = ex.male[-(8),]
both = both[-(10),]

meta.ex.male <- metagen(TE = lnor,
                      lower = lncil,
                      upper = lnciu,
                      studlab = study,
                      data = ex.male,
                      sm = "RR",
                      fixed = FALSE,
                      random = TRUE,
                      method.tau = "REML")
summary(meta.ex.male)

meta.ex.female <- metagen(TE = lnor,
                        lower = lncil,
                        upper = lnciu,
                        level.ci = 0.95,
                        studlab = study,
                        data = ex.female,
                        sm = "RR",
                        fixed = FALSE,
                        random = TRUE,
                        method.tau = "REML")
summary(meta.ex.female)

forest.meta(meta.ex.male,
            print.tau2 = FALSE, leftcols = c("studlab"), 
            leftlabs = c("Studies"), text.random = "Overall effect")

forest.meta(meta.ex.female,
            print.tau2 = FALSE, leftcols = c("studlab"), 
            leftlabs = c("Studies"), text.random = "Overall effect")

meta.ex.both <- metagen(TE = lnor,
                          lower = lncil,
                          upper = lnciu,
                          level.ci = 0.95,
                          studlab = study,
                          data = both,
                          sm = "RR",
                          fixed = FALSE,
                          random = TRUE,
                          method.tau = "REML")
summary(meta.ex.both)

##PROPORTIONS

p.male <- exdrinkers %>%
  filter(sex %in% c(0,1) & alc == 1)
p.female <- exdrinkers %>%
  filter(sex %in% c(0,2) & alc == 1)

p.male = p.male[-(7),]
p.female = p.female[-(7),]
p.both = both[-(9),]

prop.male <- metaprop(event = n.group,
                   n = total,
                   studlab = study,
                   data = p.male,
                   method = "Inverse",
                   sm = "PLOGIT",
                   fixed = FALSE,
                   random = TRUE,
                   hakn = TRUE,
                   title = "Proportion of ex-drinkers")
summary(prop.male)

prop.female <- metaprop(event = n.group,
                      n = total,
                      studlab = study,
                      data = p.female,
                      method = "Inverse",
                      sm = "PLOGIT",
                      fixed = FALSE,
                      random = TRUE,
                      hakn = TRUE,
                      title = "Proportion of ex-drinkers")
summary(prop.female)

prop.both <- metaprop(event = n.group,
                        n = total,
                        studlab = study,
                        data = p.both,
                        method = "Inverse",
                        sm = "PLOGIT",
                        fixed = FALSE,
                        random = TRUE,
                        hakn = TRUE,
                        title = "Proportion of ex-drinkers")
summary(prop.both)
