# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: Sunday sales ban / prepare data  
## State: Indiana, Minnesota
## Author: Carolin Kilian
## Start Date: 28/02/2023
# ----------------------------------------------------------------
# ----------------------------------------------------------------

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# LIBARIES
# ----------------------------------------------------------------
# ----------------------------------------------------------------

library(tidyverse)
library(data.table)
library(dplyr)
library(openxlsx)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")
DATE <- 20230228

datUNEMP_IND <- data.table(read.xlsx("acp_brfss/data/20230209_UNEMPLOYMENT_Indiana.xlsx", sheet = 1, startRow = 12))
datUNEMP_MIN <- data.table(read.xlsx("acp_brfss/data/20230214_UNEMPLOYMENT_Minnesota.xlsx", sheet = 1, startRow = 12))
datUNEMP_SEX <- data.table(read.xlsx("acp_brfss/data/20230228_US_unemployment_by_SEX.xlsx", sheet = 1, startRow = 1))

datPOP_IND <- data.table(read.xlsx("acp_brfss/data/20230228_Indiana_POP_2000-2010.xlsx", sheet = 1, rows = c(4:97)))
datPOP_MIN <- data.table(read.xlsx("acp_brfss/data/20230228_Minnesota_POP_2000-2010.xlsx", sheet = 1, rows = c(4:97)))
datPOP_US <- data.table(read.xlsx("acp_brfss/data/20230228_US POP_2010-2020.xlsx", sheet = 1, startRow = 1))


# --------------------------------------------------------------------------------------
# UNEMPLOYMENT
# --------------------------------------------------------------------------------------

# aggregate monthly unemployment data

datUNEMP_IND <- datUNEMP_IND %>% 
  mutate(Q = ifelse(Period %like% "M01|M02|M03", 125, ifelse(Period %like% "M04|M05|M06", 375, ifelse(Period %like% "M07|M08|M09", 625, ifelse(Period %like% "M10|M11|M12", 875, NA)))),
         QYEAR = paste0(Year, ".", Q)) %>%
  group_by(QYEAR) %>%
  summarise(unemp.rate = mean(Observation.Value)) %>% 
  filter(as.numeric(QYEAR) < 2020.375) %>% as.data.table()

datUNEMP_MIN <- datUNEMP_MIN %>% 
  mutate(Q = ifelse(Period %like% "M01|M02|M03", 125, ifelse(Period %like% "M04|M05|M06", 375, ifelse(Period %like% "M07|M08|M09", 625, ifelse(Period %like% "M10|M11|M12", 875, NA)))),
         QYEAR = paste0(Year, ".", Q)) %>%
  group_by(QYEAR) %>%
  summarise(unemp.rate = mean(Observation.Value)) %>% 
  filter(as.numeric(QYEAR) < 2020.375) %>% as.data.table()

# account for sex difference

datUNEMP_IND[, year := trunc(as.numeric(QYEAR))]
datUNEMP_IND <- merge(datUNEMP_IND, datUNEMP_SEX, by = "year", all.x = T)
datUNEMP_IND[, w.unemp.rate := (2 * unemp.rate) / (unemp_MtoW + 1)]
datUNEMP_IND[, m.unemp.rate := (2 * unemp.rate) - w.unemp.rate]
datUNEMP_IND <- datUNEMP_IND[,.(year, QYEAR, unemp.rate, w.unemp.rate, m.unemp.rate)]

datUNEMP_MIN[, year := trunc(as.numeric(QYEAR))]
datUNEMP_MIN <- merge(datUNEMP_MIN, datUNEMP_SEX, by = "year", all.x = T)
datUNEMP_MIN[, w.unemp.rate := (2 * unemp.rate) / (unemp_MtoW + 1)]
datUNEMP_MIN[, m.unemp.rate := (2 * unemp.rate) - w.unemp.rate]
datUNEMP_MIN <- datUNEMP_MIN[,.(year, QYEAR, unemp.rate, w.unemp.rate, m.unemp.rate)]

# export
write.csv(datUNEMP_IND, paste0("acp_brfss/data/", DATE, "_INDIANAunempPREP.csv"), row.names=FALSE)
write.csv(datUNEMP_MIN, paste0("acp_brfss/data/", DATE, "_MINNESOTAunempPREP.csv"), row.names=FALSE)

# --------------------------------------------------------------------------------------
# POPULATION 18-25
# --------------------------------------------------------------------------------------

# prepare US file

datPOP_US <- datPOP_US[, c("NAME", "SEX", "AGE", "POPEST2010_CIV", "POPEST2011_CIV", "POPEST2012_CIV", "POPEST2013_CIV", "POPEST2014_CIV", "POPEST2015_CIV", "POPEST2016_CIV", "POPEST2017_CIV", "POPEST2018_CIV", "POPEST2019_CIV", "POPEST2020_CIV")] %>% 
  filter(NAME == "Indiana" | NAME == "Minnesota") %>% filter(AGE >= 18 & AGE <25 | AGE == 999) %>% 
  mutate(age = ifelse(AGE >= 18 & AGE <25, "18_24", ifelse(AGE == 999, "0_99", NA))) %>% 
  group_by(NAME, SEX, age) %>% summarise(pop2010 = sum(POPEST2010_CIV),
                                         pop2011 = sum(POPEST2011_CIV),
                                         pop2012 = sum(POPEST2012_CIV),
                                         pop2013 = sum(POPEST2013_CIV),
                                         pop2014 = sum(POPEST2014_CIV),
                                         pop2015 = sum(POPEST2015_CIV),
                                         pop2016 = sum(POPEST2016_CIV),
                                         pop2017 = sum(POPEST2017_CIV),
                                         pop2018 = sum(POPEST2018_CIV),
                                         pop2019 = sum(POPEST2019_CIV),
                                         pop2020 = sum(POPEST2020_CIV)) %>% as.data.table()

datPOP_US <- reshape(datPOP_US, idvar = c("NAME", "SEX", "age"), v.name = "pop", 
                      varying = c("pop2010", "pop2011", "pop2012", "pop2013", "pop2014", "pop2015", "pop2016", "pop2017", "pop2018", "pop2019", "pop2020"),
                      times = c("pop2010", "pop2011", "pop2012", "pop2013", "pop2014", "pop2015", "pop2016", "pop2017", "pop2018", "pop2019", "pop2020"),
                      direction = "long")

datPOP_US <- reshape(datPOP_US, idvar = c("NAME", "SEX", "time"), v.name = "pop", timevar = "age", direction = "wide") 

datPOP_US <- datPOP_US %>% 
  mutate(time = str_sub(time, start = -4), 
         sex = ifelse(SEX == 0, "total", ifelse(SEX == 1, "men", ifelse(SEX == 2, "women", NA)))) %>% 
  select(- c(SEX)) %>% as.data.table() 

# prepare Indiana data file

datPOP_IND <- datPOP_IND[, c("sex", "X2", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009")] %>% 
  filter(X2 == ".18 to 24 years" | X2 == "BOTH SEXES" | X2 == "MALE" | X2 == "FEMALE") %>% 
  mutate(age = ifelse(X2 == "BOTH SEXES" | X2 == "MALE" | X2 == "FEMALE", "0_99", ifelse(X2 == ".18 to 24 years", "18_24", NA))) %>% as.data.table()

datPOP_IND <- reshape(datPOP_IND[, c("sex", "age", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009")], 
                      varying = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009"),
                      times = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009"), 
                      idvar = c("sex", "age"), v.name = "pop", direction = "long")

datPOP_IND <- reshape(datPOP_IND, idvar = c("sex", "time"), v.name = "pop", timevar = "age", direction = "wide")

# prepare Minnesota data file

datPOP_MIN <- datPOP_MIN[, c("sex", "X2", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009")] %>% 
  filter(X2 == ".18 to 24 years" | X2 == "BOTH SEXES" | X2 == "MALE" | X2 == "FEMALE") %>% 
  mutate(age = ifelse(X2 == "BOTH SEXES" | X2 == "MALE" | X2 == "FEMALE", "0_99", ifelse(X2 == ".18 to 24 years", "18_24", NA))) %>% as.data.table()

datPOP_MIN <- reshape(datPOP_MIN[, c("sex", "age", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009")], 
                      varying = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009"),
                      times = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009"), 
                      idvar = c("sex", "age"), v.name = "pop", direction = "long")

datPOP_MIN <- reshape(datPOP_MIN, idvar = c("sex", "time"), v.name = "pop", timevar = "age", direction = "wide")

# merge files

datPOP_IND <- rbind(datPOP_IND, datPOP_US[NAME == "Indiana",.(sex, time, pop.0_99, pop.18_24)])
datPOP_MIN <- rbind(datPOP_MIN, datPOP_US[NAME == "Minnesota",.(sex, time, pop.0_99, pop.18_24)])

# calculate % young population 

datPOP_IND <- datPOP_IND %>% mutate(pop18_24 = pop.18_24 / pop.0_99) %>% select(-c(pop.0_99, pop.18_24))
datPOP_MIN <- datPOP_MIN %>% mutate(pop18_24 = pop.18_24 / pop.0_99) %>% select(-c(pop.0_99, pop.18_24))

# reshape

datPOP_IND <- reshape(datPOP_IND, idvar = c("time"), v.name = "pop18_24", timevar = "sex", direction = "wide")
names(datPOP_IND) <- c("year", "pop18_24", "m.pop18_24", "w.pop18_24")

datPOP_MIN <- reshape(datPOP_MIN, idvar = c("time"), v.name = "pop18_24", timevar = "sex", direction = "wide")
names(datPOP_MIN) <- c("year", "pop18_24", "m.pop18_24", "w.pop18_24")

# export
write.csv(datPOP_IND, paste0("acp_brfss/data/", DATE, "_INDIANApopPREP.csv"), row.names=FALSE)
write.csv(datPOP_MIN, paste0("acp_brfss/data/", DATE, "_MINNESOTApopPREP.csv"), row.names=FALSE)

