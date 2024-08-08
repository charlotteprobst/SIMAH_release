# ==================================================================================================================================================================
# ==================================================================================================================================================================

# PROJECT TITLE:  SIMAH
# CODE AUTHOR:    CAROLIN
# DATE STARTED:   20230417

# ==================================================================================================================================================================
# ==================================================================================================================================================================

# clean workspace
rm(list=ls())

# set working directory
setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/mortality/")

# current date:
DATE <- 20230417

# load libraries
library( data.table )
library( tidyverse )

# ==================================================================================================================================================================
# ==================================================================================================================================================================

# LOAD DATA

# 2010

dat1 <- data.table(read.csv("demography/ACSDT1Y2010.B15001-Data.csv"))
dat1 <- copy(dat1[,.(NAME, B15001_004E, B15001_005E, B15001_006E, B15001_007E, B15001_008E, B15001_009E, B15001_010E, #M18-24
                     B15001_012E, B15001_013E, B15001_014E, B15001_015E, B15001_016E, B15001_017E, B15001_018E, #M25-34
                     B15001_020E, B15001_021E, B15001_022E, B15001_023E, B15001_024E, B15001_025E, B15001_026E, #M35-44
                     B15001_028E, B15001_029E, B15001_030E, B15001_031E, B15001_032E, B15001_033E, B15001_034E, #M45-64
                     B15001_036E, B15001_037E, B15001_038E, B15001_039E, B15001_040E, B15001_041E, B15001_042E, #M65+
                     B15001_045E, B15001_046E, B15001_047E, B15001_048E, B15001_049E, B15001_050E, B15001_051E, #F18-24
                     B15001_053E, B15001_054E, B15001_055E, B15001_056E, B15001_057E, B15001_058E, B15001_059E, #F25-34
                     B15001_061E, B15001_062E, B15001_063E, B15001_064E, B15001_065E, B15001_066E, B15001_067E, #F35-44
                     B15001_069E, B15001_070E, B15001_071E, B15001_072E, B15001_073E, B15001_074E, B15001_075E, #F45-64
                     B15001_077E, B15001_078E, B15001_079E, B15001_080E, B15001_081E, B15001_082E, B15001_083E #65+
                     )])
colnames(dat1) <- c("NAME", "M18-24LEHS1", "M18-24LEHS2", "M18-24LEHS3", "M18-24SomeC1", "M18-24SomeC2", "M18-24College1", "M18-24College2",
                    "M25-34LEHS1", "M25-34LEHS2", "M25-34LEHS3", "M25-34SomeC1", "M25-34SomeC2", "M25-34College1", "M25-34College2",
                    "M35-44LEHS1", "M35-44LEHS2", "M35-44LEHS3", "M35-44SomeC1", "M35-44SomeC2", "M35-44College1", "M35-44College2",
                    "M45-64LEHS1", "M45-64LEHS2", "M45-64LEHS3", "M45-64SomeC1", "M45-64SomeC2", "M45-64College1", "M45-64College2",
                    "M65+LEHS1", "M65+LEHS2", "M65+LEHS3", "M65+SomeC1", "M65+SomeC2", "M65+College1", "M65+College2",
                    "F18-24LEHS1", "F18-24LEHS2", "F18-24LEHS3", "F18-24SomeC1", "F18-24SomeC2", "F18-24College1", "F18-24College2",
                    "F25-34LEHS1", "F25-34LEHS2", "F25-34LEHS3", "F25-34SomeC1", "F25-34SomeC2", "F25-34College1", "F25-34College2",
                    "F35-44LEHS1", "F35-44LEHS2", "F35-44LEHS3", "F35-44SomeC1", "F35-44SomeC2", "F35-44College1", "F35-44College2",
                    "F45-64LEHS1", "F45-64LEHS2", "F45-64LEHS3", "F45-64SomeC1", "F45-64SomeC2", "F45-64College1", "F45-64College2",
                    "F65+LEHS1", "F65+LEHS2", "F65+LEHS3", "F65+SomeC1", "F65+SomeC2", "F65+College1", "F65+College2")
dat1$YEAR <- 2010

# 2011

dat2 <- data.table(read.csv("demography/ACSDT1Y2011.B15001-Data.csv"))
dat2 <- copy(dat2[,.(NAME, B15001_004E, B15001_005E, B15001_006E, B15001_007E, B15001_008E, B15001_009E, B15001_010E, #M18-24
                     B15001_012E, B15001_013E, B15001_014E, B15001_015E, B15001_016E, B15001_017E, B15001_018E, #M25-34
                     B15001_020E, B15001_021E, B15001_022E, B15001_023E, B15001_024E, B15001_025E, B15001_026E, #M35-44
                     B15001_028E, B15001_029E, B15001_030E, B15001_031E, B15001_032E, B15001_033E, B15001_034E, #M45-64
                     B15001_036E, B15001_037E, B15001_038E, B15001_039E, B15001_040E, B15001_041E, B15001_042E, #M65+
                     B15001_045E, B15001_046E, B15001_047E, B15001_048E, B15001_049E, B15001_050E, B15001_051E, #F18-24
                     B15001_053E, B15001_054E, B15001_055E, B15001_056E, B15001_057E, B15001_058E, B15001_059E, #F25-34
                     B15001_061E, B15001_062E, B15001_063E, B15001_064E, B15001_065E, B15001_066E, B15001_067E, #F35-44
                     B15001_069E, B15001_070E, B15001_071E, B15001_072E, B15001_073E, B15001_074E, B15001_075E, #F45-64
                     B15001_077E, B15001_078E, B15001_079E, B15001_080E, B15001_081E, B15001_082E, B15001_083E #65+
                  )])
colnames(dat2) <- c("NAME", "M18-24LEHS1", "M18-24LEHS2", "M18-24LEHS3", "M18-24SomeC1", "M18-24SomeC2", "M18-24College1", "M18-24College2",
                    "M25-34LEHS1", "M25-34LEHS2", "M25-34LEHS3", "M25-34SomeC1", "M25-34SomeC2", "M25-34College1", "M25-34College2",
                    "M35-44LEHS1", "M35-44LEHS2", "M35-44LEHS3", "M35-44SomeC1", "M35-44SomeC2", "M35-44College1", "M35-44College2",
                    "M45-64LEHS1", "M45-64LEHS2", "M45-64LEHS3", "M45-64SomeC1", "M45-64SomeC2", "M45-64College1", "M45-64College2",
                    "M65+LEHS1", "M65+LEHS2", "M65+LEHS3", "M65+SomeC1", "M65+SomeC2", "M65+College1", "M65+College2",
                    "F18-24LEHS1", "F18-24LEHS2", "F18-24LEHS3", "F18-24SomeC1", "F18-24SomeC2", "F18-24College1", "F18-24College2",
                    "F25-34LEHS1", "F25-34LEHS2", "F25-34LEHS3", "F25-34SomeC1", "F25-34SomeC2", "F25-34College1", "F25-34College2",
                    "F35-44LEHS1", "F35-44LEHS2", "F35-44LEHS3", "F35-44SomeC1", "F35-44SomeC2", "F35-44College1", "F35-44College2",
                    "F45-64LEHS1", "F45-64LEHS2", "F45-64LEHS3", "F45-64SomeC1", "F45-64SomeC2", "F45-64College1", "F45-64College2",
                    "F65+LEHS1", "F65+LEHS2", "F65+LEHS3", "F65+SomeC1", "F65+SomeC2", "F65+College1", "F65+College2")
dat2$YEAR <- 2011

# 2012

dat3 <- data.table(read.csv("demography/ACSDT1Y2011.B15001-Data.csv"))
dat3 <- copy(dat3[,.(NAME, B15001_004E, B15001_005E, B15001_006E, B15001_007E, B15001_008E, B15001_009E, B15001_010E, #M18-24
                     B15001_012E, B15001_013E, B15001_014E, B15001_015E, B15001_016E, B15001_017E, B15001_018E, #M25-34
                     B15001_020E, B15001_021E, B15001_022E, B15001_023E, B15001_024E, B15001_025E, B15001_026E, #M35-44
                     B15001_028E, B15001_029E, B15001_030E, B15001_031E, B15001_032E, B15001_033E, B15001_034E, #M45-64
                     B15001_036E, B15001_037E, B15001_038E, B15001_039E, B15001_040E, B15001_041E, B15001_042E, #M65+
                     B15001_045E, B15001_046E, B15001_047E, B15001_048E, B15001_049E, B15001_050E, B15001_051E, #F18-24
                     B15001_053E, B15001_054E, B15001_055E, B15001_056E, B15001_057E, B15001_058E, B15001_059E, #F25-34
                     B15001_061E, B15001_062E, B15001_063E, B15001_064E, B15001_065E, B15001_066E, B15001_067E, #F35-44
                     B15001_069E, B15001_070E, B15001_071E, B15001_072E, B15001_073E, B15001_074E, B15001_075E, #F45-64
                     B15001_077E, B15001_078E, B15001_079E, B15001_080E, B15001_081E, B15001_082E, B15001_083E #65+
)])
colnames(dat3) <- c("NAME", "M18-24LEHS1", "M18-24LEHS2", "M18-24LEHS3", "M18-24SomeC1", "M18-24SomeC2", "M18-24College1", "M18-24College2",
                    "M25-34LEHS1", "M25-34LEHS2", "M25-34LEHS3", "M25-34SomeC1", "M25-34SomeC2", "M25-34College1", "M25-34College2",
                    "M35-44LEHS1", "M35-44LEHS2", "M35-44LEHS3", "M35-44SomeC1", "M35-44SomeC2", "M35-44College1", "M35-44College2",
                    "M45-64LEHS1", "M45-64LEHS2", "M45-64LEHS3", "M45-64SomeC1", "M45-64SomeC2", "M45-64College1", "M45-64College2",
                    "M65+LEHS1", "M65+LEHS2", "M65+LEHS3", "M65+SomeC1", "M65+SomeC2", "M65+College1", "M65+College2",
                    "F18-24LEHS1", "F18-24LEHS2", "F18-24LEHS3", "F18-24SomeC1", "F18-24SomeC2", "F18-24College1", "F18-24College2",
                    "F25-34LEHS1", "F25-34LEHS2", "F25-34LEHS3", "F25-34SomeC1", "F25-34SomeC2", "F25-34College1", "F25-34College2",
                    "F35-44LEHS1", "F35-44LEHS2", "F35-44LEHS3", "F35-44SomeC1", "F35-44SomeC2", "F35-44College1", "F35-44College2",
                    "F45-64LEHS1", "F45-64LEHS2", "F45-64LEHS3", "F45-64SomeC1", "F45-64SomeC2", "F45-64College1", "F45-64College2",
                    "F65+LEHS1", "F65+LEHS2", "F65+LEHS3", "F65+SomeC1", "F65+SomeC2", "F65+College1", "F65+College2")
dat3$YEAR <- 2012

# 2013

dat4 <- data.table(read.csv("demography/ACSDT1Y2011.B15001-Data.csv"))
dat4 <- copy(dat4[,.(NAME, B15001_004E, B15001_005E, B15001_006E, B15001_007E, B15001_008E, B15001_009E, B15001_010E, #M18-24
                     B15001_012E, B15001_013E, B15001_014E, B15001_015E, B15001_016E, B15001_017E, B15001_018E, #M25-34
                     B15001_020E, B15001_021E, B15001_022E, B15001_023E, B15001_024E, B15001_025E, B15001_026E, #M35-44
                     B15001_028E, B15001_029E, B15001_030E, B15001_031E, B15001_032E, B15001_033E, B15001_034E, #M45-64
                     B15001_036E, B15001_037E, B15001_038E, B15001_039E, B15001_040E, B15001_041E, B15001_042E, #M65+
                     B15001_045E, B15001_046E, B15001_047E, B15001_048E, B15001_049E, B15001_050E, B15001_051E, #F18-24
                     B15001_053E, B15001_054E, B15001_055E, B15001_056E, B15001_057E, B15001_058E, B15001_059E, #F25-34
                     B15001_061E, B15001_062E, B15001_063E, B15001_064E, B15001_065E, B15001_066E, B15001_067E, #F35-44
                     B15001_069E, B15001_070E, B15001_071E, B15001_072E, B15001_073E, B15001_074E, B15001_075E, #F45-64
                     B15001_077E, B15001_078E, B15001_079E, B15001_080E, B15001_081E, B15001_082E, B15001_083E #65+
)])
colnames(dat4) <- c("NAME", "M18-24LEHS1", "M18-24LEHS2", "M18-24LEHS3", "M18-24SomeC1", "M18-24SomeC2", "M18-24College1", "M18-24College2",
                    "M25-34LEHS1", "M25-34LEHS2", "M25-34LEHS3", "M25-34SomeC1", "M25-34SomeC2", "M25-34College1", "M25-34College2",
                    "M35-44LEHS1", "M35-44LEHS2", "M35-44LEHS3", "M35-44SomeC1", "M35-44SomeC2", "M35-44College1", "M35-44College2",
                    "M45-64LEHS1", "M45-64LEHS2", "M45-64LEHS3", "M45-64SomeC1", "M45-64SomeC2", "M45-64College1", "M45-64College2",
                    "M65+LEHS1", "M65+LEHS2", "M65+LEHS3", "M65+SomeC1", "M65+SomeC2", "M65+College1", "M65+College2",
                    "F18-24LEHS1", "F18-24LEHS2", "F18-24LEHS3", "F18-24SomeC1", "F18-24SomeC2", "F18-24College1", "F18-24College2",
                    "F25-34LEHS1", "F25-34LEHS2", "F25-34LEHS3", "F25-34SomeC1", "F25-34SomeC2", "F25-34College1", "F25-34College2",
                    "F35-44LEHS1", "F35-44LEHS2", "F35-44LEHS3", "F35-44SomeC1", "F35-44SomeC2", "F35-44College1", "F35-44College2",
                    "F45-64LEHS1", "F45-64LEHS2", "F45-64LEHS3", "F45-64SomeC1", "F45-64SomeC2", "F45-64College1", "F45-64College2",
                    "F65+LEHS1", "F65+LEHS2", "F65+LEHS3", "F65+SomeC1", "F65+SomeC2", "F65+College1", "F65+College2")
dat4$YEAR <- 2013

# 2014

dat5 <- data.table(read.csv("demography/ACSDT1Y2011.B15001-Data.csv"))
dat5 <- copy(dat5[,.(NAME, B15001_004E, B15001_005E, B15001_006E, B15001_007E, B15001_008E, B15001_009E, B15001_010E, #M18-24
                     B15001_012E, B15001_013E, B15001_014E, B15001_015E, B15001_016E, B15001_017E, B15001_018E, #M25-34
                     B15001_020E, B15001_021E, B15001_022E, B15001_023E, B15001_024E, B15001_025E, B15001_026E, #M35-44
                     B15001_028E, B15001_029E, B15001_030E, B15001_031E, B15001_032E, B15001_033E, B15001_034E, #M45-64
                     B15001_036E, B15001_037E, B15001_038E, B15001_039E, B15001_040E, B15001_041E, B15001_042E, #M65+
                     B15001_045E, B15001_046E, B15001_047E, B15001_048E, B15001_049E, B15001_050E, B15001_051E, #F18-24
                     B15001_053E, B15001_054E, B15001_055E, B15001_056E, B15001_057E, B15001_058E, B15001_059E, #F25-34
                     B15001_061E, B15001_062E, B15001_063E, B15001_064E, B15001_065E, B15001_066E, B15001_067E, #F35-44
                     B15001_069E, B15001_070E, B15001_071E, B15001_072E, B15001_073E, B15001_074E, B15001_075E, #F45-64
                     B15001_077E, B15001_078E, B15001_079E, B15001_080E, B15001_081E, B15001_082E, B15001_083E #65+
)])
colnames(dat5) <- c("NAME", "M18-24LEHS1", "M18-24LEHS2", "M18-24LEHS3", "M18-24SomeC1", "M18-24SomeC2", "M18-24College1", "M18-24College2",
                    "M25-34LEHS1", "M25-34LEHS2", "M25-34LEHS3", "M25-34SomeC1", "M25-34SomeC2", "M25-34College1", "M25-34College2",
                    "M35-44LEHS1", "M35-44LEHS2", "M35-44LEHS3", "M35-44SomeC1", "M35-44SomeC2", "M35-44College1", "M35-44College2",
                    "M45-64LEHS1", "M45-64LEHS2", "M45-64LEHS3", "M45-64SomeC1", "M45-64SomeC2", "M45-64College1", "M45-64College2",
                    "M65+LEHS1", "M65+LEHS2", "M65+LEHS3", "M65+SomeC1", "M65+SomeC2", "M65+College1", "M65+College2",
                    "F18-24LEHS1", "F18-24LEHS2", "F18-24LEHS3", "F18-24SomeC1", "F18-24SomeC2", "F18-24College1", "F18-24College2",
                    "F25-34LEHS1", "F25-34LEHS2", "F25-34LEHS3", "F25-34SomeC1", "F25-34SomeC2", "F25-34College1", "F25-34College2",
                    "F35-44LEHS1", "F35-44LEHS2", "F35-44LEHS3", "F35-44SomeC1", "F35-44SomeC2", "F35-44College1", "F35-44College2",
                    "F45-64LEHS1", "F45-64LEHS2", "F45-64LEHS3", "F45-64SomeC1", "F45-64SomeC2", "F45-64College1", "F45-64College2",
                    "F65+LEHS1", "F65+LEHS2", "F65+LEHS3", "F65+SomeC1", "F65+SomeC2", "F65+College1", "F65+College2")
dat5$YEAR <- 2014

# 2015

dat6 <- data.table(read.csv("demography/ACSDT1Y2011.B15001-Data.csv"))
dat6 <- copy(dat6[,.(NAME, B15001_004E, B15001_005E, B15001_006E, B15001_007E, B15001_008E, B15001_009E, B15001_010E, #M18-24
                     B15001_012E, B15001_013E, B15001_014E, B15001_015E, B15001_016E, B15001_017E, B15001_018E, #M25-34
                     B15001_020E, B15001_021E, B15001_022E, B15001_023E, B15001_024E, B15001_025E, B15001_026E, #M35-44
                     B15001_028E, B15001_029E, B15001_030E, B15001_031E, B15001_032E, B15001_033E, B15001_034E, #M45-64
                     B15001_036E, B15001_037E, B15001_038E, B15001_039E, B15001_040E, B15001_041E, B15001_042E, #M65+
                     B15001_045E, B15001_046E, B15001_047E, B15001_048E, B15001_049E, B15001_050E, B15001_051E, #F18-24
                     B15001_053E, B15001_054E, B15001_055E, B15001_056E, B15001_057E, B15001_058E, B15001_059E, #F25-34
                     B15001_061E, B15001_062E, B15001_063E, B15001_064E, B15001_065E, B15001_066E, B15001_067E, #F35-44
                     B15001_069E, B15001_070E, B15001_071E, B15001_072E, B15001_073E, B15001_074E, B15001_075E, #F45-64
                     B15001_077E, B15001_078E, B15001_079E, B15001_080E, B15001_081E, B15001_082E, B15001_083E #65+
)])
colnames(dat6) <- c("NAME", "M18-24LEHS1", "M18-24LEHS2", "M18-24LEHS3", "M18-24SomeC1", "M18-24SomeC2", "M18-24College1", "M18-24College2",
                    "M25-34LEHS1", "M25-34LEHS2", "M25-34LEHS3", "M25-34SomeC1", "M25-34SomeC2", "M25-34College1", "M25-34College2",
                    "M35-44LEHS1", "M35-44LEHS2", "M35-44LEHS3", "M35-44SomeC1", "M35-44SomeC2", "M35-44College1", "M35-44College2",
                    "M45-64LEHS1", "M45-64LEHS2", "M45-64LEHS3", "M45-64SomeC1", "M45-64SomeC2", "M45-64College1", "M45-64College2",
                    "M65+LEHS1", "M65+LEHS2", "M65+LEHS3", "M65+SomeC1", "M65+SomeC2", "M65+College1", "M65+College2",
                    "F18-24LEHS1", "F18-24LEHS2", "F18-24LEHS3", "F18-24SomeC1", "F18-24SomeC2", "F18-24College1", "F18-24College2",
                    "F25-34LEHS1", "F25-34LEHS2", "F25-34LEHS3", "F25-34SomeC1", "F25-34SomeC2", "F25-34College1", "F25-34College2",
                    "F35-44LEHS1", "F35-44LEHS2", "F35-44LEHS3", "F35-44SomeC1", "F35-44SomeC2", "F35-44College1", "F35-44College2",
                    "F45-64LEHS1", "F45-64LEHS2", "F45-64LEHS3", "F45-64SomeC1", "F45-64SomeC2", "F45-64College1", "F45-64College2",
                    "F65+LEHS1", "F65+LEHS2", "F65+LEHS3", "F65+SomeC1", "F65+SomeC2", "F65+College1", "F65+College2")
dat6$YEAR <- 2015

# 2016

dat7 <- data.table(read.csv("demography/ACSDT1Y2011.B15001-Data.csv"))
dat7 <- copy(dat7[,.(NAME, B15001_004E, B15001_005E, B15001_006E, B15001_007E, B15001_008E, B15001_009E, B15001_010E, #M18-24
                     B15001_012E, B15001_013E, B15001_014E, B15001_015E, B15001_016E, B15001_017E, B15001_018E, #M25-34
                     B15001_020E, B15001_021E, B15001_022E, B15001_023E, B15001_024E, B15001_025E, B15001_026E, #M35-44
                     B15001_028E, B15001_029E, B15001_030E, B15001_031E, B15001_032E, B15001_033E, B15001_034E, #M45-64
                     B15001_036E, B15001_037E, B15001_038E, B15001_039E, B15001_040E, B15001_041E, B15001_042E, #M65+
                     B15001_045E, B15001_046E, B15001_047E, B15001_048E, B15001_049E, B15001_050E, B15001_051E, #F18-24
                     B15001_053E, B15001_054E, B15001_055E, B15001_056E, B15001_057E, B15001_058E, B15001_059E, #F25-34
                     B15001_061E, B15001_062E, B15001_063E, B15001_064E, B15001_065E, B15001_066E, B15001_067E, #F35-44
                     B15001_069E, B15001_070E, B15001_071E, B15001_072E, B15001_073E, B15001_074E, B15001_075E, #F45-64
                     B15001_077E, B15001_078E, B15001_079E, B15001_080E, B15001_081E, B15001_082E, B15001_083E #65+
)])
colnames(dat7) <- c("NAME", "M18-24LEHS1", "M18-24LEHS2", "M18-24LEHS3", "M18-24SomeC1", "M18-24SomeC2", "M18-24College1", "M18-24College2",
                    "M25-34LEHS1", "M25-34LEHS2", "M25-34LEHS3", "M25-34SomeC1", "M25-34SomeC2", "M25-34College1", "M25-34College2",
                    "M35-44LEHS1", "M35-44LEHS2", "M35-44LEHS3", "M35-44SomeC1", "M35-44SomeC2", "M35-44College1", "M35-44College2",
                    "M45-64LEHS1", "M45-64LEHS2", "M45-64LEHS3", "M45-64SomeC1", "M45-64SomeC2", "M45-64College1", "M45-64College2",
                    "M65+LEHS1", "M65+LEHS2", "M65+LEHS3", "M65+SomeC1", "M65+SomeC2", "M65+College1", "M65+College2",
                    "F18-24LEHS1", "F18-24LEHS2", "F18-24LEHS3", "F18-24SomeC1", "F18-24SomeC2", "F18-24College1", "F18-24College2",
                    "F25-34LEHS1", "F25-34LEHS2", "F25-34LEHS3", "F25-34SomeC1", "F25-34SomeC2", "F25-34College1", "F25-34College2",
                    "F35-44LEHS1", "F35-44LEHS2", "F35-44LEHS3", "F35-44SomeC1", "F35-44SomeC2", "F35-44College1", "F35-44College2",
                    "F45-64LEHS1", "F45-64LEHS2", "F45-64LEHS3", "F45-64SomeC1", "F45-64SomeC2", "F45-64College1", "F45-64College2",
                    "F65+LEHS1", "F65+LEHS2", "F65+LEHS3", "F65+SomeC1", "F65+SomeC2", "F65+College1", "F65+College2")
dat7$YEAR <- 2016

# 2017

dat8 <- data.table(read.csv("demography/ACSDT1Y2011.B15001-Data.csv"))
dat8 <- copy(dat8[,.(NAME, B15001_004E, B15001_005E, B15001_006E, B15001_007E, B15001_008E, B15001_009E, B15001_010E, #M18-24
                     B15001_012E, B15001_013E, B15001_014E, B15001_015E, B15001_016E, B15001_017E, B15001_018E, #M25-34
                     B15001_020E, B15001_021E, B15001_022E, B15001_023E, B15001_024E, B15001_025E, B15001_026E, #M35-44
                     B15001_028E, B15001_029E, B15001_030E, B15001_031E, B15001_032E, B15001_033E, B15001_034E, #M45-64
                     B15001_036E, B15001_037E, B15001_038E, B15001_039E, B15001_040E, B15001_041E, B15001_042E, #M65+
                     B15001_045E, B15001_046E, B15001_047E, B15001_048E, B15001_049E, B15001_050E, B15001_051E, #F18-24
                     B15001_053E, B15001_054E, B15001_055E, B15001_056E, B15001_057E, B15001_058E, B15001_059E, #F25-34
                     B15001_061E, B15001_062E, B15001_063E, B15001_064E, B15001_065E, B15001_066E, B15001_067E, #F35-44
                     B15001_069E, B15001_070E, B15001_071E, B15001_072E, B15001_073E, B15001_074E, B15001_075E, #F45-64
                     B15001_077E, B15001_078E, B15001_079E, B15001_080E, B15001_081E, B15001_082E, B15001_083E #65+
)])
colnames(dat8) <- c("NAME", "M18-24LEHS1", "M18-24LEHS2", "M18-24LEHS3", "M18-24SomeC1", "M18-24SomeC2", "M18-24College1", "M18-24College2",
                    "M25-34LEHS1", "M25-34LEHS2", "M25-34LEHS3", "M25-34SomeC1", "M25-34SomeC2", "M25-34College1", "M25-34College2",
                    "M35-44LEHS1", "M35-44LEHS2", "M35-44LEHS3", "M35-44SomeC1", "M35-44SomeC2", "M35-44College1", "M35-44College2",
                    "M45-64LEHS1", "M45-64LEHS2", "M45-64LEHS3", "M45-64SomeC1", "M45-64SomeC2", "M45-64College1", "M45-64College2",
                    "M65+LEHS1", "M65+LEHS2", "M65+LEHS3", "M65+SomeC1", "M65+SomeC2", "M65+College1", "M65+College2",
                    "F18-24LEHS1", "F18-24LEHS2", "F18-24LEHS3", "F18-24SomeC1", "F18-24SomeC2", "F18-24College1", "F18-24College2",
                    "F25-34LEHS1", "F25-34LEHS2", "F25-34LEHS3", "F25-34SomeC1", "F25-34SomeC2", "F25-34College1", "F25-34College2",
                    "F35-44LEHS1", "F35-44LEHS2", "F35-44LEHS3", "F35-44SomeC1", "F35-44SomeC2", "F35-44College1", "F35-44College2",
                    "F45-64LEHS1", "F45-64LEHS2", "F45-64LEHS3", "F45-64SomeC1", "F45-64SomeC2", "F45-64College1", "F45-64College2",
                    "F65+LEHS1", "F65+LEHS2", "F65+LEHS3", "F65+SomeC1", "F65+SomeC2", "F65+College1", "F65+College2")
dat8$YEAR <- 2017

# 2018

dat9 <- data.table(read.csv("demography/ACSDT1Y2011.B15001-Data.csv"))
dat9 <- copy(dat9[,.(NAME, B15001_004E, B15001_005E, B15001_006E, B15001_007E, B15001_008E, B15001_009E, B15001_010E, #M18-24
                     B15001_012E, B15001_013E, B15001_014E, B15001_015E, B15001_016E, B15001_017E, B15001_018E, #M25-34
                     B15001_020E, B15001_021E, B15001_022E, B15001_023E, B15001_024E, B15001_025E, B15001_026E, #M35-44
                     B15001_028E, B15001_029E, B15001_030E, B15001_031E, B15001_032E, B15001_033E, B15001_034E, #M45-64
                     B15001_036E, B15001_037E, B15001_038E, B15001_039E, B15001_040E, B15001_041E, B15001_042E, #M65+
                     B15001_045E, B15001_046E, B15001_047E, B15001_048E, B15001_049E, B15001_050E, B15001_051E, #F18-24
                     B15001_053E, B15001_054E, B15001_055E, B15001_056E, B15001_057E, B15001_058E, B15001_059E, #F25-34
                     B15001_061E, B15001_062E, B15001_063E, B15001_064E, B15001_065E, B15001_066E, B15001_067E, #F35-44
                     B15001_069E, B15001_070E, B15001_071E, B15001_072E, B15001_073E, B15001_074E, B15001_075E, #F45-64
                     B15001_077E, B15001_078E, B15001_079E, B15001_080E, B15001_081E, B15001_082E, B15001_083E #65+
)])
colnames(dat9) <- c("NAME", "M18-24LEHS1", "M18-24LEHS2", "M18-24LEHS3", "M18-24SomeC1", "M18-24SomeC2", "M18-24College1", "M18-24College2",
                    "M25-34LEHS1", "M25-34LEHS2", "M25-34LEHS3", "M25-34SomeC1", "M25-34SomeC2", "M25-34College1", "M25-34College2",
                    "M35-44LEHS1", "M35-44LEHS2", "M35-44LEHS3", "M35-44SomeC1", "M35-44SomeC2", "M35-44College1", "M35-44College2",
                    "M45-64LEHS1", "M45-64LEHS2", "M45-64LEHS3", "M45-64SomeC1", "M45-64SomeC2", "M45-64College1", "M45-64College2",
                    "M65+LEHS1", "M65+LEHS2", "M65+LEHS3", "M65+SomeC1", "M65+SomeC2", "M65+College1", "M65+College2",
                    "F18-24LEHS1", "F18-24LEHS2", "F18-24LEHS3", "F18-24SomeC1", "F18-24SomeC2", "F18-24College1", "F18-24College2",
                    "F25-34LEHS1", "F25-34LEHS2", "F25-34LEHS3", "F25-34SomeC1", "F25-34SomeC2", "F25-34College1", "F25-34College2",
                    "F35-44LEHS1", "F35-44LEHS2", "F35-44LEHS3", "F35-44SomeC1", "F35-44SomeC2", "F35-44College1", "F35-44College2",
                    "F45-64LEHS1", "F45-64LEHS2", "F45-64LEHS3", "F45-64SomeC1", "F45-64SomeC2", "F45-64College1", "F45-64College2",
                    "F65+LEHS1", "F65+LEHS2", "F65+LEHS3", "F65+SomeC1", "F65+SomeC2", "F65+College1", "F65+College2")
dat9$YEAR <- 2018

# 2019

dat10 <- data.table(read.csv("demography/ACSDT1Y2011.B15001-Data.csv"))
dat10 <- copy(dat10[,.(NAME, B15001_004E, B15001_005E, B15001_006E, B15001_007E, B15001_008E, B15001_009E, B15001_010E, #M18-24
                     B15001_012E, B15001_013E, B15001_014E, B15001_015E, B15001_016E, B15001_017E, B15001_018E, #M25-34
                     B15001_020E, B15001_021E, B15001_022E, B15001_023E, B15001_024E, B15001_025E, B15001_026E, #M35-44
                     B15001_028E, B15001_029E, B15001_030E, B15001_031E, B15001_032E, B15001_033E, B15001_034E, #M45-64
                     B15001_036E, B15001_037E, B15001_038E, B15001_039E, B15001_040E, B15001_041E, B15001_042E, #M65+
                     B15001_045E, B15001_046E, B15001_047E, B15001_048E, B15001_049E, B15001_050E, B15001_051E, #F18-24
                     B15001_053E, B15001_054E, B15001_055E, B15001_056E, B15001_057E, B15001_058E, B15001_059E, #F25-34
                     B15001_061E, B15001_062E, B15001_063E, B15001_064E, B15001_065E, B15001_066E, B15001_067E, #F35-44
                     B15001_069E, B15001_070E, B15001_071E, B15001_072E, B15001_073E, B15001_074E, B15001_075E, #F45-64
                     B15001_077E, B15001_078E, B15001_079E, B15001_080E, B15001_081E, B15001_082E, B15001_083E #65+
)])
colnames(dat10) <- c("NAME", "M18-24LEHS1", "M18-24LEHS2", "M18-24LEHS3", "M18-24SomeC1", "M18-24SomeC2", "M18-24College1", "M18-24College2",
                    "M25-34LEHS1", "M25-34LEHS2", "M25-34LEHS3", "M25-34SomeC1", "M25-34SomeC2", "M25-34College1", "M25-34College2",
                    "M35-44LEHS1", "M35-44LEHS2", "M35-44LEHS3", "M35-44SomeC1", "M35-44SomeC2", "M35-44College1", "M35-44College2",
                    "M45-64LEHS1", "M45-64LEHS2", "M45-64LEHS3", "M45-64SomeC1", "M45-64SomeC2", "M45-64College1", "M45-64College2",
                    "M65+LEHS1", "M65+LEHS2", "M65+LEHS3", "M65+SomeC1", "M65+SomeC2", "M65+College1", "M65+College2",
                    "F18-24LEHS1", "F18-24LEHS2", "F18-24LEHS3", "F18-24SomeC1", "F18-24SomeC2", "F18-24College1", "F18-24College2",
                    "F25-34LEHS1", "F25-34LEHS2", "F25-34LEHS3", "F25-34SomeC1", "F25-34SomeC2", "F25-34College1", "F25-34College2",
                    "F35-44LEHS1", "F35-44LEHS2", "F35-44LEHS3", "F35-44SomeC1", "F35-44SomeC2", "F35-44College1", "F35-44College2",
                    "F45-64LEHS1", "F45-64LEHS2", "F45-64LEHS3", "F45-64SomeC1", "F45-64SomeC2", "F45-64College1", "F45-64College2",
                    "F65+LEHS1", "F65+LEHS2", "F65+LEHS3", "F65+SomeC1", "F65+SomeC2", "F65+College1", "F65+College2")
dat10$YEAR <- 2019

# ==================================================================================================================================================================
# ==================================================================================================================================================================

# PREPARE DATA

# combine files

pdat <- rbind(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9, dat10)

# calculate total counts

pdat <- pdat %>% mutate_at(c(2:72), as.numeric) %>% 
  filter(!NAME %like% "Geographic Area Name") %>% 
  mutate(
  `M18-24LEHS` = `M18-24LEHS1` + `M18-24LEHS2` + `M18-24LEHS3`,
  `M18-24SomeC` = `M18-24SomeC1` + `M18-24SomeC2`,
  `M18-24College` = `M18-24College1` + `M18-24College2`,
  `M25-34LEHS` = `M25-34LEHS1` + `M25-34LEHS2` + `M25-34LEHS3`,
  `M25-34SomeC` = `M25-34SomeC1` + `M25-34SomeC2`,
  `M25-34College` = `M25-34College1` + `M25-34College2`,
  `M35-44LEHS` = `M35-44LEHS1` + `M35-44LEHS2` + `M35-44LEHS3`,
  `M35-44SomeC` = `M35-44SomeC1` + `M35-44SomeC2`,
  `M35-44College` = `M35-44College1` + `M35-44College2`,
  `M45-64LEHS` = `M45-64LEHS1` + `M45-64LEHS2` + `M45-64LEHS3`,
  `M45-64SomeC` = `M45-64SomeC1` + `M45-64SomeC2`,
  `M45-64College` = `M45-64College1` + `M45-64College2`,
  `M65+LEHS` = `M65+LEHS1` + `M65+LEHS2` + `M65+LEHS3`,
  `M65+SomeC` = `M65+SomeC1` + `M65+SomeC2`,
  `M65+College` = `M65+College1` + `M65+College2`,
  `F18-24LEHS` = `F18-24LEHS1` + `F18-24LEHS2` + `F18-24LEHS3`,
  `F18-24SomeC` = `F18-24SomeC1` + `F18-24SomeC2`,
  `F18-24College` = `F18-24College1` + `F18-24College2`,
  `F25-34LEHS` = `F25-34LEHS1` + `F25-34LEHS2` + `F25-34LEHS3`,
  `F25-34SomeC` = `F25-34SomeC1` + `F25-34SomeC2`,
  `F25-34College` = `F25-34College1` + `F25-34College2`,
  `F35-44LEHS` = `F35-44LEHS1` + `F35-44LEHS2` + `F35-44LEHS3`,
  `F35-44SomeC` = `F35-44SomeC1` + `F35-44SomeC2`,
  `F35-44College` = `F35-44College1` + `F35-44College2`,
  `F45-64LEHS` = `F45-64LEHS1` + `F45-64LEHS2` + `F45-64LEHS3`,
  `F45-64SomeC` = `F45-64SomeC1` + `F45-64SomeC2`,
  `F45-64College` = `F45-64College1` + `F45-64College2`,
  `F65+LEHS` = `F65+LEHS1` + `F65+LEHS2` + `F65+LEHS3`,
  `F65+SomeC` = `F65+SomeC1` + `F65+SomeC2`,
  `F65+College` = `F65+College1` + `F65+College2`) %>%
  select(c("NAME", "YEAR", "M18-24LEHS", "M18-24SomeC", "M18-24College", "M25-34LEHS", "M25-34SomeC", "M25-34College", 
           "M35-44LEHS", "M35-44SomeC", "M35-44College", "M45-64LEHS", "M45-64SomeC", "M45-64College", 
           "M65+LEHS", "M65+SomeC", "M65+College", "M18-24LEHS", "F18-24SomeC", "F18-24College", 
           "F25-34LEHS", "F25-34SomeC", "F25-34College", "F35-44LEHS", "F35-44SomeC", "F35-44College", 
           "F45-64LEHS", "F45-64SomeC", "F45-64College", "F65+LEHS", "F65+SomeC", "F65+College"))

# reshape

pdat.long <- reshape(pdat, idvar = c("NAME", "YEAR"), 
                     varying = c("M18-24LEHS", "M18-24SomeC", "M18-24College", "M25-34LEHS", "M25-34SomeC", "M25-34College", 
                                 "M35-44LEHS", "M35-44SomeC", "M35-44College", "M45-64LEHS", "M45-64SomeC", "M45-64College", 
                                 "M65+LEHS", "M65+SomeC", "M65+College", "M18-24LEHS", "F18-24SomeC", "F18-24College", 
                                 "F25-34LEHS", "F25-34SomeC", "F25-34College", "F35-44LEHS", "F35-44SomeC", "F35-44College", 
                                 "F45-64LEHS", "F45-64SomeC", "F45-64College", "F65+LEHS", "F65+SomeC", "F65+College"),
                     times = c("M18-24LEHS", "M18-24SomeC", "M18-24College", "M25-34LEHS", "M25-34SomeC", "M25-34College", 
                               "M35-44LEHS", "M35-44SomeC", "M35-44College", "M45-64LEHS", "M45-64SomeC", "M45-64College", 
                               "M65+LEHS", "M65+SomeC", "M65+College", "M18-24LEHS", "F18-24SomeC", "F18-24College", 
                               "F25-34LEHS", "F25-34SomeC", "F25-34College", "F35-44LEHS", "F35-44SomeC", "F35-44College", 
                               "F45-64LEHS", "F45-64SomeC", "F45-64College", "F65+LEHS", "F65+SomeC", "F65+College"),
                     v.name = "POP", direction = "long")  

pdat.long <- pdat.long %>% mutate(
  SEX = ifelse(time %like% "M1|M2|M3|M4|M6", 1, 2),
  AGE_GP = ifelse(time %like% "18-24", "18", ifelse(time %like% "25-34", "25", ifelse(time %like% "35-44", "35",
              ifelse(time %like% "45-64", "45", ifelse(time %like% "65+", "65", NA))))),
  EDCLASS = ifelse(time %like% "LEHS", "LEHS", ifelse(time %like% "SomeC", "SomeC", ifelse(time %like% "College", "College", NA)))) %>%
  select(-c("time")) %>%
  mutate(FIPSSTR = ifelse(NAME %like% "Alabama", "AL", ifelse(NAME %like% "Alaska", "AK", ifelse(NAME %like% "Arizona", "AZ",
                       ifelse(NAME %like% "Arkansas", "AR", ifelse(NAME %like% "California", "CA", ifelse(NAME %like% "Colorado", "CO",
                              ifelse(NAME %like% "Connecticut", "CT", ifelse(NAME %like% "Delaware", "DE", ifelse(NAME %like% "District of Columbia", "DC", 
                                    ifelse(NAME %like% "Florida", "FL", ifelse(NAME %like% "Georgia", "GA", ifelse(NAME %like% "Hawaii", "HI", ifelse(NAME %like% "Idaho", "ID",
                 ifelse(NAME %like% "Illinois", "IL", ifelse(NAME %like% "Indiana", "IN", ifelse(NAME %like% "Iowa", "IA", NA)))))))))))))))),
         FIPSSTR = ifelse(NAME %like% "Kansas", "KS", ifelse(NAME %like% "Kentucky", "KY", ifelse(NAME %like% "Louisiana", "LA",
                              ifelse(NAME %like% "Maine", "ME", ifelse(NAME %like% "Maryland", "MD", ifelse(NAME %like% "Massachusetts", "MA",
                                    ifelse(NAME %like% "Michigan", "MI", ifelse(NAME %like% "Minnesota", "MN", ifelse(NAME %like% "Missouri", "MO", ifelse(NAME %like% "Mississippi", "MS", 
                 ifelse(NAME %like% "Montana", "MT", ifelse(NAME %like% "Nebraska", "NE", ifelse(NAME %like% "Nevada", "NV",
                       ifelse(NAME %like% "New Hampshire", "NH", ifelse(NAME %like% "New Jersey", "NJ", ifelse(NAME %like% "New Mexico", "NM",
                             ifelse(NAME %like% "New York", "NY", ifelse(NAME %like% "North Carolina", "NC", ifelse(NAME %like% "North Dakota", "ND",
                                    ifelse(NAME %like% "Ohio", "OH", ifelse(NAME %like% "Oklahoma", "OK", ifelse(NAME %like% "Oregon", "OR", 
                 ifelse(NAME %like% "Pennsylvania", "PA", ifelse(NAME %like% "Puerto Rico", "PR", ifelse(NAME %like% "Rhode Island", "RI", ifelse(NAME %like% "South Carolina", "SC",
                       ifelse(NAME %like% "South Dakota", "SD", ifelse(NAME %like% "Tennessee", "TN", ifelse(NAME %like% "Texas", "TX",
                             ifelse(NAME %like% "Utah", "UT", ifelse(NAME %like% "Vermont", "VT", ifelse(NAME %like% "Virginia", "VA",
                                   ifelse(NAME %like% "Washington", "WA", ifelse(NAME %like% "West Virginia", "WV", ifelse(NAME %like% "Wisconsin", "WI", 
                 ifelse(NAME %like% "Wyoming", "WY", FIPSSTR)))))))))))))))))))))))))))))))))))))
  
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# EXPORT

write.csv(pdat.long, paste0("3_out data/", DATE, "_ACS_DEMOGRAPHY.csv"), row.names = FALSE, na = ".")



