# Public use mortality files for 2018

data <- read.delim("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/mortality/raw mortality data/Mort2018US.PubUse.txt")

map <- data.frame(widths=c(19, 1,40,2,1,1,2,2,1,1,1,1,1,1,2,2,2,2,1,1,1,16,4,1,1,1,1,34,1,1,4,
                           3,1,3,3,2,1,2,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
                           36,2,1,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,1,2,1,1,1,1,33,3,
                           1,1))
map$cn <- c("blank", # cols 1-19
            "res_status",  #20
            "blank2", # 21-60
            "ed_v89",#61-62
            "ed_v03",#63
            "ed_flag", #64
            "death_month", #65-66
            "blank3",
            "sex", 
            "age_years",
            "age_months", 
            "age_3",
            "age_4", 
            "age_sub_flag", 
            "age_recode_52", 
            "age_recode_27",
            "age_recode_12", 
            "infant_age_recode_22", 
            "place_of_death", 
            "marital_status",
            "death_day", 
            "blank4", 
            "current_year", 
            "work_injury", 
            "death_manner", 
            "disposition",
            "autopsy", 
            "blank5", 
            "activity_code", 
            "place_injured", 
            "icd_cause_of_death", 
            "cause_recode358",
            "blank6", 
            "cause_recode113", 
            "infant_cause_recode130", 
            "cause_recode39", 
            "blank7",
            "num_entity_axis",
            "cond1","cond2","cond3","cond4","cond5","cond6","cond7","cond8","cond9","cond10",
            "cond11","cond12","cond13","cond14","cond15","cond16","cond17","cond18","cond19",
            "cond20",
            "blank7",
            "num_rec_axis_cond", 
            "blank8", 
            "acond1", "acond2", "acond3",  "acond4",  "acond5",  "acond6",  "acond7",  
            "acond8",  "acond9", "acond10", "acond11", "acond12", "acond13", "acond14", 
            "acond15", "acond16", "acond17", "acond18", "acond19", "acond20", 
            "blank9",
            "race",
            "bridged_race_flag",
            "race_imp_flag", 
            "race_recode3", 
            "race_recode5", 
            "blank10",
            "hisp",
            "blank11", 
            "hisp_recode")
map$lastcol <- cumsum(map$widths)
mort2018    <- read.fwf(file="C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/mortality/raw mortality data/Mort2018US.PubUse.txt",
                        widths=map$widths,
                        stringsAsFactors=F)
colnames(mort2018) <- map$cn
saveRDS(mort2018, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/mortality/processed mortality data/mort2018.RDS")