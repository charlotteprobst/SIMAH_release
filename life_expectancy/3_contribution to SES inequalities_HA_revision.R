# libraries required:
library("tidyverse")
library("dplyr")
library("reshape")
library("data.table")

## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")

dCont <- read.csv("SIMAH_workplace/life_expectancy/2_out_data/dResults_contrib_2000_2018CPS_v4.csv")

dCont <- dCont %>% mutate(AAF100= AUDmort + LVDCmort,
                          AAF20=  PANCmort + UIJmort + MVACCmort + IJmort,
                          AAF0= IHDmort + HYPHDmort + HSTRmort +  
                             LRImort + CANmort + DMmort, 
                          TOTAL = AUDmort + LVDCmort + PANCmort + 
                             UIJmort + MVACCmort + IJmort + 
                             IHDmort + HYPHDmort + HSTRmort +  
                             LRImort + CANmort + DMmort + RESTmort) 

dCont <- reshape(data = dCont, idvar = c("sex", "start_year",  "end_year"), timevar = "edclass", direction = "wide")

keyDat <- data.frame(start_year = c(2000:2017), 
                     range = c(rep(1, 5), rep(2, 5), rep(3, 5), rep(4, 3)))
dat5year <- dCont %>%
      left_join(., keyDat) %>% 
      group_by(sex, Range = range) %>%
      summarise(AAF100.College = sum(AAF100.College),
                AAF100.SomeC = sum(AAF100.SomeC),
                AAF100.LEHS = sum(AAF100.LEHS),
                
                AAF20.College = sum(AAF20.College),
                AAF20.SomeC = sum(AAF20.SomeC),
                AAF20.LEHS = sum(AAF20.LEHS),
                
                AAF0.College = sum(AAF0.College),
                AAF0.SomeC = sum(AAF0.SomeC),
                AAF0.LEHS = sum(AAF0.LEHS),
                
                AUDmort.College = sum(AUDmort.College),
                AUDmort.SomeC = sum(AUDmort.SomeC),
                AUDmort.LEHS = sum(AUDmort.LEHS),
                
                LVDCmort.College = sum(LVDCmort.College),
                LVDCmort.SomeC = sum(LVDCmort.SomeC),
                LVDCmort.LEHS = sum(LVDCmort.LEHS),
                
                PANCmort.College = sum(PANCmort.College),
                PANCmort.SomeC = sum(PANCmort.SomeC),
                PANCmort.LEHS = sum(PANCmort.LEHS),
                
                IJmort.College = sum(IJmort.College),
                IJmort.SomeC = sum(IJmort.SomeC),
                IJmort.LEHS = sum(IJmort.LEHS),
                
                MVACCmort.College = sum(MVACCmort.College),
                MVACCmort.SomeC = sum(MVACCmort.SomeC),
                MVACCmort.LEHS = sum(MVACCmort.LEHS),
                
                UIJmort.College = sum(UIJmort.College),
                UIJmort.SomeC = sum(UIJmort.SomeC),
                UIJmort.LEHS = sum(UIJmort.LEHS),
                
                IHDmort.College = sum(IHDmort.College),
                IHDmort.SomeC = sum(IHDmort.SomeC),
                IHDmort.LEHS = sum(IHDmort.LEHS),
                
                HYPHDmort.College = sum(HYPHDmort.College),
                HYPHDmort.SomeC = sum(HYPHDmort.SomeC),
                HYPHDmort.LEHS = sum(HYPHDmort.LEHS),
                
                HSTRmort.College = sum(HSTRmort.College),
                HSTRmort.SomeC = sum(HSTRmort.SomeC),
                HSTRmort.LEHS = sum(HSTRmort.LEHS),
                
                LRImort.College = sum(LRImort.College),
                LRImort.SomeC = sum(LRImort.SomeC),
                LRImort.LEHS = sum(LRImort.LEHS),
                
                CANmort.College = sum(CANmort.College),
                CANmort.SomeC = sum(CANmort.SomeC),
                CANmort.LEHS = sum(CANmort.LEHS),
                
                DMmort.College = sum(DMmort.College),
                DMmort.SomeC = sum(DMmort.SomeC),
                DMmort.LEHS = sum(DMmort.LEHS),
                
                RESTmort.College = sum(RESTmort.College),
                RESTmort.SomeC = sum(RESTmort.SomeC),
                RESTmort.LEHS = sum(RESTmort.LEHS),
                
                TOTAL.College = sum(TOTAL.College), 
                TOTAL.SomeC = sum(TOTAL.SomeC), 
                TOTAL.LEHS = sum(TOTAL.LEHS))

dat5year <- dat5year %>%
      mutate(Years = recode(Range, 
                            `1` = "2000-2005", 
                            `2` = "2005-2010",
                            `3` = "2010-2015", 
                            `4` = "2015-2018"))
cod.vec <- c("TOTAL", "AAF100", "AAF20", "AAF0", "AUDmort", "LVDCmort", "PANCmort", 
             "IJmort", "MVACCmort", "UIJmort",
             "IHDmort", "HYPHDmort", "HSTRmort",
             "LRImort", "CANmort", "DMmort", "RESTmort")
for (i in cod.vec) {
   dat5year[paste0(i, "diff_2")] <- dat5year[paste0(i, ".College")] - dat5year[paste0(i, ".SomeC")]
   dat5year[paste0(i, "diff_3")] <- dat5year[paste0(i, ".College")] - dat5year[paste0(i, ".LEHS")]
   if(i != "TOTAL"){
      dat5year[paste0(i, "contrib_2")] <- dat5year[paste0(i, "diff_2")]/dat5year$TOTALdiff_2 * 100
      dat5year[paste0(i, "contrib_3")] <- dat5year[paste0(i, "diff_3")]/dat5year$TOTALdiff_3 * 100     
   }
}

dat5year <- dat5year[, !grepl("College", names(dat5year)) &
                        !grepl("SomeC", names(dat5year)) &
                        !grepl("LEHS", names(dat5year))] 

write.csv(dat5year, "SIMAH_workplace/life_expectancy/2_out_data/3_contribution to change_5year.csv")

v.select <- names(dat5year)[grepl("contrib", names(dat5year))]
group1gathered <- dat5year %>% select(sex, Years, v.select)
   
group1gathered <- gather(data = group1gathered, key = "key", value = "value", 
                         -sex, -Years )
group1gathered <- separate(group1gathered, col = key, sep = "_", into = c("Cause_of_death", "SES"))
group1gathered$Cause_of_death <- as.factor(group1gathered$Cause_of_death)
levels(group1gathered$Cause_of_death) <- list("AAF 50% to 100%" = "AAF100contrib",
                                              "AAF 20% to <50%" = "AAF20contrib",
                                              "AAF <20%" = "AAF0contrib",
                                              "Alcohol use disorder" = "AUDmortcontrib", 
                                              "Liver disease & cirrhosis" = "LVDCmortcontrib", 
                                              "Pancreatitis" = "PANCmortcontrib",
                                              "Suicide" = "IJmortcontrib",
                                              "Motor vehicle accident" = "MVACCmortcontrib", 
                                              "Unintentional injury*" = "UIJmortcontrib",   
                                              "IHD & ischemic stroke" = "IHDmortcontrib", 
                                              "Hypertensive heart disease" = "HYPHDmortcontrib", 
                                              "Hemorrhagic stroke" = "HSTRmortcontrib",
                                              "Cancer" = "CANmortcontrib",
                                              "LRI" = "LRImortcontrib",
                                              "Diabetes mellitus"= "DMmortcontrib",
                                              "Rest" = "RESTmortcontrib")

write.csv(group1gathered, "SIMAH_workplace/life_expectancy/2_out_data/Percent contribution to change_5year_long.csv")

################## Now do the same for the absolute contribution
v.select <- names(dat5year)[grepl("diff", names(dat5year))]
group1gathered <- dat5year %>% select(sex, Years, v.select)

group1gathered <- gather(data = group1gathered, key = "key", value = "value", 
                         -sex, -Years )
group1gathered <- separate(group1gathered, col = key, sep = "_", into = c("Cause_of_death", "SES"))
group1gathered$sex <- as.factor(group1gathered$sex)
levels(group1gathered$sex) <- list("Men" = 1, "Women" = 2)
group1gathered$SES <- as.factor(group1gathered$SES)
levels(group1gathered$SES) <- list("Middle vs. high education" = 2, "Low vs. high education" = 3)
                                              
group1gathered$Cause_of_death <- as.factor(group1gathered$Cause_of_death)
levels(group1gathered$Cause_of_death) <- list("Rest" = "RESTmortdiff",
                                              "AAF 50% to 100%" = "AAF100diff",
                                              "AAF 20% to <50%" = "AAF20diff",
                                              "AAF <20%" = "AAF0diff",
                                              "Alcohol use disorder" = "AUDmortdiff", 
                                              "Liver disease & cirrhosis" = "LVDCmortdiff", 
                                              "Pancreatitis" = "PANCmortdiff",
                                              "Suicide" = "IJmortdiff",
                                              "Motor vehicle accident" = "MVACCmortdiff", 
                                              "Unintentional injury*" = "UIJmortdiff",   
                                              "IHD & ischemic stroke" = "IHDmortdiff", 
                                              "Hypertensive heart disease" = "HYPHDmortdiff", 
                                              "Hemorrhagic stroke" = "HSTRmortdiff",
                                              "Cancer" = "CANmortdiff",
                                              "LRI" = "LRImortdiff",
                                              "Diabetes mellitus"= "DMmortdiff",
                                              "Total" = "TOTALdiff")
write.csv(group1gathered, "SIMAH_workplace/life_expectancy/2_out_data/absolute contribution to change_5year_long.csv")

color.vec <- c("grey", "#001219", "#005F73", "#0A9396", "#732946" ,"#93AEBF", "#447a9e", "#69AA9E", "blue", "red", "yellow", "black", "green")

group1gathered
group1gathered <- group1gathered[group1gathered$Cause_of_death == "AAF 50% to 100%" |
                                    group1gathered$Cause_of_death == "AAF 20% to <50%" |
                                    group1gathered$Cause_of_death == "AAF <20%"|
                                    group1gathered$Cause_of_death == "Rest",]

write.csv(group1gathered, "SIMAH_workplace/life_expectancy/2_out_data/exhibit contribution to inequality data.csv")
## 
ggplot(data = group1gathered, aes(y = value, x = Years, fill = Cause_of_death)) +
   geom_bar(position = "stack", stat = "identity", width = 0.8) +
   facet_grid(cols = vars(SES), rows = vars(sex),  scales = "free") +
   theme_light() +
   theme(strip.background = element_rect(fill = "white")) +
   theme(strip.text = element_text(colour = 'black'), text = element_text(size = 14)) +
   #theme(legend.position = "right") +
   scale_fill_manual("Cause of death", values = color.vec)+ 
   ylab("Contribution to changes in inequality (years)") +
   #xlab("Years") +
   geom_hline(aes(yintercept=0)) + 
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("SIMAH_workplace/life_expectancy/3_graphs/3_Contribution to inequality.jpeg", dpi=600, width=23, height=15, units="cm")
