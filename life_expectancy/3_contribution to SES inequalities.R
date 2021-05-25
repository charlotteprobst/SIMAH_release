# libraries required:
library("tidyverse")
library("dplyr")
library("reshape")
library("data.table")

## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")

dCont <- read.csv("LE/3_out data/dResults_contrib_2000_2018CPS_v3.csv")

dCont <- dCont %>% mutate(AAF= LVDCmort + AUDmort + UIJmort + MVACCmort + IJmort, 
                          TOTAL = LVDCmort + DMmort + IHDmort + HYPHDmort + AUDmort + UIJmort + MVACCmort + IJmort + RESTmort) 

dCont <- reshape(data = dCont, idvar = c("sex", "start_year",  "end_year"), timevar = "edclass", direction = "wide")

keyDat <- data.frame(start_year = c(2000:2017), 
                     range = c(rep(1, 5), rep(2, 5), rep(3, 5), rep(4, 3)))
dat5year <- dCont %>%
      left_join(., keyDat) %>% 
      group_by(sex, Range = range) %>%
      summarise(AAF.College = sum(AAF.College),
                AAF.SomeC = sum(AAF.SomeC),
                AAF.LEHS = sum(AAF.LEHS),
                
                AUDmort.College = sum(AUDmort.College),
                AUDmort.SomeC = sum(AUDmort.SomeC),
                AUDmort.LEHS = sum(AUDmort.LEHS),
                
                LVDCmort.College = sum(LVDCmort.College),
                LVDCmort.SomeC = sum(LVDCmort.SomeC),
                LVDCmort.LEHS = sum(LVDCmort.LEHS),
                
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
cod.vec <- c("TOTAL", "AAF", "AUDmort", "LVDCmort", "IJmort", "MVACCmort", "UIJmort",
             "IHDmort", "HYPHDmort", "DMmort", "RESTmort")
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

write.csv(dat5year, "LE/3_out data/3_contribution to change_5year.csv")
#dat5year <- dat5year[dat5year$TOTALdiff_3 > 0.5,]

v.select <- names(dat5year)[grepl("contrib", names(dat5year))]
group1gathered <- dat5year %>% select(sex, Years, v.select)
   
group1gathered <- gather(data = group1gathered, key = "key", value = "value", 
                         -sex, -Years )
group1gathered <- separate(group1gathered, col = key, sep = "_", into = c("Cause_of_death", "SES"))
group1gathered$Cause_of_death <- as.factor(group1gathered$Cause_of_death)
levels(group1gathered$Cause_of_death) <- list("AAF" = "AAFcontrib",
                                              "Alcohol use disorder" = "AUDmortcontrib", 
                                              "Liver disease & cirrhosis" = "LVDCmortcontrib", 
                                              "Suicide" = "IJmortcontrib",
                                              "Motor vehicle accident" = "MVACCmortcontrib", 
                                              "Unintentional injury*" = "UIJmortcontrib",   
                                              "IHD & ischemic stroke" = "IHDmortcontrib", 
                                              "Hypertensive heart disease" = "HYPHDmortcontrib", 
                                              "Diabetes mellitus"= "DMmortcontrib",
                                              "Rest" = "RESTmortcontrib")

color.vec <- c("#ed7a6d", "#d72c40", "#cf82a6", "#a14d72", "#732946" ,"#93AEBF", "#447a9e", "#69AA9E", "blue")

group1gathered <- group1gathered[group1gathered$Cause_of_death != "Rest" & 
                                    group1gathered$Cause_of_death != "AAF"
                                 & group1gathered$SES == 3 ,]

## 
ggplot(data = group1gathered, aes(y = value, x = Years, fill = Cause_of_death)) +
   geom_bar(position = "stack", stat = "identity", width = 0.8) +
   #facet_grid(cols = vars(Sex), rows = vars(SES),  scales = "free") +
   facet_grid(rows = vars(sex),  scales = "free") +
   theme_light() +
   theme(strip.background = element_rect(fill = "white")) +
   theme(strip.text = element_text(colour = 'black'), text = element_text(size = 14)) +
   #theme(legend.position = "right") +
   scale_fill_manual("Cause of death", values = color.vec)+ 
   ylab("Percent contribution to increases in inequality in life expectancy") +
   #xlab("Years") +
   geom_hline(aes(yintercept=0)) + 
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#ggsave("2_LE decomposition_5year_v1.jpeg", dpi = 600, width = 23, height = 15, units = "cm")
ggsave("LE/4_graphs/2_LE contribution_5year.jpeg", dpi=600, width=23, height=15, units="cm")

write.csv(group1gathered, "LE/3_out data/3_contribution to change_5year_long.csv")

################## Now do the same for the absolute contribution
v.select <- names(dat5year)[grepl("diff", names(dat5year))]
group1gathered <- dat5year %>% select(sex, Years, v.select)

group1gathered <- gather(data = group1gathered, key = "key", value = "value", 
                         -sex, -Years )
group1gathered <- separate(group1gathered, col = key, sep = "_", into = c("Cause_of_death", "SES"))
group1gathered$sex <- as.factor(group1gathered$sex)
levels(group1gathered$sex) <- list("Men" = 1, "Women" = 2)
group1gathered$SES <- as.factor(group1gathered$SES)
levels(group1gathered$SES) <- list("Middle vs. high SES" = 2, "Low vs. high SES" = 3)
                                              
group1gathered$Cause_of_death <- as.factor(group1gathered$Cause_of_death)
levels(group1gathered$Cause_of_death) <- list("Rest" = "RESTmortdiff",
                                              "AAF" = "AAFdiff",
                                              "Alcohol use disorder" = "AUDmortdiff", 
                                              "Liver disease & cirrhosis" = "LVDCmortdiff", 
                                              "Suicide" = "IJmortdiff",
                                              "Motor vehicle accident" = "MVACCmortdiff", 
                                              "Unintentional injury*" = "UIJmortdiff",   
                                              "IHD & ischemic stroke" = "IHDmortdiff", 
                                              "Hypertensive heart disease" = "HYPHDmortdiff", 
                                              "Diabetes mellitus"= "DMmortdiff",
                                              "Total" = "TOTALdiff")

color.vec <- c("#D3D3D3", "#ed7a6d", "#d72c40", "#cf82a6", "#a14d72", "#732946" ,"#93AEBF", "#447a9e", "#69AA9E")

group1gathered <- group1gathered[group1gathered$Cause_of_death != "AAF" &
                                    group1gathered$Cause_of_death != "Total",]

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
#ggsave("2_LE decomposition_5year_v1.jpeg", dpi = 600, width = 23, height = 15, units = "cm")
ggsave("LE/4_graphs/3_Contribution to inequality.jpeg", dpi=600, width=23, height=15, units="cm")
