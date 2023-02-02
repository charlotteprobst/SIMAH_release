# SIMAH - protocol paper. June 2021
# Figure 2
# This code reads in the baseline population and generates a Figure for the 
# protocol paper showing drinking patterns in the baseline population

# baseline alcohol consumption of population 
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(readr)
library(plotrix) 


k.wd <- c("C:/Users/Marie/Dropbox/NIH2020/")
#k.wd <- c("~/Google Drive/SIMAH Sheffield")
setwd(k.wd)

basepop <- read.csv("SIMAH_workplace/microsim/1_input_data/agent_files/USAbasepop1000000.csv") %>% dplyr::select(microsim.init.age, microsim.init.sex,
                                                      microsim.init.race, microsim.init.education, microsim.init.drinkingstatus,
                                                      microsim.init.alc.gpd)
# code each of the drinking categories
basepop <- basepop %>% mutate(drinkercat = ifelse(microsim.init.alc.gpd==0, "Abstainer",
                                                  ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>0 & microsim.init.alc.gpd<=40, "Category I",
                                                         ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>40 & microsim.init.alc.gpd<=60, "Category II",
                                                                ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>60 & microsim.init.alc.gpd<=100, "Category III",
                                                                       ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>100, "Category IV",
                                                                              ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>0 & microsim.init.alc.gpd<=20, "Category I",
                                                                                     ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>20 & microsim.init.alc.gpd<=40, "Category II",
                                                                                            ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>40 & microsim.init.alc.gpd<=60, "Category III",
                                                                                                   ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>60, "Category IV", NA))))))
                                                                ))),
                              microsim.init.sex = ifelse(microsim.init.sex=="m","Men","Women"),
                              microsim.init.race = recode(microsim.init.race, "BLA"="non-Hispanic Black",
                                                          "WHI"="non-Hispanic White",
                                                          "SPA"="Hispanic",
                                                          "OTH"="Other"),
                              microsim.init.education = recode(microsim.init.education,
                                                               "LEHS"="High school degree or less",
                                                               "SomeC"="Some college",
                                                               "College"="College degree or more"),
                              microsim.init.education = factor(microsim.init.education,
                                                               levels=c("High school degree or less",
                                                                        "Some college",
                                                                        "College degree or more")),
                              microsim.init.race = factor(microsim.init.race, levels=c("non-Hispanic White","non-Hispanic Black",
                                                                                       "Hispanic", "Other")))

# in percentages
summary <- basepop %>% group_by(microsim.init.sex, microsim.init.education, drinkercat) %>% tally() %>% 
  ungroup() %>% group_by(microsim.init.sex, microsim.init.education) %>% 
  mutate(percent=n/sum(n)*100, data="Microsimulation") 


# read in the processed brfss data - to save time associated with reading full BRFSS
# summarybrfss <- read.csv("SIMAH_workplace/protocol/output_data/0_summarybrfss.csv")
if(FALSE) {
  
brfss <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_states_upshifted.RDS") %>% 
  filter(age_var<=79) %>% filter(YEAR==1999 | YEAR==2000 | YEAR==2001) %>% 
  filter(State=="USA") %>% 
  mutate(microsim.init.sex = recode(sex_recode,"Male"="m","Female"="f"),
         microsim.init.education = education_summary,
         microsim.init.alc.gpd = gramsperday,
         drinkercat = ifelse(microsim.init.alc.gpd==0, "Abstainer",
                                    ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>0 & microsim.init.alc.gpd<=40, "Category I",
                                           ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>40 & microsim.init.alc.gpd<=60, "Category II",
                                                  ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>60 & microsim.init.alc.gpd<=100, "Category III",
                                                         ifelse(microsim.init.sex=="m" & microsim.init.alc.gpd>100, "Category IV",
                                                                ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>0 & microsim.init.alc.gpd<=20, "Category I",
                                                                       ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>20 & microsim.init.alc.gpd<=40, "Category II",
                                                                              ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>40 & microsim.init.alc.gpd<=60, "Category III",
                                                                                     ifelse(microsim.init.sex=="f" & microsim.init.alc.gpd>60, "Category IV", NA))))))))),
         microsim.init.sex = recode(microsim.init.sex, "m"="Men","f"="Women"),
         microsim.init.education = recode(microsim.init.education, "LEHS"="High school degree or less",
                                          "SomeC"="Some college",
                                          "College" = "College degree or more")) %>% 
  group_by(microsim.init.sex, microsim.init.education, drinkercat) %>% tally() %>% 
  ungroup() %>% group_by(microsim.init.sex, microsim.init.education) %>% mutate(percent=n/sum(n)*100,
                                                                                data="BRFSS")
saveRDS(brfss, "SIMAH_workplace/brfss/processed_data/BRFSS_summary.rds")
}

brfss <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_summary.rds")

# summary <- left_join(summary, summarybrfss)
summarycompare <- rbind(summary, brfss) %>% 
  mutate(microsim.init.education = factor(microsim.init.education,
                                          levels=c(
                                          "High school degree or less",
                                          "Some college",
                                          "College degree or more")),
         drinkercat = factor(drinkercat,
                             levels=c("Abstainer","Category I","Category II","Category III",
                                      "Category IV")),
         drinkercat = fct_rev(drinkercat))

col.vec <- c('#062D59', '#576F81','#A8B0AA', '#F9F2D2') #Heavy to light use

summarycompare <- summarycompare[summarycompare$drinkercat != "Abstainer",]

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

summarycompare$cat <- paste(summarycompare$microsim.init.education, summarycompare$data, sep=" ")
summarycompare$cat <- factor(summarycompare$cat,
                             levels=c("High school degree or less microsimulation",
                                      "High school degree or less brfss",
                                      "Some college microsimulation",
                                      "Some college brfss",
                                      "College degree or more microsimulation",
                                      "College degree or more brfss"))

summarycompare <- subset(summarycompare, !is.na(microsim.init.education))


windowsFonts() # To identify available fonts, and their 'name' in R; Arial font is called "sans"
# plot graph
graph_dat <- summarycompare %>%
  mutate(drinkercat = recode(drinkercat, "Category I"= "I", "Category II"= "II","Category III"= "III","Category IV"= "IV")) %>%
  mutate(microsim.init.education = recode(microsim.init.education, 
                                          "High school degree or less" = "High School Degree or Less",
                                          "Some college" = "Some College",
                                          "College degree or more" = "College Degree or More"))
figure2 <-  ggplot(data = graph_dat, aes(x=data, y=percent, fill=drinkercat)) + 
  geom_col(position=position_stack(reverse=T), width = 0.7) +
  #facet_grid(rows=vars(microsim.init.sex), cols=vars(microsim.init.education)) +
  facet_wrap(c("microsim.init.sex", "microsim.init.education"), scales = "free") + 
  theme_light() + 
  theme(strip.background = element_rect(fill = "white"), 
    strip.text = element_text(size = 9, colour = 'black'), 
    axis.text = element_text(size = 9, colour="black"), 
    axis.ticks = element_line(colour="black", size = 0.352), 
    axis.line = element_line(colour="black", size = 0.352),
    text = element_text(size = 9, colour="black"),
    legend.background = element_rect(size=0.352, linetype = "solid", color = "black", fill=NA),
    legend.position= "right",
    legend.justification = "top",
    legend.title.align = 0.5,
    panel.grid = element_blank(),
    panel.border = element_blank(), 
    strip.placement = "outside") +
  labs(fill=expression(underline("Category")), colour = "black", 
       y = "Prevalence, %", 
       x= "Data Source") +
  scale_fill_manual(values=col.vec) + 
  scale_y_continuous(breaks = seq(0, 90, 10), expand=c(0,0.05), limits=c(0,90)) 
  #scale_fill_grey() 
figure2  
ggsave("SIMAH_workplace/protocol/graphs/AJE-00063-2022 Probst Figure 2.pdf", width = 7, height = 5, units = "in")

# Using the cowplot package
library("grid") #For Load grid package
library("gridExtra") #For Install gridExtra package
library("cowplot")

legend <- cowplot::get_legend(figure2)

postscript(file = "SIMAH_workplace/protocol/graphs/AJE-00063-2022 Probst Figure 2_legend.eps", 
    width = 1, height = 1.5, horizontal = FALSE)

grid.newpage()
grid.draw(legend)
dev.off()

pdf(file = "SIMAH_workplace/protocol/graphs/AJE-00063-2022 Probst Figure 2_legend.pdf", 
    width = 1, height = 1.5)

grid.newpage()
grid.draw(legend)
dev.off()

write.csv(summary, "SIMAH_workplace/protocol/output_data/0_alcohol_use_by_SES_and_sex.csv", row.names=F)
k <- 0

for (i in unique(summarycompare$microsim.init.sex)) {
  for (j in unique(summarycompare$microsim.init.education)) {
    k <- k+1
    v.letter<- LETTERS[k]
    ggplot(data=summarycompare[summarycompare$microsim.init.sex == i & summarycompare$microsim.init.education == j,],
           aes(x=data, y=percent, fill=drinkercat)) + 
      geom_col(position=position_stack(reverse=T), width = 0.7 ) +
      theme_light() + 
      ggtitle(paste0(i,", ",sep="\n", j)) +
      labs(fill=expression(underline("Category")), colour = "black") +  
      theme(strip.background = element_rect(fill = "white"), 
            strip.text = element_text(size = 9, colour = 'black'), 
            axis.text = element_text(size = 9, colour="black"), 
            axis.ticks = element_line(colour="black"), 
            axis.line = element_line(colour="black"),
            text = element_text(size = 9, colour="black"),
            panel.grid = element_blank(),
            panel.border = element_blank(), 
            strip.placement = "outside", 
            legend.position="none") +
      ylab("Prevalence, %")+ xlab("Data Source") + 
      scale_fill_manual(values=col.vec) +  #scale_fill_grey() 
      scale_y_continuous(breaks = seq(0, 90, 10), expand=c(0,0.05), limits=c(0,90)) 
  
    ggsave(paste0("SIMAH_workplace/protocol/graphs/AJE-00063-2022 Probst Figure 2",v.letter,".eps"), 
           width = 2.5, height = 2.2, units = "in")
    
  }
}
