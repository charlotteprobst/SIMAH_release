# libraries required:
library("tidyverse")
library("DemoDecomp")
library("dplyr")
library("reshape")
library("data.table")
library("epitools")

## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")

#load aggregated mortality data:
dMort <- read.csv("SIMAH_workplace/mortality/3_out data/allethn_rates_0019_final.csv")

class(dMort$edclass)
dMort$edclass <- as.factor(dMort$edclass)

## To aggregate some cause of death categories
dMort$RESTmort <- dMort$RESTmort + dMort$CANmort
dMort$RESTmort <- dMort$RESTmort + dMort$PANCmort
dMort$RESTmort <- dMort$RESTmort + dMort$HSTRmort

dMort$IHDmort <- dMort$IHDmort + dMort$ISTRmort

#  Delete variables that are no longer needed 
dMort <- dMort %>% select(-c(CANmort, PANCmort, ISTRmort, HSTRmort))

## Aggregate: summarize the data to collapse one demographic dimension 
## Specify all factor variables you want to keep (and omit the one 
## you want to collapse)
lapply(dMort, class)
dMort$race <- as.factor(dMort$race)
dMort <- aggregate(.~ year + edclass + sex + age_gp, data =  dMort, FUN=sum)
dDescStat <- aggregate(.~ year , data =  dMort, FUN=sum)
dDescStat <- dDescStat %>% mutate(sumrow= LVDCmort + DMmort + IHDmort + HYPHDmort + AUDmort + UIJmort + MVACCmort + IJmort) %>% 
   mutate(propInd = sumrow/Tmort, propRest = RESTmort/Tmort)  %>% mutate(test = propInd + propRest)


v.totals <- c("Tmort", "LVDCmort", "DMmort", "IHDmort",
              "HYPHDmort", "AUDmort", "UIJmort", "MVACCmort", "IJmort",  "RESTmort")

v.rates <- c("Trate", "mx_LVDCrate", "mx_DMrate", "mx_IHDrate", 
             "mx_HYPHDrate", "mx_AUDrate", "mx_UIJrate", "mx_MVACCrate", 
             "mx_IJrate",  "mx_RESTrate") 

sel.vars <- c("year", "edclass", "sex", "TPop", "age_gp", v.rates) 

# Calculate the rates for all relevant causes of death
for (i in 1:length(v.totals)){
      dMort[, v.rates[i]] <- (dMort[, v.totals[i]]/dMort$TPop)
} 

dMort <- dMort %>% select(sel.vars)

age2018 <- dMort %>% filter(year == 2018) %>%  group_by(age_gp)  %>% 
   summarise(pop_sum=sum(TPop)) %>%
    mutate(percent=pop_sum/sum(pop_sum))  %>%  select(c(age_gp, percent))

# check all add up to one for each sex/ed/data category
#age2018 %>% group_by(sex, edclass) %>% summarise(sum=sum(percent))
age2018 <- data.frame(age2018)

# join 2018 age percentages with data for all years - to only use age splits for 2018
df <- left_join(dMort, age2018)

# calculate age specific rates - percentage in each age category multiplied by rate per 100,000 in that category
for (i in v.rates){
      string <- paste0(i, "_as")
      df[,string] <- df[,i]*df$percent
}
# calculate age standardised rates - sum of age-specific rates (i.e. weighted sum)
summary <- df %>% group_by(year, sex, edclass) %>% 
      summarise(T_as = sum(Trate_as) *100000, LVDC_as = sum(mx_LVDCrate_as)*100000, DM_as = sum(mx_DMrate_as)*100000, 
                IHD_as = sum(mx_IHDrate_as)*100000,  HYPHD_as = sum(mx_HYPHDrate_as)*100000, 
                AUD_as = sum(mx_AUDrate_as)*100000, UIJ_as = sum(mx_UIJrate_as)*100000, MVACC_as = sum(mx_MVACCrate_as)*100000, 
                IJ_as = sum(mx_IJrate_as)*100000, REST_as = sum(mx_RESTrate_as)*100000)


summary <-  summary %>% select(year, sex, edclass, LVDC_as, DM_as, IHD_as, 
                               HYPHD_as, AUD_as, UIJ_as, MVACC_as, IJ_as)

data_graph <- gather(data = summary, key = "cause", value = "rate", 
                         -sex , -edclass, -year)

levels(data_graph$edclass) <- list("High" = "College", "Middle" = "SomeC", "Low" = "LEHS")
data_graph$sex <- as.factor(data_graph$sex)
levels(data_graph$sex) <- list(Men = 1, Women = 2)
data_graph$cause <- as.factor(data_graph$cause)
levels(data_graph$cause) <-  list("Alcohol use disorder" = "AUD_as", 
                                  "Liver disease & cirrhosis" = "LVDC_as", 
                                  "Suicide" = "IJ_as",
                                  "MVA" = "MVACC_as",
                                  "Unintentional injury*" = "UIJ_as",  
                                  "IHD & ischemic stroke" = "IHD_as",
                                  "Hypertensive heart disease" = "HYPHD_as",
                                  "Diabetes mellitus"= "DM_as")

color.vec <- c("#ed7a6d", "#d72c40", "#cf82a6", "#a14d72", "#732946" ,"#93AEBF", "#447a9e", "#69AA9E")
color.vec <- c("#ed7a6d", "#d72c40", "#cf82a6", "#a14d72", "#732946", "#447a9e", "#69AA9E")

ggplot(data=data_graph[which(data_graph$cause != "IHD & ischemic stroke"),], aes(x=year, y=rate, colour=cause)) + 
      facet_grid(rows = vars(sex), cols = vars(edclass), scales = "free") +
      geom_line(aes(color = cause)) + 
      theme_light() +
      theme(strip.background = element_rect(fill = "white")) +
      theme(strip.text = element_text(colour = 'black'), text = element_text(size = 16)) +
      ylab("Age standardized mortality rate per 100,000") + xlab("Year") +
      ylim(0, NA) +
      scale_color_manual(values = color.vec) +
      labs(color="Cause of death")
ggsave("SIMAH_workplace/life_expectancy/3_graphs/1b_as mortality rates_by ses and sex.jpeg", dpi=600, width=23, height=15, units="cm")

color.vec <- c("#cf82a6", "#a14d72", "#732946")
color.vec <- c("#69AA9E", "#447a9e",  "#d72c40") # high  middle low
ggplot(data=data_graph[data_graph$sex == "Men",], aes(x=year, y=rate, colour=edclass)) + 
      facet_grid(rows = vars(cause), scales = "free") +
      #facet_wrap(facets = vars(cause, sex), ncol = 2, scales = "free") +
      geom_line(aes(color = edclass), size = 0.8) + 
      theme_light() +
      theme(strip.background = element_rect(fill = "white"), legend.position = "bottom") +
      theme(strip.text = element_text(colour = 'black'), text = element_text(size = 16), strip.text.y = element_text(angle = 0, hjust = 0)) +
      ylab("Age-standardized mortality rate per 100,000") + xlab("Year") +
      ylim(0, NA) +
      scale_color_manual(values = color.vec) +
      labs(color="SES")
ggsave("SIMAH_workplace/life_expectancy/3_graphs/1b_as mortality rates by cause and sex_men.jpeg", dpi=600, width=20, height=23, units="cm")

ggplot(data=data_graph[data_graph$sex == "Women",], aes(x=year, y=rate, colour=edclass)) + 
   facet_grid(rows = vars(cause), scales = "free") +
   #facet_wrap(facets = vars(cause, sex), ncol = 2, scales = "free") +
   geom_line(aes(color = edclass), size = 0.8) + 
   theme_light() +
   theme(strip.background = element_rect(fill = "white"), legend.position = "bottom") +
   theme(strip.text = element_text(colour = 'black'), text = element_text(size = 16), strip.text.y = element_text(angle = 0, hjust = 0)) +
   ylab("Age-standardized mortality rate per 100,000") + xlab("Year") +
   ylim(0, NA) +
   scale_color_manual(values = color.vec) +
   labs(color="SES")
ggsave("SIMAH_workplace/life_expectancy/3_graphs/1b_as mortality rates by cause and sex_women.jpeg", dpi=600, width=20, height=23, units="cm")

write.csv(data_graph, "SIMAH_workplace/life_expectancy/2_out_data/age_stand_mortality_rates.csv")
