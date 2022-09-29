# Plot LE decomposition results for 2018 to 2020 
# by SES and race and ethnicity 
# Project: SIMAH

# libraries required:
library("tidyverse")
library("dplyr")
library("RColorBrewer")

## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")

dResults_contrib_race <- read.csv(
  "SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/dResults_contrib_2018_2020race_ACS.csv")

#### Graph results by sex, SES and race
# Graph results by sex and SES
dResults_contrib <- as.data.frame(dResults_contrib_race[, -1])
dResults_contrib <- filter(dResults_contrib, start_year == 2019)
dResults_contrib$INFmort <- dResults_contrib$FLUmort + dResults_contrib$OTHINFmort
dResults_contrib$NCDmort <- dResults_contrib$HEARTmort + dResults_contrib$CANCERmort +
  dResults_contrib$STROKEmort + dResults_contrib$RESPmort + dResults_contrib$ALZHmort +
  dResults_contrib$DMmort + dResults_contrib$KIDNEYmort + dResults_contrib$OTHNCDmort
dResults_contrib$ALCmort <- dResults_contrib$ALCPOImort + dResults_contrib$AUDmort +
  dResults_contrib$LIVERmort
dResults_contrib$INJmort <- dResults_contrib$SIJmort + dResults_contrib$UIJmort +
  dResults_contrib$OTHJmort + dResults_contrib$OPDPOImort + dResults_contrib$MVACCmort

dResults_contrib <- dResults_contrib %>%
  select(c("COVIDmort", "ALCmort", "NCDmort", "INFmort",   
           "INJmort", "RESTmort",  "sex", "edclass", "race", "start_year",
           "end_year", "LE1", "LE2"))
group1gathered <- gather(data = dResults_contrib, key = "mort", value = "value", 
                          -sex, -edclass, -race, -start_year, -end_year, -LE1, -LE2)


# have to be factor variables
group1gathered$race <- factor(group1gathered$race, levels = c("White", "Black", "Hispanic"))

group1gathered$edclass <- as.factor(group1gathered$edclass)
levels(group1gathered$edclass) <- list("Low" = "LEHS", "Middle" = "SomeC", "High" = "College")
#group1gathered$SES <- factor(group1gathered$SES, levels = c("High", "Middle", "Low"))

group1gathered$mort = gsub(pattern = "mort", replacement = "", x = group1gathered$mort)
group1gathered$mort <- as.factor(group1gathered$mort)

levels(group1gathered$mort) <- list("COVID-19" = "COVID",
                                    "Alcohol-related causes" = "ALC",
                                    "Non-communicable diseases" = "NCD", 
                                    "Infectious diseases" = "INF", 
                                    "Injuries" = "INJ",
                                    "Rest" = "REST")
group1gathered <- rename(group1gathered, 
       "Race" = "race", 
       "Cause_of_death" = "mort", 
       "Contribution"= "value", 
       "Education" = "edclass")

color.vec <- c("#3182BD", 
               "#F03B20", 
               "#FCC5C0",
               brewer.pal(n = 9, name = "Blues")[5],  
               #"#9ECAE1", 
               brewer.pal(n = 9, name = "YlGnBu")[2],  
               brewer.pal(n = 9, name = "Greys")[3])
color.vec <- c("#7BB9CF", 
               "#F03B20", 
               "#FCC5C0",
               #brewer.pal(n = 9, name = "Blues")[5],  
               "#B9DBE5", 
               brewer.pal(n = 9, name = "YlGnBu")[2],  
               brewer.pal(n = 9, name = "Greys")[3])

group1gathered <- group1gathered[group1gathered$start_year == 2019 & 
                                   group1gathered$sex == 1, ] 
ggplot(data = group1gathered, aes(x = Education, y = Contribution, fill = Cause_of_death)) +
  geom_bar(position = position_stack(reverse = T), stat = "identity") +
  scale_fill_manual("Cause of death", values = color.vec)+ 
  facet_grid(rows = vars(Race)) + 
  coord_flip() +
  theme_bw() +
  ylab("Contribution to changes in life expectancy in years")
write.csv(group1gathered, "SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/2_LE decomp_race_2020_supplement.csv")
ggsave("SIMAH_workplace/life_expectancy/3_graphs/2020_decomp/2_LE decomp_race_2020_supplement.jpeg", dpi=600, width=18, height=6, units="cm")
