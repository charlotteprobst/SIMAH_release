# Analyse LE decomposition for 2020 weighted data
# Mortality data NCHS
# Population data: ACS
# Project: SIMAH


# libraries required:
library("tidyverse")
library("DemoDecomp")
library("dplyr")
library("reshape")
library("data.table")


## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")
#setwd("~/Documents/Promotion/Mortality US")
setwd("~/Google Drive/SIMAH Sheffield/")

Results_SES <- read.csv(paste0("SIMAH_workplace/life_expectancy/2_out_data/dResults_contrib_", 2018, "_", 2020, "ACS_2020weights.csv") )

# calculate min and max life expectancy by SES 

Summary_SES <- Results_SES %>% 
  group_by(sex,edclass) %>% 
  summarise(LE1 = round(mean(LE1), digits=2),
            minLE2 = round(min(LE2),digits=2),
            maxLE2 = round(max(LE2), digits=2))

Results_race <- read.csv(paste0("SIMAH_workplace/life_expectancy/2_out_data/dResults_contrib_", 2018, "_", 2020, "race_ACSweights.csv") )

Summary_SES_race <- Results_race %>% 
  group_by(sex, edclass, race) %>% 
  summarise(LE1 = round(mean(LE1), digits=2),
            minLE2 = round(min(LE2), digits=2),
            maxLE2 = round(max(LE2), digits=2),
            rangeLE2 = abs(minLE2 - maxLE2),
            mindifferenceLE1_LE2 = abs(LE1-maxLE2),
            maxdifferenceLE1_LE2 = abs(LE1-minLE2))

write.csv(Summary_SES_race, "SIMAH_workplace/life_expectancy/2_out_data/LE_summary_SES_race.csv",
          row.names=F)

library(ggplot2)

Results_race <- Results_race %>% 
  pivot_longer(LE1:LE2) %>% 
  mutate(sex = ifelse(sex==1,"Men","Women"),
         edclass = factor(edclass, levels=c("LEHS","SomeC","College")))

plot <- ggplot(data=Results_race, aes(x=edclass, y=value, colour=name)) + 
  geom_boxplot() + facet_grid(cols=vars(race), rows=vars(sex)) + 
  theme_bw() +
  theme(strip.background=element_rect(fill="white"),
        text=element_text(size=18),
        legend.title=element_blank(),
        legend.position="bottom")
plot
ggsave()

ggsave("SIMAH_workplace/life_expectancy/2_out_data/LE_summary_SES_race.png", plot,
       dpi=300, width=33, height=19, units="cm")
