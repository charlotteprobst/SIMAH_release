# libraries required:
library("tidyverse")
library("dplyr")
library("reshape")
library("data.table")

## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")

# for race and SES graphs
dle_results <- read.csv("SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/LifeExpectancy_detail_ACS_0020.csv")
dle_results_weight <- read.csv(("SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/Results_contrib_2018_2020_detail_ACSweights.csv"))

dle_results <- dle_results %>% filter(Race != "Other", Year >2014)
dle_results <- dle_results %>% mutate_at(vars(Sex, SES), as.factor)

levels(dle_results$SES) <- list("High" = "College", "Middle" = "SomeC", "Low" = "LEHS")
levels(dle_results$Sex) <- list(Men = "1", Women = "2")

color.vec <- c("#cf82a6", "#a14d72", "#732946")
color.vec <- c("#69AA9E", "#447a9e",  "#d72c40") # high  middle low

# Plot on life expectancy by SES over time
le_graph <- ggplot(data = dle_results, aes(x = Year, y = Life_expectancy, colour = SES)) + 
   facet_grid(rows = vars(Sex), cols = vars(Race), scales = "free") +
   theme(legend.position = "none") +
   ylab("Life expectancy") +
   theme_light()+
   theme(strip.background = element_rect(fill = "white"))+
   theme(strip.text = element_text(colour = 'black'), text=element_text(size = 16)) +
   theme(legend.position = "right") +
   scale_color_manual(name = "SES", breaks = c("High", "Middle", "Low"), values = color.vec, 
                      labels = c("High", "Middle", "Low")) +   
   geom_line(aes(color = SES), size = .9, alpha = .7) 
   #geom_point(size = 1, aes(color = SES)) 
#ggsave("1_LE_by_sex_and_SES_v1.jpg", dpi=600, width = 15, height = 10, units = "cm")
ggsave("SIMAH_workplace/life_expectancy/3_graphs/2020_decomp/1_LE_by_sex_SES_race_2020.jpg", dpi=600, width=20, height=13, units="cm")

## Display results for weights
dle_results_weight <- dle_results_weight %>% 
   select(sex, edclass,	race, end_year, LE2, weight) %>% 
   filter(race != "Other", end_year == 2020)
names(dle_results_weight) <- c("Sex", "SES", "Race", "Year", 
                               "Life_expectancy", "weight")
ggplot(data = dle_results_weight, 
       aes(x = SES, y = Life_expectancy, colour = SES, group = SES)) + 
   geom_boxplot(aes(fill=SES)) +
   facet_grid(rows = vars(Sex), cols = vars(Race), scales = "free")

dle_ranges <- dle_results_weight %>% group_by(Sex, SES, Race, Year) %>%
   summarise(low = min(Life_expectancy),
   high = max(Life_expectancy)) %>% mutate(difference = high-low) %>%
   mutate_at(vars(Sex, SES, Race), as.factor)
levels(dle_ranges$SES) <- list("High" = "College", "Middle" = "SomeC", "Low" = "LEHS")
levels(dle_ranges$Sex) <- list(Men = "1", Women = "2")

dle_results <- left_join(dle_results, dle_ranges)
le_graph + geom_linerange(data = dle_results, aes(ymin=low,ymax=high), color="red") 

write.csv(dle_results, 
          "SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/LifeExpectancy_detail_ACS_0020.csv")
