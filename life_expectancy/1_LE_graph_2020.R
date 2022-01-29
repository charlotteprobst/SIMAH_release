# libraries required:
library("tidyverse")
library("dplyr")
library("reshape")
library("data.table")

## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")

dle_results <- read.csv("SIMAH_workplace/life_expectancy/2_out_data/LifeExpectancy_sex_SES_ACS_2020.csv")
dle_results$Sex <- as.factor(dle_results$Sex)
dle_results$SES <- as.factor(dle_results$SES)

levels(dle_results$SES) <- list("High" = "College", "Middle" = "SomeC", "Low" = "LEHS")
levels(dle_results$Sex) <- list(Men = "1", Women = "2")


color.vec <- c("#cf82a6", "#a14d72", "#732946")
color.vec <- c("#69AA9E", "#447a9e",  "#d72c40") # high  middle low

# Plot on life expectancy by SES over time
ggplot(data = dle_results, aes(x = Year, y = Life_expectancy, colour = SES)) + 
   facet_grid(cols = vars(Sex), scales = "free") +
   theme(legend.position = "none") +
   ylab("Life expectancy") +
   theme_light()+
   theme(strip.background = element_rect(fill = "white"))+
   theme(strip.text = element_text(colour = 'black'), text=element_text(size = 16)) +
   theme(legend.position = "right") +
   scale_color_manual(name = "SES", breaks = c("High", "Middle", "Low"), values = color.vec, 
                      labels = c("High", "Middle", "Low")) +   
   geom_line(aes(color = SES), size = .9, alpha = .7) +
   geom_point(size = 1, aes(color = SES)) 
#ggsave("1_LE_by_sex_and_SES_v1.jpg", dpi=600, width = 15, height = 10, units = "cm")
ggsave("SIMAH_workplace/life_expectancy/3_graphs/1_LE_by_sex_and_SES_2020.jpg", dpi=600, width=18, height=13, units="cm")
