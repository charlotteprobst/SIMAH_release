# Plot LE decomposition results for 2018 to 2020 
# by SES and race and ethnicity 
# Project: SIMAH

# libraries required:
library("dplyr")
library("RColorBrewer")
library("tidyverse")

## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")

dResults_contrib_SES <- read.csv(
  "SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/Results_contrib_2018_2020_ses_ACS.csv")
dResults_contrib_detail <- read.csv(
  "SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/Results_contrib_2018_2020_detail_ACS.csv")

# Graph results by sex and SES or by sex, ses, and race
k.run <- "detail" # "ses" or "detail"

if (k.run == "detail"){
  dgathered <- gather(data = dResults_contrib_detail, key = "mort", value = "value", 
                      -sex , -edclass, -race, -start_year, -end_year, -LE1, -LE2)
  
} else if (k.run == "ses") {
  dgathered <- gather(data = dResults_contrib_SES, key = "mort", value = "value", 
                      -sex , -edclass, -start_year, -end_year, -LE1, -LE2)
  
}

# have to be factor variables
glimpse(dgathered)
dgathered <- dgathered %>% 
  mutate(mort = gsub(pattern = "mort", replacement = "", x = dgathered$mort)) %>% 
  mutate_at(vars(sex, edclass, mort), as.factor)
if (k.run == "ses") {
  names(dgathered) <- c("Sex", "SES", "start_year", "end_year", "LE1", "LE2", "Cause_of_death", "Contribution")     
} else if (k.run == "detail") {
  dgathered <- mutate_at(dgathered, vars(race), as.factor)
  names(dgathered) <- c("Sex", "SES", "Race", "start_year", "end_year", "LE1", "LE2", "Cause_of_death", "Contribution")     
  #dgathered <- filter(dgathered, Race != "Other") 
}

levels(dgathered$SES) <- list("High" = "College", "Middle" = "SomeC", "Low" = "LEHS")
levels(dgathered$Sex) <- list(Men = "1", Women = "2")
levels(dgathered$Cause_of_death) <- list("Covid 19" = "cov",
                                              "Influenza and pneumonia" = "flu", 
                                              "Other infectious diseases" = "othinf", 
                                              
                                              "Alcohol poisoning" = "alcpoi",
                                              "Opioid poisoning" = "opioid",
                                              "Suicide" = "sij",
                                              "Motor vehicle accident" = "mvacc", 
                                              "Unintentional injury*" = "uij",   
                                              "Other injury" = "othj",   
                                              
                                              "Alcohol use disorder" = "aud",
                                              "Liver disease & cirrhosis" = "liver", 
                                              "Kidney disease" = "kidney",
                                              "Diabetes Mellitus" = "dm",
                                              "Dementia" = "dementia",
                                              "Cerebrovascular diseases" = "heart", 
                                              "Diseases of the heart" = "stroke", 
                                              "Cancer" = "cancer", 
                                              "Chronic lower respiratory diseases" = "resp", 
                                              "Other NCDs" = "othncd",
                                              
                                              "Rest" = "rest"
                                              )
	

infectious <- 5
injury1 <- 3
injury2 <- 3
ncd1 <- 3
ncd2 <- 7

other <- 1
color.vec <- c(rev(brewer.pal(infectious,"Blues"))[1:3],
               rev(brewer.pal(injury1,"Reds")),
               rev(brewer.pal(injury2,"Greens")), #Oranges
               rev(brewer.pal(ncd1,"RdPu")), #YlOrRd
               rev(brewer.pal(ncd2,"YlOrRd")),
               
               c("grey"))

write.csv(dgathered, paste0("SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/decomp_results_2020_", k.run, ".csv"))
dgathered <- filter(dgathered, start_year == 2019) 
## Plot showing changes in every year (will not be included in publication)
dcomp_plot <- ggplot(data = dgathered, 
                       aes(x = SES, y = Contribution, fill = Cause_of_death)) +
    geom_bar(position = position_stack(reverse = T), stat = "identity") +
    scale_fill_manual("Cause of death", values = color.vec)+ 
    facet_grid( rows = vars(Sex),  scales = "free")  + 
    coord_flip() +
    xlab("Contribution in years of life expectancy")
  
if (k.run == "detail") {
  dcomp_plot <- dcomp_plot + facet_grid(rows = vars(Sex, Race))
}

ggsave(plot = dcomp_plot, 
       filename =  
         paste0("SIMAH_workplace/life_expectancy/3_graphs/2020_decomp/decomp_plot_2020_",
                k.run, ".jpeg"), 
       dpi=600, width=30, height=15, units="cm", device = "jpeg")
