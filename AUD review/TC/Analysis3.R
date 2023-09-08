#Alcohol use disorders meta analysis draft 
#(AUD -> AUD mortality)

library(readxl)
library(meta)
library(metafor)
library(data.table)
library(tidyr)
library(ggplot2)
library(grid)
library(forestplot)
library(dplyr)
library(tidyverse)

install.packages("tidyverse")
install.packages("meta")
install.packages("metafor")
install.packages("metagen")
install.packages("metabin")
install.packages("forestplot")

#Name file location and output accordingly
file_location <- "/Users/tessacarr/Downloads/Data Extraction/"
output <- "/Users/tessacarr/Downloads/Data Extraction/"

# Load data
data <- read_xlsx(paste0(file_location, "Analysis_data_AUDtoAUDmortality.xlsx"), na="")
  #col_types = c("numeric", "text", "numeric", "text", "numeric", 
                #"numeric", "numeric", "numeric", "numeric", "text", "text")

data_frame <- data.frame(data) %>%
  unite(author_year, first_author:year_published, sep = ", " )

#Select subsample
data_frame2 <- data_frame %>%
  filter(outcome_n!=68, outcome_n!=437, outcome_n!=689, outcome_n!=47)
  
#Meta-analysis of single incidence rates 
m1 <- metarate(event = outcome_n, time = (person_years/1000), data = data_frame2, 
               common = FALSE, subgroup = group)

#Forest plot
forest(m1, xlim = c(0, 10), xscale = log(c(0, 10)), leftcols = c("author_year", "group", 
      "outcome_n", "time"),                                  #columns on left side of plot
      leftlabs = c("Study","Group", "Events", "Time"),       #labels on left side of plot
      comb.fixed = FALSE, 
      comb.random = TRUE,
      overall = FALSE,                                       #removes overall summary
      overall.hetstat = FALSE,                               #removes heterogeneity measures for overall model
      test.subgroup = FALSE,                                 #shows the test for subgroup differences
      lty.random = 3,                                        #creates dotted line
      #lty.fixed = 3,                                        #creates dotted line
      xlab = "Observed Mortality Rate per 1000PY",
      fs.xlab = 8,                                           #font size of x axis label
      xlab.pos = 5,                                          #position of x axis label
      fs.axis = 8,                                           #font size of x axis numbers
      text.random = "Random effects model for subgroup",     # to label the pooled random effects estimate 
      fontsize= 12
      #xlab= label on x axis
      )

