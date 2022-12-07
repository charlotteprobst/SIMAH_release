#Alcohol use disorders meta analysis draft 
#(AUD -> AUD mortality)

library(readxl)
library(meta)
library(metafor)
library(data.table)

install.packages("tidyverse")
install.packages("meta")
install.packages("metafor")
install.packages("metagen")
install.packages("metabin")

#Name file location and output accordingly
file_location <- "/Users/tessacarr/Downloads/Data Extraction/"

# Load data
data <- read_xlsx(paste0(file_location, "Analysis_data_AUDtoAUDmortality.xlsx"), na="")
  col_types = c("numeric", "text", "numeric", "text", "numeric", 
                "numeric", "numeric", "numeric", "numeric", "text", "text")
  
#Meta-analysis of single incidence rates 
m1 <- metarate(event = outcome_n, time = (person_years/1000), data = data)

#Forest plot
forest(m1, ref = , xlim = c(0, 15), leftcols = c("first_author", "group", 
      "outcome_n", "time"), 
      leftlabs = c("Author","Group", "Events", "Time"),
      comb.fixed = FALSE, 
      comb.random = TRUE, 
      #byvar = "Group", 
      #subgroup = 
      fontsize= 12
      #xlab= label on x axis
      )









print.tau2 = FALSE, leftcols = c("first_author", "year_published", 
                                 "group", "total_n"), 
leftlabs = c("Author", "Year", "Group", "Sample size"), 
ref = 1,
text.random = "Overall effect")



data_frame <- data.frame(data)

data_frame <- escalc(measure = "IR", yi = mortality_rate_1000PY, ni = "total_n", data = "data_frame")

data_frame<- escalc(measure = "IR", xi = "outcome_n", ti = "person_years", data = data)



data.1 <- metagen(TE = mortality_rate_1000PY,
                    studlab = first_author,
                    data = data_frame,
                    sm = "IR",
                    comb.fixed = FALSE,
                    comb.random = TRUE, 
                    text.random = "Random effects model",
                    method.tau = "DL")

#rma(yi, vi, method="FE")

summary(data.1)

forest.meta(data.1, print.tau2 = FALSE, leftcols = c("first_author", "year_published", 
            "group", "total_n"), 
            leftlabs = c("Author", "Year", "Group", "Sample size"), 
            ref = 1,
            text.random = "Overall effect")
sat

data <- escalc(measure = "IR", xi = "outcome_n", ti = "person_years")
  
  yi = mortallity_rate_1000PY, sei = total_n, data = data_frame, meassure = "IR"
  
  n1i = n_controls, n2i = n_patients, m1i = mean_controls, m2i = mean_patients, 
                  sd1i = sd_controls, sd2i = sd_patients, data = my_data, measure = "IRD", 
                  append = TRUE)

escalc(measure, ai, bi, ci, di, n1i, n2i, x1i, x2i, t1i, t2i,
       m1i, m2i, sd1i, sd2i, xi, mi, ri, ti, sdi, r2i, ni, yi, vi, sei,
       data, slab, subset, include,
       add=1/2, to="only0", drop00=FALSE, vtype="LS",
       var.names=c("yi","vi"), add.measure=FALSE,
       append=TRUE, replace=TRUE, digits, …)





data_frame <- data.frame(data)

meta_2 <- metacont(data$first_author,
                   data$year_published,
                   data$group,
                   data$total_n,
                   data$outcome_n,
                   data$person_years,
                   data$mortality_rate_1000PY,
                   data = data_frame,
                   byvar = group,
                   comb.fixed = TRUE,
                   sm="SMD")


forest.meta(data, 
            sortvar = "mortality_rate_1000PY",
            comb.random = TRUE, 
            text.random = TRUE
            leftlabs = c("first_author", "total_n", "outcome_n"))


escalc(measure="mortality_rate_1000PY", formula= )

m.bin <- metabin(outcome_n, person_years, mortality_rate_1000PY,
                 data = binarydata,
                 studlab = paste(first_author),
                 comb.fixed = T,comb.random = T,
                 method.tau = 'DL',sm = "IR") # Mantel Haenszel weighting



