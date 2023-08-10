# Replicate results generated from stata maihda

# Change working directory
setwd("U:/STATA HED/logit model")

# CALCULATE PERCENTAGES OF INTEREST (p = pA + pB)
  
# Percentage p based on fixed and random part
m3data_prepped <- read_dta("m3data_prepped.dta")

m3data_prepped <- m3data_prepped %>% mutate(
  p = 100*inv.logit(b_cons*cons 
                      + b_female*female 
                      + b_younger_adult*younger_adult 
                      + b_older_adult*older_adult 
                      + b_Black*Black 
                      + b_Asian*Asian 
                      + b_Other*Other 
                      + b_Hispanic*Hispanic 
                      + b_med*med 
                      + b_high*high 
                      + b_second_decade*second_decade 
                      + u)
)

# Percentage pA based only on the fixed-part
m3data_prepped <- m3data_prepped %>% mutate(
  pA = 100*inv.logit(
                              b_cons*cons 
                              + b_female*female 
                              + b_younger_adult*younger_adult 
                              + b_older_adult*older_adult 
                              + b_Black*Black 
                              + b_Asian*Asian 
                              + b_Other*Other 
                              + b_Hispanic*Hispanic 
                              + b_med*med
                              + b_high*high
                              + b_second_decade*second_decade)
)


# Percentage calculated as the difference between p and pA  
m3data_prepped <- m3data_prepped %>% mutate(
  pB = p - pA)

# Calculate the mean, 2.5th and 97.5th percentiles of the MCMC chains

m3data_prepped <- m3data_prepped %>% 
  group_by(intersections) %>%
  mutate(pmn = mean(p),
         plo = quantile(p,.25),
         phi = quantile(p,.75),
         pAmn = mean(pA),
         pAlo = quantile(pA,.25),
         pAhi = quantile(pA,.75),
         pBmn = mean(pB),
         pBlo = quantile(pB,.25),
         pBhi = quantile(pB,.75))


# Drop chains and just keep their summaries (mean, 2.5th and 97.5th)
m3data_results <- m3data_prepped %>% 
  dplyr::select(-"iteration", -"p",  -"pA", -contains(c("b_", "u_" ))) %>%
  distinct(keep.all=TRUE)

# save results
saveRDS(m3data_results, "m3results.rds")