# SIMAH project 2024 - script for generating samples from markov model for calibration

# model <- read_rds("SIMAH_workplace/nesarc/Models/msm3b.RDS")

model <- read_rds("SIMAH_workplace/nesarc/Models/msm_brfss_alltransitions.RDS")

# model <- read_rds("SIMAH_workplace/nesarc/Models/psid_alcohol_model_2005_2010.RDS")

# model <- read_rds("SIMAH_workplace/nesarc/Models/msm3a.RDS")
# originalsample <- 34165
# inflatedsample <- 2043174

# first sample from the markov model to get nsamples new estimates
samples <- sample_from_markov(model, nsamples, inflation=1, 1, 1)

# with selective inflation
# samples <- sample_from_markov_inflated(model, nsamples, inflation=10, originalsample, inflatedsample, "edu")

# now convert each new sampled estimate into transition probabilities
#first specify the covariates for the model
# every age sex race combination
# note these have to be in exactly the same format of the covariates specified in the model
# if unsure of this run model$covariates to check and e.g. model$data$mf$sex
# covariates <- data.frame(expand.grid(female_w1=c("Men","Women"),
#                                      # age7=c("18-20","21-25","26-29","30-39",
#                                      #        "40-49","50-64","65+"),
#                                      age3 = c("18-24","25-64","65+"),
#                                      edu3=c("Low","Med","High"),
#                                      race_w1=c("White, non-Hispanic",
#                                                "Black, non-Hispanic",
#                                                "Hispanic",
#                                                "Other, non-Hispanic")))
covariates <- data.frame(expand.grid(microsim.init.sex=c("m","f"),
                                     # age7=c("18-20","21-25","26-29","30-39",
                                     #        "40-49","50-64","65+"),
                                     age3 = c("18-24","25-64","65+"),
                                     microsim.init.education=c("LEHS","SomeC","College"),
                                     microsim.init.race=c("WHI","BLA","SPA","OTH")))
covariates$cat <- paste(covariates$age3, covariates$microsim.init.sex, covariates$microsim.init.race, covariates$microsim.init.education, sep="_")

# covariates <- data.frame(expand.grid(sex=c(0,1),
#                                      # age7=c("18-20","21-25","26-29","30-39",
#                                      #        "40-49","50-64","65+"),
#                                      age_cat = c("18-24","25-64","65+"),
#                                      education=c("Less than or equal to high school",
#                                                  "Some college",
#                                                  "College +"),
#                                      race=c("white",
#                                                "black",
#                                                "hispanic",
#                                                "other")))
# covariates$cat <- paste(covariates$age_cat, covariates$sex, covariates$race, covariates$education, sep="_")
samples <- data.frame(t(model$estimates))
samples <- cbind(samplenum=1, samples)

probs <- convert_to_probability(samples, model, covariates)

# format for calibration - label categories and put into list format
probs <- probs %>%
  pivot_longer(cols=State.1:State.4,
                                                        names_to="StateTo", values_to="prob") %>%
  mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
                             endsWith(StateTo,"2") ~ "State 2",
                             endsWith(StateTo,"3") ~ "State 3",
                             endsWith(StateTo,"4") ~ "State 4")) %>%
  separate(cov, into=c("age","sex","race","edu"), sep="_") %>%
  mutate(
    # sex=ifelse(sex=="Men", "m","f"),
    #      race=case_when(grepl("Black", race) ~ "BLA",
    #                     grepl("White", race) ~ "WHI",
    #                     grepl("Other", race) ~ "OTH",
    #                     grepl("Hispanic", race) ~ "SPA"),
    #      edu=case_when(edu=="Low" ~ "LEHS",
    #                    edu=="Med" ~ "SomeC",
    #                    edu=="High" ~ "College"),
         StateFrom=case_when(StateFrom==1 ~ "Non-drinker",
                             StateFrom==2 ~ "Low risk",
                             StateFrom==3 ~ "Medium risk",
                             StateFrom==4 ~ "High risk"),
         StateTo=case_when(grepl(1,StateTo) ~ "Non-drinker",
                           grepl(2,StateTo) ~ "Low risk",
                           grepl(3,StateTo) ~ "Medium risk",
                           grepl(4,StateTo) ~ "High risk"))

# probs <- probs %>% 
#   pivot_longer(cols=State.1:State.4,
#                names_to="StateTo", values_to="prob") %>%
#   mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
#                              endsWith(StateTo,"2") ~ "State 2",
#                              endsWith(StateTo,"3") ~ "State 3",
#                              endsWith(StateTo,"4") ~ "State 4")) %>%
#   separate(cov, into=c("age","sex","race","edu"), sep="_") %>% 
#   mutate(sex=ifelse(sex==0, "m","f"),
#          race=case_when(grepl("black", race) ~ "BLA",
#                         grepl("white", race) ~ "WHI",
#                         grepl("other", race) ~ "OTH",
#                         grepl("hispanic", race) ~ "SPA"),
#          edu=case_when(edu=="Less than or equal to high school" ~ "LEHS",
#                        edu=="Some college" ~ "SomeC",
#                        edu=="College +" ~ "College"),
#          StateFrom=case_when(StateFrom==1 ~ "Non-drinker",
#                              StateFrom==2 ~ "Low risk",
#                              StateFrom==3 ~ "Medium risk",
#                              StateFrom==4 ~ "High risk"),
#          StateTo=case_when(grepl(1,StateTo) ~ "Non-drinker",
#                            grepl(2,StateTo) ~ "Low risk",
#                            grepl(3,StateTo) ~ "Medium risk",
#                            grepl(4,StateTo) ~ "High risk"))

transitionsList <- list()
for(i in 1:length(unique(samples$samplenum))){
  transitionsList[[paste(i)]] <- probs %>% filter(samplenum==i) %>%
    mutate(cat = paste(age, sex, race, edu, StateFrom, sep="_")) %>%
    group_by(cat) %>% mutate(cumsum=cumsum(prob)) %>%
    dplyr::select(cat, StateTo, cumsum)
}

# save samples - for wave 1 in Output Directory
saveRDS(transitionsList, paste0(OutputDirectory, "/transitionsList-1",".RDS"))
colnames(samples) <- make.unique(colnames(samples))
write.csv(samples, paste0(OutputDirectory, "/sampled_markov-1", ".csv"), row.names=F)
