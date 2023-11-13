# SIMAH Restricted-access Data
## Functions for Causal Mediation 


# Data preparation for Causal Mediation -----------------------------------------------------------------------------------

# Function for data preparation (Race as exposure), to repeat analyses for men and women
CMed_race3_prep <- function(data) {
  
  ### Step 0: Select data to use *****************************************************************************************************************
  # **********************************************************************************************************************************************
  mydata <- data %>%
    mutate(
      A.race = factor(ethnicity, levels=c(1,2,3), labels = c("White", "Black", "Hispanic")),
      M1.alc = factor(alcohol5v2),
      M2.smk = factor(smk),
      M3.bmi = factor(bmi_cat),
      M4.phy = factor(phy_act3)) %>%
    dplyr::select(A.race, M1.alc, M2.smk, M3.bmi, M4.phy, allcause_death, bl_age, end_age, married.factor, edu.factor, srvy_yr) %>%
    filter(complete.cases(A.race, M1.alc, M2.smk, M3.bmi, M4.phy, allcause_death, bl_age, end_age, married.factor, edu.factor, srvy_yr))
  
  cat("Step 0 complete (Select data)", "\n") # progress indicator
  
 
  # NOTE: For technical reasons, the mediators should be coded as integers starting with 1
  
  
  
  ### Step 1: Fit a model for each mediator, , conditioning on exposure and all confounders *******************************************************
  # ***********************************************************************************************************************************************
  
  # Fit model for each mediator, conditioning on exposure (race) and all confounders
  
  mydata$ATemp <- mydata$A.race # first, create and use a copy of the exposure variable (for technical reasons related to R)
  fitM1 <- vglm(M1.alc ~ ATemp + bl_age + married.factor + edu.factor + srvy_yr, data = mydata, family=multinomial(refLevel = 3))
  fitM2 <- vglm(M2.smk ~ ATemp + bl_age + married.factor + edu.factor + srvy_yr, data = mydata, family=multinomial(refLevel = 1))
  fitM3 <- vglm(M3.bmi ~ ATemp + bl_age + married.factor + edu.factor + srvy_yr, data = mydata, family=multinomial(refLevel = 2))
  fitM4 <- vglm(M4.phy ~ ATemp + bl_age + married.factor + edu.factor + srvy_yr, data = mydata, family=multinomial(refLevel = 3))
  
  cat("Step 1 complete (fit model for each mediator)", "\n")  # progress indicator
  
  
  ### Step 2: Construct copies of ID and exposure *************************************************************************************************
  # ***********************************************************************************************************************************************
  
  #Create ID Variable
  mydata$ID <- 1:nrow(mydata) # construct id variable
  
  # Create counterfactual version of exposure (race); repeated 4 times because there are 4 mediators
  levelsOfRACE <- unique(mydata$A.race)
  myData1 <- mydata
  myData2 <- mydata
  myData3 <- mydata
  myData1$race_M1.alc <- levelsOfRACE[1]
  myData2$race_M1.alc <- levelsOfRACE[2]
  myData3$race_M1.alc <- levelsOfRACE[3]
  tempMyData <- rbind(myData1, myData2, myData3)
  
  myData1 <- tempMyData
  myData2 <- tempMyData
  myData3 <- tempMyData
  myData1$race_M2.smk <- levelsOfRACE[1]
  myData2$race_M2.smk <- levelsOfRACE[2]
  myData3$race_M2.smk <- levelsOfRACE[3]
  tempMyData <- rbind(myData1, myData2, myData3)
  
  myData1 <- tempMyData
  myData2 <- tempMyData
  myData3 <- tempMyData
  myData1$race_M3.bmi <- levelsOfRACE[1]
  myData2$race_M3.bmi <- levelsOfRACE[2]
  myData3$race_M3.bmi <- levelsOfRACE[3]
  tempMyData <- rbind(myData1, myData2, myData3)
  
  myData1 <- tempMyData
  myData2 <- tempMyData
  myData3 <- tempMyData
  myData1$race_M4.phy <- levelsOfRACE[1]
  myData2$race_M4.phy <- levelsOfRACE[2]
  myData3$race_M4.phy <- levelsOfRACE[3]
  newMyData <- rbind(myData1, myData2, myData3)
  
  
  cat("Step 2 complete (duplicate data)", "\n")  # progress indicator
  
  
  ### Step 3: Construct weights  *********************************************************************************************************************
  # **************************************************************************************************************************************************
  
  # M1: alcohol
  newMyData$ATemp <- newMyData$A.race
  tempDir1 <- as.matrix(predict(fitM1,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M1.alc)]
  
  newMyData$ATemp <- newMyData$race_M1.alc
  tempIndir1 <- as.matrix(predict(fitM1,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M1.alc)]
  
  newMyData$weight1 <- tempIndir1/tempDir1
  
  cat("Step 3.1 complete (Construct weights for alcohol)", "\n")  # progress indicator
  
  
  #M2: Smoking
  newMyData$ATemp <- newMyData$A.race
  tempDir2 <- as.matrix(predict(fitM2,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M2.smk)]
  
  newMyData$ATemp <- newMyData$race_M2.smk
  tempIndir2 <- as.matrix(predict(fitM2,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M2.smk)]
  
  newMyData$weight2 <- tempIndir2/tempDir2
  
  cat("Step 3.2 complete (Construct weights for smoking)", "\n")  # progress indicator
  
  
  #M3: BMI
  newMyData$ATemp <- newMyData$A.race
  tempDir3 <- as.matrix(predict(fitM3,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M3.bmi)]
  
  newMyData$ATemp <- newMyData$race_M3.bmi
  tempIndir3 <- as.matrix(predict(fitM3,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M3.bmi)]
  
  newMyData$weight3 <- tempIndir3/tempDir3
  
  cat("Step 3.3 complete (Construct weights for BMI)", "\n")  # progress indicator
  
  
  #M4: Physical activity
  newMyData$ATemp <- newMyData$A.race
  tempDir4 <- as.matrix(predict(fitM4,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M4.phy)]
  
  newMyData$ATemp <- newMyData$race_M4.phy
  tempIndir4 <- as.matrix(predict(fitM4,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M4.phy)]
  
  newMyData$weight4 <- tempIndir4/tempDir4
  
  cat("Step 3.4 complete (Construct weights for physical activity)", "\n")  # progress indicator
  
  
  # Final weight
  newMyData$weightM <- newMyData$weight1 * newMyData$weight2 * newMyData$weight3 * newMyData$weight4
  
  cat("Step 3 complete (final data complete)", "\n")  # progress indicator
  
  newMyData <- newMyData %>%
    dplyr::select(ID, bl_age, end_age, allcause_death, A.race, race_M1.alc, race_M2.smk, race_M3.bmi, race_M4.phy, married.factor, edu.factor, srvy_yr, weightM) %>%
    filter(complete.cases(ID, bl_age, end_age, allcause_death, A.race, race_M1.alc, race_M2.smk, race_M3.bmi, race_M4.phy, married.factor, edu.factor, srvy_yr, weightM))
  
  return(newMyData)
}


# Extract Data from Causal Mediation ----------------------------------------------------------------

# Direct, indirect and mediated interactive effects and standard errors are derived directly from the summary() command below
# Total effect is obtained by the sum of the three separate effects
# Confidence intervals for total effects and mediated proportions are computed using the code below:

# Function to get the (Not-Robust) total effect and proportion mediated
getTE_NotRobust <- function(CMed_model, v){
  TE <- sum(CMed_model$coefficients[v])
  mu <- CMed_model$coefficients[v]
  Omega <- CMed_model$var[v,v]    # To obtain non-robust estimates
  temp <- mvrnorm(n=10^4, mu=mu, Sigma=Omega)
  temp_TE <- apply(temp,1,sum)
  med_prop <- c(mu/TE,1)
  med_prop_CI <- rbind(t(apply(temp/temp_TE, 2, quantile, c(0.025, 0.975))), c(1,1))
  output <- cbind(c(mu,TE), c(apply(temp,2,sd),sd(temp_TE)), med_prop, med_prop_CI)
  colnames(output) <- c("Est.", "SE", "med_prop", "lowerCI", "UpperCI")
  rownames(output)[length(rownames(output))] <- "TE"
  return(output)}



# Function to get the (Not-Robust) total combined indirect effect  
getIE_NotRobust <- function(CMed_model, v){
  IE <- sum(CMed_model$coefficients[v])
  mu <- CMed_model$coefficients[v]
  Omega <- CMed_model$var[v,v] # To obtain non-robust estimates
  require(MASS)
  temp <- mvrnorm(n=10^4, mu=mu, Sigma=Omega)
  temp_IE <- apply(temp,1,sum)
  med_prop <- c(mu/IE,1)
  med_prop_CI <- rbind(t(apply(temp/temp_IE, 2, quantile, c(0.025, 0.975))), c(1,1))
  output <- cbind(c(mu,IE), c(apply(temp,2,sd),sd(temp_IE)), med_prop, med_prop_CI)
  colnames(output) <- c("Est.", "SE", "med_prop", "lowerCI", "UpperCI")
  rownames(output)[length(rownames(output))] <- "IE"
  return(output)}



# Function to get the (Not Robust)  proportion mediated of the combined indirect effect
getTE_IE_NotRobust <- function(CMed_model, v, z){
  #total effect
  TE <- sum(CMed_model$coefficients[v])
  mu <- CMed_model$coefficients[v]
  Omega <- CMed_model$var[v,v]     # To obtain non-robust estimates
  require(MASS)
  temp <- mvrnorm(n=10^4, mu=mu, Sigma=Omega)
  temp_TE <- apply(temp,1,sum)
  IE <- sum(CMed_model$coefficients[z])
  muIE <- CMed_model$coefficients[z]
  OmegaIE <- CMed_model$var[z,z]    # To obtain non-robust estimates
  require(MASS)
  tempIE <- mvrnorm(n=10^4, mu=muIE, Sigma=OmegaIE)
  temp_IE <- apply(tempIE,1,sum)
  med_prop <- c(IE/TE,1)
  med_prop_CI <- (temp_IE/temp_TE)
  output <- cbind(IE, med_prop, quantile)
  quantile <- quantile(med_prop_CI, c(0.025, 0.975))
  output <- cbind(IE, med_prop, quantile)
  return(output)}

      
      
# Format Data from Causal Mediation ----------------------------------------------------------------

format_CMed <- function (model, coef_list) {
  
  group <- enexpr(coef_list)
  
  # All_coef model coefficients   
  All_coef <- broom::tidy(model, exponentiate = T, conf.int = T) %>%
    as.data.frame() %>% 
    rename(variable = term) %>% 
    mutate(HR = round(estimate, 2),
           lower = round(conf.low, 2),
           upper = round(conf.high, 2),
      HR_CI = paste0(HR, " (", lower, ", ", upper, ")")) %>% 
    dplyr::select (variable, HR_CI)
  
  coef <- slice(All_coef, coef_list)
  
  
  # Function to get the total effect and proportion mediated
  TE_prop <- getTE_NotRobust(model, coef_list) %>% 
    as.data.frame() %>% rownames_to_column(var = "variable") 
  
  TE <- filter(TE_prop, variable =="TE") %>% 
    mutate (lower = round(exp(`Est.` - (1.96 * SE)), 2), 
            upper = round(exp(`Est.` + (1.96 * SE)), 2), 
            HR = round(exp(`Est.`), 2),
            HR_CI = paste0(HR, " (", lower, ", ", upper, ")")) %>%
    dplyr::select(variable, HR_CI)
  
  prop <- filter(TE_prop, variable !="TE") %>%
    mutate(prop = round(med_prop * 100,0),
           lower = round(lowerCI * 100,0),
           upper = round(UpperCI * 100,0),
           prop_CI = paste0(prop, " (", lower, ", ", upper, ")")) %>% 
    dplyr::select(variable, prop_CI) 
  
  
  # Function to get the total combined indirect effect  (coef_list excluding 1st item)
  IE <- getIE_NotRobust(model, coef_list[-1]) %>% 
    as.data.frame() %>% rownames_to_column(var = "variable") %>% 
    filter (variable == "IE") %>% 
    mutate (lower = round(exp(`Est.` - (1.96 * SE)), 2), 
            upper = round(exp(`Est.` + (1.96 * SE)), 2), 
            HR = round(exp(`Est.`), 2),
            HR_CI = paste0(HR, " (", lower, ", ", upper, ")")) %>%
    dplyr::select(variable, HR_CI) 
  
  
  # Function to get the proportion mediated of the combined indirect effect
  IE_prop <- getTE_IE_NotRobust(model, coef_list, coef_list[-1]) %>% 
    as.data.frame() %>% rownames_to_column(var = "variable") %>%
    pivot_wider(names_from="variable", values_from=c("IE", "med_prop", "quantile")) %>%
    mutate (IE_prop = round(`med_prop_2.5%` * 100, 0),
      lower = round(`quantile_2.5%` *100, 0), 
      upper = round(`quantile_97.5%` *100, 0),
      prop_CI = paste0(IE_prop, " (", lower, ", ", upper, ")"),
      variable = "IE") %>% 
    dplyr::select(variable, prop_CI) 
  
  
  one <- full_join(coef, prop, by="variable") %>%
    mutate(label = c(paste0("02 Direct effect of ", group),
                            "04 Alcohol use: mediated",
                            "05 Smoking: mediated",
                            "06 BMI: mediated",
                            "07 Physical activity: mediated")) %>% 
    dplyr::select(label, HR_CI, prop_CI) 
  
  two <- TE %>%
    mutate (label = ifelse(variable == "TE", paste0("01 Total effect of ", group), NA),
      prop_CI = "100") %>% 
    dplyr::select(label, HR_CI, prop_CI)
  
  three <- full_join(IE, IE_prop, by="variable") %>%
    mutate (label = ifelse(variable == "IE", paste0("03 Indirect effect of ", group), NA)) %>% 
    dplyr::select(label, HR_CI, prop_CI) 
  
  
  final <- rbind(one, two, three) %>%
    arrange(label) 
  
  return(final)
}
