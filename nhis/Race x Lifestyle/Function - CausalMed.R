
# Race x Lifestyle Differential Vulnerability & Exposure Project
## Functions for Causal Mediation 


# Function for data preparation, to repeat analyses for men and women
CMed_prep <-function(data) {
  
  # Data Preparation -----------------------------------------------------------------------------------------------------------------------------
  
  ### Step 0: Select data to use *****************************************************************************************************************
  # **********************************************************************************************************************************************
  mydata <- data %>%
    mutate(
      A.race = factor(ethnicity, levels=c(1,2,3,4), labels = c("White", "Black", "Hispanic", "Other")),
      M1.alc = alcohol5v2,
      M2.smk = smoking4,
      M3.bmi = bmi_cat,
      M4.phy = phy_act3) %>%
    dplyr::select(A.race, M1.alc, M2.smk, M3.bmi, M4.phy, allcause_death, bl_age, end_age, married, edu, srvy_yr)
  
  cat("Step 0 complete (Select data)", "\n") # progress indicator
  
  # NOTE: For technical reasons, the mediators should be coded as integers starting with 1
  
  
  ### Step 1: Fit a model for each mediator, , conditioning on exposure and all confounders *******************************************************
  # ***********************************************************************************************************************************************
  
  # Fit model for each mediator, conditioning on exposure (race) and all confounders
  
  mydata$ATemp <- mydata$A.race # first, create and use a copy of the exposure variable (for technical reasons related to R)
  fitM1 <- vglm(M1.alc ~ ATemp + bl_age + married + factor(edu) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 3))
  fitM2 <- vglm(M2.smk ~ ATemp + bl_age + married + factor(edu) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 1))
  fitM3 <- vglm(M3.bmi ~ ATemp + bl_age + married + factor(edu) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 2))
  fitM4 <- vglm(M4.phy ~ ATemp + bl_age + married + factor(edu) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 3))
  
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
  myData4 <- mydata
  myData1$race_M1.alc <- levelsOfRACE[1]
  myData2$race_M1.alc <- levelsOfRACE[2]
  myData3$race_M1.alc <- levelsOfRACE[3]
  myData4$race_M1.alc <- levelsOfRACE[4]
  tempMyData <- rbind(myData1, myData2, myData3, myData4)
  
  myData1 <- tempMyData
  myData2 <- tempMyData
  myData3 <- tempMyData
  myData4 <- tempMyData
  myData1$race_M2.smk <- levelsOfRACE[1]
  myData2$race_M2.smk <- levelsOfRACE[2]
  myData3$race_M2.smk <- levelsOfRACE[3]
  myData4$race_M2.smk <- levelsOfRACE[4]
  tempMyData <- rbind(myData1, myData2, myData3, myData4)
  
  myData1 <- tempMyData
  myData2 <- tempMyData
  myData3 <- tempMyData
  myData4 <- tempMyData
  myData1$race_M3.bmi <- levelsOfRACE[1]
  myData2$race_M3.bmi <- levelsOfRACE[2]
  myData3$race_M3.bmi <- levelsOfRACE[3]
  myData4$race_M3.bmi <- levelsOfRACE[4]
  tempMyData <- rbind(myData1, myData2, myData3, myData4)
  
  myData1 <- tempMyData
  myData2 <- tempMyData
  myData3 <- tempMyData
  myData4 <- tempMyData
  myData1$race_M4.phy <- levelsOfRACE[1]
  myData2$race_M4.phy <- levelsOfRACE[2]
  myData3$race_M4.phy <- levelsOfRACE[3]
  myData4$race_M4.phy <- levelsOfRACE[4]
  newMyData <- rbind(myData1, myData2, myData3, myData4)
  
  
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
    dplyr::select(ID, bl_age, end_age, allcause_death, A.race, race_M1.alc, race_M2.smk, race_M3.bmi, race_M4.phy, married, edu, srvy_yr, weightM)
  
  
  return(newMyData)
  
}





# Direct, indirect and mediated interactive effects and standard errors are derived directly from the summary() command below
# Total effect is obtained by the sum of the three separate effects
# Confidence intervals for total effects and mediated proportions are computed using the code below:

# Function to get the (Not-Robust) total effect and proportion mediated
getTE_NotRobust <- function(CMed_model, v){
  TE <- sum(CMed_model$gamma[v])
  mu <- CMed_model$gamma[v]
  Omega <- CMed_model$var.gamma[v,v]    # To obtain non-robust estimates
  temp <- mvrnorm(n=10^4, mu=mu, Sigma=Omega)
  temp_TE <- apply(temp,1,sum)
  med_prop <- c(mu/TE,1)
  med_prop_CI <- rbind(t(apply(temp/temp_TE, 2, quantile, c(0.025, 0.975))), c(1,1))
  output <- cbind(c(mu,TE), c(apply(temp,2,sd),sd(temp_TE)), med_prop, med_prop_CI)
  colnames(output) <- c("Est.", "SE", "med_prop", "lowerCI", "UpperCI")
  rownames(output) <- c(rownames(CMed_model$gamma)[v],"TE")
  return(output)}



        # # Function to get the (Robust) total effect and proportion mediated
        getTE_Robust <- function(CMed_model, v){
          TE <- sum(CMed_model$gamma[v])
          mu <- CMed_model$gamma[v]
          Omega <- CMed_model$robvar.gamma[v,v] # To obtain robust estimates
          temp <- mvrnorm(n=10^4, mu=mu, Sigma=Omega)
          temp_TE <- apply(temp,1,sum)
          med_prop <- c(mu/TE,1)
          med_prop_CI <- rbind(t(apply(temp/temp_TE, 2, quantile, c(0.025, 0.975))), c(1,1))
          output <- cbind(c(mu,TE), c(apply(temp,2,sd),sd(temp_TE)), med_prop, med_prop_CI)
          colnames(output) <- c("Est.", "SE", "med_prop", "lowerCI", "UpperCI")
          rownames(output) <- c(rownames(CMed_model$gamma)[v],"TE")
          return(output)}





# Function to get the (Not-Robust) total combined indirect effect  
getIE_NotRobust <- function(CMed_model, v){
  IE <- sum(CMed_model$gamma[v])
  mu <- CMed_model$gamma[v]
  Omega <- CMed_model$var.gamma[v,v] # To obtain non-robust estimates
  require(MASS)
  temp <- mvrnorm(n=10^4, mu=mu, Sigma=Omega)
  temp_IE <- apply(temp,1,sum)
  med_prop <- c(mu/IE,1)
  med_prop_CI <- rbind(t(apply(temp/temp_IE, 2, quantile, c(0.025, 0.975))), c(1,1))
  output <- cbind(c(mu,IE), c(apply(temp,2,sd),sd(temp_IE)), med_prop, med_prop_CI)
  colnames(output) <- c("Est.", "SE", "med_prop", "lowerCI", "UpperCI")
  rownames(output) <- c(rownames(CMed_model$gamma)[v],"IE")
  return(output)}



        # Function to get the (Robust) total combined indirect effect  
        getIE_Robust <- function(CMed_model, v){
          IE <- sum(CMed_model$gamma[v])
          mu <- CMed_model$gamma[v]
          Omega <- CMed_model$robvar.gamma[v,v] # To obtain robust estimates
          require(MASS)
          temp <- mvrnorm(n=10^4, mu=mu, Sigma=Omega)
          temp_IE <- apply(temp,1,sum)
          med_prop <- c(mu/IE,1)
          med_prop_CI <- rbind(t(apply(temp/temp_IE, 2, quantile, c(0.025, 0.975))), c(1,1))
          output <- cbind(c(mu,IE), c(apply(temp,2,sd),sd(temp_IE)), med_prop, med_prop_CI)
          colnames(output) <- c("Est.", "SE", "med_prop", "lowerCI", "UpperCI")
          rownames(output) <- c(rownames(CMed_model$gamma)[v],"IE")
          return(output)}




# Function to get the (Not Robust)  proportion mediated of the combined indirect effect
getTE_IE_NotRobust <- function(CMed_model, v, z){
  #total effect
  TE <- sum(CMed_model$gamma[v])
  mu <- CMed_model$gamma[v]
  # Omega <- CMed_model$robvar.gamma[v,v]  # To obtain robust estimates
  Omega <- CMed_model$var.gamma[v,v]     # To obtain non-robust estimates
  require(MASS)
  temp <- mvrnorm(n=10^4, mu=mu, Sigma=Omega)
  temp_TE <- apply(temp,1,sum)
  IE <- sum(CMed_model$gamma[z])
  muIE <- CMed_model$gamma[z]
  #OmegaIE <- CMed_model$robvar.gamma[z,z] # To obtain robust estimates
  OmegaIE <- CMed_model$var.gamma[z,z]    # To obtain non-robust estimates
  require(MASS)
  tempIE <- mvrnorm(n=10^4, mu=muIE, Sigma=OmegaIE)
  temp_IE <- apply(tempIE,1,sum)
  med_prop <- c(IE/TE,1)
  med_prop_CI <- (temp_IE/temp_TE)
  output <- cbind(IE, med_prop, quantile)
  quantile <- quantile(med_prop_CI, c(0.025, 0.975))
  output <- cbind(IE, med_prop, quantile)
  return(output)}


      

      # Function to get the (Not Robust)  proportion mediated of the combined indirect effect
      getTE_IE_Robust <- function(CMed_model, v, z){
        #total effect
        TE <- sum(CMed_model$gamma[v])
        mu <- CMed_model$gamma[v]
        Omega <- CMed_model$robvar.gamma[v,v]  # To obtain robust estimates
        require(MASS)
        temp <- mvrnorm(n=10^4, mu=mu, Sigma=Omega)
        temp_TE <- apply(temp,1,sum)
        IE <- sum(CMed_model$gamma[z])
        muIE <- CMed_model$gamma[z]
        OmegaIE <- CMed_model$robvar.gamma[z,z] # To obtain robust estimates
        require(MASS)
        tempIE <- mvrnorm(n=10^4, mu=muIE, Sigma=OmegaIE)
        temp_IE <- apply(tempIE,1,sum)
        med_prop <- c(IE/TE,1)
        med_prop_CI <- (temp_IE/temp_TE)
        output <- cbind(IE, med_prop, quantile)
        quantile <- quantile(med_prop_CI, c(0.025, 0.975))
        output <- cbind(IE, med_prop, quantile)
        return(output)}
